# =============================================================================
# Reproducibility analysis
# Field-measured wood density vs GWDD v.2.2 in restored Atlantic Forest stands
# =============================================================================

required <- c("readr", "dplyr", "tidyr", "stringr", "purrr", "ggplot2", "ggrepel")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required packages before running this script: ", paste(missing, collapse = ", "))
}

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(ggrepel)

options(stringsAsFactors = FALSE)

# ---- 0. Paths ----------------------------------------------------------------

get_script_dir <- function() {
  ofile <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
                    error = function(e) NA_character_)
  if (!is.na(ofile)) return(dirname(ofile))
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p, winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

script_dir <- get_script_dir()
data_dir <- file.path(script_dir, "data")
out_dir <- file.path(script_dir, "outputs")
fig_dir <- file.path(script_dir, "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

input_file <- function(name) {
  p <- file.path(data_dir, name)
  if (!file.exists(p)) stop("Required input file not found: ", p)
  p
}

# ---- 1. Inputs ---------------------------------------------------------------

field_ind <- read_csv(input_file("field_WD_measurements.csv"), show_col_types = FALSE)
trees0 <- read_csv(input_file("sampled_trees.csv"), show_col_types = FALSE)
inv0 <- read_csv(input_file("inventory_2020_2021.csv"), show_col_types = FALSE)
meta <- read_csv(input_file("species_metadata.csv"), show_col_types = FALSE)
ref <- read_csv(input_file("GWDDv2_reference_values_study_species.csv"), show_col_types = FALSE)
raw <- read_csv(input_file("GWDDv2_filtered_records_study_species.csv"), show_col_types = FALSE)

# The bundled GWDD raw-record file contains the retained species-level records used for
# Welch comparisons. The filtering criteria were: numerical WD; species-level taxonomic
# rank; exclusion of bark, branch/root samples, plantation records, and records explicitly
# associated with fertilizer or treatment experiments.

# ---- 2. Field WD summaries ---------------------------------------------------

field <- field_ind %>%
  filter(is.finite(wood_density_g_cm3)) %>%
  group_by(species) %>%
  summarise(
    n_field = n(),
    field_mean = mean(wood_density_g_cm3),
    field_sd = sd(wood_density_g_cm3),
    .groups = "drop"
  ) %>%
  left_join(meta, by = "species") %>%
  left_join(ref, by = "species")

if (any(is.na(field$gwdd_trunk_WD_g_cm3))) {
  stop("Missing GWDD reference WD for: ", paste(field$species[is.na(field$gwdd_trunk_WD_g_cm3)], collapse = ", "))
}

# ---- 3. Species-level reference comparisons ---------------------------------

raw_summary <- raw %>%
  filter(is.finite(wsg)) %>%
  group_by(species) %>%
  summarise(
    n_gwdd_filtered = n(),
    gwdd_filtered_mean = mean(wsg),
    gwdd_filtered_sd = sd(wsg),
    .groups = "drop"
  )

tests <- map_dfr(field$species, function(sp_name) {
  x <- field_ind$wood_density_g_cm3[field_ind$species == sp_name]
  y <- raw$wsg[raw$species == sp_name]

  if (length(x) < 2L || length(y) < 3L) {
    return(tibble(
      species = sp_name,
      n_field_test = length(x),
      n_gwdd_test = length(y),
      t_welch = NA_real_,
      df_welch = NA_real_,
      p_raw = NA_real_
    ))
  }

  z <- t.test(x, y, var.equal = FALSE)
  tibble(
    species = sp_name,
    n_field_test = length(x),
    n_gwdd_test = length(y),
    t_welch = unname(z$statistic),
    df_welch = unname(z$parameter),
    p_raw = z$p.value
  )
})

tests$p_BH <- NA_real_
idx <- which(!is.na(tests$p_raw))
tests$p_BH[idx] <- p.adjust(tests$p_raw[idx], method = "BH")

sp <- field %>%
  left_join(raw_summary, by = "species") %>%
  left_join(tests, by = "species") %>%
  mutate(
    n_gwdd_filtered = if_else(reference_level == "species",
                              coalesce(as.integer(n_gwdd_filtered), 0L),
                              NA_integer_),
    wd_difference = gwdd_trunk_WD_g_cm3 - field_mean,
    wd_difference_pct = 100 * wd_difference / field_mean,
    significant_BH = !is.na(p_BH) & p_BH < 0.05
  )

# Overall inference uses only the 30 taxa with species-level GWDD reference estimates.
sp_global <- sp %>% filter(reference_level == "species")

global_t <- t.test(sp_global$gwdd_trunk_WD_g_cm3, sp_global$field_mean, paired = TRUE)
global_w <- wilcox.test(sp_global$gwdd_trunk_WD_g_cm3, sp_global$field_mean,
                        paired = TRUE, exact = FALSE)

global_summary <- tibble(
  n_species = nrow(sp_global),
  mean_field_WD_g_cm3 = mean(sp_global$field_mean),
  mean_GWDD_trunk_WD_g_cm3 = mean(sp_global$gwdd_trunk_WD_g_cm3),
  mean_difference_GWDD_minus_field_g_cm3 =
    mean(sp_global$gwdd_trunk_WD_g_cm3 - sp_global$field_mean),
  paired_t = unname(global_t$statistic),
  df = unname(global_t$parameter),
  p_t = global_t$p.value,
  CI95_low = global_t$conf.int[1],
  CI95_high = global_t$conf.int[2],
  paired_Wilcoxon_V = unname(global_w$statistic),
  p_Wilcoxon = global_w$p.value
)

# ---- 4. AGB equations and species-level sensitivity -------------------------

chave <- function(D, H, wd) 0.0673 * (wd * D^2 * H)^0.976
nogueira <- function(D, H, wd) exp(-1.305 + 1.055 * log(D^2) + 0.34 * log(H) + 1.077 * log(wd))

sp <- sp %>%
  mutate(
    Chave_AGB_difference_pct = 100 * ((gwdd_trunk_WD_g_cm3 / field_mean)^0.976 - 1),
    Nogueira_AGB_difference_pct = 100 * ((gwdd_trunk_WD_g_cm3 / field_mean)^1.077 - 1)
  )

# ---- 5. Recalculation for the 172 sampled trees -----------------------------

wd_lookup <- sp %>%
  select(species, reference_level, field_mean, reference_wd = gwdd_trunk_WD_g_cm3)

trees <- trees0 %>%
  left_join(wd_lookup, by = "species") %>%
  mutate(
    Chave_field_kg = chave(DBH_cm, height_m, field_mean),
    Chave_GWDD_kg = chave(DBH_cm, height_m, reference_wd),
    Chave_difference_kg = Chave_GWDD_kg - Chave_field_kg,
    Chave_difference_pct = 100 * Chave_difference_kg / Chave_field_kg,
    Nogueira_field_kg = nogueira(DBH_cm, height_m, field_mean),
    Nogueira_GWDD_kg = nogueira(DBH_cm, height_m, reference_wd),
    Nogueira_difference_kg = Nogueira_GWDD_kg - Nogueira_field_kg,
    Nogueira_difference_pct = 100 * Nogueira_difference_kg / Nogueira_field_kg
  )

sampled_tree_summary <- bind_rows(
  tibble(
    equation = "Chave et al. (2014)",
    n_trees = nrow(trees),
    mean_AGB_field_kg = mean(trees$Chave_field_kg),
    mean_AGB_reference_kg = mean(trees$Chave_GWDD_kg),
    mean_difference_kg = mean(trees$Chave_difference_kg),
    total_AGB_field_kg = sum(trees$Chave_field_kg),
    total_AGB_reference_kg = sum(trees$Chave_GWDD_kg),
    total_difference_kg = sum(trees$Chave_difference_kg),
    aggregate_difference_pct = 100 * sum(trees$Chave_difference_kg) / sum(trees$Chave_field_kg),
    mean_individual_difference_pct = mean(trees$Chave_difference_pct)
  ),
  tibble(
    equation = "Nogueira Junior et al. (2014)",
    n_trees = nrow(trees),
    mean_AGB_field_kg = mean(trees$Nogueira_field_kg),
    mean_AGB_reference_kg = mean(trees$Nogueira_GWDD_kg),
    mean_difference_kg = mean(trees$Nogueira_difference_kg),
    total_AGB_field_kg = sum(trees$Nogueira_field_kg),
    total_AGB_reference_kg = sum(trees$Nogueira_GWDD_kg),
    total_difference_kg = sum(trees$Nogueira_difference_kg),
    aggregate_difference_pct = 100 * sum(trees$Nogueira_difference_kg) / sum(trees$Nogueira_field_kg),
    mean_individual_difference_pct = mean(trees$Nogueira_difference_pct)
  )
)

# ---- 6. Inventory-based scaling ---------------------------------------------

canon <- c(
  "Dipteryx alata Vogel" = "Dipteryx alata",
  "Machaerium brasiliensis" = "Machaerium brasiliense",
  "Lonchocaspus cultratus" = "Lonchocarpus cultratus",
  "Mimosa caesalpininiifolia" = "Mimosa caesalpiniifolia",
  "Myroxylum peruiferum" = "Myroxylon peruiferum"
)

binomial <- function(x) {
  x <- str_squish(as.character(x))
  b <- vapply(str_split(x, "\\s+"), function(z) paste(head(z, 2), collapse = " "), character(1))
  direct <- unname(canon[x])
  mapped <- unname(canon[b])
  ifelse(!is.na(direct), direct, ifelse(!is.na(mapped), mapped, b))
}

dbh_cols <- paste0("DBH", 1:11, "_cm")

inv_all <- inv0 %>%
  mutate(
    species = binomial(scientific_name),
    across(all_of(dbh_cols), as.numeric),
    height_m = as.numeric(height_m)
  ) %>%
  rowwise() %>%
  mutate(
    D_eq_cm = sqrt(sum(c_across(all_of(dbh_cols))^2, na.rm = TRUE)),
    basal_area_m2 = if_else(D_eq_cm > 0, pi * (D_eq_cm / 200)^2, NA_real_)
  ) %>%
  ungroup()

# Stand scaling is deliberately restricted to species-level GWDD matches.
# Ficus sp. inventory records are not assigned to Ficus guaranitica.
wd_inventory_lookup <- sp %>%
  filter(reference_level == "species") %>%
  select(species, field_mean, reference_wd = gwdd_trunk_WD_g_cm3)

inv <- inv_all %>%
  inner_join(wd_inventory_lookup, by = "species") %>%
  filter(is.finite(height_m), height_m > 0, is.finite(D_eq_cm), D_eq_cm > 0) %>%
  mutate(
    Chave_field_kg = chave(D_eq_cm, height_m, field_mean),
    Chave_GWDD_kg = chave(D_eq_cm, height_m, reference_wd),
    Nogueira_field_kg = nogueira(D_eq_cm, height_m, field_mean),
    Nogueira_GWDD_kg = nogueira(D_eq_cm, height_m, reference_wd)
  )

area_ha <- 30 * 15 * 15 / 10000  # 30 subplots × 225 m2 = 0.675 ha per site

stand <- bind_rows(
  inv %>%
    group_by(site) %>%
    summarise(n_inventory_individuals = n(),
              AGB_field_Mg_ha = sum(Chave_field_kg) / 1000 / area_ha,
              AGB_GWDD_Mg_ha = sum(Chave_GWDD_kg) / 1000 / area_ha,
              .groups = "drop") %>%
    mutate(equation = "Chave et al. (2014)"),
  inv %>%
    group_by(site) %>%
    summarise(n_inventory_individuals = n(),
              AGB_field_Mg_ha = sum(Nogueira_field_kg) / 1000 / area_ha,
              AGB_GWDD_Mg_ha = sum(Nogueira_GWDD_kg) / 1000 / area_ha,
              .groups = "drop") %>%
    mutate(equation = "Nogueira Junior et al. (2014)")
) %>%
  mutate(
    site = recode(as.character(site), `1` = "Lageado", `2` = "Edgárdia"),
    difference_Mg_ha = AGB_GWDD_Mg_ha - AGB_field_Mg_ha,
    difference_pct = 100 * difference_Mg_ha / AGB_field_Mg_ha,
    illustrative_carbon_difference_Mg_C_ha_at_0.47 = difference_Mg_ha * 0.47
  ) %>%
  select(site, equation, n_inventory_individuals, AGB_field_Mg_ha,
         AGB_GWDD_Mg_ha, difference_Mg_ha, difference_pct,
         illustrative_carbon_difference_Mg_C_ha_at_0.47) %>%
  arrange(site, equation)

inventory_scaling_summary <- tibble(
  total_inventory_individuals = nrow(inv0),
  inventory_individuals_in_scaling = nrow(inv),
  species_in_scaling = n_distinct(inv$species),
  inventory_individuals_coverage_pct = 100 * nrow(inv) / nrow(inv0),
  total_basal_area_coverage_pct = 100 * sum(inv$basal_area_m2, na.rm = TRUE) /
    sum(inv_all$basal_area_m2, na.rm = TRUE),
  inventoried_area_per_site_ha = area_ha
)

inventory_taxonomic_handling <- tibble(
  study_species = c("Myroxylon peruiferum", "Ficus guaranitica"),
  inventory_label = c("Myroxylum peruiferum L. f.", "Ficus sp."),
  handling = c(
    "Harmonized to Myroxylon peruiferum and included at species level",
    "Not assigned to Ficus guaranitica; excluded from inventory scaling"
  ),
  GWDD_reference_level = c(
    "species",
    "genus fallback used only for descriptive and sampled-tree scenarios"
  )
)

# ---- 7. Output tables --------------------------------------------------------

table_s1 <- sp %>%
  transmute(
    species,
    WD_measurement_method,
    n_field,
    field_WD_mean_g_cm3 = field_mean,
    field_WD_SD_g_cm3 = field_sd,
    GWDD_reference_level = reference_level,
    GWDD_match = gwdd_match,
    GWDD_trunk_WD_g_cm3 = gwdd_trunk_WD_g_cm3,
    GWDD_n_all = gwdd_records,
    GWDD_n_filtered = n_gwdd_filtered,
    Welch_t = t_welch,
    p_raw,
    p_BH,
    BH_significant = significant_BH
  )

table_s2 <- sp %>%
  transmute(
    species,
    field_WD_g_cm3 = field_mean,
    GWDD_reference_level = reference_level,
    GWDD_trunk_WD_g_cm3 = gwdd_trunk_WD_g_cm3,
    WD_difference_pct = wd_difference_pct,
    Chave_AGB_difference_pct,
    Nogueira_AGB_difference_pct,
    BH_significant_WD_difference = significant_BH
  )

write_csv(table_s1, file.path(out_dir, "Table_S1_species_WD_GWDDv2.csv"))
write_csv(table_s2, file.path(out_dir, "Table_S2_species_AGB_sensitivity.csv"))
write_csv(stand, file.path(out_dir, "Table_2_inventory_AGB_species_in_study.csv"))
write_csv(global_summary, file.path(out_dir, "global_paired_WD_test.csv"))
write_csv(sampled_tree_summary, file.path(out_dir, "sampled_tree_AGB_summary.csv"))
write_csv(trees, file.path(out_dir, "tree_sample_AGB_recalculated.csv"))
write_csv(inventory_scaling_summary, file.path(out_dir, "inventory_scaling_summary.csv"))
write_csv(inventory_taxonomic_handling, file.path(out_dir, "inventory_taxonomic_handling.csv"))

# ---- 8. Figures --------------------------------------------------------------

palette_ref <- c(species = "#1f78b4", genus = "#d95f02")
palette_eq <- c("Chave et al. (2014)" = "#1f78b4",
                "Nogueira Junior et al. (2014)" = "#d95f02")

# Figure 1
p1 <- ggplot(sp, aes(x = field_mean, y = gwdd_trunk_WD_g_cm3,
                     shape = reference_level, color = reference_level)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.6, color = "grey40") +
  geom_point(size = 3.0, stroke = 0.4) +
  geom_text_repel(
    data = filter(sp, significant_BH),
    aes(label = paste0("bolditalic('", species, "')")),
    parse = TRUE, color = "black", min.segment.length = 0,
    box.padding = 0.35, max.overlaps = Inf
  ) +
  annotate("text", x = -Inf, y = Inf,
           label = paste0("31 species with field-measured WD\npaired comparison: n = ", nrow(sp_global)),
           hjust = -0.08, vjust = 1.15, size = 3.8) +
  scale_shape_manual(values = c(species = 16, genus = 17),
                     labels = c(species = "Species-level GWDD v.2",
                                genus = "Genus-level Ficus reference"), name = NULL) +
  scale_color_manual(values = palette_ref,
                     labels = c(species = "Species-level GWDD v.2",
                                genus = "Genus-level Ficus reference"), name = NULL) +
  guides(shape = guide_legend(order = 1), color = "none") +
  labs(
    x = expression(paste("Field-measured WD (g ", cm^{-3}, ")")),
    y = expression(paste("GWDD v.2 trunk WD (g ", cm^{-3}, ")"))
  ) +
  coord_equal() +
  theme_classic(base_size = 11)

ggsave(file.path(fig_dir, "Figure_1_WD_field_vs_GWDDv2.png"), p1,
       width = 7.2, height = 6.4, dpi = 600, bg = "white")

# Figure 2: BH-significant WD differences are highlighted in bold italic + asterisk.
plot2 <- sp %>%
  select(species, significant_BH, Chave_AGB_difference_pct, Nogueira_AGB_difference_pct) %>%
  pivot_longer(c(Chave_AGB_difference_pct, Nogueira_AGB_difference_pct),
               names_to = "equation", values_to = "difference_pct") %>%
  mutate(
    equation = recode(equation,
                      Chave_AGB_difference_pct = "Chave et al. (2014)",
                      Nogueira_AGB_difference_pct = "Nogueira Junior et al. (2014)")
  )

species_order <- plot2 %>%
  group_by(species) %>%
  summarise(mean_difference = mean(difference_pct), .groups = "drop") %>%
  arrange(mean_difference) %>%
  pull(species)

sig_species <- sp %>% filter(significant_BH) %>% pull(species)
plot2 <- plot2 %>% mutate(species = factor(species, levels = species_order))

species_axis_labels <- function(x) {
  parse(text = ifelse(
    x %in% sig_species,
    paste0("bolditalic('", x, "')~'*'"),
    paste0("italic('", x, "')")
  ))
}

p2 <- ggplot(plot2, aes(x = difference_pct, y = species, shape = equation, color = equation)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.6, color = "grey40") +
  geom_point(size = 2.8, position = position_dodge(width = 0.45)) +
  scale_y_discrete(labels = species_axis_labels) +
  scale_shape_manual(values = c("Chave et al. (2014)" = 16,
                                "Nogueira Junior et al. (2014)" = 17), name = NULL) +
  scale_color_manual(values = palette_eq, name = NULL) +
  labs(
    x = "Change in estimated above-ground biomass after substituting\nGWDD v.2 for field WD (%)",
    y = NULL,
    caption = "* pBH < 0.05 in the field vs GWDD v.2 WD comparison"
  ) +
  theme_classic(base_size = 11) +
  theme(legend.position = "top", plot.caption = element_text(hjust = 0))

ggsave(file.path(fig_dir, "Figure_2_species_AGB_sensitivity.png"), p2,
       width = 9.5, height = 10.8, dpi = 600, bg = "white")

# ---- 9. Reproducibility checks and session information ----------------------

stopifnot(
  nrow(field) == 31,
  sum(field$n_field) == 172,
  nrow(sp_global) == 30,
  sum(sp$significant_BH) == 7,
  nrow(trees) == 172,
  nrow(inv) == 1136,
  n_distinct(inv$species) == 30
)

capture.output(sessionInfo(), file = file.path(out_dir, "sessionInfo.txt"))

message("Analysis completed successfully.")
message("Outputs: ", normalizePath(out_dir, winslash = "/", mustWork = TRUE))
message("Figures: ", normalizePath(fig_dir, winslash = "/", mustWork = TRUE))
