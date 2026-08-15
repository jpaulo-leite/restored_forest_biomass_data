# =============================================================================
# Austral Ecology manuscript 1910502
# Revised complete analysis script
# Field-measured wood density vs GWDD v.2
# AGB sensitivity at species, tree and stand scales
#
# Compatible with R 4.4.2
#
# IMPORTANT:
# - This script is designed to be robust to the working directory.
# - It first searches for the required files next to the script and inside
#   a "source_inputs_used" subfolder.
# - If a required file cannot be found, an interactive file chooser is opened.
# - Outputs are written to "outputs_Austral_Ecology" next to the script.
# =============================================================================

# ---- 0. Packages -------------------------------------------------------------

required <- c(
  "readxl", "readr", "dplyr", "tidyr",
  "stringr", "purrr", "ggplot2", "ggrepel"
)

missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing)) {
  stop(
    "Install required packages before running the script: ",
    paste(missing, collapse = ", ")
  )
}

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(ggrepel)

options(stringsAsFactors = FALSE)

# ---- 0.1. Locate script and input files robustly ----------------------------

get_script_dir <- function() {
  # 1) When the file is executed with source()
  ofile <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )
  if (!is.na(ofile)) return(dirname(ofile))

  # 2) When running inside RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )
    if (nzchar(p)) {
      return(dirname(normalizePath(p, winslash = "/", mustWork = FALSE)))
    }
  }

  # 3) Fallback
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

script_dir <- get_script_dir()

message("Script directory: ", script_dir)
message("Initial working directory: ", normalizePath(getwd(), winslash = "/", mustWork = FALSE))

# Keep outputs beside the script, not in a temporary working directory.
out_dir <- file.path(script_dir, "outputs_Austral_Ecology")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Candidate directories in which input files may exist.
candidate_dirs <- unique(c(
  file.path(script_dir, "source_inputs_used"),
  script_dir,
  file.path(dirname(script_dir), "source_inputs_used"),
  dirname(script_dir),
  file.path(getwd(), "source_inputs_used"),
  getwd()
))

candidate_dirs <- candidate_dirs[dir.exists(candidate_dirs)]

find_input_file <- function(
  preferred_names,
  label,
  required = TRUE,
  allow_choose = interactive()
) {
  preferred_names <- unique(preferred_names)

  # Exact names in likely directories
  for (d in candidate_dirs) {
    for (nm in preferred_names) {
      p <- file.path(d, nm)
      if (file.exists(p)) {
        return(normalizePath(p, winslash = "/", mustWork = TRUE))
      }
    }
  }

  # Recursive search around the script
  roots <- unique(c(script_dir, dirname(script_dir)))
  for (root in roots) {
    if (!dir.exists(root)) next
    all_files <- tryCatch(
      list.files(
        root,
        recursive = TRUE,
        full.names = TRUE,
        include.dirs = FALSE
      ),
      error = function(e) character(0)
    )
    if (length(all_files)) {
      bn <- basename(all_files)
      hit <- which(tolower(bn) %in% tolower(preferred_names))
      if (length(hit)) {
        return(
          normalizePath(
            all_files[hit[1]],
            winslash = "/",
            mustWork = TRUE
          )
        )
      }
    }
  }

  # Interactive fallback
  if (allow_choose) {
    message("")
    message("Could not automatically find: ", label)
    message("Please select the corresponding file in the file chooser.")
    chosen <- tryCatch(file.choose(), error = function(e) "")
    if (nzchar(chosen) && file.exists(chosen)) {
      return(normalizePath(chosen, winslash = "/", mustWork = TRUE))
    }
  }

  if (required) {
    stop(
      "\nRequired input file not found: ", label,
      "\nExpected one of: ", paste(preferred_names, collapse = ", "),
      "\nScript directory: ", script_dir,
      "\nWorking directory: ", getwd(),
      "\n\nIf you are opening the script directly from inside a ZIP file, ",
      "extract the package first, or rerun this script and select the missing ",
      "input file when prompted."
    )
  }

  NA_character_
}

field_file <- find_input_file(
  preferred_names = c(
    "Dados brutos com densidade.xlsx"
  ),
  label = "field wood-density workbook"
)

inv_file <- find_input_file(
  preferred_names = c(
    "Inventario_Lageado_Edgardia_2020-21_LERF.xlsx",
    "Inventário_Lageado_Edgárdia 2020-21_LERF.xlsx"
  ),
  label = "Lageado/Edgárdia inventory workbook"
)

gw_agg <- find_input_file(
  preferred_names = c(
    "gwddagg_v2.2_species(1).csv",
    "gwddagg_v2.2_species.csv"
  ),
  label = "GWDD v.2 species aggregation"
)

gw_gen <- find_input_file(
  preferred_names = c(
    "gwddagg_v2.2_genus(1).csv",
    "gwddagg_v2.2_genus.csv"
  ),
  label = "GWDD v.2 genus aggregation"
)

# Prefer the exact filtered focal-species records bundled with the analysis.
gw_filtered <- find_input_file(
  preferred_names = c(
    "GWDDv2_filtered_records_focal_species.csv"
  ),
  label = "filtered focal-species GWDD v.2 records",
  required = FALSE,
  allow_choose = FALSE
)

# Full raw GWDD is optional if the focal filtered file is available.
gw_raw <- NA_character_
if (is.na(gw_filtered)) {
  gw_raw <- find_input_file(
    preferred_names = c(
      "gwdd_v2.2(1).csv",
      "gwdd_v2.2.csv"
    ),
    label = "full raw GWDD v.2 database"
  )
}

message("")
message("Input files resolved:")
message("  Field WD:       ", field_file)
message("  Inventory:      ", inv_file)
message("  GWDD species:   ", gw_agg)
message("  GWDD genus:     ", gw_gen)
if (!is.na(gw_filtered)) {
  message("  GWDD focal raw: ", gw_filtered)
} else {
  message("  GWDD full raw:  ", gw_raw)
}
message("  Output folder:  ", out_dir)
message("")

# ---- 1. Taxonomic harmonisation ---------------------------------------------

canon <- c(
  "Dipteryx alata Vogel" = "Dipteryx alata",
  "Dipteryx alata" = "Dipteryx alata",
  "Machaerium brasiliensis" = "Machaerium brasiliense",
  "Machaerium brasiliense" = "Machaerium brasiliense",
  "Lonchocaspus cultratus" = "Lonchocarpus cultratus",
  "Lonchocarpus cultratus" = "Lonchocarpus cultratus",
  "Mimosa caesalpininiifolia" = "Mimosa caesalpiniifolia",
  "Myroxylum peruiferum" = "Myroxylon peruiferum",
  "Myroxylon peruiferum" = "Myroxylon peruiferum"
)

binomial <- function(x) {
  x <- str_squish(as.character(x))

  direct <- unname(canon[x])

  b <- vapply(
    str_split(x, "\\s+"),
    function(z) paste(head(z, 2), collapse = " "),
    character(1)
  )

  mapped <- unname(canon[b])

  ifelse(
    !is.na(direct),
    direct,
    ifelse(!is.na(mapped), mapped, b)
  )
}

display_name <- function(x) {
  b <- vapply(
    str_split(str_squish(as.character(x)), "\\s+"),
    function(z) paste(head(z, 2), collapse = " "),
    character(1)
  )

  b[b == "Machaerium brasiliensis"] <- "Machaerium brasiliense"
  b[b == "Lonchocaspus cultratus"] <- "Lonchocarpus cultratus"
  b[b == "Mimosa caesalpininiifolia"] <- "Mimosa caesalpiniifolia"
  b[b == "Myroxylum peruiferum"] <- "Myroxylon peruiferum"

  b
}

# ---- 2. Field wood-density data ---------------------------------------------

field_sheets <- excel_sheets(field_file)

if (!"Objetivo_a_R" %in% field_sheets) {
  stop(
    "Sheet 'Objetivo_a_R' was not found in: ", field_file,
    "\nAvailable sheets: ", paste(field_sheets, collapse = ", ")
  )
}

wd <- read_excel(
  field_file,
  sheet = "Objetivo_a_R"
)

required_wd_cols <- c("Especie", "Densidade", "Origem")
missing_wd_cols <- setdiff(required_wd_cols, names(wd))

if (length(missing_wd_cols)) {
  stop(
    "Missing columns in 'Objetivo_a_R': ",
    paste(missing_wd_cols, collapse = ", ")
  )
}

wd <- wd %>%
  filter(
    !is.na(Especie),
    !is.na(Densidade)
  ) %>%
  mutate(
    Densidade = as.numeric(Densidade),
    species_key = binomial(Especie),
    species = display_name(Especie)
  ) %>%
  filter(is.finite(Densidade))

field <- wd %>%
  filter(Origem == "Amostrada") %>%
  group_by(species_key) %>%
  summarise(
    species = first(species),
    n_field = n(),
    field_mean = mean(Densidade),
    field_sd = sd(Densidade),
    .groups = "drop"
  )

if (nrow(field) == 0) {
  stop("No field-measured WD records with Origem == 'Amostrada' were found.")
}

message("Field species: ", nrow(field))
message("Field individuals: ", sum(field$n_field))

# ---- 3. GWDD v.2 species-level trunk estimates -----------------------------

agg0 <- read_csv(
  gw_agg,
  show_col_types = FALSE
)

needed_agg <- c(
  "species", "nb", "nb_sources", "nb_countries",
  "nb_sites", "wsg_est", "wsg_est_trunk", "wsg_raw"
)

miss_agg <- setdiff(needed_agg, names(agg0))
if (length(miss_agg)) {
  stop(
    "Missing columns in GWDD species aggregation: ",
    paste(miss_agg, collapse = ", ")
  )
}

agg <- agg0 %>%
  select(
    species, nb, nb_sources, nb_countries,
    nb_sites, wsg_est, wsg_est_trunk, wsg_raw
  ) %>%
  rename(species_key = species)

gen0 <- read_csv(
  gw_gen,
  show_col_types = FALSE
)

needed_gen <- c(
  "genus", "nb", "nb_sources", "nb_countries",
  "nb_sites", "wsg_est", "wsg_est_trunk", "wsg_raw"
)

miss_gen <- setdiff(needed_gen, names(gen0))
if (length(miss_gen)) {
  stop(
    "Missing columns in GWDD genus aggregation: ",
    paste(miss_gen, collapse = ", ")
  )
}

genagg <- gen0 %>%
  select(
    genus,
    nb_genus = nb,
    nb_sources_genus = nb_sources,
    nb_countries_genus = nb_countries,
    nb_sites_genus = nb_sites,
    wsg_est_genus = wsg_est,
    wsg_est_trunk_genus = wsg_est_trunk,
    wsg_raw_genus = wsg_raw
  )

field <- field %>%
  left_join(
    agg,
    by = "species_key"
  ) %>%
  mutate(
    genus = word(species_key, 1)
  ) %>%
  left_join(
    genagg,
    by = "genus"
  ) %>%
  mutate(
    reference_level = case_when(
      !is.na(wsg_est_trunk) ~ "species",
      is.na(wsg_est_trunk) & !is.na(wsg_est_trunk_genus) ~ "genus",
      TRUE ~ NA_character_
    ),
    gwdd_match = case_when(
      reference_level == "species" ~ species_key,
      reference_level == "genus" ~ genus,
      TRUE ~ NA_character_
    ),
    reference_wd = coalesce(
      wsg_est_trunk,
      wsg_est_trunk_genus
    ),
    nb = if_else(
      reference_level == "species",
      nb,
      nb_genus
    ),
    nb_sources = if_else(
      reference_level == "species",
      nb_sources,
      nb_sources_genus
    ),
    nb_countries = if_else(
      reference_level == "species",
      nb_countries,
      nb_countries_genus
    ),
    nb_sites = if_else(
      reference_level == "species",
      nb_sites,
      nb_sites_genus
    )
  )

if (any(is.na(field$reference_wd))) {
  bad <- field %>%
    filter(is.na(reference_wd)) %>%
    pull(species_key)

  stop(
    "No GWDD v.2 species- or genus-level trunk estimate was found for: ",
    paste(bad, collapse = ", ")
  )
}

# ---- 3.1 Raw GWDD records for species-level inferential comparisons ---------

if (!is.na(gw_filtered) && file.exists(gw_filtered)) {

  raw <- read_csv(
    gw_filtered,
    show_col_types = FALSE
  )

  if (!all(c("species", "wsg") %in% names(raw))) {
    stop(
      "The filtered GWDD file must contain columns 'species' and 'wsg'."
    )
  }

  raw <- raw %>%
    mutate(
      species = binomial(species),
      wsg = as.numeric(wsg)
    ) %>%
    filter(
      species %in% field$species_key,
      is.finite(wsg)
    )

} else {

  raw0 <- read_csv(
    gw_raw,
    show_col_types = FALSE
  )

  required_raw_cols <- c(
    "species", "rank_taxonomic", "wsg",
    "type_tissue", "location_sample",
    "type_forest", "experiment_design"
  )

  miss_raw <- setdiff(
    required_raw_cols,
    names(raw0)
  )

  if (length(miss_raw)) {
    stop(
      "Missing columns in raw GWDD v.2 file: ",
      paste(miss_raw, collapse = ", ")
    )
  }

  raw <- raw0 %>%
    mutate(
      species = binomial(species),
      wsg = as.numeric(wsg)
    ) %>%
    filter(
      species %in% field$species_key,
      rank_taxonomic == "species",
      is.finite(wsg),
      tolower(coalesce(type_tissue, "")) != "bark",
      !(tolower(coalesce(location_sample, "")) %in% c("branch", "root")),
      tolower(coalesce(type_forest, "")) != "plantation",
      !str_detect(
        tolower(coalesce(experiment_design, "")),
        "fertilizer|treatment"
      )
    )
}

raw_summary <- raw %>%
  group_by(species) %>%
  summarise(
    n_gwdd_filtered = n(),
    gwdd_filtered_mean = mean(wsg),
    gwdd_filtered_sd = sd(wsg),
    .groups = "drop"
  ) %>%
  rename(species_key = species)

# ---- 3.2 Species-level Welch comparisons + BH -------------------------------

field_ind <- wd %>%
  filter(Origem == "Amostrada") %>%
  select(
    species_key,
    field_wd = Densidade
  )

tests <- map_dfr(
  field$species_key,
  function(sp_name) {

    x <- field_ind$field_wd[
      field_ind$species_key == sp_name
    ]

    y <- raw$wsg[
      raw$species == sp_name
    ]

    if (length(y) < 3L || length(x) < 2L) {
      return(
        tibble(
          species_key = sp_name,
          n_field_test = length(x),
          n_gwdd_test = length(y),
          t_welch = NA_real_,
          df_welch = NA_real_,
          p_raw = NA_real_
        )
      )
    }

    z <- t.test(
      x,
      y,
      var.equal = FALSE
    )

    tibble(
      species_key = sp_name,
      n_field_test = length(x),
      n_gwdd_test = length(y),
      t_welch = unname(z$statistic),
      df_welch = unname(z$parameter),
      p_raw = z$p.value
    )
  }
)

tests$p_BH <- NA_real_

idx_tested <- which(!is.na(tests$p_raw))

tests$p_BH[idx_tested] <- p.adjust(
  tests$p_raw[idx_tested],
  method = "BH"
)

sp <- field %>%
  left_join(
    raw_summary,
    by = "species_key"
  ) %>%
  left_join(
    tests,
    by = "species_key"
  ) %>%
  mutate(
    wd_difference = reference_wd - field_mean,
    wd_difference_pct = 100 * wd_difference / field_mean,
    significant_BH = !is.na(p_BH) & p_BH < 0.05
  )

# ---- 3.3 Global paired comparison -------------------------------------------

# Global inferential comparison is restricted to taxa with a species-level
# GWDD v.2 estimate. A genus fallback, if needed, is not treated as an
# independent species-level reference estimate for this test.

sp_global <- sp %>%
  filter(reference_level == "species")

if (nrow(sp_global) < 2) {
  stop("Fewer than two species-level GWDD matches are available for the paired comparison.")
}

global_t <- t.test(
  sp_global$reference_wd,
  sp_global$field_mean,
  paired = TRUE
)

global_w <- wilcox.test(
  sp_global$reference_wd,
  sp_global$field_mean,
  paired = TRUE,
  exact = FALSE
)

global_summary <- tibble(
  n_species = nrow(sp_global),
  mean_field_wd = mean(sp_global$field_mean),
  mean_reference_wd = mean(sp_global$reference_wd),
  mean_difference_reference_minus_field =
    mean(sp_global$reference_wd - sp_global$field_mean),
  t = unname(global_t$statistic),
  df = unname(global_t$parameter),
  p_t = global_t$p.value,
  ci_low = global_t$conf.int[1],
  ci_high = global_t$conf.int[2],
  wilcoxon_V = unname(global_w$statistic),
  p_wilcoxon = global_w$p.value
)

message("")
message("=== Global paired comparison ===")
print(global_t)
print(global_w)
print(global_summary)

message("")
message("=== BH-significant species-level WD comparisons ===")
print(
  sp %>%
    filter(significant_BH) %>%
    arrange(p_BH) %>%
    select(
      species,
      n_field_test,
      n_gwdd_test,
      field_mean,
      reference_wd,
      wd_difference,
      p_raw,
      p_BH
    )
)

# ---- 4. Allometric equations ------------------------------------------------

chave <- function(D, H, wd) {
  0.0673 * (wd * D^2 * H)^0.976
}

nogueira <- function(D, H, wd) {
  exp(
    -1.305 +
      1.055 * log(D^2) +
      0.34 * log(H) +
      1.077 * log(wd)
  )
}

# ---- 4.1 Updated AGB for the 172 sampled trees ------------------------------

if (!"Objetivo_b_R" %in% field_sheets) {
  stop(
    "Sheet 'Objetivo_b_R' was not found in: ", field_file,
    "\nAvailable sheets: ", paste(field_sheets, collapse = ", ")
  )
}

trees0 <- read_excel(
  field_file,
  sheet = "Objetivo_b_R"
)

required_tree_cols <- c(
  "Especie", "DAP", "Altura", "Area", "no_arv"
)

miss_tree_cols <- setdiff(
  required_tree_cols,
  names(trees0)
)

if (length(miss_tree_cols)) {
  stop(
    "Missing columns in 'Objetivo_b_R': ",
    paste(miss_tree_cols, collapse = ", ")
  )
}

trees <- trees0 %>%
  filter(
    !is.na(Especie),
    !is.na(DAP),
    !is.na(Altura)
  ) %>%
  mutate(
    DAP = as.numeric(DAP),
    Altura = as.numeric(Altura),
    species_key = binomial(Especie)
  ) %>%
  filter(
    is.finite(DAP),
    DAP > 0,
    is.finite(Altura),
    Altura > 0
  ) %>%
  select(
    Especie,
    species_key,
    Area,
    no_arv,
    DAP,
    Altura
  ) %>%
  left_join(
    sp %>%
      select(
        species_key,
        species,
        reference_level,
        field_mean,
        reference_wd
      ),
    by = "species_key"
  )

if (any(is.na(trees$field_mean)) || any(is.na(trees$reference_wd))) {
  bad <- trees %>%
    filter(is.na(field_mean) | is.na(reference_wd)) %>%
    distinct(species_key) %>%
    pull(species_key)

  stop(
    "Missing WD values after joining sampled trees for: ",
    paste(bad, collapse = ", ")
  )
}

trees <- trees %>%
  mutate(
    Chave_field = chave(DAP, Altura, field_mean),
    Chave_GWDD = chave(DAP, Altura, reference_wd),
    Nogueira_field = nogueira(DAP, Altura, field_mean),
    Nogueira_GWDD = nogueira(DAP, Altura, reference_wd),
    Chave_difference_kg = Chave_GWDD - Chave_field,
    Chave_difference_pct =
      100 * Chave_difference_kg / Chave_field,
    Nogueira_difference_kg =
      Nogueira_GWDD - Nogueira_field,
    Nogueira_difference_pct =
      100 * Nogueira_difference_kg / Nogueira_field
  )

message("")
message("Sampled trees included in AGB recalculation: ", nrow(trees))

# Species-specific proportional change is deterministic when D and H are
# held constant and one species-level WD is substituted for another.
# Therefore no LMM is fitted to this contrast.

sp <- sp %>%
  mutate(
    Chave_AGB_diff_pct =
      100 * ((reference_wd / field_mean)^0.976 - 1),
    Nogueira_AGB_diff_pct =
      100 * ((reference_wd / field_mean)^1.077 - 1)
  )

sampled_tree_summary <- tibble(
  equation = c(
    "Chave et al. (2014)",
    "Nogueira Junior et al. (2014)"
  ),
  n_trees = nrow(trees),
  mean_AGB_field_kg = c(
    mean(trees$Chave_field),
    mean(trees$Nogueira_field)
  ),
  mean_AGB_reference_kg = c(
    mean(trees$Chave_GWDD),
    mean(trees$Nogueira_GWDD)
  ),
  mean_difference_kg = c(
    mean(trees$Chave_difference_kg),
    mean(trees$Nogueira_difference_kg)
  ),
  total_AGB_field_kg = c(
    sum(trees$Chave_field),
    sum(trees$Nogueira_field)
  ),
  total_AGB_reference_kg = c(
    sum(trees$Chave_GWDD),
    sum(trees$Nogueira_GWDD)
  )
) %>%
  mutate(
    total_difference_kg =
      total_AGB_reference_kg - total_AGB_field_kg,
    aggregate_difference_pct =
      100 * total_difference_kg / total_AGB_field_kg,
    mean_individual_difference_pct = c(
      mean(trees$Chave_difference_pct),
      mean(trees$Nogueira_difference_pct)
    )
  )

message("")
message("=== Sampled-tree AGB sensitivity ===")
print(sampled_tree_summary)

# ---- 5. Inventory-based stand scaling for focal species represented in inventory ----

# NOTE:
# Field species means were measured in selected trees with DBH >= 10 cm.
# For this sensitivity analysis, those species-level means are applied to
# inventory trees beginning at DBH >= 5 cm. This is therefore a substitution/
# scaling scenario and should not be interpreted as direct tree-specific WD
# measurement across the complete inventory diameter range.

inv_sheets <- excel_sheets(inv_file)

if (!"Dados ajustados" %in% inv_sheets) {
  stop(
    "Sheet 'Dados ajustados' was not found in: ", inv_file,
    "\nAvailable sheets: ", paste(inv_sheets, collapse = ", ")
  )
}

inv0 <- read_excel(
  inv_file,
  sheet = "Dados ajustados",
  col_types = "text"
)

required_inv_base <- c(
  "Nome científico",
  "Tratamento",
  "Área",
  "Altura"
)

miss_inv_base <- setdiff(
  required_inv_base,
  names(inv0)
)

if (length(miss_inv_base)) {
  stop(
    "Missing columns in inventory sheet 'Dados ajustados': ",
    paste(miss_inv_base, collapse = ", ")
  )
}

dap_cols <- intersect(
  paste0("DAP", 1:11),
  names(inv0)
)

if (length(dap_cols) == 0) {
  stop(
    "No DAP columns (DAP1 to DAP11) were found in the inventory."
  )
}

parse_num_flexible <- function(x) {
  x <- str_trim(as.character(x))
  x[x == ""] <- NA_character_

  out <- suppressWarnings(as.numeric(x))

  idx <- is.na(out) & !is.na(x)
  if (any(idx)) {
    out[idx] <- suppressWarnings(
      as.numeric(str_replace_all(x[idx], ",", "."))
    )
  }

  out
}

# Keep original inventory name for the taxonomic audit, then harmonize the
# species key. In particular, "Myroxylum peruiferum" is normalized to the
# focal/GWDD name "Myroxylon peruiferum". Inventory records identified only
# as "Ficus sp." remain at genus-only identification and are NOT assigned to
# Ficus guaranitica.
inv_all <- inv0 %>%
  mutate(
    inventory_name_original = as.character(`Nome científico`),
    species_key = binomial(`Nome científico`),
    treatment = recode(
      as.character(Tratamento),
      CMLM = "CML",
      CMLT = "CML",
      SAFM = "SAF",
      SAFT = "SAF",
      .default = as.character(Tratamento)
    )
  )

for (dc in dap_cols) {
  inv_all[[dc]] <- parse_num_flexible(inv_all[[dc]])
}
inv_all$Altura <- parse_num_flexible(inv_all$Altura)

inv_all <- inv_all %>%
  rowwise() %>%
  mutate(
    D_eq = sqrt(
      sum(
        c_across(all_of(dap_cols))^2,
        na.rm = TRUE
      )
    ),
    basal_area_m2 =
      if_else(
        is.finite(D_eq) & D_eq > 0,
        pi * (D_eq / 200)^2,
        NA_real_
      )
  ) %>%
  ungroup()

inv <- inv_all %>%
  filter(
    species_key %in% sp$species_key,
    !is.na(Altura),
    is.finite(Altura),
    Altura > 0,
    is.finite(D_eq),
    D_eq > 0
  ) %>%
  left_join(
    sp %>%
      select(
        species_key,
        species,
        reference_level,
        field_mean,
        reference_wd
      ),
    by = "species_key"
  )

if (any(is.na(inv$field_mean)) || any(is.na(inv$reference_wd))) {
  bad <- inv %>%
    filter(is.na(field_mean) | is.na(reference_wd)) %>%
    distinct(species_key) %>%
    pull(species_key)

  stop(
    "Missing WD values after joining inventory for: ",
    paste(bad, collapse = ", ")
  )
}

inv <- inv %>%
  mutate(
    Chave_field =
      chave(D_eq, Altura, field_mean),
    Chave_GWDD =
      chave(D_eq, Altura, reference_wd),
    Nogueira_field =
      nogueira(D_eq, Altura, field_mean),
    Nogueira_GWDD =
      nogueira(D_eq, Altura, reference_wd)
  )

# Experimental design used in the manuscript:
# 5 treatments x 3 blocks x 2 fixed 15 x 15 m subplots
# = 30 subplots per site
# = 6750 m² = 0.675 ha inventoried per site.

area_ha <- 30 * 225 / 10000

stand <- bind_rows(

  inv %>%
    group_by(`Área`) %>%
    summarise(
      n = n(),
      AGB_field =
        sum(Chave_field, na.rm = TRUE) /
        1000 / area_ha,
      AGB_ref =
        sum(Chave_GWDD, na.rm = TRUE) /
        1000 / area_ha,
      .groups = "drop"
    ) %>%
    mutate(
      equation = "Chave et al. (2014)"
    ),

  inv %>%
    group_by(`Área`) %>%
    summarise(
      n = n(),
      AGB_field =
        sum(Nogueira_field, na.rm = TRUE) /
        1000 / area_ha,
      AGB_ref =
        sum(Nogueira_GWDD, na.rm = TRUE) /
        1000 / area_ha,
      .groups = "drop"
    ) %>%
    mutate(
      equation = "Nogueira Junior et al. (2014)"
    )

) %>%
  mutate(
    site = recode(
      as.character(`Área`),
      `1` = "Lageado",
      `2` = "Edgárdia",
      .default = as.character(`Área`)
    ),
    difference_Mg_ha =
      AGB_ref - AGB_field,
    difference_pct =
      100 * difference_Mg_ha / AGB_field
  ) %>%
  select(
    site,
    `Área`,
    equation,
    n,
    AGB_field,
    AGB_ref,
    difference_Mg_ha,
    difference_pct
  ) %>%
  arrange(site, equation)

message("")
message("=== Inventory-based stand scaling for focal species represented in inventory ===")
print(stand)

# Additional audit of inventory coverage
inventory_audit <- tibble(
  total_inventory_rows = nrow(inv0),
  focal_valid_inventory_rows = nrow(inv),
  focal_species_in_inventory =
    n_distinct(inv$species_key),
  focal_inventory_individuals_pct =
    100 * nrow(inv) / nrow(inv0),
  focal_basal_area_pct =
    100 *
    sum(inv$basal_area_m2, na.rm = TRUE) /
    sum(inv_all$basal_area_m2, na.rm = TRUE),
  inventoried_area_per_site_ha =
    area_ha
)

inventory_taxonomic_audit <- tibble(
  focal_taxon = c(
    "Myroxylon peruiferum",
    "Ficus guaranitica"
  ),
  inventory_label = c(
    "Myroxylum peruiferum L. f.",
    "Ficus sp."
  ),
  inventory_records = c(
    sum(str_detect(inv0$`Nome científico`, "^Myroxylum peruiferum")),
    sum(str_squish(inv0$`Nome científico`) == "Ficus sp.")
  ),
  handling = c(
    "Harmonized to Myroxylon peruiferum and included at species level",
    "Excluded from stand scaling because inventory identification was genus-only"
  ),
  reference_handling = c(
    "Species-level GWDD v.2 trunk estimate",
    "Genus-level Ficus estimate used only for descriptive and sampled-tree substitution scenarios"
  )
)

message("")
message("=== Inventory audit ===")
print(inventory_audit)

message("")
message("=== Inventory taxonomic audit ===")
print(inventory_taxonomic_audit)

# ---- 6. Outputs --------------------------------------------------------------

write_csv(
  sp,
  file.path(
    out_dir,
    "Table_S1_species_WD_GWDDv2.csv"
  )
)

write_csv(
  sp %>%
    select(
      species,
      species_key,
      reference_level,
      n_field,
      field_mean,
      field_sd,
      n_gwdd_filtered,
      gwdd_filtered_mean,
      gwdd_filtered_sd,
      reference_wd,
      wd_difference,
      wd_difference_pct,
      n_field_test,
      n_gwdd_test,
      t_welch,
      df_welch,
      p_raw,
      p_BH,
      Chave_AGB_diff_pct,
      Nogueira_AGB_diff_pct
    ),
  file.path(
    out_dir,
    "Table_S2_species_AGB_sensitivity.csv"
  )
)

write_csv(
  stand,
  file.path(
    out_dir,
    "Table_2_stand_AGB_focal_species.csv"
  )
)

write_csv(
  trees,
  file.path(
    out_dir,
    "tree_sample_updated_AGB.csv"
  )
)

write_csv(
  global_summary,
  file.path(
    out_dir,
    "global_paired_WD_test.csv"
  )
)

write_csv(
  sampled_tree_summary,
  file.path(
    out_dir,
    "sampled_tree_AGB_summary.csv"
  )
)

write_csv(
  inventory_audit,
  file.path(
    out_dir,
    "inventory_scaling_audit.csv"
  )
)

write_csv(
  inventory_taxonomic_audit,
  file.path(
    out_dir,
    "inventory_taxonomic_audit.csv"
  )
)

# ---- Figure palette --------------------------------------------------------

fig_palette_ref <- c(
  species = "#1f78b4",
  genus = "#d95f02"
)

fig_palette_eq <- c(
  "Chave et al. (2014)" = "#1f78b4",
  "Nogueira Junior et al. (2014)" = "#d95f02"
)


# ---- 6.1 Figure 1 ------------------------------------------------------------

# Labels are restricted to BH-significant species.
# Scientific names are rendered in italics.
# Color palette:
#   species-level GWDD v.2 = blue
#   genus-level fallback   = orange

p1 <- ggplot(
  sp,
  aes(
    x = field_mean,
    y = reference_wd,
    shape = reference_level,
    color = reference_level
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2,
    linewidth = 0.6,
    color = "grey40"
  ) +
  geom_point(
    size = 3.0,
    stroke = 0.4
  ) +
  geom_text_repel(
    data = filter(sp, significant_BH),
    aes(
      label = paste0(
        "italic('",
        species,
        "')"
      )
    ),
    parse = TRUE,
    color = "black",
    min.segment.length = 0,
    box.padding = 0.35,
    max.overlaps = Inf
  ) +
  labs(
    x = expression(
      paste(
        "Field-measured wood density (g ",
        cm^{-3},
        ")"
      )
    ),
    y = expression(
      paste(
        "GWDD v.2 trunk wood density estimate (g ",
        cm^{-3},
        ")"
      )
    )
  ) +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = paste0(
      "n = ",
      nrow(sp),
      " focal species; paired test n = ",
      nrow(sp_global)
    ),
    hjust = -0.15,
    vjust = 1.3,
    color = "black"
  ) +
  scale_shape_manual(
    values = c(
      species = 16,
      genus = 17
    ),
    labels = c(
      species = "Species-level GWDD v.2",
      genus = "Genus-level fallback"
    ),
    name = NULL
  ) +
  scale_color_manual(
    values = fig_palette_ref,
    labels = c(
      species = "Species-level GWDD v.2",
      genus = "Genus-level fallback"
    ),
    name = NULL
  ) +
  guides(
    shape = guide_legend(order = 1),
    color = "none"
  ) +
  coord_equal() +
  theme_classic(
    base_size = 11
  )

ggsave(
  filename = file.path(
    out_dir,
    "Figure_1_WD_field_vs_GWDDv2.png"
  ),
  plot = p1,
  width = 7.2,
  height = 6.4,
  dpi = 300
)

# ---- 6.2 Figure 2 ------------------------------------------------------------

plot2 <- sp %>%
  select(
    species,
    Chave_AGB_diff_pct,
    Nogueira_AGB_diff_pct
  ) %>%
  pivot_longer(
    -species,
    names_to = "equation",
    values_to = "difference_pct"
  ) %>%
  mutate(
    equation = recode(
      equation,
      Chave_AGB_diff_pct =
        "Chave et al. (2014)",
      Nogueira_AGB_diff_pct =
        "Nogueira Junior et al. (2014)"
    )
  )

species_order <- plot2 %>%
  group_by(species) %>%
  summarise(
    mean_difference = mean(difference_pct),
    .groups = "drop"
  ) %>%
  arrange(mean_difference) %>%
  pull(species)

plot2 <- plot2 %>%
  mutate(
    species = factor(
      species,
      levels = species_order
    )
  )

p2 <- ggplot(
  plot2,
  aes(
    x = difference_pct,
    y = species,
    shape = equation,
    color = equation
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = 2,
    linewidth = 0.6,
    color = "grey40"
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(
      width = 0.45
    )
  ) +
  scale_y_discrete(
    labels = function(x) {
      parse(
        text = paste0(
          "italic('",
          x,
          "')"
        )
      )
    }
  ) +
  scale_shape_manual(
    values = c(
      "Chave et al. (2014)" = 16,
      "Nogueira Junior et al. (2014)" = 17
    ),
    name = NULL
  ) +
  scale_color_manual(
    values = fig_palette_eq,
    name = NULL
  ) +
  labs(
    x = paste0(
      "Change in estimated AGB when GWDD v.2 WD replaces ",
      "field-measured WD (%)"
    ),
    y = NULL
  ) +
  theme_classic(
    base_size = 11
  ) +
  theme(
    legend.position = "top"
  )

ggsave(
  filename = file.path(
    out_dir,
    "Figure_2_species_AGB_difference_GWDDv2.png"
  ),
  plot = p2,
  width = 9.5,
  height = 10.8,
  dpi = 300
)

# ---- 6.3 Session information -------------------------------------------------

sink(
  file.path(
    out_dir,
    "sessionInfo.txt"
  )
)

print(
  sessionInfo()
)

sink()

# ---- 7. Final console report -------------------------------------------------

message("")
message("==============================================================")
message("=== RUN COMPLETED SUCCESSFULLY ===============================")
message("==============================================================")
message("Outputs written to:")
message(normalizePath(out_dir, winslash = "/", mustWork = TRUE))
message("")
message("Key files:")
message("  - Table_S1_species_WD_GWDDv2.csv")
message("  - Table_S2_species_AGB_sensitivity.csv")
message("  - Table_2_stand_AGB_focal_species.csv")
message("  - tree_sample_updated_AGB.csv")
message("  - global_paired_WD_test.csv")
message("  - sampled_tree_AGB_summary.csv")
message("  - inventory_scaling_audit.csv")
message("  - inventory_taxonomic_audit.csv")
message("  - Figure_1_WD_field_vs_GWDDv2.png")
message("  - Figure_2_species_AGB_difference_GWDDv2.png")
message("  - sessionInfo.txt")
message("")
