#### WOOD DENSITY ANALYSES ####

Sys.setlocale("LC_NUMERIC", "C")
options(OutDec = ".")

Sys.getlocale()

library(dplyr)
library(readr)

data <- read_csv("analysis_dataset_density.csv")

summary(data)
str(data)

# Check the first rows to ensure correct data import
head(data)

# Filter Sampled and Literature data
sampled_data <- data %>% filter(Source == "Sampled")
literature_data <- data %>% filter(Source == "Literature")

# Check subsets
head(sampled_data)
head(literature_data)

# Unpaired t-test for overall wood density
t_test_overall <- t.test(sampled_data$Wood_density,
                         literature_data$Wood_density,
                         var.equal = FALSE)
print(t_test_overall)

# Count number of literature records per species
species_counts <- literature_data %>%
  group_by(sp) %>%
  summarise(count = n())

selected_species <- species_counts %>%
  filter(count >= 3) %>%
  pull(sp)

selected_species

# Run species-level t-tests
results <- list()

for (sp_id in selected_species) {
  
  sampled_density <- sampled_data %>%
    filter(sp == sp_id) %>%
    pull(Wood_density)
  
  literature_density <- literature_data %>%
    filter(sp == sp_id) %>%
    pull(Wood_density)
  
  if (length(sampled_density) > 0 && length(literature_density) > 0) {
    t_test <- t.test(sampled_density,
                     literature_density,
                     var.equal = FALSE)
    
    results[[as.character(sp_id)]] <- t_test
  }
}

# Display results
for (sp_id in names(results)) {
  cat("Species", sp_id,
      ": t-stat =", results[[sp_id]]$statistic,
      ", p-value =", results[[sp_id]]$p.value, "\n")
}

# Identify species with significant differences
species_with_significant_difference <- c()

for (sp_id in selected_species) {
  
  sampled_density <- sampled_data %>%
    filter(sp == sp_id) %>%
    pull(Wood_density)
  
  literature_density <- literature_data %>%
    filter(sp == sp_id) %>%
    pull(Wood_density)
  
  if (length(sampled_density) > 0 && length(literature_density) > 0) {
    t_test <- t.test(sampled_density,
                     literature_density,
                     var.equal = FALSE)
    
    if (t_test$p.value < 0.05) {
      species_with_significant_difference <-
        c(species_with_significant_difference, sp_id)
    }
  }
}

significant_species_names <- data %>%
  filter(sp %in% species_with_significant_difference) %>%
  distinct(Species)

print(significant_species_names)



#### PUBLICATION-READY PLOT ####

library(ggplot2)

plot_density <- ggplot(data,
                       aes(x = Source,
                           y = Wood_density,
                           fill = Source)) +
  geom_boxplot(color = "black",
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4")) +
  labs(title = NULL,
       x = NULL,
       y = "Wood density (g/cm³)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, 1.30),
                     breaks = seq(0, 1.25, by = 0.25)) +
  geom_hline(yintercept = seq(0, 1.30, by = 0.25),
             color = "lightgray",
             size = 0.7,
             alpha = 0.3) +
  geom_text(aes(x = Source,
                y = 1.20,
                label = ifelse(Source == "Literature", "a", "b")),
            size = 6,
            vjust = 0.5)

print(plot_density)


###### SPECIES-SPECIFIC PLOTS ######

library(ggplot2)
library(dplyr)
library(cowplot)
library(readr)

# Load data
data <- read_csv("analysis_dataset_density.csv")

# Rename for plotting consistency
data <- data %>%
  rename(Sampling = Source)

# Color palette
color_palette <- c("#A6CEE3", "#1F78B4")

# Plot function
create_density_plot <- function(data, species_name) {
  
  ggplot(data, aes(x = Sampling, y = Wood_density, fill = Sampling)) +
    geom_boxplot(color = "black", alpha = 0.7) +
    labs(x = NULL, y = "Wood density (g/cm³)") +
    scale_fill_manual(values = color_palette) +
    theme_minimal(base_size = 22, base_family = "serif") + 
    theme(axis.text = element_text(colour = "black", size = 14),
          axis.title = element_text(size = 15),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 20),
          axis.text.x = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(limits = c(0.3, 1.30), breaks = seq(0.30, 1.20, by = 0.20)) +
    ggtitle(species_name) +
    geom_hline(yintercept = seq(0.30, 1.20, by = 0.20),
               color = "lightgray", linewidth = 0.7, alpha = 0.3)
}

# Species list (matching your previous ones)
species_list <- c(
  "Anadenanthera colubrina",
  "Cedrela fissilis",
  "Dipteryx alata",
  "Luehea divaricata",
  "Myroxylon peruiferum",
  "Peltophorum dubium"
)

# Create plots
plots <- lapply(species_list, function(sp_name) {
  sp_data <- data %>% filter(Species == sp_name)
  create_density_plot(sp_data, bquote(italic(.(sp_name))))
})

# Function to add significance letters
add_letters <- function(plot) {
  plot +
    annotate("text", x = 1, y = 1.25, label = "a", size = 5) +
    annotate("text", x = 2, y = 1.25, label = "b", size = 5)
}

plots <- lapply(plots, add_letters)

# Combine
combined_plot <- plot_grid(
  plots[[1]],
  plots[[2]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()),
  plots[[3]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()),
  plots[[4]],
  plots[[5]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()),
  plots[[6]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
)

print(combined_plot)

