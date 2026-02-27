#### PAPER – BIOMASS SCRIPT ####

Sys.setlocale("LC_NUMERIC", "C")
options(OutDec = ".")

Sys.getlocale()

# Clear environment
rm(list = ls())

#### BIOMASS ANALYSES ####

# Load required packages
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("matrixStats", quietly = TRUE)) install.packages("matrixStats")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("ggsignif", quietly = TRUE)) install.packages("ggsignif")

library(tidyverse)
library(matrixStats)
library(ggplot2)
library(ggsignif)

#### LOAD DATA ####

data_a <- read.csv("analysis_dataset_biomass.csv", sep = ";", dec = ".", stringsAsFactors = FALSE)

summary(data_a)
str(data_a)
head(data_a)

#### FACTOR CONVERSION ####

data_a$Species   <- as.factor(data_a$Species)
data_a$sp        <- as.factor(data_a$sp)
data_a$Site      <- as.factor(data_a$Site)
data_a$Treatment <- as.factor(data_a$Treatment)
data_a$Block     <- as.factor(data_a$Block)
data_a$Plot      <- as.factor(data_a$Plot)
data_a$Tree_ID   <- as.factor(data_a$Tree_ID)
data_a$Family    <- as.factor(data_a$Family)

#### DATA EXPLORATION ####

data_a %>% group_by(Tree_ID) %>% summarise(n = n()) %>% arrange(desc(n))
data_a %>% group_by(sp) %>% summarise(n = n()) %>% print(n = 100)
data_a %>% group_by(Site) %>% summarise(n = n())
data_a %>% group_by(Species, Treatment) %>% summarise(n = n()) %>% print(n = 100)
data_a %>% group_by(Species, Site) %>% summarise(n = n()) %>% print(n = 100)
data_a %>% group_by(Species) %>% summarise(n = n()) %>% print(n = 100)

#### NORMALITY TESTS ####

shapiro.test(data_a$DBH)
shapiro.test(data_a$Height)
shapiro.test(data_a$Sampled_density)
shapiro.test(data_a$Chave_local)
shapiro.test(data_a$Nogueira_local)
shapiro.test(data_a$Literature_density)

#### NON-PARAMETRIC COMPARISON ####

wilcox.test(data_a$Chave_local, data_a$Chave_literature, paired = TRUE)
wilcox.test(data_a$Nogueira_local, data_a$Nogueira_literature, paired = TRUE)

#### LONG FORMAT FOR BIOMASS ####

data_a2 <- pivot_longer(
  data_a,
  cols = c(11,12,14,15),
  names_to = "equation",
  values_to = "biomass",
  values_drop_na = FALSE
)

data_a2$source <- ifelse(
  data_a2$equation %in% c("Chave_local", "Nogueira_local"),
  "Sampled",
  "Literature"
)

data_a2$source <- as.factor(data_a2$source)

#### PARCEL AGGREGATION ####

data_a2 <- data_a2 %>%
  mutate(parcel = paste(Treatment, Site, Block, Plot))

data_b <- data_a2 %>%
  group_by(parcel, equation, source) %>%
  summarise(biomass = sum(biomass), .groups = "drop")

data_b <- data_b %>%
  mutate(
    parcel = as.factor(parcel),
    equation = as.factor(equation),
    source = as.factor(source)
  )

data_b$equation_type <- ifelse(
  data_b$equation %in% c("Chave_local", "Chave_literature"),
  "Chave Eq.",
  "Nogueira Eq."
)

data_b <- data_b %>%
  select(-equation) %>%
  mutate(
    parcel = as.factor(parcel),
    equation_type = as.factor(equation_type),
    source = as.factor(source)
  )

summary(data_b)

#### MODELS ####

model <- lm(biomass ~ equation_type * source, data_b)
summary(model)
anova(model)

model2 <- lm(biomass ~ equation_type + source, data_b)
summary(model2)
anova(model2)

#### GRAPH FOR PAPER ####

letters <- data.frame(
  equation_type = factor(
    c("Chave Eq.", "Chave Eq.", "Nogueira Eq.", "Nogueira Eq."),
    levels = levels(data_b$equation_type)
  ),
  source = factor(
    c("Literature", "Sampled", "Literature", "Sampled"),
    levels = levels(data_b$source)
  ),
  y = max(data_b$biomass, na.rm = TRUE) + 80,
  label = c("a", "b", "a", "b")
)

biomass_plot <- ggplot(data_b, aes(x = equation_type, y = biomass, fill = source)) +
  geom_boxplot(color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4")) +
  labs(
    title = NULL,
    x = NULL,
    y = "Biomass (kg)",
    fill = "Data Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    limits = c(0, max(data_b$biomass, na.rm = TRUE) + 120),
    breaks = seq(0, max(data_b$biomass, na.rm = TRUE), by = 250)
  ) +
  geom_hline(
    yintercept = seq(0, max(data_b$biomass, na.rm = TRUE), by = 250),
    color = "lightgray",
    size = 0.5,
    alpha = 0.3
  ) +
  geom_text(
    data = letters,
    aes(x = equation_type, y = y, label = label, group = source),
    position = position_dodge(width = 0.75),
    size = 5,
    vjust = 0,
    inherit.aes = FALSE
  )

print(biomass_plot)

# Save if needed
# ggsave("biomass_plot.png", plot = biomass_plot, width = 10, height = 6, dpi = 600, bg = "white")



################################################################################



######### T TEST OR wilcox.test BY Species ###########

shapiro.test(data_a$Chave_local)
shapiro.test(data_a$Chave_literature)
shapiro.test(data_a$Nogueira_local)
shapiro.test(data_a$Nogueira_literature)

wilcox.test(data_a$Chave_local, data_a$Chave_literature, paired = TRUE)
wilcox.test(data_a$Nogueira_local, data_a$Nogueira_literature, paired = TRUE)

### BIOMASS VALUES FROM EQUATIONS USING SAMPLED DENSITY DIFFER SIGNIFICANTLY
### FROM BIOMASS VALUES FROM THE SAME EQUATIONS WHEN USING LITERATURE DENSITY.


######## COMPARING BIOMASS BY Species ##############

## TEST NORMALITY BY Species

resultado.shapiro1 <- cbind(rep(NA, 31),rep(NA, 31)) 
colnames(resultado.shapiro1) <- c("sp", "pvalue")
resultado.shapiro1 <- as.data.frame(resultado.shapiro1)

for(i in c(1:31)){
  
  shapiro.c.loc <- shapiro.test(data_a$Chave_local[which(data_a$sp==i)])
  
  resultado.shapiro1[i,1] <- i
  resultado.shapiro1[i,2] <- shapiro.c.loc$p.value
  
}
resultado.shapiro1

shap1 <- resultado.shapiro1$sp[which(resultado.shapiro1$pvalue < 0.05)]
shap1

## Shapiro < 0.05 are Species 1, 5, 14, 23.

#################

resultado.shapiro2 <- cbind(rep(NA, 31),rep(NA, 31)) 
colnames(resultado.shapiro2) <- c("sp", "pvalue")
resultado.shapiro2 <- as.data.frame(resultado.shapiro2)

for(i in c(1:31)){
  
  shapiro.c.lit <- shapiro.test(data_a$Chave_literature[which(data_a$sp==i)])
  
  resultado.shapiro2[i,1] <- i
  resultado.shapiro2[i,2] <- shapiro.c.lit$p.value
  
}
resultado.shapiro2

shap2 <- resultado.shapiro2$sp[which(resultado.shapiro2$pvalue < 0.05)]
shap2

## Shapiro > 0.05 are Species 7, 12, 17, 30.

#################

resultado.shapiro3 <- cbind(rep(NA, 31),rep(NA, 31)) 
colnames(resultado.shapiro3) <- c("sp", "pvalue")
resultado.shapiro3 <- as.data.frame(resultado.shapiro3)

for(i in c(1:31)){
  
  shapiro.n.loc <- shapiro.test(data_a$Nogueira_local[which(data_a$sp==i)])
  
  resultado.shapiro3[i,1] <- i
  resultado.shapiro3[i,2] <- shapiro.n.loc$p.value
  
}
resultado.shapiro3

shap3 <- resultado.shapiro3$sp[which(resultado.shapiro3$pvalue < 0.05)]
shap3

## Shapiro > 0.05 are Species 7, 12, 15, 17, 30.
# Species 15 is Luehea divaricata

#################

resultado.shapiro4 <- cbind(rep(NA, 31),rep(NA, 31)) 
colnames(resultado.shapiro4) <- c("sp", "pvalue")
resultado.shapiro4 <- as.data.frame(resultado.shapiro4)

for(i in c(1:31)){
  
  shapiro.n.lit <- shapiro.test(data_a$Nogueira_literature[which(data_a$sp==i)])
  
  resultado.shapiro4[i,1] <- i
  resultado.shapiro4[i,2] <- shapiro.n.lit$p.value
  
}
resultado.shapiro4

shap4 <- resultado.shapiro4$sp[which(resultado.shapiro4$pvalue < 0.05)]
shap4

## Shapiro > 0.05 are Species 7, 12, 15, 17, 30.
# Species 15 is Luehea divaricata


##### APPLYING T TEST OR WILCOX #####

### CHAVE EQUATION ###

## Species 1, 5, 14 and 23 DO NOT SHOW NORMAL DISTRIBUTION
## → APPLYING WILCOX TEST

wilcox.test(data_a$Chave_local[which(data_a$sp=="1")], data_a$Chave_literature[which(data_a$sp=="1")], paired = TRUE)
wilcox.test(data_a$Chave_local[which(data_a$sp=="5")], data_a$Chave_literature[which(data_a$sp=="5")], paired = TRUE)
wilcox.test(data_a$Chave_local[which(data_a$sp=="14")], data_a$Chave_literature[which(data_a$sp=="14")], paired = TRUE)
wilcox.test(data_a$Chave_local[which(data_a$sp=="23")], data_a$Chave_literature[which(data_a$sp=="23")], paired = TRUE)


### T TEST FOR NORMAL DISTRIBUTION

t.test(data_a$Chave_local[which(data_a$sp=="2")], data_a$Chave_literature[which(data_a$sp=="2")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="3")], data_a$Chave_literature[which(data_a$sp=="3")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="4")], data_a$Chave_literature[which(data_a$sp=="4")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="6")], data_a$Chave_literature[which(data_a$sp=="6")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="7")], data_a$Chave_literature[which(data_a$sp=="7")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="8")], data_a$Chave_literature[which(data_a$sp=="8")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="9")], data_a$Chave_literature[which(data_a$sp=="9")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="10")], data_a$Chave_literature[which(data_a$sp=="10")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="11")], data_a$Chave_literature[which(data_a$sp=="11")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="12")], data_a$Chave_literature[which(data_a$sp=="12")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="13")], data_a$Chave_literature[which(data_a$sp=="13")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="15")], data_a$Chave_literature[which(data_a$sp=="15")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="16")], data_a$Chave_literature[which(data_a$sp=="16")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="17")], data_a$Chave_literature[which(data_a$sp=="17")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="18")], data_a$Chave_literature[which(data_a$sp=="18")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="19")], data_a$Chave_literature[which(data_a$sp=="19")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="20")], data_a$Chave_literature[which(data_a$sp=="20")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="21")], data_a$Chave_literature[which(data_a$sp=="21")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="22")], data_a$Chave_literature[which(data_a$sp=="22")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="24")], data_a$Chave_literature[which(data_a$sp=="24")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="25")], data_a$Chave_literature[which(data_a$sp=="25")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="26")], data_a$Chave_literature[which(data_a$sp=="26")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="27")], data_a$Chave_literature[which(data_a$sp=="27")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="28")], data_a$Chave_literature[which(data_a$sp=="28")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="29")], data_a$Chave_literature[which(data_a$sp=="29")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="30")], data_a$Chave_literature[which(data_a$sp=="30")], paired = TRUE)
t.test(data_a$Chave_local[which(data_a$sp=="31")], data_a$Chave_literature[which(data_a$sp=="31")], paired = TRUE)



### NOGUEIRA EQUATION ###

# Species 1, 5, 14 and 23 DO NOT PRESENT NORMAL DISTRIBUTION,
# THEREFORE, THE wilcox.test WILL BE APPLIED  

wilcox.test(data_a$Nogueira_local[which(data_a$sp=="1")], data_a$Nogueira_literature[which(data_a$sp=="1")], paired = TRUE)

wilcox.test(data_a$Nogueira_local[which(data_a$sp=="5")], data_a$Nogueira_literature[which(data_a$sp=="5")], paired = TRUE)

wilcox.test(data_a$Nogueira_local[which(data_a$sp=="14")], data_a$Nogueira_literature[which(data_a$sp=="14")], paired = TRUE)

wilcox.test(data_a$Nogueira_local[which(data_a$sp=="23")], data_a$Nogueira_literature[which(data_a$sp=="23")], paired = TRUE)


# T TEST FOR THOSE WITH NORMAL DISTRIBUTION

t.test(data_a$Nogueira_local[which(data_a$sp=="2")], data_a$Nogueira_literature[which(data_a$sp=="2")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="3")], data_a$Nogueira_literature[which(data_a$sp=="3")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="4")], data_a$Nogueira_literature[which(data_a$sp=="4")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="6")], data_a$Nogueira_literature[which(data_a$sp=="6")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="7")], data_a$Nogueira_literature[which(data_a$sp=="7")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="8")], data_a$Nogueira_literature[which(data_a$sp=="8")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="9")], data_a$Nogueira_literature[which(data_a$sp=="9")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="10")], data_a$Nogueira_literature[which(data_a$sp=="10")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="11")], data_a$Nogueira_literature[which(data_a$sp=="11")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="12")], data_a$Nogueira_literature[which(data_a$sp=="12")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="13")], data_a$Nogueira_literature[which(data_a$sp=="13")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="15")], data_a$Nogueira_literature[which(data_a$sp=="15")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="16")], data_a$Nogueira_literature[which(data_a$sp=="16")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="17")], data_a$Nogueira_literature[which(data_a$sp=="17")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="18")], data_a$Nogueira_literature[which(data_a$sp=="18")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="19")], data_a$Nogueira_literature[which(data_a$sp=="19")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="20")], data_a$Nogueira_literature[which(data_a$sp=="20")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="21")], data_a$Nogueira_literature[which(data_a$sp=="21")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="22")], data_a$Nogueira_literature[which(data_a$sp=="22")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="24")], data_a$Nogueira_literature[which(data_a$sp=="24")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="25")], data_a$Nogueira_literature[which(data_a$sp=="25")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="26")], data_a$Nogueira_literature[which(data_a$sp=="26")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="27")], data_a$Nogueira_literature[which(data_a$sp=="27")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="28")], data_a$Nogueira_literature[which(data_a$sp=="28")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="29")], data_a$Nogueira_literature[which(data_a$sp=="29")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="30")], data_a$Nogueira_literature[which(data_a$sp=="30")], paired = TRUE)

t.test(data_a$Nogueira_local[which(data_a$sp=="31")], data_a$Nogueira_literature[which(data_a$sp=="31")], paired = TRUE)

#### Species X AND Y PRESENTED SIGNIFICANT DIFFERENCES
#### BETWEEN BIOMASS VALUES FROM THE EQUATIONS

################################################################################



##### LMM MODEL ####

data_a2 <- pivot_longer(data_a, cols = c(11,12,14,15),
                        names_to = "equation",
                        values_to = "biomass",
                        values_drop_na = FALSE)

data_a2$type <- ifelse(data_a2$equation %in% c("Chave_local", "Nogueira_local"),
                       "Sampled",
                       "Literature")

data_a2$type <- as.factor(data_a2$type)


#### DIEGO'S SUGGESTION #####

data_a2 %>% names
names(data_a2)

data_a2 %>% summary

data_a2 <- data_a2 %>%
  mutate(plot_id = paste(Treatment, Site, Block, Plot))



data_a2$equation <- ifelse(data_a2$equation %in% c("Chave_local", "Chave_literature"),
                           "chave",
                           "Nogueira")



# Install and load packages
if (!requireNamespace("lme4", quietly = TRUE)) {
  install.packages("lme4")
}
library(lme4)       # Mixed linear models

if (!requireNamespace("emmeans", quietly = TRUE)) {
  install.packages("emmeans")
}
library(emmeans)    # Post-hoc comparisons in mixed models

if (!requireNamespace("arules", quietly = TRUE)) {
  install.packages("arules")
}
library(arules)     # Discretization

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)    # Plotting




# Calculate interval limits
min_dbh <- min(data_a2$DBH, na.rm = TRUE)
max_dbh <- max(data_a2$DBH, na.rm = TRUE)
interval_size <- (max_dbh - min_dbh) / 5

# Define interval breaks
breaks <- seq(min_dbh, max_dbh, by = interval_size)

# Show breaks
breaks


# DBH classes
data_a2$DBH <- arules::discretize(data_a2$DBH,
                                  method = "interval",
                                  breaks = 5,
                                  labels = c("small", "small-med", "med", "med-large", "large"))





#### CHAVE EQUATION ANALYSIS ####

# Fit mixed linear model
lmm_model1 <- lmerTest::lmer(biomass ~ type * Species +
                               (1| Tree_ID) +
                               (1 | DBH),
                             data = data_a2 %>% filter(equation == "chave"))

summary(lmm_model1)
anova(lmm_model1)

# Important effect dependent on Species and type

# Install and load pbkrtest if needed
if (!requireNamespace("pbkrtest", quietly = TRUE)) {
  install.packages("pbkrtest")
}
library(pbkrtest)  # Kenward-Roger adjustment

# Run emmeans
tukey_adjustment1 <- emmeans(lmm_model1,
                             list(pairwise ~ type | Species),
                             adjust = "tukey")

library(emmeans)

tukey_adjustment1 <- emmeans(lmm_model1,
                             list(pairwise ~ type | Species),
                             adjust = "tukey")

summary(tukey_adjustment1)
str(tukey_adjustment1)

# Extract comparison estimates (RR)
comparison_results1 <- as.data.frame(
  tukey_adjustment1$`pairwise differences of type | Species`
)[, c("Species", "estimate", "SE", "df", "t.ratio", "p.value")]

# Add effect column
comparison_results1$Effect <- ifelse(
  comparison_results1$p.value < 0.05 & comparison_results1$estimate > 0,
  "Underestimate",
  ifelse(comparison_results1$p.value < 0.05 & comparison_results1$estimate < 0,
         "Overestimate",
         "No Effect")
)

comparison_results1 %>% names

comparison_results1$Species[which(comparison_results1$p.value < 0.05)]

# Add effect classification
comparison_results1$Effect <- ifelse(
  comparison_results1$p.value < 0.05 & comparison_results1$estimate > 0,
  "Underestimate",
  ifelse(comparison_results1$p.value < 0.05 & comparison_results1$estimate < 0,
         "Overestimate",
         "No Effect")
)

# Show results
print(comparison_results1)






##### LMM MODEL ####

data_a2 <- pivot_longer(data_a, cols = c(11,12,14,15),
                        names_to = "equation",
                        values_to = "biomass",
                        values_drop_na = FALSE)

data_a2 <- data_a2 %>%
  mutate(
    plot_id = paste(Treatment, Site, Block, Plot),
    
    type = case_when(
      equation %in% c("Chave_local", "Nogueira_local") ~ "Sampled",
      equation %in% c("Chave_literature", "Nogueira_literature") ~ "Literature"
    ),
    
    equation_family = case_when(
      equation %in% c("Chave_local", "Chave_literature") ~ "chave",
      equation %in% c("Nogueira_local", "Nogueira_literature") ~ "Nogueira"
    )
  )

data_a2$type <- factor(data_a2$type,
                       levels = c("Sampled","Literature"))

library(lme4)
library(lmerTest)
library(emmeans)
library(arules)
library(ggplot2)
library(pbkrtest)
library(gridExtra)
library(dplyr)

##### DBH CLASSES #####

data_a2$DBH <- arules::discretize(data_a2$DBH,
                                  method = "interval",
                                  breaks = 5,
                                  labels = c("small", "small-med", "med", "med-large", "large"))


################### CHAVE EQUATION #########################

lmm_model1 <- lmer(biomass ~ type * Species +
                     (1|Tree_ID) +
                     (1|DBH),
                   data = data_a2 %>% filter(equation_family == "chave"))

emm1 <- emmeans(lmm_model1, ~ type | Species)

contrast1 <- contrast(emm1,
                      method = "pairwise",
                      adjust = "tukey")

comparison_results1 <- as.data.frame(contrast1)[,
                                                c("Species", "estimate", "SE", "df", "t.ratio", "p.value")]

##### CLASSIFICAÇÃO DO EFEITO #####
comparison_results1$Effect <- ifelse(
  comparison_results1$p.value < 0.05 & comparison_results1$estimate > 0,
  "Underestimate",
  ifelse(comparison_results1$p.value < 0.05 & comparison_results1$estimate < 0,
         "Overestimate",
         "No Effect")
)

comparison_results1$Color <- ifelse(comparison_results1$p.value < 0.05,
                                    "#D65F5F", "#5FBA7F")

comparison_results1$Species <- factor(comparison_results1$Species,
                                      levels = rev(sort(unique(comparison_results1$Species))))

final_plot_chave <- ggplot(comparison_results1,
                           aes(x = -estimate, y = Species,
                               color = Color, shape = Effect)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = -estimate - SE,
                     xmax = -estimate + SE),
                 height = 0.5) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_color_manual(values = c("#5FBA7F", "#D65F5F"), guide = FALSE) +
  scale_shape_manual(values = c("No Effect" = 16,
                                "Underestimate" = 6,
                                "Overestimate" = 2)) +
  scale_x_continuous(limits = c(-110, 150))

print(final_plot_chave)


################### NOGUEIRA EQUATION ######################

lmm_model_2 <- lmer(biomass ~ type * Species +
                      (1|Tree_ID) +
                      (1|DBH),
                    data = data_a2 %>% filter(equation_family == "Nogueira"))

emm2 <- emmeans(lmm_model_2, ~ type | Species)

contrast2 <- contrast(emm2,
                      method = "pairwise",
                      adjust = "tukey")

results_comparisons2 <- as.data.frame(contrast2)[,
                                                 c("Species", "estimate", "SE", "df", "t.ratio", "p.value")]


##### CLASSIFICAÇÃO DO EFEITO #####
results_comparisons2$Effect <- ifelse(
  results_comparisons2$p.value < 0.05 & results_comparisons2$estimate > 0,
  "Underestimate",
  ifelse(results_comparisons2$p.value < 0.05 & results_comparisons2$estimate < 0,
         "Overestimate",
         "No Effect")
)

results_comparisons2$Color <- ifelse(results_comparisons2$p.value < 0.05,
                                     "#D65F5F", "#5FBA7F")

results_comparisons2$Species <- factor(results_comparisons2$Species,
                                       levels = rev(sort(unique(results_comparisons2$Species))))

final_plot_nogueira <- ggplot(results_comparisons2,
                              aes(x = - estimate, y = Species,
                                  color = Color, shape = Effect)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = -estimate - SE,
                     xmax = -estimate + SE),
                 height = 0.5) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_color_manual(values = c("#5FBA7F", "#D65F5F"), guide = FALSE) +
  scale_shape_manual(values = c("No Effect" = 16,
                                "Underestimate" = 6,
                                "Overestimate" = 2)) +
  scale_x_continuous(limits = c(-110, 150))

print(final_plot_nogueira)


################### COMBINED PLOT ##########################

combined_plot <- grid.arrange(final_plot_chave,
                              final_plot_nogueira,
                              ncol = 1)

print(combined_plot)







#### Article Discussion ####

library(dplyr)

# =========================
# 1. Mean Biomass
# =========================

mean_chave_sampled <- mean(dados_a$Chave_local, na.rm = TRUE)
mean_chave_literature <- mean(dados_a$Chave_literatura, na.rm = TRUE)

mean_nogueira_sampled <- mean(dados_a$Nogueira_local, na.rm = TRUE)
mean_nogueira_literature <- mean(dados_a$Nogueira_literatura, na.rm = TRUE)


# =========================
# 2. Absolute Differences
# (Literature – Sampled)
# =========================

diff_chave <- mean_chave_literature - mean_chave_sampled
diff_nogueira <- mean_nogueira_literature - mean_nogueira_sampled


# =========================
# 3. Relative Differences (%)
# =========================

percent_diff_chave <- (diff_chave / mean_chave_sampled) * 100
percent_diff_nogueira <- (diff_nogueira / mean_nogueira_sampled) * 100


# =========================
# 4. Display Results
# =========================

cat("Mean biomass using Chave equation and sampled density:", mean_chave_sampled, "\n")
cat("Mean biomass using Chave equation and literature density:", mean_chave_literature, "\n")
cat("Absolute difference (Chave):", diff_chave, "\n")
cat("Relative difference (Chave):", percent_diff_chave, "%\n\n")

cat("Mean biomass using Nogueira equation and sampled density:", mean_nogueira_sampled, "\n")
cat("Mean biomass using Nogueira equation and literature density:", mean_nogueira_literature, "\n")
cat("Absolute difference (Nogueira):", diff_nogueira, "\n")
cat("Relative difference (Nogueira):", percent_diff_nogueira, "%\n")


# =========================
# 5. Total Biomass Sums
# =========================

sum_chave_sampled <- sum(dados_a$Chave_local, na.rm = TRUE)
sum_chave_literature <- sum(dados_a$Chave_literatura, na.rm = TRUE)

sum_nogueira_sampled <- sum(dados_a$Nogueira_local, na.rm = TRUE)
sum_nogueira_literature <- sum(dados_a$Nogueira_literatura, na.rm = TRUE)


# =========================
# 6. Absolute Differences
# =========================

diff_sum_chave <- sum_chave_literature - sum_chave_sampled
diff_sum_nogueira <- sum_nogueira_literature - sum_nogueira_sampled


# =========================
# 7. Relative Differences (%)
# =========================

percent_diff_sum_chave <- (diff_sum_chave / sum_chave_sampled) * 100
percent_diff_sum_nogueira <- (diff_sum_nogueira / sum_nogueira_sampled) * 100


# =========================
# 8. Display Total Results
# =========================

cat("\nTotal biomass difference (Chave):", diff_sum_chave, "\n")
cat("Relative total biomass difference (Chave):", percent_diff_sum_chave, "%\n")

cat("\nTotal biomass difference (Nogueira):", diff_sum_nogueira, "\n")
cat("Relative total biomass difference (Nogueira):", percent_diff_sum_nogueira, "%\n")


# =========================
# 9. Mean Relative Difference per Species
# =========================

percent_diff_species <- dados_a %>%
  group_by(Especie) %>%
  summarise(
    mean_percent_diff_chave = mean(percent_diff_chave, na.rm = TRUE),
    mean_percent_diff_nogueira = mean(percent_diff_nogueira, na.rm = TRUE)
  )

print(percent_diff_species)


# =========================
# 10. Biomass Sum per Species
# =========================

sum_biomass <- dados_a %>%
  group_by(Especie) %>%
  summarise(
    sum_chave_sampled = sum(Chave_local, na.rm = TRUE),
    sum_chave_literature = sum(Chave_literatura, na.rm = TRUE),
    sum_nogueira_sampled = sum(Nogueira_local, na.rm = TRUE),
    sum_nogueira_literature = sum(Nogueira_literatura, na.rm = TRUE)
  ) %>%
  mutate(
    percent_diff_chave = ((sum_chave_literature - sum_chave_sampled) / sum_chave_sampled) * 100,
    percent_diff_nogueira = ((sum_nogueira_literature - sum_nogueira_sampled) / sum_nogueira_sampled) * 100
  )

print(sum_biomass, n = 31)




