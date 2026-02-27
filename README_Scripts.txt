SCRIPT DESCRIPTION

All analyses were conducted in R.

The repository includes the following scripts used to generate the results presented in the manuscript.

--------------------------------------
1. Wood_Density_Analysis_Script.R
--------------------------------------

Purpose:

- Processes species-level wood density data.
- Calculates mean sampled and literature-based density values.
- Prepares density inputs used for biomass estimation.


--------------------------------------
2. Biomass_Section_Analysis_Script.R
--------------------------------------

Purpose:

- Estimates aboveground biomass using:

  a) Chave equation
  b) Nogueira equation

- Compares biomass estimates derived from:

  a) Locally sampled wood density
  b) Literature-based wood density

- Calculates:

  - Mean biomass values
  - Absolute differences
  - Relative differences (%)
  - Species-level summaries


--------------------------------------
SOFTWARE AND PACKAGES
--------------------------------------

Analyses were conducted using R.

Main packages required:

- tidyverse
- dplyr
- lme4
- lmerTest
- emmeans
- ggplot2