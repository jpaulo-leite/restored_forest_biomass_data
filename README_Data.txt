DATA DESCRIPTION

This repository contains the datasets used to estimate aboveground biomass and evaluate the effects of locally sampled versus literature-based wood density values in restored Atlantic Forest sites.

All datasets are provided in CSV format.

--------------------------------------
1. analysis_dataset_density.csv
--------------------------------------

Description:
Dataset containing species-level wood density values used in the study.

Columns:

Species
Scientific name of the tree species.

Wood_density
Wood density value (g cm⁻³).

Source
Indicates whether the value was obtained from:
- Sampled measurements (field-based)
- Literature sources

sp
Species abbreviation used for data merging and analysis.


--------------------------------------
2. analysis_dataset_biomass.csv
--------------------------------------

Description:
Dataset used for biomass estimation using different allometric equations and wood density sources.

Columns:

Species
Scientific name of the tree species.

Treatment
Restoration treatment applied.

Site
Study site identifier.

Block
Experimental block.

Plot
Plot identifier.

Tree_ID
Unique tree identifier.

Family
Botanical family.

DBH
Diameter at breast height (cm).

Height
Total tree height (m).

Sampled_density
Wood density obtained from local sampling (g cm⁻³).

Chave_local
Biomass estimated using the Chave equation and sampled density.

Nogueira_local
Biomass estimated using the Nogueira equation and sampled density.

Literature_density
Wood density obtained from literature sources (g cm⁻³).

Chave_literature
Biomass estimated using the Chave equation and literature density.

Nogueira_literature
Biomass estimated using the Nogueira equation and literature density.

sp
Species abbreviation used for analytical procedures.


--------------------------------------
3. analysis_dataset_biomass_mean-density.csv
--------------------------------------

Description:
Auxiliary dataset containing mean wood density values used for biomass estimation.

Columns:

Species
Scientific name of the tree species.

Mean_sampled_density
Mean wood density obtained from local sampling (g cm⁻³).

Mean_literature_density
Mean wood density obtained from literature sources (g cm⁻³).


--------------------------------------
GENERAL NOTE
--------------------------------------

These datasets were used to:

1. Calculate species-level wood density values.
2. Estimate aboveground biomass using different allometric equations.
3. Compare biomass estimates based on locally sampled versus literature-based wood density.
4. Evaluate absolute and relative differences in biomass estimation.