# Reproducibility materials: wood density and above-ground biomass in restored forests

This repository contains the data and R code used to compare field-measured wood density (WD) with reference values from the Global Wood Density Database v.2.2 (GWDD v.2.2), and to evaluate how WD substitution propagates to above-ground biomass estimates at species, sampled-tree, and inventory scales.

## Repository structure

- `analysis.R` — complete analysis workflow and figure generation.
- `data/field_WD_measurements.csv` — 172 field WD measurements from 31 species.
- `data/sampled_trees.csv` — diameter and height of the 172 trees used in the sampled-tree biomass sensitivity analysis.
- `data/inventory_2020_2021.csv` — tree inventory used for inventory-based scaling.
- `data/species_metadata.csv` — WD measurement method for each of the 31 species.
- `data/GWDDv2_reference_values_study_species.csv` — GWDD v.2.2 trunk reference values used in the analyses, including the reference taxonomic level.
- `data/GWDDv2_filtered_records_study_species.csv` — retained species-level GWDD records used in the Welch comparisons.
- `outputs/` — tabular results corresponding to the analyses reported in the manuscript.
- `figures/` — Figures 1 and 2 generated from the same analytical quantities.

## Analysis workflow

The analysis:

1. Calculates field WD means for the 31 species included in the study.
2. Matches these species to GWDD v.2.2 trunk reference estimates.
3. Performs the overall paired comparison using the 30 species with species-level GWDD estimates.
4. Performs species-specific Welch tests for species with at least three retained numerical GWDD records and applies the Benjamini–Hochberg correction for multiple comparisons.
5. Recalculates above-ground biomass for the 172 sampled trees with the Chave et al. (2014) and Nogueira Junior et al. (2014) equations, keeping observed diameter and height unchanged and substituting only WD.
6. Propagates the two WD scenarios through the observed inventory for species that can be matched at species level.
7. Converts the inventory-based biomass difference to an illustrative carbon-stock difference using a carbon fraction of 0.47.

## Important data-handling decisions

Thirty of the 31 study species have species-level GWDD v.2.2 trunk estimates. *Ficus guaranitica* does not have a species-level estimate in the database, so the genus-level *Ficus* trunk estimate is used only in the descriptive field-versus-reference comparison and in the 172-tree substitution analysis. Inventory records identified only as *Ficus* sp. are not assigned to *F. guaranitica* and are excluded from inventory scaling.

Inventory records written as *Myroxylum peruiferum* are harmonized to *Myroxylon peruiferum* before matching.

The bundled `GWDDv2_filtered_records_study_species.csv` contains the reference records retained for species-specific tests. The filtering criteria exclude non-numerical WD values, non-species taxonomic ranks, bark records, branch or root samples, plantation records, and records explicitly associated with fertilizer or treatment experiments.

## Reproducing the analysis

Use R 4.4.2 or a recent compatible R version. Required packages are:

`readr`, `dplyr`, `tidyr`, `stringr`, `purrr`, `ggplot2`, and `ggrepel`.

From R or RStudio, set the repository root as the working directory and run:

```r
source("analysis.R")
```

The script writes tables and `sessionInfo.txt` to `outputs/` and regenerates the figures in `figures/`.

## Expected checks

A successful run should recover the following central quantities:

- 172 field WD measurements from 31 species.
- 30 species in the overall paired field-versus-GWDD comparison.
- Mean field WD ≈ 0.566 g cm^-3 and mean GWDD trunk WD ≈ 0.600 g cm^-3.
- Mean paired difference ≈ 0.033 g cm^-3; paired t-test p ≈ 0.022.
- 24 species eligible for species-specific Welch tests and 7 species with pBH < 0.05.
- 172 trees in the sampled-tree biomass sensitivity analysis.
- Aggregate biomass differences of approximately +3.87% with Chave et al. (2014) and +3.66% with Nogueira Junior et al. (2014).
- 1,136 inventory individuals from 30 species in inventory scaling, representing approximately 76.9% of inventory individuals and 91.0% of total basal area.
- Inventory-based biomass differences ranging from approximately +4.15% to +7.48%, depending on site and equation.

## GWDD source

Reference data were obtained from Global Wood Density Database v.2.2, published 23 June 2026. For exact analytical reproducibility, the version-specific record is:

- GWDD v.2.2: DOI `10.5281/zenodo.20815517`

The GWDD record requests citation of the database repository and Fischer et al. (2026), *Beyond species means – the intraspecific contribution to global wood density variation*, *New Phytologist* 249: 2630–2651, DOI `10.1111/nph.70860`.

## License and external data

The study-authored material is distributed under the license provided in `LICENSE`. GWDD-derived values and records remain attributable to the original GWDD source and should be cited accordingly.
