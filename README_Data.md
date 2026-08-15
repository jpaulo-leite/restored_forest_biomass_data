# Data dictionary

## `field_WD_measurements.csv`

One row per field WD measurement.

- `species`: harmonized binomial species name.
- `wood_density_g_cm3`: field-measured WD in g cm^-3.

## `sampled_trees.csv`

One row per tree included in the 172-tree biomass sensitivity analysis.

- `species`: harmonized binomial species name.
- `treatment`, `site`, `block`, `plot`, `tree_id`: sampling identifiers.
- `family`: botanical family.
- `DBH_cm`: diameter at breast height in cm.
- `height_m`: total tree height in m.

## `inventory_2020_2021.csv`

One row per inventory individual.

- `treatment`, `site`, `block`, `plot`, `tree_id`: inventory identifiers.
- `family`: botanical family.
- `scientific_name`: scientific name as recorded in the inventory.
- `height_m`: total height in m.
- `DBH1_cm`–`DBH11_cm`: stem diameters in cm. For multistemmed individuals, `analysis.R` calculates the equivalent diameter as the square root of the sum of squared stem diameters.

## `species_metadata.csv`

- `species`: species name.
- `WD_measurement_method`: gravimetric determination or X-ray densitometry.

## `GWDDv2_reference_values_study_species.csv`

Reference values used for WD substitution.

- `species`: study species.
- `gwdd_match`: taxon used to retrieve the GWDD estimate.
- `reference_level`: `species` or `genus`.
- `gwdd_records`, `gwdd_sources`, `gwdd_countries`, `gwdd_sites`: GWDD aggregation metadata.
- `gwdd_trunk_WD_g_cm3`: trunk-specific reference WD used in the analysis.

For *Ficus guaranitica*, `gwdd_match = Ficus` and `reference_level = genus`; the associated record counts therefore describe the genus, not *F. guaranitica*.

## `GWDDv2_filtered_records_study_species.csv`

Species-level GWDD records retained for the Welch comparisons. These records are used only for the species-specific inferential comparisons; biomass substitution uses the trunk-specific reference estimate in `GWDDv2_reference_values_study_species.csv`.

## Output files

- `Table_S1_species_WD_GWDDv2.csv`: field WD summaries, GWDD reference information, Welch tests, and Benjamini–Hochberg-adjusted p-values.
- `Table_S2_species_AGB_sensitivity.csv`: species-level WD mismatch and deterministic proportional biomass sensitivity for both equations.
- `Table_2_inventory_AGB_species_in_study.csv`: inventory-based biomass scaling by site and equation, including the illustrative carbon-stock conversion.
- `global_paired_WD_test.csv`: overall paired comparison across the 30 species-level GWDD matches.
- `sampled_tree_AGB_summary.csv`: summary of the 172-tree biomass substitution analysis.
- `tree_sample_AGB_recalculated.csv`: tree-level biomass values for both WD scenarios.
- `inventory_scaling_summary.csv`: inventory coverage by individuals and basal area.
- `inventory_taxonomic_handling.csv`: taxonomic decisions relevant to inventory matching.
