<!-- See http://style.tidyverse.org/news.html for advice on writing news -->


# ggforestplot 0.0.1

Initial GitHub release

# ggforestplot 0.0.2

## Minor changes

- Change tick breaks algorithm in `forestplot()` when `logodds = TRUE` with an
example for explicit definition of tick marks.
- Correct typos in vignettes.
- Updates for tidyr 1.0.0
- Updates for dplyr 0.8.3

# ggforestplot 0.1.0

## Changes

- `plot_all_NG_biomarkers()` accepts now layout as an input parameter.
Layout can be one of predefined layouts in `df_grouping_all_NG_biomarkers` or a
user provided `tibble`.
- `forestplot()` takes also x-tick breaks as an input parameter.
- `discovery_regression()` accepts factor predictors. In this case an additional
column with term is returned.
- `df_NG_biomarker_metadata` updated to include new platform biomarkers. `unit`
column is set to "deprecated" since different quantification versions can have
different units. Users interested in plotting their biomarker data should read
units from their own files.
- `df_grouping_all_NG_biomarkers` was updated to contain layouts for 2016 and
2020 Nighitingale Heath Ltd. platforms.
- `df_linear_associations` and `df_logodds_associations` where updated with new
names for some biomarkers.
- `df_demo_metabolomic_data`, `data-raw/metabolomics_data.zip` and the same data
in `vignettes/data` was updated to include new biomarkers.
- Vignettes updated to reflect the number of biomarkers in current 2020 platform,
while `ggforce::facet_col()` is used in examples instead of
`patchwork::wrap_plots()`.
