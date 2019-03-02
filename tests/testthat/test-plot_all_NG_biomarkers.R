context("plot_all_NG_biomarkers")

library(ggplot2)
library(vdiffr)
library(magrittr)

test_that("plot_all_NG_biomarkers works ", {

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  df <-
    df_linear_associations %>%
    left_join(
      select(
        df_NG_biomarker_metadata,
        name,
        machine_readable_name
      ),
      by = "name"
    )

  expect_true({
    # Print effect sizes for all Nightingale biomarkers in a 2-page pdf
    plot_all_NG_biomarkers(
      df = df,
      machine_readable_name = machine_readable_name,
      # Notice we don't need to define explicitly 'name' as a name variable is
      # already present in df and picked up automatically as y-axis labels.
      estimate =beta,
      se = se,
      pvalue = pvalue,
      colour = study,
      filename = file.path(tmp, "plot_all.pdf"),
      xlab = "1-SD increment in BMI
    per 1-SD increment in biomarker concentration"
    )

    plot_all <- file.path(tmp, "plot_all.pdf")
    file.exists(plot_all)
  })
})
