context("plot_all_NG_biomarkers")

library(ggplot2)
library(vdiffr)
library(magrittr)

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

test_that("plot_all_NG_biomarkers produces file ", {

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  
  expect_message(
    plot_all_NG_biomarkers(
      df = df,
      machine_readable_name = machine_readable_name,
      estimate = beta,
      se = se,
      pvalue = pvalue,
      colour = trait,
      filename = file.path(tmp, "plot_all.pdf"),
      layout = "2016",
      xlab = "1-SD increment in BMI
    per 1-SD increment in biomarker concentration"
    ),
    paste("Saving", file.path(tmp, "plot_all.pdf"))
  )
  
  expect_true(file.exists(file.path(tmp, "plot_all.pdf")))
})

test_that("plot_all_NG_biomarkers throws error if layout tibble does not have required columns ", {
  
  expect_error(
    plot_all_NG_biomarkers(
      df = df,
      machine_readable_name = machine_readable_name,
      estimate = beta,
      se = se,
      pvalue = pvalue,
      colour = trait,
      layout = tibble::tibble(),
      filename = file.path(tmp, "plot_all.pdf"),
      xlab = "1-SD increment in BMI
    per 1-SD increment in biomarker concentration"
    ),
    paste(
      "layout tibble should have columns",
      "machine_readable_name, group_custom, column, page"
    ),
    fixed = TRUE
  )
})

test_that("plot_all_NG_biomarkers throws error if layout is not string or tibble ", {

  expect_error(
    plot_all_NG_biomarkers(
      df = df,
      machine_readable_name = machine_readable_name,
      estimate = beta,
      se = se,
      pvalue = pvalue,
      colour = trait,
      layout = 1,
      filename = file.path(tmp, "plot_all.pdf"),
      xlab = "1-SD increment in BMI
    per 1-SD increment in biomarker concentration"
    ),
    paste0(
      "layout has to be either string (one of ",
      paste0(ggforestplot::df_grouping_all_NG_biomarkers$version, collapse = ", "),
      ") or tibble (with columns machine_readable_name, group_custom, column and page)"
    ),
    fixed = TRUE
  )
})
