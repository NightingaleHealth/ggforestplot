context("forestplot")

library(ggplot2)
library(vdiffr)
library(magrittr)

# Define df linear case
df_linear <-
  df_linear_associations %>%
  dplyr::arrange(name) %>%
  dplyr::filter(dplyr::row_number() < 30)

test_that("forestplot plots linear results", {
  vdiffr::expect_doppelganger(
    title = "Associations to BMI",
    fig = forestplot(
      df = df_linear,
      estimate = beta,
      logodds = FALSE,
      colour = trait,
      title = "Associations to BMI"
    ),
    path = "linear"
  )
})


test_that("forestplot plots linear results of a df with non-default names", {

  # Define df linear case with other than the default names
  df_linear <-
    df_linear %>%
    dplyr::rename(
      id = name,
      pheno = trait,
      betacoef = beta,
      secoef = se,
      pval = pvalue
    )

  vdiffr::expect_doppelganger(
    title = "Associations to BMI - non-default names",
    fig = forestplot(
      df = df_linear,
      name = id,
      estimate = betacoef,
      se = secoef,
      logodds = FALSE,
      colour = pheno,
      title = "Associations to BMI"
    ),
    path = "linear"
  )
})

test_that("forestplot ci parameter works", {
  vdiffr::expect_doppelganger(
    title = "Associations to BMI - CI",
    fig = forestplot(
      df = df_linear,
      estimate = beta,
      logodds = FALSE,
      colour = trait,
      ci = 0.80,
      title = "Associations to BMI"
    ),
    path = "linear"
  )
})

# Log odds associations
df_logodds <-
  df_logodds_associations %>%
  dplyr::arrange(name) %>%
  dplyr::filter(dplyr::row_number() < 30) %>%
  # Set the study variable to a factor to preserve order of appearance
  # Set class to factor to set order of display.
  dplyr::mutate(
    study = factor(
      study,
      levels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
    )
  )

test_that("forestplot plots log odds", {

  vdiffr::expect_doppelganger(
    title = "logodds1",
    fig = forestplot(
      df = df_logodds,
      estimate = beta,
      logodds = TRUE,
      colour = study,
      title = "Associations to type 2 diabetes"
    ),
    path = "logodds"
  )

  vdiffr::expect_doppelganger(
    title = "logodds2",
    fig = forestplot(
      df = df_logodds,
      estimate = beta,
      logodds = TRUE,
      colour = study,
      shape = study,
      title = "Associations to type 2 diabetes"
    ) +
      ggplot2::coord_cartesian(xlim = c(0.2, 2.0)) +
      # You may also want to add a manual shape to mark meta-analysis with a
      # diamond shape
      ggplot2::scale_shape_manual(
        values = c(23L, 21L, 21L, 21L, 21L),
        labels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
      ),
    path = "logodds"
  )
})

test_that("forestplot plots log results of a df with non-default names", {

  df_logodds <-
    df_logodds %>%
    dplyr::rename(
      id = name,
      pheno = study,
      betacoef = beta,
      secoef = se,
      pval = pvalue
    )

  vdiffr::expect_doppelganger(
    title = "Logodds - non-default names",
    fig = forestplot(
      df = df_logodds,
      name = id,
      estimate = betacoef,
      se = secoef,
      logodds = TRUE,
      colour = pheno,
      title = "Associations to type 2 diabetes"
    ),
    path = "linear"
  )
})

test_that("forestplot plots non-significant values as hollow", {

  vdiffr::expect_doppelganger(
    title = "logodds3",
    fig = forestplot(
      df = df_logodds,
      estimate = beta,
      logodds = TRUE,
      colour = study,
      pvalue = pvalue,
      psignif = 0.03,
      title = "Associations to type 2 diabetes"
    ),
    path = "logodds"
  )
})

