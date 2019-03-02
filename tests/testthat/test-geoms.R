context("Geom testing")

library(ggplot2)
library(vdiffr)
library(magrittr)

# Define input df
df <-
  # Use built-in demo dataset
  df_linear_associations %>%
  # Arrange by name in order to filter the first few biomarkers for more
  # than one studies
  dplyr::arrange(name) %>%
  # Estimate confidence intervals
  dplyr::mutate(
    xmin = beta - qnorm(1 - (1 - 0.95) / 2) * se,
    xmax = beta + qnorm(1 - (1 - 0.95) / 2) * se
  ) %>%
  # Select only first 30 rows (10 biomarkers)
  dplyr::filter(dplyr::row_number() <= 30) %>%
  # Add a logical variable for statistical significance
  dplyr::mutate(filled = pvalue < 0.001)

g <-
  ggplot(data = df, aes(x = beta, y = name))

test_that("geom_effect works", {
  vdiffr::expect_doppelganger(
    title = "geom-effect",
    fig =
      g + geom_effect(
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          colour = study,
          shape = study,
          filled = filled
        ),
        position = ggstance::position_dodgev(height = 0.5)
      ) +
      theme(panel.grid.major = element_line()),
    path = "geom_effect"
  )
})


test_that("geom_stripes works", {
  vdiffr::expect_doppelganger(
    title = "geom-stripes",
    fig =
      g + geom_effect(
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          colour = study,
          shape = study,
          filled = filled
        ),
        position = ggstance::position_dodgev(height = 0.5)
      ) +
      theme_minimal() +
      geom_stripes(),
    path = "geom_stripes"
  )
})

test_that("theme_forest works", {
  vdiffr::expect_doppelganger(
    title = "theme-forest",
    fig =
      g + geom_effect(
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          colour = study,
          shape = study,
          filled = filled
        ),
        position = ggstance::position_dodgev(height = 0.5)
      ) +
      theme_forest(),
    path = "theme_forest"
  )
})
