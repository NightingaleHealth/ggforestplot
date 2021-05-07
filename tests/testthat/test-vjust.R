context("Geom testing (justification)")

library(ggplot2)
library(vdiffr)
library(magrittr)


testthat::test_that("vjust works",{

  vdiffr::expect_doppelganger(
    title = "stripe_vjust = 0.5 (default)",
    fig = mtcars %>% tibble::rownames_to_column() %>%
      ggplot() +
      aes(x = mpg, y = rowname) +
      geom_point() +
      geom_stripes(),
    path = "stripe"
  )

  vdiffr::expect_doppelganger(
    title = "stripe_vjust = 0",
    fig = mtcars %>% tibble::rownames_to_column() %>%
            ggplot() +
            aes(x = mpg, y = rowname) +
            geom_point(position = position_nudge(x = 0, y = 0.5)) +
            geom_stripes(vjust = 0) +
            theme(axis.text.y=element_text(hjust = 0.3, vjust = -0.5)) +
            scale_y_discrete(expand = expansion(add = c(0, 1))),
    path = "stripe"
    )

})
