context("ng_colours")

test_that("ng_colour works", {
  expect_equal(ng_colour("night"), "#323D5A")
  expect_equal(ng_colour("giraffe"), NA_character_)
  expect_equal(ng_colour(), ng_colour_list)
})

test_that("ng_palette works", {
  expect_is(ng_palette_d(), "function")
  expect_is(ng_palette_c(), "function")
  expect_equal(
    ng_palette_d("primary")(2L),
    ng_palette_list$primary[1L:2L]
  )
  expect_equal(
    ng_palette_d("primary", reverse = TRUE)(2L),
    ng_palette_list$primary[2L:1L]
  )
  expect_equal(
    ng_palette_c("nwr", reverse = TRUE)(3L),
    rev(ng_palette_c("nwr")(3L))
  )
  expect_error(ng_palette_d("giraffe"), "Unknown palette")
  expect_error(ng_palette_c("giraffe"), "Unknown palette")
  expect_warning(ng_palette_d()(99L), "n too large")
})

test_that("ng_scale works", {
  library(ggplot2)
  library(vdiffr)
  expect_doppelganger(
    "Discrete colour scale",
    ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
      geom_point() +
      scale_colour_ng_d()
  )
  expect_doppelganger(
    "Continuous colour scale",
    ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Petal.Length)) +
      geom_point() +
      scale_colour_ng_c()
  )
  expect_doppelganger(
    "Discrete fill scale",
    ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) +
      geom_raster() +
      scale_fill_ng_d(palette = "viridis")
  )
  expect_doppelganger(
    "Continuous fill scale",
    ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Petal.Length)) +
      geom_raster() +
      scale_fill_ng_c(palette = "nwr")
  )
  expect_doppelganger(
    "Available palettes",
    display_ng_palettes
  )
  expect_doppelganger(
    "Available colours",
    display_ng_colours
  )
})

