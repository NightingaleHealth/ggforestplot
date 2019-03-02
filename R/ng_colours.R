ng_colour_list <-
  c(
    `night` = "#323D5A",
    `warm red` = "#FF4A4F",
    `electro` = "#00C0C0",
    `violet` = "#8F87FF",
    `pesto` = "#CCCF40",

    `mid night` = "#508CC8",
    `dark red` = "#A0192D",
    `dark electro` = "#2D506E",
    `dark violet` = "#5028C8",
    `dark pesto` = "#006E00",

    `light night` = "#C8DCF0",
    `light red` = "#FFC9CA",
    `light electro` = "#BEF0F0",
    `light violet` = "#DCDCFF",
    `light pesto` = "#78D700",

    `yellow` = "#FFFF00",

    `black` = "#000000",
    `graphite` = "#4D4D4D",
    `fog` = "#E6E6E6",
    `white` = "#FFFFFF"
  )

#' Nightingale's colours
#'
#' \code{ng_colour()} returns Nightingale's colours' hex codes.
#' \code{display_ng_colours()} displays the avaliable colours, with their names
#' and hex codes, in a vertical layout.
#'
#' @param ... Character names of Nightingale's colours.
#' @author Ilari Scheinin
#' @export
#' @examples
#' # Display Nightingale's colours
#' display_ng_colours()
#'
#' # Request for the hex code of colour 'dark pesto'
#' ng_colour("dark pesto")
ng_colour <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(ng_colour_list)
  }

  unname(ng_colour_list[cols])
}

#' @export
#' @rdname ng_colour
display_ng_colours <- function() {
  plot_label <- function(labels, ...) {
    graphics::plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
    graphics::text(0.5, 0.5, labels = labels, ...)
  }

  old_par <- graphics::par(
    mfrow = c(length(ng_colour_list) + 1L, 3L),
    mar = c(0, 0, 0, 0) + 0.1
  )

  plot_label("Name", cex = 2)
  plot_label("Colour", cex = 2)
  plot_label("Hex", cex = 2)

  purrr::iwalk(ng_colour_list, function(colour, name) {
    plot_label(name, cex = 1.5)

    graphics::barplot(1L, col = ng_colour(name), axes = FALSE)

    plot_label(colour, cex = 1.5)
  })

  graphics::par(old_par)

  invisible()
}

ng_palette_list <-
  list(
    primary = ng_colour("night", "warm red", "electro", "violet", "pesto"),

    dark = ng_colour(
      "mid night", "dark red", "dark electro",
      "dark violet", "dark pesto"
    ),
    light = ng_colour(
      "light night", "light red", "light electro",
      "light violet", "light pesto"
    ),
    accents = ng_colour(
      "mid night", "light night", "dark red", "light red",
      "dark electro", "light electro", "dark violet", "light violet",
      "dark pesto", "light pesto"
    ),

    supportive = ng_colour("yellow"),

    blacks = ng_colour("black", "graphite", "fog", "white"),

    night = ng_colour("night", "mid night", "light night"),
    red = ng_colour("dark red", "warm red", "light red"),
    electro = ng_colour("dark electro", "electro", "light electro"),
    violet = ng_colour("dark violet", "violet", "light violet"),
    pesto = ng_colour("dark pesto", "pesto", "light pesto"),

    nwr = ng_colour("night", "electro", "white", "warm red", "dark red"),
    pwr = ng_colour(
      "dark pesto", "light pesto", "white", "warm red",
      "dark red"
    ),
    nwp = ng_colour("night", "electro", "white", "light pesto", "dark pesto"),
    pyr = ng_colour("dark pesto", "yellow", "warm red"),
    py = ng_colour("dark pesto", "yellow"),
    ne = ng_colour("night", "electro", "light electro"),
    new = ng_colour("night", "electro", "white"),
    rw = ng_colour("dark red", "warm red", "white"),
    pw = ng_colour("dark pesto", "light pesto", "white"),

    all = ng_colour(names(ng_colour_list))
  )

#' Nightingale's colour palettes
#'
#' \code{ng_palette_d()} and \code{ng_palette_c()} (respectively for discrete
#' and continuous palettes) return functions that take an integer argument (the
#' required number of colours) and return a character vector of colours' hex
#' codes.
#' In addition, the functions also recognize the
#' \code{\link[viridisLite:viridis]{viridis}} palettes: "magma" (or "A"),
#' "inferno" ("B"), "plasma" ("C"), "viridis" ("D"), or "cividis" ("D").
#'
#' @param name Character name of the Nightingale (or viridis) colour palette.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments to pass to
#'        \code{\link[grDevices:colorRampPalette]{colorRampPalette()}}
#'
#' @rdname ng_palette
#' @author Ilari Scheinin
#' @export
#' @examples
#' #' # Display Nightingale's colour palettes
#' display_ng_palettes()
#'
#' # Get 4 colours along the spectrum of the nwr palette
#' ng_palette_d("nwr")(4)
#'
#' # Notice that the discrete palette "light", cannot return more than 5 colours
#' ng_palette_d("light")(6)
#'
ng_palette_d <- function(name = "all", reverse = FALSE) {
  if (name %in% names(ng_palette_list)) {
    function(n) {
      pal <- ng_palette_list[[name]]
      if (n > length(pal)) {
        warning(
          'n too large, allowed maximum for palette "',
          name, '" is ', length(pal),
          call. = FALSE
        )
        pal <- rep(pal, length.out = n)
      } else {
        pal <- pal[seq_len(n)]
      }
      if (reverse) pal <- rev(pal)
      pal
    }
  } else if (name %in% c(
    "A", "magma",
    "B", "inferno",
    "C", "plasma",
    "D", "viridis",
    "E", "cividis"
  )) {
    scales::viridis_pal(
      option = name,
      direction = dplyr::if_else(reverse, true = -1L, false = 1L)
    )
  } else {
    stop("Unknown palette: ", name, call. = FALSE)
  }
}

#' @rdname ng_palette
#' @export
#'
ng_palette_c <- function(name = "magma", reverse = FALSE, ...) {
  if (name %in% names(ng_palette_list)) {
    pal <- ng_palette_list[[name]]
    if (reverse) pal <- rev(pal)
    grDevices::colorRampPalette(pal, ...)
  } else if (name %in% c(
    "A", "magma",
    "B", "inferno",
    "C", "plasma",
    "D", "viridis",
    "E", "cividis"
  )) {
    scales::viridis_pal(
      option = name,
      direction = dplyr::if_else(reverse, true = -1L, false = 1L)
    )
  } else {
    stop("Unknown palette: ", name, call. = FALSE)
  }
}

#' @export
#' @rdname ng_palette
display_ng_palettes <- function() {
  plot_label <- function(labels, ...) {
    graphics::plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
    graphics::text(0.5, 0.5, labels = labels, ...)
  }

  old_par <- graphics::par(
    mfrow = c(length(ng_palette_list) + 1L, 3L),
    mar = c(0, 0, 0, 0) + 0.1
  )

  plot_label("Palette", cex = 2)
  plot_label("Discrete", cex = 2)
  plot_label("Continuous", cex = 2)

  purrr::iwalk(ng_palette_list, function(colours, name) {
    plot_label(name, cex = 1.5)

    graphics::barplot(
      rep(1L, length(colours)),
      col = ng_palette_d(name)(length(colours)),
      axes = FALSE)

    graphics::image(
      z = matrix(1L:256L, ncol = 1L),
      col = ng_palette_c(name)(256L),
      axes = FALSE
    )
    graphics::rect(0, -1, 1, 1)
  })

  graphics::par(old_par)

  invisible()
}

#' Colour scale constructor for Nightingale colours
#'
#' @inheritParams ng_palette_d
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams scales::gradient_n_pal
#' @param ... Additional arguments passed to
#'        \code{\link[ggplot2:discrete_scale]{discrete_scale()}} or
#'        \code{\link[ggplot2:continuous_scale]{continuous_scale()}} to control
#'        name, limits, breaks, labels and so forth.
#' @param palette Character name of the Nightingale (or viridis) colour palette.
#' @param aesthetics Character string or vector of character strings listing the
#'        name(s) of the aesthetic(s) that this scale works with. This can be
#'        useful, for example, to apply colour settings to the `colour` and
#'        `fill` aesthetics at the same time, via
#'        `aesthetics = c("colour", "fill")`.
#'
#' @rdname ng_scale
#' @author Ilari Scheinin
#' @export
#' @examples
#' # Example taken from ggplot2::scale_colour_discrete()
#' dsamp <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000), ]
#' d <- ggplot2::ggplot(dsamp, ggplot2::aes(carat, price)) +
#'   ggplot2::geom_point(ggplot2::aes(colour = clarity)) +
#'   ggforestplot::scale_colour_ng_d()
#' print(d)
scale_colour_ng_d <- function(..., palette = "all", reverse = FALSE,
                              aesthetics = "colour") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = paste0("ng_d_", palette),
    palette = ng_palette_d(name = palette, reverse = reverse),
    ...
  )
}

#' @rdname ng_scale
#' @export
#'
scale_fill_ng_d <- function(..., palette = "all", reverse = FALSE,
                            aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = paste0("ng_d_", palette),
    palette = ng_palette_d(name = palette, reverse = reverse),
    ...
  )
}

#' @rdname ng_scale
#' @export
#'
scale_colour_ng_c <- function(..., palette = "magma", reverse = FALSE,
                              values = NULL, space = "Lab", na.value = "grey50",
                              guide = "colourbar", aesthetics = "colour") {
  ggplot2::continuous_scale(
    aesthetics = aesthetics,
    scale_name = paste0("ng_c_", palette),
    palette = scales::gradient_n_pal(
      ng_palette_c(name = palette, reverse = reverse)(6L),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @rdname ng_scale
#' @export
#'
scale_fill_ng_c <- function(..., palette = "magma", reverse = FALSE,
                            values = NULL, space = "Lab", na.value = "grey50",
                            guide = "colourbar", aesthetics = "fill") {
  ggplot2::continuous_scale(
    aesthetics = aesthetics,
    scale_name = paste0("ng_c_", palette),
    palette = scales::gradient_n_pal(
      ng_palette_c(name = palette, reverse = reverse)(6L),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}
