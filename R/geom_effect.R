#' Horizontal Study Effects with Confidence Intervals
#'
#' Builds a custom version of \code{\link[ggstance]{geom_pointrangeh}}.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggstance::geom_pointrangeh
#' @author Ilari Scheinin
#' @export
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' df <-
#'   # Use built-in demo dataset
#'   df_linear_associations %>%
#'     # Arrange by name in order to filter the first few biomarkers for more
#'     # than one studies
#'     dplyr::arrange(name) %>%
#'     # Estimate confidence intervals
#'     dplyr::mutate(
#'       xmin = beta - qnorm(1 - (1 - 0.95) / 2) * se,
#'       xmax = beta + qnorm(1 - (1 - 0.95) / 2) * se
#'     ) %>%
#'     # Select only first 30 rows (10 biomarkers)
#'     dplyr::filter(dplyr::row_number() <= 30) %>%
#'     # Add a logical variable for statistical significance
#'     dplyr::mutate(filled = pvalue < 0.001)
#'
#' g <-
#'   ggplot(data = df, aes(x = beta, y = name)) +
#'   # And point+errorbars
#'   geom_effect(
#'     ggplot2::aes(
#'       xmin = xmin,
#'       xmax = xmax,
#'       colour = study,
#'       shape = study,
#'       filled = filled
#'     ),
#'     position = ggstance::position_dodgev(height = 0.5)
#'   )
#' print(g)
#'
#' # Add custom theme, horizontal gray rectangles, vertical line to signify the
#' # NULL point, custom color palettes.
#' g <-
#'   g +
#'   # Add custom theme
#'   theme_forest() +
#'   # Add striped background
#'   geom_stripes() +
#'   # Add vertical line at null point
#'   geom_vline(
#'     xintercept = 0,
#'     linetype = "solid",
#'     size = 0.4,
#'     colour = "black"
#'   )
#' print(g)
geom_effect <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = ggstance::position_dodgev(height = 0.5),
                        ...,
                        fatten = 2,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEffect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fatten = fatten,
      na.rm = na.rm,
      ...
    )
  )
}

GeomEffect <- ggproto("GeomEffect", Geom,
  default_aes = aes(
    colour = "black", size = 0.6, linetype = 1, shape = 21,
    fill = "black", alpha = NA, stroke = 1, filled = TRUE
  ),

  # Implement draw_key
  draw_key = function(data, params, size) {
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    grid::pointsGrob(
      0.5, 0.5,
      pch = data$shape,
      gp = grid::gpar(
        col = scales::alpha(data$colour, data$alpha),
        # fill = scales::alpha(data$fill, data$alpha),
        fill = scales::alpha(data$colour, data$alpha),
        fontsize = data$size * .pt + data$stroke * .stroke / 2,
        lwd = data$stroke * .stroke / 2
      )
    )
  },

  # TODO: guide for significance, when specified

  required_aes = c("x", "y", "xmin", "xmax"),

  # TODO: check for the number of shapes?

  draw_panel = function(data,
                          panel_params,
                          coord,
                          fatten = 2) {
    ggstance::GeomPointrangeh$draw_panel(
      # @Ilari Indeed, the transform() is needed here for fatten to go through
      transform(data, fatten = fatten) %>%
        dplyr::mutate(
          fill = dplyr::case_when(
            is.na(.data$filled) ~ "#00000000",
            !.data$filled ~ "white",
            TRUE ~ .data$colour
          )
        ),
      panel_params,
      coord
    )
  }
)
