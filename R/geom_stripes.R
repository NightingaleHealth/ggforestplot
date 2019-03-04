#' Alternating Background Colour
#'
#' Add alternating background color along the y-axis. The geom takes default
#' aesthetics \code{odd} and \code{even} that receive color codes. The codes
#' would preferably be in the 8-hex ARGB format to allow for transparency if
#' the geom is meant to be used as visual background.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
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
#'       colour = trait,
#'       shape = trait,
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
#'   geom_stripes(odd = "#33333333", even = "#00000000") +
#'   # Add vertical line at null point
#'   geom_vline(
#'     xintercept = 0,
#'     linetype = "solid",
#'     size = 0.4,
#'     colour = "black"
#'   )
#' print(g)
geom_stripes <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStripes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

GeomStripes <- ggplot2::ggproto("GeomStripes", ggplot2::Geom,
  required_aes = c("y"),

  default_aes = ggplot2::aes(
    xmin = -Inf, xmax = Inf,
    odd = "#22222222", even = "#00000000",
    # Change 'size' below from 0 to NA.
    # When not NA then when *printing in pdf device* borders are there despite
    # requested 0th size. Seems to be some ggplot2 bug caused by grid overriding
    # an lwd parameter somewhere, unless the size is set to NA. Found solution here
    # https://stackoverflow.com/questions/43417514/getting-rid-of-border-in-pdf-output-for-geom-label-for-ggplot2-in-r
    alpha = NA, colour = "black", linetype = "solid", size = NA
  ),

  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(data, panel_params, coord) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          y = round(.data$y),
          ymin = .data$y - 0.5,
          ymax = .data$y + 0.5
        ) %>%
        dplyr::select(
          .data$xmin, .data$xmax,
          .data$ymin, .data$ymax,
          .data$odd, .data$even,
          .data$alpha, .data$colour, .data$linetype, .data$size
        ) %>%
        unique() %>%
        dplyr::arrange(.data$ymin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .data$.n %% 2L == 1L,
            true = .data$odd,
            false = .data$even
          )
        ) %>%
        dplyr::select(-.data$.n, -.data$odd, -.data$even),
      panel_params,
      coord
    )
  }
)
