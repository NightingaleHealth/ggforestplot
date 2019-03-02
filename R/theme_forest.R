#' Forestplot Theme
#'
#' A custom theme used in \code{\link{forestplot}} that builts upon
#' \link[ggplot2]{theme_minimal}.
#'
#' @inheritParams ggplot2::theme_minimal
#' @import ggplot2
#' @importFrom grid unit.c
#' @export
#' @author Maria Kalimeri
#' @seealso \code{\link{forestplot}}, \code{\link{geom_effect}}, \code{\link{geom_stripes}}

theme_forest <- function(base_size = 13,
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      rect = element_blank(),
      text = element_text(
        colour = "black"
      )
    ) %+replace%
    ggplot2::theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0
      ),
      axis.text.y = element_text(
        colour = "black"
      ),
      axis.text.y.right = element_text(
        hjust = 1
      ),
      axis.text.x = element_text(
        colour = "black"
      ),
      panel.border = element_blank(),
      strip.text = element_text(
        face = "bold",
        hjust = 0
      ),
      panel.background = element_rect(
        colour = NA,
        fill = NA
      ),
      panel.grid.major.x = element_line(
        colour = "gray50",
        size = 0.25,
        linetype = 2
      ),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}
