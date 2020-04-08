#' Draw a Forestplot of Measures of Effects
#'
#' Visualize multiple measures of effect with their confidence intervals in a
#' vertical layout.
#'
#' @param df A data frame with the data to plot. It must contain at least three
#' variables, a character column with the names to be displayed on the y-axis
#' (see parameter \code{name}), a numeric column with the value (or the log of the
#' value) to display (see parameter \code{estimate}) and a numeric value with
#' the corresponding standard errors (see parameter \code{se}). It may contain
#' additional columns, e.g. the corresponding p-values (see parameter \code{pvalue})
#' in which case, in conjuction with the threshold given in \code{psignif}, the
#' non-significant results will be displayed as hollow points. Other variables
#' may be used as aesthetics to define the colour and the shape of the points
#' to be plotted.
#' @param name the variable in \code{df} that contains the y-axis
#' names. This argument is automatically \link[rlang:quotation]{quoted} and
#' \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.
#' See Note.
#' @param estimate the variable in \code{df} that contains the values (or log of
#' values) to be displayed. This argument is automatically
#' \link[rlang:quotation]{quoted} and \link[rlang:eval_tidy]{evaluated} in the
#' context of the \code{df} data frame.
#' See Note.
#' @param se the variable in the \code{df} data frame that contains the standard
#' error values. This argument is automatically \link[rlang:quotation]{quoted}
#' and \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data
#' frame. See Note.
#' @param pvalue the variable in \code{df} that contains the
#' p-values. Defaults to NULL. When explicitly defined, in conjuction with
#' the p-value threshold provided in the \code{psignif}, the non-significant
#' entries will be drawn as hollow points. This argument is automatically
#' \link[rlang:quotation]{quoted} and \link[rlang:eval_tidy]{evaluated} in the
#' context of the \code{df} data frame. See Note.
#' @param colour the variable in \code{df} by which to colour the different
#' groups of points. This argument is automatically \link[rlang:quotation]{quoted} and
#' \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.
#' See Note.
#' @param shape the variable in \code{df} by which to shape the different groups of
#' points. This argument is automatically \link[rlang:quotation]{quoted} and
#' \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.
#' See Note.
#' @param logodds logical (defaults to FALSE) specifying whether the \code{estimate}
#' parameter should be treated as log odds/hazards ratio (TRUE) or not (FALSE). When
#' \code{logodds} = TRUE the estimates and corresponding confidence intervals will be
#' exponentiated and a log scale will be used for the x-axis.
#' @param psignif numeric, defaults to 0.05. The p-value threshold
#' for statistical significance. Entries with larger than \code{psignif} will be
#' drawn with a hollow point.
#' @param ci A number between 0 and 1 (defaults to 0.95) indicating the type of
#' confidence interval to be drawn.
#' @param ... \code{ggplot2} graphical parameters such as \code{title},
#' \code{ylab}, \code{xlab}, \code{xtickbreaks} etc. to be passed along.
#' @return A \code{ggplot} object.
#' @note  See \code{vignette(programming, package = "dplyr")} for an
#' introduction to non-standard evaluation.
#' @author Maria Kalimeri, Ilari Scheinin, Vilma Jagerroos
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom stats qnorm
#' @importFrom magrittr %>%
#' @importFrom rlang := !! enquo quo_is_null
#' @importFrom grDevices axisTicks
#' @importFrom scales trans_breaks
#' @examples
#' library(magrittr)
#'
#' # Linear associations
#' # Get subset of example data frame
#' df_linear <-
#'   df_linear_associations %>%
#'   dplyr::arrange(name) %>%
#'   dplyr::filter(dplyr::row_number() <= 30)
#'
#' # Forestplot
#' forestplot(
#'   df = df_linear,
#'   estimate = beta,
#'   logodds = FALSE,
#'   colour = trait,
#'   xlab = "1-SD increment in cardiometabolic trait
#'   per 1-SD increment in biomarker concentration"
#' )
#'
#' # Log odds ratios
#' df_logodds <-
#'   df_logodds_associations %>%
#'   dplyr::arrange(name) %>%
#'   dplyr::filter(dplyr::row_number() <= 30) %>%
#'   # Set the study variable to a factor to preserve order of appearance
#'   # Set class to factor to set order of display.
#'   dplyr::mutate(
#'     study = factor(
#'       study,
#'       levels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
#'     )
#'   )
#'
#' # Forestplot
#' forestplot(
#'   df = df_logodds,
#'   estimate = beta,
#'   logodds = TRUE,
#'   colour = study,
#'   xlab = "Odds ratio for incident type 2 diabetes (95% CI)
#'   per 1-SD increment in biomarker concentration"
#' )
#'
#' # For the latter, if you want to restrain the x-axis and crop the large
#' # errorbar for Acetate you may add the following coord_cartesian layer
#' forestplot(
#'   df = df_logodds,
#'   estimate = beta,
#'   logodds = TRUE,
#'   colour = study,
#'   shape = study,
#'   xlab = "Odds ratio for incident type 2 diabetes (95% CI)
#'   per 1-SD increment in biomarker concentration",
#'   xlim = c(0.5, 2.2),
#'   # You can explicitly define x-tick breaks
#'   xtickbreaks = c(0.5, 0.8, 1.0, 1.2, 1.5, 2.0)
#' ) +
#'   # You may also want to add a manual shape to mark meta-analysis with a
#'   # diamond shape
#'   ggplot2::scale_shape_manual(
#'     values = c(23L, 21L, 21L, 21L, 21L),
#'     labels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
#'   )

forestplot <- function(df,
                       name = name,
                       estimate = estimate,
                       se = se,
                       pvalue = NULL,
                       colour = NULL,
                       shape = NULL,
                       logodds = FALSE,
                       psignif = 0.05,
                       ci = 0.95,
                       ...) {

  # Input checks
  stopifnot(is.data.frame(df))
  stopifnot(is.logical(logodds))

  # TODO: Add some warnings when name, estimate etc are missing from df and user
  # is not defining the name, estimate etc explicitly.

  # Quote input
  name <- enquo(name)
  estimate <- enquo(estimate)
  se <- enquo(se)
  pvalue <- enquo(pvalue)
  colour <- enquo(colour)
  shape <- enquo(shape)
  
  args <- list(...)

  # TODO: Allow color instead of colour. This will do it, but also breaks other
  # options at the end, so fix those before uncommenting this.
  # args <- enquos(...)
  # if (quo_is_null(colour) && "color" %in% names(args)) {
  #   colour <- args$color
  # }

  # Estimate multiplication const for CI
  const <- stats::qnorm(1 - (1 - ci) / 2)

  # Adjust data frame variables
  df <-
    df %>%
    # Convert to a factor to preserve order.
    dplyr::mutate(
      !!name := factor(
        !!name,
        levels = !!name %>% unique() %>% rev(),
        ordered = TRUE
      ),
      # Added here to estimate xbreaks for log odds later
      .xmin = !!estimate - const * !!se,
      .xmax = !!estimate + const * !!se,
      # Add a logical variable with the info on whether points will be filled.
      # Defaults to TRUE.
      .filled = TRUE,
      # Add a variable with the estimates to be printed on the right side of y-axis
      .label = sprintf("%.2f", !!estimate)
    )

  # Exponentiate the estimates and CIs if logodds
  if (logodds) {
    df <-
      df %>%
      mutate(
        .xmin = exp(.data$.xmin),
        .xmax = exp(.data$.xmax),
        !!estimate := exp(!!estimate)
      )
  }

  # If pvalue provided, adjust .filled variable
  if (!quo_is_null(pvalue)) {
    df <-
      df %>%
      dplyr::mutate(.filled = !!pvalue < !!psignif)
  }

  # Plot
  g <-
    ggplot2::ggplot(
      df,
      aes(
        x = !!estimate,
        y = !!name
      )
    )

  # If logodds, adjust axis scale
  if (logodds) {
    if ("xtickbreaks" %in% names(args)) {
      g <-
        g +
        scale_x_continuous(
          trans = "log10",
          breaks = args$xtickbreaks
        )
    } else {
      g <-
        g +
        scale_x_continuous(
          trans = "log10",
          breaks = scales::log_breaks(n = 7)
        )
    }
  }

  g <-
    g +
    # Add custom theme
    theme_forest() +
    # Add Nightingale colour palette
    scale_colour_ng_d() +
    scale_fill_ng_d() +
    # Add striped background
    geom_stripes() +
    # Add vertical line at null point
    geom_vline(
      xintercept = ifelse(test = logodds, yes = 1, no = 0),
      linetype = "solid",
      size = 0.4,
      colour = "black"
    )

  g <-
    g +
    # And point+errorbars
    geom_effect(
      ggplot2::aes(
        xmin = .data$.xmin,
        xmax = .data$.xmax,
        colour = !!colour,
        shape = !!shape,
        filled = .data$.filled
      ),
      position = ggstance::position_dodgev(height = 0.5)
    ) +
    # Define the shapes to be used manually
    ggplot2::scale_shape_manual(values = c(21L, 22L, 23L, 24L, 25L)) +
    guides(
      colour = guide_legend(reverse = TRUE),
      shape = guide_legend(reverse = TRUE)
    )

  # Limits adjustment
  #
  # # Extend the shorter x-axis side to mirror the longer one
  # xext <-
  #   c(
  #     df[[quo_name(xmin)]],
  #     df[[quo_name(xmax)]]
  #   ) %>%
  #   abs() %>%
  #   max()
  # g <-
  #   g +
  #   ggplot2::expand_limits(x = c(-xext, xext))

  # If no groups specified (through either colour or shape), show estimate values.
  # ### Note: I had to switch back to row number as the y aesthetic in order to use
  # ### continuous scale and to be able to add a secondary axis for labels on the
  # ### right. Any other solution was too time consuming.
  # ### I also had to simplify the fi statement below cause when colour and/or
  # ### shape is specified a legend is added even if they are of length 1L and
  # ### messes up right side visuals.
  # if (
  #   (quo_is_null(colour) || length(unique(df[[quo_name(colour)]])) == 1L) &&
  #   (quo_is_null(shape) || length(unique(df[[quo_name(shape)]])) == 1L)
  # ) {
  # if ( quo_is_null(colour) && quo_is_null(shape)){
  #   g <-
  #     g +
  #     geom_text(
  #       aes(label = .label),
  #       x = 1.1 * xext,
  #       hjust = 1
  #     ) +
  #     expand_limits(x = 1.1 * xext)
  # } else {
  #   g <-
  #     g +
  #     ggplot2::scale_y_continuous(
  #       breaks = df %>% pull(.name_order),
  #       labels = df %>% pull(!!name)
  #     )
  # }

  # Pass through graphical parameters and define defaults values for some.
  if ("title" %in% names(args)) {
    g <- g + labs(title = args$title)
  }
  if ("subtitle" %in% names(args)) {
    g <- g + labs(subtitle = args$subtitle)
  }
  if ("caption" %in% names(args)) {
    g <- g + labs(caption = args$caption)
  }
  if ("xlab" %in% names(args)) {
    g <- g + labs(x = args$xlab)
  }
  if (!"ylab" %in% names(args)) {
    args$ylab <- ""
  }
  g <- g + labs(y = args$ylab)
  if ("xlim" %in% names(args)) {
    g <- g + coord_cartesian(xlim = args$xlim)
  }
  if ("ylim" %in% names(args)) {
    g <- g + ylim(args$ylim)
  }
  if ("xtickbreaks" %in% names(args) & !logodds) {
    g <- g + scale_x_continuous(breaks = args$xtickbreaks)
  }
  g
}
