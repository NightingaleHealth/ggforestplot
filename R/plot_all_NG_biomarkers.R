#' Print a Forestplot for All Nightingale Biomarkers
#'
#' Save a forestplot of all Nightingale biomarker associations in a 2-page,
#' predefined layout (utilizes \code{\link{forestplot}}).
#'
#' The function uses a custom grouping specified by
#' \code{\link{df_grouping_all_NG_biomarkers}}. The input \code{df} and
#' \code{df_grouping_all_NG_biomarkers} are joined by \code{machine_readable_name},
#' while another \code{df} variable may be used for y-axis labels, defined in
#' \code{name} input parameter.
#'
#' @inheritParams forestplot
#' @param machine_readable_name the variable in df containing the machine
#' readable names of Nightingale blood biomarkers. I.e. the names in this
#' variable must be the same as in the \code{machine_readable_name} variable of
#' \code{\link{df_NG_biomarker_metadata}}.
#' (This argument is automatically \link[rlang:quotation]{quoted} and
#' \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.)
#' @param name the variable in df with the Nightingale biomarker names to be
#' displayed on the y-axis of the forestplot.
#' (This argument is automatically \link[rlang:quotation]{quoted} and
#' \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.)
#' @param filename a character string giving the name of the file.
#' @param xlims NULL or a numeric vector of length 2 specifying the common x
#' limits across all biomarker subgroups.
#' @importFrom patchwork wrap_plots
#' @importFrom grDevices pdf dev.off
#' @author Maria Kalimeri, Ilari Scheinin
#' @export
#' @examples
#' \dontrun{
#' # Join the built-in association demo dataset with a variable that contains
#' # the machine readable names of Nightingale biomarkers. (Note: if you
#' # have built your association data frame using the Nightingale CSV result file,
#' # then your data frame should already contain machine readable names.)
#' df <-
#'   df_linear_associations %>%
#'   left_join(
#'     select(
#'       df_NG_biomarker_metadata,
#'       name,
#'       machine_readable_name
#'     ),
#'     by = "name"
#'   )
#'
#' # Print effect sizes for all Nightingale biomarkers in a 2-page pdf
#' plot_all_NG_biomarkers(
#'   df = df,
#'   machine_readable_name = machine_readable_name,
#'   # Notice we don't need to define explicitly 'name' as a name variable is
#'   # already present in df and picked up automatically as y-axis labels.
#'   estimate = estimate,
#'   se = se,
#'   pvalue = pvalue,
#'   colour = trait,
#'   filename = "biomarker_linear_associations.pdf",
#'   xlab = "1-SD increment in BMI
#' per 1-SD increment in biomarker concentration"
#' )
#'
#' # log odds for type 2 diabetes
#' df <-
#'   df_logodds_associations %>%
#'   left_join(
#'     select(
#'       df_NG_biomarker_metadata,
#'       name,
#'       machine_readable_name
#'     ),
#'     by = "name"
#'   ) %>%
#'   # Set the study variable to a factor to preserve order of appearance
#'   # Set class to factor to set order of display.
#'   dplyr::mutate(
#'     study = factor(
#'       study,
#'       levels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
#'     )
#'   )
#'
#' # Print effect sizes for all Nightingale biomarkers in a 2-page pdf
#' plot_all_NG_biomarkers(
#'   df = df,
#'   machine_readable_name = machine_readable_name,
#'   # Notice we don't need to define explicitly 'name' as a name variable is
#'   # already present in df and picked up automatically as y-axis labels.
#'   estimate = estimate,
#'   se = se,
#'   pvalue = pvalue,
#'   colour = study,
#'   logodds = TRUE,
#'   filename = "biomarker_t2d_associations.pdf",
#'   xlab = "1-SD increment in BMI
#' per 1-SD increment in biomarker concentration",
#'   # Restrict limits as some studies are very weak and they take over the
#'   # overall range.
#'   xlims = c(0.5, 3.2)
#' )
#' }
#'
plot_all_NG_biomarkers <- function(df,
                                   machine_readable_name = machine_readable_name,
                                   name = name,
                                   estimate = estimate,
                                   se = se,
                                   pvalue = NULL,
                                   colour = NULL,
                                   shape = NULL,
                                   logodds = FALSE,
                                   psignif = 0.05,
                                   ci = 0.95,
                                   filename = "ng_all_biomarkers.pdf",
                                   xlims = NULL,
                                   ...) {

  # Quote input
  machine_readable_name <- enquo(machine_readable_name)
  name <- enquo(name)
  estimate <- enquo(estimate)
  se <- enquo(se)
  pvalue <- enquo(pvalue)
  colour <- enquo(colour)
  shape <- enquo(shape)
  dots <- enquos(...)

  # Extract common xlim range, if not specified
  # Currently this is somewhat not optimal as the estimation of xmin and xmax
  # is also done again inside forestplot()
  if (is.null(xlims)) {
    # Estimate multiplication const for CI
    const <- stats::qnorm(1 - (1 - ci) / 2)

    # Adjust data frame variables
    df_xrange <-
      df %>%
      # Convert to a factor to preserve order.
      dplyr::mutate(
        # Added here to estimate xbreaks for log odds later
        xmin = !!estimate - const * !!se,
        xmax = !!estimate + const * !!se
      ) %>%
      dplyr::mutate_at(
        .vars = vars("xmin", "xmax"),
        .funs = funs(
          if (logodds) {
            exp(.)
          } else {
            .
          }
        )
      )
    xmin <-
      df_xrange %>%
      dplyr::summarise_at(
        .vars = vars("xmin"),
        .funs = funs(min(., na.rm = TRUE))
      ) %>%
      pull(xmin)
    xmax <-
      df_xrange %>%
      dplyr::summarise_at(
        .vars = vars("xmax"),
        .funs = funs(max(., na.rm = TRUE))
      ) %>%
      pull(xmax)
    xlims <- c(xmin, xmax)
  }

  # Join the association data frame with group data
  df_with_groups <-
    df %>%
    # use right_join, with df_grouping on the right, to preserve the order of
    # biomarkers it specifies.
    dplyr::right_join(.,
      ggforestplot::df_grouping_all_NG_biomarkers %>%
        rename(!!machine_readable_name := machine_readable_name),
      by = quo_name(machine_readable_name)
    )

  # Define plot column function
  # clayout is a character vector of groups that exist in the group variable of
  # data frame df
  plot_column <- function(df, clayout, legend = TRUE) {

    # Filter df for only the groups defined by layout
    df <-
      df %>%
      filter(.data$group_custom %in% clayout)

    # Draw a forestplot for each group in the column
    ggplot_multi <-
      df %>%
      nest(-.data$group_custom, .key = "data") %>%
      # Apply forestplot to each group
      mutate(
        gg_groups = purrr::map2(
          .data$data, .data$group_custom, ~ forestplot(
            df = .x,
            name = !!name,
            estimate = !!estimate,
            se = !!se,
            pvalue = !!pvalue,
            colour = !!colour,
            shape = !!shape,
            logodds = logodds,
            psignif = psignif,
            title = .y,
            !!!dots
          ) +
            ggplot2::coord_cartesian(xlim = xlims)
        ),
        # Remove x-axis for all plots except the bottom one
        gg_groups = ifelse(
          test = row_number() != n(),
          yes =
            purrr::map(.data$gg_groups, ~ . +
              theme(
                axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                plot.margin = unit(c(1, 2, 1, 2), "mm")
              )),
          no = .data$gg_groups
        ),
        # If legend TRUE remove legend for all except the last
        # If legend FALSE, remove all legends
        gg_groups = ifelse(
          test = legend & row_number() == 1,
          yes = purrr::map(.data$gg_groups, ~ . +
            ggplot2::theme(legend.justification = "top")),
          no = purrr::map(.data$gg_groups, ~ . +
            ggplot2::theme(legend.position = "none"))
        ),
        rel_heights = purrr::map(
          .data$data, ~ nrow(.)
        ) %>% unlist()
      )

    patchwork::wrap_plots(
      ggplot_multi$gg_groups,
      ncol = 1,
      heights = ggplot_multi$rel_heights
    )
  }

  # Define plot page function
  # playout is a list of two vectors
  plot_page <- function(df, playout, legend = list(FALSE, TRUE)) {
    purrr::map2(playout, legend, ~ plot_column(df, .x, .y)) %>%
      patchwork::wrap_plots(ncol = 2L, nrow = 1L)
  }

  # Define plot report function
  # rlayout is a list, one element per page, each element is a list, one
  # subelement per column
  plot_report <- function(df,
                            rlayout,
                            legend = list(list(FALSE, TRUE), list(FALSE, TRUE)),
                            filename,
                            ...) {
    grDevices::pdf(filename, ...)
    purrr::map2(rlayout, legend, ~ plot_page(df, .x, .y) %>% print())
    dev.off()
  }

  # Build rlayout
  clayout1 <-
    df_with_groups %>%
    filter(.data$page == 1 & .data$column == 1) %>%
    pull(.data$group_custom) %>%
    unique()
  clayout2 <-
    df_with_groups %>%
    filter(.data$page == 1 & .data$column == 2) %>%
    pull(.data$group_custom) %>%
    unique()
  clayout3 <-
    df_with_groups %>%
    filter(.data$page == 2 & .data$column == 1) %>%
    pull(.data$group_custom) %>%
    unique()
  clayout4 <-
    df_with_groups %>%
    filter(.data$page == 2 & .data$column == 2) %>%
    pull(.data$group_custom) %>%
    unique()
  rlayout <-
    list(
      list(
        clayout1,
        clayout2
      ),
      list(
        clayout3,
        clayout4
      )
    )

  paperwidth <- 15

  plot_report(
    df_with_groups,
    rlayout = rlayout,
    filename = filename,
    height = paperwidth * sqrt(2),
    width = paperwidth
  )
}
