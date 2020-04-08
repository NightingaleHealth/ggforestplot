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
#' @param name the variable in \code{df} that contains the y-axis
#' names. If NULL, names from \code{\link{df_NG_biomarker_metadata}} are
#' used. This argument is automatically \link[rlang:quotation]{quoted}
#' and \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.
#' See Note.
#' @inheritParams forestplot
#' @param machine_readable_name the variable in df containing the machine
#' readable names of Nightingale blood biomarkers. I.e. the names in this
#' variable must be the same as in the \code{machine_readable_name} variable of
#' \code{\link{df_NG_biomarker_metadata}}.
#' (This argument is automatically \link[rlang:quotation]{quoted} and
#' \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.)
#' @param filename a character string giving the name of the file.
#' @param paperwidth page width in inches
#' @param paperheight page height in inches
#' @param xlims NULL or a numeric vector of length 2 specifying the common x
#' limits across all biomarker subgroups.
#' @param layout one of the predefined layouts in \code{\link{df_grouping_all_NG_biomarkers}}
#' or custom layout tibble following the example of predifined layouts
#' @return If filename is NULL, a list of plot objects (one for each page
#' in layout) is returned.
#' @importFrom magrittr %>%
#' @importFrom rlang !! .data :=
#' @author Maria Kalimeri, Ilari Scheinin, Vilma Jagerroos
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
#' # Print effect sizes for Nightingale biomarkers in a 2-page pdf
#' plot_all_NG_biomarkers(
#'   df = df,
#'   machine_readable_name = machine_readable_name,
#'   # Notice that when name is not defined explicitly, names from
#'   # df_NG_biomarker_metadata are used
#'   estimate = beta,
#'   se = se,
#'   pvalue = pvalue,
#'   colour = trait,
#'   filename = "biomarker_linear_associations.pdf",
#'   xlab = "1-SD increment in BMI
#' per 1-SD increment in biomarker concentration",
#'   layout = "2016"
#' )
#'
#' # Custom layout can also be provided
#' layout <- df_NG_biomarker_metadata %>%
#'   dplyr::filter(
#'     .data$group == "Fatty acids",
#'     .data$machine_readable_name %in% df$machine_readable_name
#'   ) %>%
#'   dplyr::mutate(
#'     group_custom = .data$subgroup,
#'     column = dplyr::case_when(
#'       .data$group_custom == "Fatty acids" ~ 1,
#'       .data$group_custom == "Fatty acid ratios" ~ 2
#'     ),
#'     page = 1
#'   ) %>%
#'   dplyr::select(
#'     .data$machine_readable_name,
#'     .data$group_custom,
#'     .data$column,
#'     .data$page
#'   )
#'
#' plot_all_NG_biomarkers(
#'   df = df,
#'   machine_readable_name = machine_readable_name,
#'   # Notice that when name is not defined explicitly, names from
#'   # df_NG_biomarker_metadata are used
#'   estimate = beta,
#'   se = se,
#'   pvalue = pvalue,
#'   colour = trait,
#'   xlab = "1-SD increment in BMI
#' per 1-SD increment in biomarker concentration",
#'   layout = layout
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
#' # Print effect sizes for Nightingale biomarkers in a 2-page pdf
#' plot_all_NG_biomarkers(
#'   df = df,
#'   machine_readable_name = machine_readable_name,
#'   # Notice that when name is not defined explicitly, names from
#'   # df_NG_biomarker_metadata are used
#'   estimate = beta,
#'   se = se,
#'   pvalue = pvalue,
#'   colour = study,
#'   logodds = TRUE,
#'   filename = "biomarker_t2d_associations.pdf",
#'   xlab = "Odds ratio for incident type 2 diabetes (95% CI)
#' per 1−SD increment in metabolite concentration",
#'   layout = "2016",
#'   # Restrict limits as some studies are very weak and they take over the
#'   # overall range.
#'   xlims = c(0.5, 3.2)
#' )
#' }
#'
plot_all_NG_biomarkers <- function(df,
                                   machine_readable_name = machine_readable_name,
                                   name = NULL,
                                   estimate = estimate,
                                   se = se,
                                   pvalue = NULL,
                                   colour = NULL,
                                   shape = NULL,
                                   logodds = FALSE,
                                   psignif = 0.05,
                                   ci = 0.95,
                                   filename = NULL,
                                   paperwidth = 15,
                                   paperheight = sqrt(2) * paperwidth,
                                   xlims = NULL,
                                   layout = "2020",
                                   ...) {

  # Quote input
  machine_readable_name <- rlang::enquo(machine_readable_name)
  name <- rlang::enquo(name)
  estimate <- rlang::enquo(estimate)
  se <- rlang::enquo(se)
  pvalue <- rlang::enquo(pvalue)
  colour <- rlang::enquo(colour)
  shape <- rlang::enquo(shape)

  # If layout is string, check that it matches one of the predefined layouts
  if (is.character(layout)) {
    match.arg(layout, ggforestplot::df_grouping_all_NG_biomarkers$version)

    if (layout != "2016" && any(c("HDL2_C", "HDL3_C") %in% dplyr::pull(df, !!machine_readable_name))) {
      warning("Based on biomarkers in your data, you should be using 2016-layout")
    }

    layout <- ggforestplot::df_grouping_all_NG_biomarkers %>%
      dplyr::filter(.data$version == !!layout) %>%
      dplyr::pull(.data$layout) %>%
      purrr::flatten_df()
  } else if (tibble::is_tibble(layout)) {
    # If layout is tibble, check that it has certain columns
    columns <- c("machine_readable_name", "group_custom", "column", "page")
    if (!all(columns %in% colnames(layout))) {
      stop(paste("layout tibble should have columns", paste0(columns, collapse = ", ")))
    }
  } else {
    # If layout is neither string nor tibble, throw error
    stop(paste0(
      "layout has to be either string (one of ",
      paste0(ggforestplot::df_grouping_all_NG_biomarkers$version, collapse = ", "),
      ") or tibble (with columns machine_readable_name, group_custom, column and page)"
    ))
  }

  # Select only relevant columns to avoid weird errors
  df <- df %>%
    dplyr::select(
      !!machine_readable_name, !!name,
      !!estimate, !!se, !!pvalue,
      !!colour, !!shape
    )

  # Extract common xlim range, if not specified
  # Currently this is somewhat not optimal as the estimation of xmin and xmax
  # is also done again inside forestplot()
  if (is.null(xlims)) {
    # Estimate multiplication const for CI
    const <- stats::qnorm(1 - (1 - ci) / 2)

    # Compute range of data with CIs
    xlims <- df %>%
      dplyr::mutate(
        xmin = !!estimate - const * !!se,
        xmax = !!estimate + const * !!se
      ) %>%
      dplyr::mutate_at(
        .vars = vars("xmin", "xmax"),
        .funs = ~ {
          if (logodds) exp(.x) else .x
        }
      ) %>%
      dplyr::summarize(
        xmin = min(.data$xmin, na.rm = TRUE),
        xmax = max(.data$xmax, na.rm = TRUE)
      ) %>%
      purrr::flatten_dbl()
  }

  # Join the association data frame with group data
  df_with_groups <- df %>%
    # Use right_join to get order from grouping dataframe
    dplyr::right_join(
      layout %>%
        dplyr::rename(!!machine_readable_name := machine_readable_name),
      by = rlang::quo_name(machine_readable_name)
    )

  # If name column is not provided, extract names from metadata
  # This is done after joining with grouping to get names for also
  # those biomarkers that are not present in data but are in layout
  if (rlang::quo_is_null(name)) {
    df_with_groups <- df_with_groups %>%
      dplyr::left_join(
        ggforestplot::df_NG_biomarker_metadata %>%
          dplyr::select(
            !!machine_readable_name := .data$machine_readable_name,
            .data$name
          ),
        by = rlang::quo_name(machine_readable_name)
      )
    name <- rlang::parse_quo("name", env = globalenv())
  }

  # Define plot column function
  plot_column <- function(df, legend = TRUE) {
    df <- df %>%
      dplyr::mutate(
        group_custom = factor(
          .data$group_custom,
          levels = .data$group_custom %>% unique()
        )
      )

    plot <- ggforestplot::forestplot(
      df = df,
      name = !!name,
      estimate = !!estimate,
      se = !!se,
      pvalue = !!pvalue,
      colour = !!colour,
      shape = !!shape,
      logodds = logodds,
      psignif = psignif,
      ci = ci,
      ...
    ) +
      ggplot2::coord_cartesian(xlim = xlims) +
      ggforce::facet_col(
        facets = ~group_custom,
        scales = "free_y",
        space = "free"
      ) +
      theme(strip.text.x = element_text(size = 15))

    if (legend) {
      plot <- plot + ggplot2::theme(legend.justification = "top")
    } else {
      plot <- plot + ggplot2::theme(legend.position = "none")
    }

    plot
  }

  # Define plot page function
  plot_page <- function(df) {
    # Number of columns in layout
    ncols <- max(df$column)

    # Plot columns
    purrr::pmap(
      .l = df %>% tidyr::nest(data = -.data$column),
      .f = ~ plot_column(df = ..2, legend = ..1 == ncols)
    ) %>%
      patchwork::wrap_plots(ncol = ncols)
  }

  # Define plot report function
  plot_report <- function(df, filename) {
    purrr::pmap(
      .l = df %>% tidyr::nest(data = -.data$page),
      .f = ~ plot_page(df = ..2)
    )
  }

  plots <- plot_report(
    df_with_groups,
    filename = filename
  )

  if (!is.null(filename)) {
    message("Saving ", filename)
    grDevices::pdf(filename, paperwidth, paperheight)
    purrr::walk(.x = plots, .f = ~ print(.x))
    grDevices::dev.off()
    invisible()
  } else {
    plots
  }
}
