#' Exploratory Analysis
#'
#' Fit multiple regression models in one go.
#'
#' @param df_long a data frame in a long format that contains a \code{key}
#' column with the names of the variables for which to estimate the
#' measures of effect, e.g. Nightingale NMR biomarker names, a \code{value} column
#' with the numeric values of the respective keys and other columns with the
#' response and other variables to adjust for. Note: You may use
#' \code{tidyr::\link[tidyr]{gather}} to collapse your data frame to key-value
#' pairs; it is recommended to \code{dplyr::\link[dplyr]{select}} only the
#' variables that you want to go into your model, in order to avoid memory
#' overheads in case of large datasets.
#' @param model a character, setting the type of model to fit on the input data
#' frame. Must be one of \code{'lm'}, \code{'glm'} or \code{'coxph'}, for linear,
#' logistic and cox proportional hazards regression, respectively.
#' @param formula a formula object. For the case of models \code{'lm'} and
#' \code{'glm'} it must be an object of class \code{stats::\link[stats]{formula}}
#' (or one that can be coerced to that class): a symbolic description of the model to be
#' fitted. The details of model specification are given under ‘Details’ in the
#' documentation of \code{stats::\link[stats]{formula}}. For the case of model
#' \code{'coxph'}, the formula object must have the response on the left of a ~
#' operator, and the rest of the terms on the right. The response must be a
#' survival object, as returned by the \code{\link[survival]{Surv}} function.
#' @param key the name of the \code{key} variable.
#' @param predictor the name of the variable for which (adjusted)
#' univariate associations are estimated. This is stated here in order to
#' individuate the predictor of interest over many, possible cofactors that are
#' also present in \code{formula}.
#' @param verbose logical (default FALSE). If TRUE it prints a message with the
#' names of the predictor and outcome. This may come in handy when, for example,
#' fitting multiple outcomes.
#' @return A data frame with the following: a character variable with the same
#' name as the \code{key} parameter and numeric variables \code{estimate},
#' \code{se} and \code{pvalue} with the values of the respective variables
#' of the linear model.
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom stats lm
#' @importFrom stats glm
#' @import survival
#' @importFrom stringr str_extract
#' @importFrom lazyeval is_formula
#' @importFrom tidyr nest
#' @importFrom broom tidy
#' @author Maria Kalimeri, Emmi Tikkanen, Juuso Parkkinen
#' @export
#' @examples
#' library(magrittr)
#'
#' # Linear Regression Example
#'
#' # We will use the simulated demo data that come with the package,
#' # ggforestplot::df_demo_metabolic_data
#'
#' # Extract the names of the NMR biomarkers for discovery analysis
#' nmr_biomarkers <- names(df_demo_metabolic_data)[7:234]
#'
#' # Select only variables to be used for the model and collapse to a long
#' # format
#' df_long <-
#'   df_demo_metabolic_data %>%
#'   # Select only model variables
#'   dplyr::select(nmr_biomarkers, gender, BMI) %>%
#'   # log-tranform biomarkers
#'   dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = list(~log1p(.))) %>%
#'   # Scale biomarkers
#'   dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = list(~as.numeric(scale(.)))) %>%
#'   # Collapse to a long format
#'   tidyr::gather(key = machine_readable_name, value = biomarkervalue, nmr_biomarkers)
#'
#' df_assoc_per_biomarker <-
#'   discovery_regression(
#'     df_long = df_long,
#'     model = "lm",
#'     formula =
#'       formula(
#'         biomarkervalue ~ BMI + factor(gender)
#'       ),
#'     key = machine_readable_name,
#'     predictor = BMI
#'   )
#'
#' # Filter Nightingale metadata data frame for biomarkers of interest
#' df_grouping <-
#'   ggforestplot::df_NG_biomarker_metadata %>%
#'   dplyr::filter(group %in% "Fatty acids")
#'
#' # Join the association data frame with the group data above
#' df <-
#'   df_assoc_per_biomarker %>%
#'   # use right_join, with df_grouping on the right, to preserve the order of
#'   # biomarkers it specifies.
#'   dplyr::right_join(., df_grouping, by = "machine_readable_name")
#'
#' # Draw a forestplot of the results
#' ggforestplot::forestplot(
#'   df = df,
#'   name = name,
#'   estimate = estimate,
#'   se = se,
#'   pvalue = pvalue,
#'   psignif = 0.001,
#'   xlab = "1-SD increment in biomarker concentration
#' per 1-SD increment in BMI",
#'   title = "Associations of fatty acids to BMI",
#'   logodds = TRUE
#' )
#'
#' # Logistic Regression Example
#'
#' # Extract names of relevant NMR biomarkers
#' nmr_biomarkers <- names(df_demo_metabolic_data)[7:234]
#'
#' # Select only variables to be used for the model and
#' # collapse to a long format
#' df_long <-
#'   df_demo_metabolic_data %>%
#'   # Select only model variables (avoid memory overhead)
#'   dplyr::select(
#'     nmr_biomarkers,
#'     gender,
#'     incident_diabetes,
#'     BMI,
#'     baseline_age
#'   ) %>%
#'   # log-tranform biomarkers
#'   dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = list(~log1p(.))) %>%
#'   # Scale biomarkers
#'   dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = list(~as.numeric(scale(.)))) %>%
#'   # Collapse to a long format
#'   tidyr::gather(key = machine_readable_name, value = biomarkervalue, nmr_biomarkers)
#'
#' df_assoc_per_biomarker_gender <-
#'   discovery_regression(
#'     df_long = df_long,
#'     model = "glm",
#'     formula =
#'       formula(
#'         incident_diabetes ~ biomarkervalue + factor(gender) + BMI + baseline_age
#'       ),
#'     key = machine_readable_name,
#'     predictor = biomarkervalue
#'   )
#'
#' # Filter Nightingale metadata data frame for biomarkers of interest
#' df_grouping <-
#'   ggforestplot::df_NG_biomarker_metadata %>%
#'   dplyr::filter(group %in% "Cholesterol")
#'
#' # Join the association data frame with the group data above
#' df <-
#'   df_assoc_per_biomarker_gender %>%
#'   # use right_join, with df_grouping on the right, to preserve the order of
#'   # biomarkers it specifies.
#'   dplyr::right_join(., df_grouping, by = "machine_readable_name")
#'
#' # Draw a forestplot of the results
#' ggforestplot::forestplot(
#'   df = df,
#'   name = name,
#'   estimate = estimate,
#'   se = se,
#'   pvalue = pvalue,
#'   psignif = 0.001,
#'   xlab = "Odds ratio for incident type 2 diabetes (95% CI)
#' per 1-SD increment in biomarker concentration",
#'   title = "Cholesterol and risk of future type 2 diabetes",
#'   logodds = TRUE
#' )
discovery_regression <- function(df_long,
                                 model = c("lm", "glm", "coxph"),
                                 formula,
                                 key = key,
                                 predictor = predictor,
                                 verbose = FALSE) {
  model <- match.arg(model)

  # Quote input
  key <- enquo(key)
  predictor <- enquo(predictor)

  # Check that formula is formula
  if (!is_formula(formula)) {
    stop("'formula' must be a formula object")
  }

  # Check that the predictor param is int the formula
  righthandside <-
    as.character(formula)[3] %>%
    stringr::str_split(pattern = " \\+ ") %>%
    unlist()
  if (!quo_name(predictor) %in% righthandside) {
    stop(paste(quo_name(predictor), "must be one of the predictors in 'formula'"))
  }

  if (verbose) {
    lefthandside <-
      ifelse(
        test = model == "coxph",
        yes = stringr::str_extract(
          string = as.character(formula)[2],
          pattern = "(?<=event = )\\w+"
        ),
        no = as.character(formula)[2]
      )
    message(paste(
      "Estimating associations of",
      quo_name(predictor),
      "to",
      lefthandside
    ))
  }

  res <-
    df_long %>%
    tidyr::nest(-!!key, .key = "data")


  # # Apply desired model
  if (model == "lm") {
    res <-
      res %>%
      mutate(
        model = purrr::map(.data$data, ~ stats::lm(
          formula,
          data = .x
        ))
      )
  } else if (model == "glm") {
    res <-
      res %>%
      mutate(
        model = purrr::map(.data$data, ~ stats::glm(
          formula,
          data = .x,
          family = "binomial"
        ))
      )
  } else {
    res <-
      res %>%
      mutate(
        model = purrr::map(.data$data, ~ survival::coxph(
          formula,
          data = .x
        ))
      )
  }

  # Extract variables from model
  res %>%
    mutate(
      tidymodel = purrr::map(.data$model, ~ broom::tidy(.x)),
      estimate = purrr::map_dbl(.data$tidymodel, ~ .x %>%
        filter(term == quo_name(predictor)) %>%
        pull(estimate)),
      se = purrr::map_dbl(.data$tidymodel, ~ .x %>%
        filter(term == quo_name(predictor)) %>%
        pull(std.error)),
      pvalue = purrr::map_dbl(.data$tidymodel, ~ .x %>%
        filter(term == quo_name(predictor)) %>%
        pull(p.value))
    ) %>%
    select(!!key, .data$estimate, .data$se, .data$pvalue)
}
