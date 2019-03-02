context("discovery_regression")

library(magrittr)

# Extract names of relevant NMR biomarkers
nmr_biomarkers <- names(df_demo_metabolic_data)[7:234]

test_that("discovery_regression returns expected output for linear regression", {

  # Select only variables to be used for the model and collapse to a long format
  df_long <-
    df_demo_metabolic_data %>%
    # Select only model variables
    dplyr::select(nmr_biomarkers, gender, BMI) %>%
    # log-tranform biomarkers
    dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = dplyr::funs(log1p(.))) %>%
    # Scale biomarkers
    dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = dplyr::funs(as.numeric(scale(.)))) %>%
    # Collapse to a long format
    tidyr::gather(key = biomarkerid, value = biomarkervalue, nmr_biomarkers)

  df_assoc_per_biomarker <-
    discovery_regression(
      df_long = df_long,
      formula =
        formula(
          biomarkervalue ~ BMI + factor(gender)
        ),
      key = "biomarkerid",
      predictor = "BMI",
      model = "lm"
    )

  expect_output(
    str(df_assoc_per_biomarker),
    "4 variables"
  )

  expect_true(
    all(df_assoc_per_biomarker$biomarkerid %in% df_long$biomarkerid)
  )

  val <-
    df_assoc_per_biomarker %>%
    filter(biomarkerid == "Esterified_C") %>%
    pull(estimate) %>%
    round(4)

  expect_equal(val , 0.028)

  expect_message(
    discovery_regression(
      df_long = df_long,
      formula =
        formula(
          biomarkervalue ~ BMI + factor(gender)
        ),
      key = biomarkerid,
      predictor = BMI,
      model = "lm",
      verbose = TRUE
    ),
    paste(
      "Estimating associations of",
      "BMI",
      "to",
      "biomarkervalue"
    )
  )
})

test_that("discovery_regression returns expected output for cox regression", {

  # Select only variables to be used for the model and collapse to a long format
  df_long <-
    df_demo_metabolic_data %>%
    # Select only model variables
    dplyr::select(nmr_biomarkers, gender, baseline_age, age_at_diabetes, incident_diabetes) %>%
    # log-tranform biomarkers
    dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = dplyr::funs(log1p(.))) %>%
    # Scale biomarkers
    dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = dplyr::funs(as.numeric(scale(.)))) %>%
    # Collapse to a long format
    tidyr::gather(key = biomarkerid, value = biomarkervalue, nmr_biomarkers)

  df_assoc_per_biomarker_diab <-
    discovery_regression(
      df_long = df_long,
      formula =
        formula(
          survival::Surv(
            time = baseline_age,
            time2 = age_at_diabetes,
            event = incident_diabetes
          ) ~ biomarkervalue + factor(gender)
        ),
      key = "biomarkerid",
      predictor = "biomarkervalue",
      model = "coxph"
    )

  expect_output(
    str(df_assoc_per_biomarker_diab),
    "4 variables"
  )

  expect_true(
    all(df_assoc_per_biomarker_diab$biomarkerid %in% df_long$biomarkerid)
  )

  val <-
    df_assoc_per_biomarker_diab %>%
    filter(biomarkerid == "Esterified_C") %>%
    pull(estimate) %>%
    round(4)

  expect_equal(val, 0.3141)
})

test_that("discovery_regression returns expected output for logistic regression", {

  # Select only variables to be used for the model and
  # collapse to a long format
  df_long <-
    df_demo_metabolic_data %>%
    # Select only model variables (avoid memory overhead)
    dplyr::select(nmr_biomarkers, gender) %>%
    # log-tranform biomarkers
    dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = dplyr::funs(log1p(.))) %>%
    # Scale biomarkers
    dplyr::mutate_at(.vars = c(nmr_biomarkers), .funs = dplyr::funs(as.numeric(scale(.)))) %>%
    # Collapse to a long format
    tidyr::gather(key = biomarkerid, value = biomarkervalue, nmr_biomarkers)

  df_assoc_per_biomarker_gender <-
    discovery_regression(
      df_long = df_long,
      formula =
        formula(
          factor(gender) ~ biomarkervalue
        ),
      key = "biomarkerid",
      predictor = "biomarkervalue",
      model = "glm"
    )

  expect_output(
    str(df_assoc_per_biomarker_gender),
    "4 variables"
  )

  expect_true(
    all(df_assoc_per_biomarker_gender$biomarkerid %in% df_long$biomarkerid)
  )

  val <-
    df_assoc_per_biomarker_gender %>%
    filter(biomarkerid == "Esterified_C") %>%
    pull(estimate) %>%
    round(4)

  expect_equal(val , -0.0934)

  # Check you get errors when ...
  expect_error(
    discovery_regression(
      df_long = df_long,
      formula = "dummy",
      key = "biomarkerid",
      predictor = "biomarkervalue",
      model = "glm"
    ),
    "'formula' must be a formula object"
  )
  expect_error(
    discovery_regression(
      df_long = df_long,
      formula =
        formula(
          factor(gender) ~ biomarkervalue
        ),
      key = biomarkerid,
      predictor = value,
      model = "glm"
    ),
    paste("value must be one of the predictors in 'formula'")
  )
})

