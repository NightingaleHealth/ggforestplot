context("discovery_regression")

library(magrittr)

# Extract names of relevant NMR biomarkers
nmr_biomarkers <- dplyr::intersect(
  colnames(df_demo_metabolic_data),
  df_NG_biomarker_metadata$machine_readable_name
)

test_that(
  "discovery_regression returns expected output for linear regression", {

  # Select only variables to be used for the model and collapse to a long format
  df_long <-
    df_demo_metabolic_data %>%
    # Select only model variables
    dplyr::select(.data$gender, .data$BMI, dplyr::one_of(nmr_biomarkers)) %>%
    # log-tranform and scale biomarkers
    dplyr::mutate_at(
      .vars = dplyr::vars(nmr_biomarkers),
      .funs = ~ .x %>% log1p() %>% scale() %>% as.numeric()
    ) %>%
    # Collapse to a long format
    tidyr::gather(key = biomarkerid, value = biomarkervalue, nmr_biomarkers)

  df_assoc_per_biomarker <-
    discovery_regression(
      df_long = df_long,
      formula = formula(biomarkervalue ~ BMI + factor(gender)),
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
    filter(biomarkerid == "Total_CE") %>%
    pull(estimate) %>%
    round(4)

  expect_equal(val , 0.0342)

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
    dplyr::select(
      .data$gender, .data$baseline_age,
      .data$age_at_diabetes, .data$incident_diabetes,
      dplyr::one_of(nmr_biomarkers)
    ) %>%
    # log-tranform and scale biomarkers
    dplyr::mutate_at(
      .vars = dplyr::vars(nmr_biomarkers),
      .funs = ~ .x %>% log1p() %>% scale() %>% as.numeric()
    ) %>%
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
    filter(biomarkerid == "Total_CE") %>%
    pull(estimate) %>%
    round(4)

  expect_equal(val, 0.0534)
})

test_that(
  "discovery_regression returns expected output for logistic regression", {

  # Select only variables to be used for the model and
  # collapse to a long format
  df_long <-
    df_demo_metabolic_data %>%
    # Select only model variables (avoid memory overhead)
    dplyr::select(.data$gender, dplyr::one_of(nmr_biomarkers)) %>%
    # log-tranform and scale biomarkers
    dplyr::mutate_at(
      .vars = dplyr::vars(nmr_biomarkers),
      .funs = ~ .x %>% log1p() %>% scale() %>% as.numeric()
    ) %>%
    # Collapse to a long format
    tidyr::gather(key = biomarkerid, value = biomarkervalue, nmr_biomarkers)

  df_assoc_per_biomarker_gender <-
    discovery_regression(
      df_long = df_long,
      formula = formula(factor(gender) ~ biomarkervalue),
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
    filter(biomarkerid == "Total_CE") %>%
    pull(estimate) %>%
    round(4)

  expect_equal(val , -0.0702)

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

test_that(
  "discovery_regression returns expected output for linear regression with factor predictor",
  {
    df_long <- tibble::tibble(
      name = c(rep("a", 3), rep("b", 3)),
      x = factor(rep(1:3, 2)),
      y = c(1:3, seq(from = 1, to = 5, by = 2))
    )
    
    df_assoc <-
      discovery_regression(
        df_long = df_long,
        formula = formula(y ~ x),
        key = "name",
        predictor = "x",
        model = "lm"
      )
    
    expect_output(
      str(df_assoc),
      "5 variables"
    )
    
    expect_true(
      all(df_assoc$name %in% df_long$name)
    )
    
    expect_true(
      all(df_assoc$term %in% paste0("x", levels(df_long$x)[-1]))
    )
    
    expect_equal(df_assoc$estimate, c(1, 2, 2, 4))
  }
)
