#' Information and Grouping for Nightingale's Blood Biomarkers
#'
#' A data frame with information on the Nightingale Health Ltd. NMR-quantified
#' blood biomarkers.
#'
#' @format A data frame (tibble) with 252 rows and 8 columns:
#' \describe{
#'   \item{abbreviation}{Biomarker abbreviation, i.e. the one delivered with the
#'   xlsx data format.}
#'   \item{machine_readable_name}{Biomarker machine readable name, i.e. the one
#'   delivered with the csv data format.}
#'   \item{name}{Biomarker name.}
#'   \item{alternative_names}{Alternative biomarker names.}
#'   \item{description}{Biomarker description.}
#'   \item{group}{The group the biomarker belongs to.}
#'   \item{subgroup}{The subgroup the biomarker belongs to.}
#'   \item{unit}{Unit is deprecated. Use the units delivered with biomarkers.}
#' }
#' @source Nightingale Health Ltd. \url{https://nightingalehealth.com/}
"df_NG_biomarker_metadata"

#' Simulated Metabolic Data
#'
#' A simulated, demo data frame that contains metabolic profiles, basic
#' information and diabetes outcomes for 1887 fictional individuals. The data frame
#' contains values for 250 serum biomarkers quantified by the NMR platform of
#' Nightingale Health Ltd.
#'
#' @format A data frame (tibble) with 1887 rows and 256 columns. \code{id} is a
#' character variable with the ID number of fictional individual; \code{gender}
#' is a character variable with the gender information on each individual;
#' \code{baseline_age} is a numeric variable with the age at the time of
#' blood draw; \code{BMI} is a numeric variable with the BMI of each individual
#' at the time of blood draw; \code{incident_diabetes} is a numeric variable
#' with values 1 or 0 for whether the diabetes event occured or not during the
#' observed time, respectively; \code{age_at_diabetes} is a numeric variable
#' with the age at the end of the study and the rest of the variables are numeric
#' containing the machine readable names of Nightingale NMR blood biomarkers.
#' @source Simulated NMR data; Nightingale Health Ltd, \url{https://nightingalehealth.com/}.
#' @seealso \code{\link{df_NG_biomarker_metadata}}
"df_demo_metabolic_data"

#' Linear Associations of Blood Biomarkers to BMI, HOMA-IR and Fasting Glucose
#'
#' A data frame containing cross-sectional associations of the Nightingale blood
#' biomarkers to Body Mass Index (BMI), insulin resistance (log(HOMA-IR)) and
#' fasting glucose. For these values a linear regression model was used, adjusted
#' for age and sex.
#'
#' "Values are beta-correlations from cross-sectional metabolite associations
#' with BMI, log(HOMA-IR) and fasting glucose. For comparison of the patterns of
#' associations, magnitudes are scaled to 1-SD in each of the outcomes
#' (corresponding to 4.2 kg/m2 for BMI, 0.57 for log(HOMA-IR) and 0.56 mmol/l
#' for glucose) per 1-SD log-transformed metabolite concentration. Results were
#' adjusted for sex and age, and meta-analyzed for 11,896 individuals from the
#' four cohorts. Error bars denote 95\% confidence intervals; the large sample
#' size and consistency across cohorts make confidence intervals narrow for the
#' cross-sectional linear regression analyses."
#' The values are shown in Figure S5 of A. V. Ahola-Olli et al. (2019):
#' \url{https://www.biorxiv.org/content/early/2019/01/08/513648}
#'
#' @format A data frame (tibble) with 687 rows and 5 columns:
#' \describe{
#'   \item{name}{Blood biomarker name. Note: glucose is missing as the results
#'   are adjusted for this biomarker.}
#'   \item{trait}{The response variable of the regression model, either BMI,
#'   log(HOMA-IR) or fasting glucose.}
#'   \item{beta}{Linear regression coefficient \eqn{\beta}.}
#'   \item{se}{Standard error.}
#'   \item{pvalue}{P-value.}
#' }
#' @source These data are taken from the Supplementary material of
#' A. V. Ahola-Olli et al. (2019).
#' \url{https://www.biorxiv.org/content/early/2019/01/08/513648}
"df_linear_associations"

#' Odds Ratios of Blood Biomarkers with Incident Type 2 Diabetes
#'
#' A data frame containing log odds ratios of the Nightingale blood biomarkers
#' with risk for future type 2 diabetes.
#'
#' "Values are odds ratios (95\% confidence intervals) per 1-SD log-transformed
#' metabolite concentration. Odds ratios were adjusted for sex, baseline age,
#' BMI, and fasting glucose. YFS, Cardiovascular risk in Young Finns Study;
#' NFBC, Northern Finland Birth Cohort."
#' The values are shown in Figure S3 of A. V. Ahola-Olli et al. (2019):
#' \url{https://www.biorxiv.org/content/early/2019/01/08/513648}
#'
#' @format A data frame (tibble) with 228 rows and 5 columns:
#' \describe{
#'   \item{name}{Biomarker abbreviation.}
#'   \item{study}{Short description of the association type, here the cohort
#'   name.}
#'   \item{beta}{Log odds for incident type 2 diabetes.}
#'   \item{se}{Standard error.}
#'   \item{pvalue}{P-value.}
#'   \item{n}{Sample size.}
#' }
#' @source These data are taken from the Supplementary material of
#' A. V. Ahola-Olli et al. (2019).
#' \url{https://www.biorxiv.org/content/early/2019/01/08/513648}
"df_logodds_associations"

#' Example of Biomarker Grouping Data Frames for Nightingale Biomarkers
#'
#' A data frame containing example custom groupings for the Nightingale Health
#' biomarkers in a machine readable name format. There are two custom groupings
#' available for 2016 and 2020 biomarker platforms. These groupings are used
#' with \code{\link{plot_all_NG_biomarkers}}.
#'
#' @format A data frame (tibble) with 2 rows and 2 columns:
#' \describe{
#'   \item{version}{Biomarker platform version.}
#'   \item{layout}{A data frame (tibble) defining layout:
#'   \itemize{
#'   \item \code{machine_readable_name} Biomarker machine readable name, i.e. the one
#'   delivered with the csv data format.
#'   \item \code{group_custom} A character indication group titles.
#'   \item \code{column} An integer indicating the column number in a
#'   layout, see \code{\link{plot_all_NG_biomarkers}}.
#'   \item \code{page} An integer indicating the page number in a
#'   layout, see \code{\link{plot_all_NG_biomarkers}}.
#'   }
#' }
#' }
#' @seealso Data frame \code{\link{df_NG_biomarker_metadata}} with information
#' on the Nightingale Health Ltd. NMR-quantified blood biomarkers
"df_grouping_all_NG_biomarkers"
