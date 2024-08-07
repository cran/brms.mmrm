#' @title Create and preprocess an MMRM dataset.
#' @export
#' @family data
#' @description Create a dataset to analyze with an MMRM.
#' @section Preprocessing:
#'   The preprocessing steps in `brm_data()` are as follows:
#'   * Perform basic assertions to make sure the data and other arguments
#'     are properly formatted.
#'   * Convert the group and time columns to character vectors.
#'   * Sanitize the levels of the group and time columns using
#'     `make.names(unique = FALSE, allow_ = TRUE)` to ensure agreement
#'     between the data and the output of `brms`.
#'   * For each implicitly missing outcome observation, add explicit row
#'     with the outcome variable equal to `NA_real_`. Missing values
#'     in the predictors are implicitly filled using [zoo::na.locf()]
#'     on within each patient, which is not valid for time-varying
#'     covariates. If any covariates are time-varying, please
#'     manually perform this step before calling [brm_data()].
#'   * Arrange the rows of the data by group, then patient, then discrete time.
#'   * Select only the columns of the data relevant to an MMRM analysis.
#' @section Separation string:
#'   Post-processing in [brm_marginal_draws()] names each of the
#'   group-by-time marginal means with the delimiting character string
#'   from `Sys.getenv("BRM_SEP", unset = "|")`. Neither the column names
#'   nor element names of the group and time variables can contain
#'   this string. To set a custom string yourself, use
#'   `Sys.setenv(BRM_SEP = "YOUR_CUSTOM_STRING")`.
#' @return A classed tibble with attributes which denote features of
#'   the data such as the treatment group and discrete time variables.
#' @param data Data frame or tibble with longitudinal data.
#' @param outcome Character of length 1, name of the continuous
#'   outcome variable.
#'   Example possibilities from clinical trial datasets include
#'   `"CHG"` and `"AVAL"`.
#'   The `outcome` column in the data should be a numeric vector.
#' @param baseline Character of length 1,
#'   name of the baseline response variable (for example, `"BASE"`
#'   in many clinical trial datasets).
#'   Only relevant if the response variable is change from baseline.
#'   Supply `NULL` to ignore or omit.
#' @param group Character of length 1, name of the treatment group variable.
#'   Example possibilities from clinical trial datasets include
#'   `"TRT01P"`, `"TREATMENT"`, `"TRT"`, and `"GROUP"`.
#'   The `group` column in the data should be a
#'   character vector or unordered factor.
#' @param subgroup Character of length 1, optional name of the a
#'   discrete subgroup variable. Set to `NULL` to omit the subgroup (default).
#'   If present, the `subgroup` column in the data should be a
#'   character vector or unordered factor.
#' @param time Character of length 1, name of the discrete time variable.
#'   Example possibilities from clinical trial datasets include
#'   `"AVISIT"` and `"VISIT"`.
#'   For most analyses, please ensure the time column in the data
#'   is an ordered factor. You can easily turn
#'   the time variable into an ordered factor using
#'   [brm_data_chronologize()], either before or immediately after
#'   [brm_data()] (but before any `brm_archetype_*()` functions).
#'   This ensures the time points sort in chronological order,
#'   which ensures the correctness of informative prior archetypes and
#'   autoregressive / moving average correlation structures.
#'
#'   Ordinarily, ordered factors automatically use polynomial contrasts from
#'   [contr.poly()]. This is undesirable for MMRMs, so if the time variable
#'   is an ordered factor, then [brm_data()]
#'   manually sets `contrasts(data[[time]])` to a set of treatment contrasts
#'   using [contr.treatment()]. If you prefer different contrasts, please
#'   manually set `contrasts(data[[time]])` to something else after
#'   calling [brm_data()].
#' @param patient Character of length 1, name of the patient ID variable.
#'   Example possibilities from clinical trial datasets include
#'   `"USUBJID"`, `"SUBJID"`, `"PATIENT"`, `"PATIENTID"`, `"SUBJECT"`,
#'   `"SUBJIDID"`, `"SBJID"`, `"STYSID1A"`, `"SBJ1N"`, and `"ID"`.
#'   The `patient` column in the data should be a factor or character vector.
#' @param covariates Character vector of names of other covariates.
#'   All these covariates are assumed to be non-time-varying. For time-varying
#'   covariates, please manually expand the data to the full grid of patients
#'   and time points before you call [brm_data()]. See the "Preprocessing"
#'   section for details.
#' @param missing Character of length 1, name of an optional variable
#'   in a simulated dataset to indicate which outcome values should be missing.
#'   Set to `NULL` to omit.
#' @param reference_group Atomic value of length 1, Level of the `group`
#'   column to indicate the control group.
#'   Example possibilities from clinical trial datasets include
#'   `"Placebo"`, `"PLACEBO"`, `"PBO"`, `"PLB"`, `"CONTROL"`, `"CTRL"`,
#'   `"REFERENCE"`, and `"REF"`.
#'   `reference_group` only applies to the post-processing that happens
#'   in functions like [brm_marginal_draws()] downstream of the model.
#'   It does not control the fixed effect mapping in the
#'   model matrix that `brms` derives from the formula from `brm_formula()`.
#' @param reference_subgroup Atomic value of length 1,
#'   level of the `subgroup` column
#'   to use as a reference for pairwise differences in when computing
#'   marginal means downstream of the model.
#'   It does not control the fixed effect mapping in the
#'   model matrix that `brms` derives from the formula from `brm_formula()`.
#' @param reference_time Atomic value of length 1 or `NULL`,
#'   level of the `time` column to indicate the baseline time point.
#'   Leave as `NULL` if there is no baseline or baseline is not included
#'   in `data[[time]]`.
#'
#'   If `reference_time` is not `NULL`, then [brm_marginal_draws()] will
#'   calculate change from baseline, and it will calculate treatment
#'   differences as differences between change-from-baseline values.
#'   If `reference_time` is not `NULL`, then [brm_marginal_draws()] will
#'   not calculate change from baseline, and it will calculate treatment
#'   differences as differences between response values.
#'
#'   Note: `reference_time` only applies to the post-processing that happens
#'   in functions like [brm_marginal_draws()] downstream of the model.
#'   It does not control the fixed effect mapping in the
#'   model matrix that `brms` derives from the formula from `brm_formula()`.
#' @param role Deprecated as unnecessary on 2024-07-11 (version 1.0.1.9007).
#'   Use `reference_time` to supply a baseline time point value if it exists.
#' @param level_baseline Deprecated on 2024-01-11 (version 0.2.0.9002).
#'   Use `reference_time` instead.
#' @param level_control Deprecated on 2024-01-11 (version 0.2.0.9002).
#'   Use `reference_group` instead.
#' @examples
#' set.seed(0)
#' data <- brm_simulate_simple()$data
#' colnames(data) <- paste0("col_", colnames(data))
#' data
#' brm_data(
#'   data = data,
#'   outcome = "col_response",
#'   group = "col_group",
#'   time = "col_time",
#'   patient = "col_patient",
#'   reference_group = "group_1",
#'   reference_time = "time_1"
#' )
brm_data <- function(
  data,
  outcome,
  baseline = NULL,
  group,
  subgroup = NULL,
  time,
  patient,
  covariates = character(0L),
  missing = NULL,
  reference_group,
  reference_subgroup = NULL,
  reference_time = NULL,
  role = NULL,
  level_baseline = NULL,
  level_control = NULL
) {
  assert(is.data.frame(data), message = "data arg must be a data frame.")
  if (!is.null(role)) {
    brm_deprecate(
      "The 'role' argument was deprecated as unnecessary on 2024-07-11 ",
      "(version 1.0.1.9007). Use reference_time to specify a baseline ",
      "time value if it exists."
    )
  }
  if (!is.null(level_control)) {
    brm_deprecate(
      "level_control was deprecated on 2024-01-11 (version 0.2.0.9002). ",
      "Use reference_group instead. Setting reference_group <- level_control",
      " for this one call to brm_data()."
    )
    reference_group <- level_control
  }
  if (!is.null(level_baseline)) {
    brm_deprecate(
      "level_baseline was deprecated on 2024-01-11 (version 0.2.0.9002). ",
      "Use reference_time instead. Setting reference_time <- level_baseline",
      " for this one call to brm_data()."
    )
    reference_time <- level_baseline
  }
  out <- brm_data_new(
    data = data,
    brm_outcome = as.character(outcome),
    brm_baseline = baseline,
    brm_group = as.character(group),
    brm_subgroup = subgroup,
    brm_time = as.character(time),
    brm_patient = as.character(patient),
    brm_covariates = as.character(covariates),
    brm_missing = missing,
    brm_reference_group = reference_group,
    brm_reference_subgroup = reference_subgroup,
    brm_reference_time = reference_time
  )
  brm_data_validate(data = out)
  brm_data_preprocess(out)
}

brm_data_new <- function(
  data,
  brm_outcome = NULL,
  brm_baseline = NULL,
  brm_group = NULL,
  brm_subgroup = NULL,
  brm_time = NULL,
  brm_patient = NULL,
  brm_covariates = NULL,
  brm_missing = NULL,
  brm_reference_group = NULL,
  brm_reference_subgroup = NULL,
  brm_reference_time = NULL
) {
  out <- tibble::new_tibble(x = data, class = "brms_mmrm_data")
  structure(
    out,
    brm_outcome = brm_outcome,
    brm_baseline = brm_baseline,
    brm_group = brm_group,
    brm_subgroup = brm_subgroup,
    brm_time = brm_time,
    brm_patient = brm_patient,
    brm_covariates = brm_covariates,
    brm_missing = brm_missing,
    brm_reference_group = brm_reference_group,
    brm_reference_subgroup = brm_reference_subgroup,
    brm_reference_time = brm_reference_time
  )
}

brm_data_preprocess <- function(out) {
  out <- brm_data_fill(out)
  out <- brm_time_contrasts(out)
  out
}

brm_data_validate <- function(data) {
  UseMethod("brm_data_validate")
}

#' @export
brm_data_validate.default <- function(data) {
  outcome <- attr(data, "brm_outcome")
  baseline <- attr(data, "brm_baseline")
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  missing <- attr(data, "brm_missing")
  reference_group <- attr(data, "brm_reference_group")
  reference_subgroup <- attr(data, "brm_reference_subgroup")
  reference_time <- attr(data, "brm_reference_time")
  assert(is.data.frame(data), message = "data must be a data frame")
  assert(
    inherits(data, "brms_mmrm_data"),
    message = "please use brm_data() to preprocess your data"
  )
  assert_chr(outcome, "outcome of data must be a nonempty character string")
  assert_chr(
    baseline %|||% "x",
    "baseline must NULL or a nonempty character string"
  )
  assert_chr(group, "group of data must be a nonempty character string")
  assert_chr(
    subgroup %|||% "x",
    "subgroup of data must be NULL or a nonempty character string"
  )
  assert_chr(time, "time of data must be a nonempty character string")
  assert_chr(patient, "patient of data must be a nonempty character string")
  assert_chr_vec(covariates, "covariates of data must be a character vector")
  assert_chr(missing %|||% "missing", "missing must NULL or character")
  assert_chr(reference_group, "reference_group must be a nonempty string")
  assert_chr(
    reference_subgroup %|||% "x",
    paste(
      "reference_subgroup must NULL or a nonempty element of the subgroup",
      "column in the data"
    )
  )
  assert_chr(
    reference_time %|||% "x",
    paste(
      "reference_time must NULL or a nonempty element of the time column",
      "in the data"
    )
  )
  assert_col(outcome, data)
  assert_col(baseline, data)
  assert_col(group, data)
  assert_col(subgroup, data)
  assert_col(time, data)
  assert_col(patient, data)
  assert_col(covariates, data)
  assert_col(missing, data)
  assert_machine_names(outcome)
  assert_machine_names(baseline %|||% "baseline")
  assert_machine_names(group)
  assert_machine_names(subgroup)
  assert_machine_names(time)
  assert_machine_names(patient)
  assert_machine_names(covariates)
  assert_machine_names(missing)
  assert(
    reference_group,
    !anyNA(.),
    length(.) == 1L,
    . %in% data[[group]],
    message = "reference_group must be an element of data[[group]]"
  )
  if (!is.null(subgroup)) {
    assert(
      reference_subgroup,
      !anyNA(.),
      length(.) == 1L,
      . %in% data[[subgroup]],
      message = paste(
        "reference_subgroup must be an element of data[[subgroup]]"
      )
    )
  }
  if (!is.null(reference_time)) {
    assert(
      reference_time,
      !anyNA(.),
      length(.) == 1L,
      . %in% data[[time]],
      message = "reference_time must be an element of data[[time]]"
    )
  }
  sep <- brm_sep()
  elements <- c(
    group,
    subgroup,
    time,
    data[[group]],
    data[[time]]
  )
  if (!is.null(subgroup)) {
    elements <- c(elements, data[[subgroup]])
  }
  assert(
    !any(grepl(pattern = sep, x = unique(elements), fixed = TRUE)),
    message = sprintf(
      paste(
        "The separation string \"%s\" must not be contained in",
        "the names or elements of the group, subgroup,",
        "or time columns in the data.",
        "Either remove this string or set a different separation string",
        "with Sys.setenv(BRM_SEP = \"YOUR_SEPARATION_STRING\")."
      ),
      sep
    )
  )
  assert(
    is.numeric(data[[outcome]]),
    message = "outcome variable in the data must be numeric."
  )
  if (!is.null(baseline)) {
    assert(
      is.numeric(data[[outcome]]),
      message = "baseline variable must be numeric if supplied."
    )
  }
  for (column in c(baseline, group, subgroup, time, patient, covariates)) {
    assert(
      !anyNA(data[[column]]),
      message = sprintf(
        "no missing values allowed in column \"%s\"",
        column
      )
    )
  }
  for (column in c(group, subgroup, time)) {
    assert(
      is.atomic(data[[column]]) || is.factor(data[[column]]),
      !is.factor(data[[column]]) || !anyNA(levels(data[[column]])),
      message = paste(
        column,
        paste(
          "column in the data must be an atomic or factor type,",
          "and all factor levels must be non-missing."
        )
      )
    )
  }
}

brm_data_fill <- function(data) {
  UseMethod("brm_data_fill")
}

#' @export
brm_data_fill.default <- function(data) {
  brm_error(
    "brm_data_fill() is only valid for brm_data() objects,",
    "not arbitrary data or informative prior archetypes."
  )
}

#' @export
brm_data_fill.brms_mmrm_data <- function(data) {
  class <- class(data)
  attributes <- brm_data_attributes(data)
  baseline <- attr(data, "brm_baseline")
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  patient <- attr(data, "brm_patient")
  covariates <- attr(data, "brm_covariates")
  missing <- attr(data, "brm_missing")
  interest <- attr(data, "brm_archetype_interest")
  nuisance <- attr(data, "brm_archetype_nuisance")
  data <- droplevels(data)
  args <- list(data = data, as.symbol(patient), as.symbol(time))
  data <- do.call(what = tidyr::complete, args = args)
  args <- list(.data = data, as.symbol(patient), as.symbol(time))
  data <- do.call(what = dplyr::arrange, args = args)
  columns <- c(
    baseline,
    group,
    subgroup,
    covariates,
    missing,
    interest,
    nuisance
  )
  for (column in columns) {
    data[[column]] <- brm_data_fill_column(data[[column]], data[[patient]])
  }
  args <- if_any(
    is.null(subgroup),
    list(
      .data = data,
      as.symbol(group),
      as.symbol(patient),
      as.symbol(time)
    ),
    list(
      .data = data,
      as.symbol(group),
      as.symbol(subgroup),
      as.symbol(patient),
      as.symbol(time)
    )
  )
  out <- do.call(what = dplyr::arrange, args = args)
  class(out) <- class
  for (name in names(attributes)) {
    attr(out, name) <- attributes[[name]]
  }
  times <- brm_levels(out[[time]])
  assert(
    length(unique(table(out[[time]]))) == 1L,
    message = paste(
      "data could not be filled. Please submit a bug report to",
      "https://github.com/openpharma/brms.mmrm/issues",
      "and include a reproducible example."
    )
  )
  out
}

brm_time_contrasts <- function(data) {
  time <- attr(data, "brm_time")
  if (is.ordered(data[[time]])) {
    n <- length(unique(data[[time]]))
    contrasts(data[[time]]) <- stats::contr.treatment(n = n)
  }
  data
}

brm_levels <- function(x) {
  if_any(is.factor(x), intersect(levels(x), unique(x)), sort(unique(x)))
}

brm_data_fill_column <- function(x, index) {
  out <- tapply(
    X = x,
    INDEX = index,
    FUN = brm_data_locf
  )
  unlist(out, use.names = FALSE)
}

brm_data_locf <- function(x) {
  x <- zoo::na.locf(x, fromLast = FALSE, na.rm = FALSE)
  x <- zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)
  x
}

brm_data_attributes <- function(data) {
  out <- attributes(data)
  out <- out[grep("^brm_", names(out), value = TRUE)]
  out
}

brm_data_has_subgroup <- function(data) {
  !is.null(attr(data, "brm_subgroup"))
}
