#' @title Cell-means-like successive differences archetype
#' @export
#' @family informative prior archetypes
#' @description Create an informative prior archetype where the fixed effects
#'   are successive differences between adjacent time points.
#' @details In this archetype, each fixed effect is either an intercept
#'   on the first time point or the difference between two adjacent time
#'   points, and each treatment group has its own set of fixed effects
#'   independent of the other treatment groups.
#'
#'   To illustrate, suppose the dataset has two treatment groups A and B,
#'   time points 1, 2, and 3, and no other covariates.
#'   Let `mu_gt` be the marginal mean of the response at group
#'   `g` time `t` given data and hyperparameters.
#'   The model has fixed effect parameters `beta_1`, `beta_2`, ..., `beta_6`
#'   which express the marginal means `mu_gt` as follows:
#'
#'       `mu_A1 = beta_1`
#'       `mu_A2 = beta_1 + beta_2`
#'       `mu_A3 = beta_1 + beta_2 + beta_3`
#'
#'       `mu_B1 = beta_4`
#'       `mu_B2 = beta_4 + beta_5`
#'       `mu_B3 = beta_4 + beta_5 + beta_6`
#'
#'   For group A, `beta_1` is the time 1 intercept, `beta_2` represents
#'   time 2 minus time 1, and `beta_3` represents time 3 minus time 2.
#'   `beta_4`, `beta_5`, and `beta_6` behave analogously for group B.
#' @section Nuisance variables:
#'   In the presence of covariate adjustment, functions like
#'   [brm_archetype_successive_cells()] convert nuisance factors into binary
#'   dummy variables, then center all those dummy variables and any
#'   continuous nuisance variables at their means in the data.
#'   This ensures that the main model coefficients
#'   of interest are not implicitly conditional on a subset of the data.
#'   In other words, preprocessing nuisance variables this way preserves
#'   the interpretations of the fixed effects of interest, and it ensures
#'   informative priors can be specified correctly.
#' @inheritSection brm_prior_archetype Prior labeling
#' @section Prior labeling for [brm_archetype_successive_cells()]:
#'   Within each treatment group, each intercept is labeled by the earliest
#'   time point, and each successive difference term gets the successive
#'   time point as the label.
#'   To illustrate, consider the example in the Details section.
#'   In the labeling scheme for [brm_archetype_successive_cells()],
#'   you can label the prior on `beta_1` using
#'   `brm_prior_label(code = "normal(1.2, 5)", group = "A", time = "1")`.
#'   Similarly, you cal label the prior on `beta_5` with
#'   `brm_prior_label(code = "normal(1.3, 7)", group = "B", time = "2")`.
#'   To confirm that you set the prior correctly, compare the `brms` prior
#'   with the output of `summary(your_archetype)`.
#'   See the examples for details.
#' @return A special classed `tibble` with data tailored to
#'   the successive differences archetype. The dataset is augmented with
#'   extra columns with the `"archetype_"` prefix, as well as special
#'   attributes to tell downstream functions like [brm_formula()] what to
#'   do with the object.
#' @inheritParams brm_formula
#' @inheritParams brm_model
#' @param prefix_interest Character string to prepend to the new columns
#'   of generated fixed effects of interest (relating to group, subgroup,
#'   and/or time).
#'   In rare cases, you may need to set a non-default prefix to prevent
#'   name conflicts with existing columns in the data, or rename
#'   the columns in your data.
#'   `prefix_interest` must not be the same value as `prefix_nuisance`.
#' @param prefix_nuisance Same as `prefix_interest`, but relating to
#'   generated fixed effects NOT of interest (not relating to group,
#'   subgroup, or time). Must not be the same value as `prefix_interest`.
#' @param intercept `TRUE` to make one of the parameters an intercept,
#'   `FALSE` otherwise. If `TRUE`, then the interpretation of the
#'   parameters in the "Details" section will change, and you are
#'   responsible for manually calling `summary()` on the archetype
#'   and interpreting the parameters according to the output.
#'   In addition, you are responsible for setting an
#'   appropriate prior on the intercept. In normal usage, `brms` looks for
#'   a model parameter called `"Intercept"` and uses the data to set the prior
#'   to help the MCMC runs smoothly. If `intercept = TRUE` for informative
#'   prior archetypes, the intercept will be called something else, and
#'   `brms` cannot auto-generate a sensible default prior.
#' @param clda `TRUE` to opt into constrained longitudinal data analysis
#'   (cLDA), `FALSE` otherwise. To use cLDA, `reference_time` must have been
#'   non-`NULL` in the call to [brm_data()] used to construct the data.
#'
#'   Some archetypes cannot support cLDA
#'   (e.g. [brm_archetype_average_cells()] and
#'   [brm_archetype_average_effects()]).
#'
#'   In cLDA, the fixed effects parameterization
#'   is restricted such that all treatment groups are pooled at baseline.
#'   (If you supplied a `subgroup` variable in [brm_data()], then
#'   this constraint is applied separately within each subgroup variable.)
#'   cLDA may result in more precise estimates when the `time` variable
#'   has a baseline level and the baseline outcomes are recorded
#'   before randomization in a clinical trial.
#' @examples
#' set.seed(0L)
#' data <- brm_simulate_outline(
#'   n_group = 2,
#'   n_patient = 100,
#'   n_time = 4,
#'   rate_dropout = 0,
#'   rate_lapse = 0
#' ) |>
#'   dplyr::mutate(response = rnorm(n = dplyr::n())) |>
#'   brm_data_change() |>
#'   brm_simulate_continuous(names = c("biomarker1", "biomarker2")) |>
#'   brm_simulate_categorical(
#'     names = c("status1", "status2"),
#'     levels = c("present", "absent")
#'   )
#' dplyr::select(
#'   data,
#'   group,
#'   time,
#'   patient,
#'   starts_with("biomarker"),
#'   starts_with("status")
#' )
#' archetype <- brm_archetype_successive_cells(data)
#' archetype
#' summary(archetype)
#' formula <- brm_formula(archetype)
#' formula
#' prior <- brm_prior_label(
#'   code = "normal(1, 2.2)",
#'   group = "group_1",
#'   time = "time_2"
#' ) |>
#'   brm_prior_label("normal(1, 3.3)", group = "group_1", time = "time_3") |>
#'   brm_prior_label("normal(1, 4.4)", group = "group_1", time = "time_4") |>
#'   brm_prior_label("normal(2, 2.2)", group = "group_2", time = "time_2") |>
#'   brm_prior_label("normal(2, 3.3)", group = "group_2", time = "time_3") |>
#'   brm_prior_label("normal(2, 4.4)", group = "group_2", time = "time_4") |>
#'   brm_prior_archetype(archetype)
#' prior
#' class(prior)
#' if (identical(Sys.getenv("BRM_EXAMPLES", unset = ""), "true")) {
#' tmp <- utils::capture.output(
#'   suppressMessages(
#'     suppressWarnings(
#'       model <- brm_model(
#'         data = archetype,
#'         formula = formula,
#'         prior = prior,
#'         chains = 1,
#'         iter = 100,
#'         refresh = 0
#'       )
#'     )
#'   )
#' )
#' suppressWarnings(print(model))
#' brms::prior_summary(model)
#' draws <- brm_marginal_draws(
#'   data = archetype,
#'   formula = formula,
#'   model = model
#' )
#' summaries_model <- brm_marginal_summaries(draws)
#' summaries_data <- brm_marginal_data(data)
#' brm_plot_compare(model = summaries_model, data = summaries_data)
#' }
brm_archetype_successive_cells <- function(
  data,
  intercept = FALSE,
  baseline = !is.null(attr(data, "brm_baseline")),
  baseline_subgroup = !is.null(attr(data, "brm_baseline")) &&
    !is.null(attr(data, "brm_subgroup")),
  baseline_subgroup_time = !is.null(attr(data, "brm_baseline")) &&
    !is.null(attr(data, "brm_subgroup")),
  baseline_time = !is.null(attr(data, "brm_baseline")),
  covariates = TRUE,
  clda = FALSE,
  prefix_interest = "x_",
  prefix_nuisance = "nuisance_"
) {
  brm_data_validate.default(data)
  data <- brm_data_remove_archetype(data)
  data <- brm_data_fill(data)
  brm_archetype_assert_prefixes(
    prefix_interest = prefix_interest,
    prefix_nuisance = prefix_nuisance
  )
  archetype <- if_any(
    brm_data_has_subgroup(data),
    archetype_successive_cells_subgroup(data, clda, prefix_interest),
    archetype_successive_cells(data, clda, prefix_interest)
  )
  brm_archetype_init(
    data = data,
    interest = archetype$interest,
    mapping = archetype$mapping,
    intercept = intercept,
    baseline = baseline,
    baseline_subgroup = baseline_subgroup,
    baseline_subgroup_time = baseline_subgroup_time,
    baseline_time = baseline_time,
    covariates = covariates,
    prefix_nuisance = prefix_nuisance,
    subclass = "brms_mmrm_successive_cells"
  )
}

archetype_successive_cells <- function(data, clda, prefix) {
  group <- attr(data, "brm_group")
  time <- attr(data, "brm_time")
  reference_group <- attr(data, "brm_reference_group")
  reference_time <- attr(data, "brm_reference_time")
  levels_group <- brm_levels(data[[group]])
  levels_time <- brm_levels(data[[time]])
  n_time <- length(levels_time)
  data_first <- data[data[[time]] == levels_time[1L], ]
  matrix_group <- NULL
  for (name in levels_group) {
    matrix_group <- cbind(
      matrix_group,
      as.integer(data_first[[group]] == name)
    )
  }
  matrix_time <- diag(n_time) + lower.tri(diag(n_time))
  matrix <- kronecker(X = matrix_group, Y = matrix_time)
  names_group <- rep(levels_group, each = n_time)
  names_time <- rep(levels_time, times = length(levels_group))
  names <- paste0(prefix, paste(names_group, names_time, sep = "_"))
  colnames(matrix) <- names
  interest <- tibble::as_tibble(as.data.frame(matrix))
  mapping <- tibble::tibble(
    group = names_group,
    time = names_time,
    variable = names
  )
  if (clda) {
    index_keep <- mapping$group == reference_group &
      mapping$time == reference_time
    keep <- mapping$variable[index_keep]
    for (name_group in setdiff(levels_group, reference_group)) {
      index_drop <- mapping$group == name_group &
        mapping$time == reference_time
      drop <- mapping$variable[index_drop]
      interest[[keep]] <- as.integer(interest[[keep]] | interest[[drop]])
      interest[[drop]] <- NULL
      mapping <- mapping[mapping$variable != drop,, drop = FALSE] # nolint
    }
  }
  list(interest = interest, mapping = mapping)
}

archetype_successive_cells_subgroup <- function(data, clda, prefix) {
  group <- attr(data, "brm_group")
  subgroup <- attr(data, "brm_subgroup")
  time <- attr(data, "brm_time")
  reference_group <- attr(data, "brm_reference_group")
  reference_time <- attr(data, "brm_reference_time")
  levels_group <- brm_levels(data[[group]])
  levels_subgroup <- brm_levels(data[[subgroup]])
  levels_time <- brm_levels(data[[time]])
  n_group <- length(levels_group)
  n_subgroup <- length(levels_subgroup)
  n_time <- length(levels_time)
  data_first <- data[data[[time]] == data[[time]][1L], ]
  matrix_group <- NULL
  for (name_group in levels_group) {
    for (name_subgroup in levels_subgroup) {
      in_group_subgroup <- (data_first[[group]] == name_group) &
        (data_first[[subgroup]] == name_subgroup)
      matrix_group <- cbind(matrix_group, as.integer(in_group_subgroup))
    }
  }
  matrix_time <- diag(n_time) + lower.tri(diag(n_time))
  matrix <- kronecker(X = matrix_group, Y = matrix_time)
  names_group <- rep(levels_group, each = n_time * n_subgroup)
  names_subgroup <- rep(rep(levels_subgroup, times = n_group), each = n_time)
  names_time <- rep(levels_time, times = n_group * n_subgroup)
  names <- paste0(
    prefix,
    paste(names_group, names_subgroup, names_time, sep = "_")
  )
  colnames(matrix) <- names
  interest <- tibble::as_tibble(as.data.frame(matrix))
  mapping <- tibble::tibble(
    group = names_group,
    subgroup = names_subgroup,
    time = names_time,
    variable = names
  )
  if (clda) {
    for (name_subgroup in levels_subgroup) {
      index_keep <- mapping$group == reference_group &
        mapping$subgroup == name_subgroup &
        mapping$time == reference_time
      keep <- mapping$variable[index_keep]
      for (name_group in setdiff(levels_group, reference_group)) {
        index_drop <- mapping$group == name_group &
          mapping$subgroup == name_subgroup &
          mapping$time == reference_time
        drop <- mapping$variable[index_drop]
        interest[[keep]] <- as.integer(interest[[keep]] | interest[[drop]])
        interest[[drop]] <- NULL
        mapping <- mapping[mapping$variable != drop,, drop = FALSE] # nolint
      }
    }
  }
  list(interest = interest, mapping = mapping)
}
