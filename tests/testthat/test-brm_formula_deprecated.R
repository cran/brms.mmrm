test_that("brm_formula() with default names and all terms", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  suppressWarnings(
    expect_warning(
      out <- brm_formula(
        data = data,
        intercept = TRUE,
        effect_group = TRUE,
        effect_time = TRUE,
        effect_baseline = TRUE,
        interaction_baseline = TRUE,
        interaction_group = TRUE,
        check_rank = FALSE
      ),
      class = "brm_deprecate"
    )
  )
  expect_s3_class(out, "brmsformula")
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})

test_that("brm_formula() with all user-supplied columns and all terms", {
  data <- brm_data(
    data = tibble::tibble(
      y = c(1, 1),
      t = c("x", "y"),
      b = c(2, 2),
      g = c("x", "y"),
      p = c("x", "y"),
      a = c(1, 2)
    ),
    outcome = "y",
    group = "g",
    time = "t",
    baseline = "b",
    patient = "p",
    covariates = c("a", "b"),
    reference_group = "x"
  )
  suppressWarnings(
    expect_warning(
      out <- brm_formula(
        data = data,
        intercept = TRUE,
        effect_group = TRUE,
        effect_time = TRUE,
        effect_baseline = TRUE,
        interaction_baseline = TRUE,
        interaction_group = TRUE,
        check_rank = FALSE
      ),
      class = "brm_deprecate"
    )
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    "y ~ b + b:t + g + g:t + t + a + b + unstr(time = t, gr = p)"
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + t"
    )
  )
})

test_that("brm_formula() without intercept", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  suppressWarnings(
    expect_warning(
      out <- brm_formula(
        data = data,
        intercept = FALSE,
        effect_group = TRUE,
        effect_time = TRUE,
        effect_baseline = TRUE,
        interaction_baseline = TRUE,
        interaction_group = TRUE,
        check_rank = FALSE
      ),
      class = "brm_deprecate"
    )
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ 0 + baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})

test_that("brm_formula() without group effect", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  suppressWarnings(
    expect_warning(
      out <- brm_formula(
        data = data,
        intercept = TRUE,
        effect_group = FALSE,
        effect_time = TRUE,
        effect_baseline = TRUE,
        interaction_baseline = TRUE,
        interaction_group = TRUE,
        check_rank = FALSE
      ),
      class = "brm_deprecate"
    )
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P:AVISIT + AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})

test_that("brm_formula() without time effect", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 3),
      AVISIT = c("x", "y"),
      baseline = c(2, 2),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  suppressWarnings(
    expect_warning(
      out <- brm_formula(
        data = data,
        intercept = TRUE,
        effect_group = TRUE,
        effect_time = FALSE,
        effect_baseline = TRUE,
        interaction_baseline = TRUE,
        interaction_group = TRUE,
        check_rank = FALSE
      ),
      class = "brm_deprecate"
    )
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + TRT01P:AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})

test_that("brm_formula() without baseline effect", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  suppressWarnings(
    expect_warning(
      out <- brm_formula(
        data = data,
        intercept = TRUE,
        effect_group = TRUE,
        effect_time = TRUE,
        effect_baseline = FALSE,
        interaction_baseline = TRUE,
        interaction_group = TRUE,
        check_rank = FALSE
      ),
      class = "brm_deprecate"
    )
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline:AVISIT + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})

test_that("brm_formula() without baseline interaction", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  suppressWarnings(
    expect_warning(
      out <- brm_formula(
        data = data,
        intercept = TRUE,
        effect_group = TRUE,
        effect_time = TRUE,
        effect_baseline = TRUE,
        interaction_baseline = FALSE,
        interaction_group = TRUE,
        check_rank = FALSE
      ),
      class = "brm_deprecate"
    )
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + TRT01P + TRT01P:AVISIT + AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})

test_that("brm_formula() without group interaction", {
  data <- brm_data(
    data = tibble::tibble(
      CHG = c(1, 2),
      AVISIT = c("x", "y"),
      baseline = c(2, 3),
      TRT01P = c("x", "y"),
      USUBJID = c("x", "y")
    ),
    outcome = "CHG",
    group = "TRT01P",
    time = "AVISIT",
    baseline = "baseline",
    patient = "USUBJID",
    reference_group = "x"
  )
  suppressWarnings(
    expect_warning(
      out <- brm_formula(
        data = data,
        intercept = TRUE,
        effect_group = TRUE,
        effect_time = TRUE,
        effect_baseline = TRUE,
        interaction_baseline = TRUE,
        interaction_group = FALSE,
        check_rank = FALSE
      ),
      class = "brm_deprecate"
    )
  )
  expect_equal(
    deparse(out[[1L]], width.cutoff = 500L),
    paste(
      "CHG ~ baseline + baseline:AVISIT + TRT01P + AVISIT",
      "+ unstr(time = AVISIT, gr = USUBJID)"
    )
  )
  expect_equal(
    deparse(out[[2L]][[1L]], width.cutoff = 500L),
    paste(
      "sigma ~ 0 + AVISIT"
    )
  )
})
