test_that("brm_marginal_draws() on response, no subgroup", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = brm_simulate_simple()$data,
    outcome = "response",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1",
    reference_time = "time_1"
  )
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  out <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data
  )
  formula_exclude <- formula
  attr(formula_exclude, "brm_allow_effect_size") <- FALSE
  expect_warning(
    excluded <- brm_marginal_draws(
      model = model,
      formula = formula_exclude,
      data = data,
      effect_size = TRUE
    ),
    class = "brm_warn"
  )
  expect_false("effect" %in% names(excluded))
  expect_warning(
    brm_marginal_draws(
      model = model,
      formula = formula,
      data = data,
      control = "group_1"
    ),
    class = "brm_deprecate"
  )
  expect_warning(
    brm_marginal_draws(
      model = model,
      formula = formula,
      data = data,
      baseline = "time_1"
    ),
    class = "brm_deprecate"
  )
  fields <- c(
    "response",
    "difference_time",
    "difference_group",
    "effect",
    "sigma"
  )
  columns_df <- expand.grid(
    group = sort(unique(data$group)),
    time = sort(unique(data$time)),
    stringsAsFactors = FALSE
  )
  columns <- paste(columns_df$group, columns_df$time, sep = brm_sep())
  expect_equal(sort(names(out)), sort(fields))
  for (field in fields) {
    x <- out[[field]]
    expect_true(tibble::is_tibble(x))
    expect_true(all(colnames(x) %in% c(columns, names_mcmc)))
    expect_false(any(unlist(lapply(x, anyNA))))
    expect_equal(nrow(x), 50)
  }
  expect_equal(
    sort(colnames(out$response)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$time != "time_1", ]
  columns <- paste(columns_df$group, columns_df$time, sep = brm_sep())
  expect_equal(
    sort(colnames(out$difference_time)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$group != "group_1", ]
  columns <- paste(columns_df$group, columns_df$time, sep = brm_sep())
  expect_equal(
    sort(colnames(out$difference_group)),
    sort(c(columns, names_mcmc))
  )
  draws <- tibble::as_tibble(posterior::as_draws_df(model))
  draws <- draws[, grep("^b_sigma_", colnames(draws), value = TRUE)]
  colnames(draws) <- gsub("^b_sigma_", "", colnames(draws))
  colnames(draws) <- gsub(paste0("^time"), "", x = colnames(draws))
  sigma <- exp(draws)
  for (group in setdiff(unique(data$group), "group_1")) {
    for (time in setdiff(unique(data$time), "time_1")) {
      name1 <- paste("group_1", time, sep = brm_sep())
      name2 <- paste(group, time, sep = brm_sep())
      expect_equal(
        out$difference_group[[name2]],
        out$difference_time[[name2]] - out$difference_time[[name1]]
      )
      expect_equal(
        out$effect[[name2]],
        out$difference_group[[name2]] / sigma[[time]]
      )
      expect_equal(out$sigma[[name2]], sigma[[time]])
    }
  }
  for (group in unique(data$group)) {
    for (time in setdiff(unique(data$time), "time_1")) {
      name1 <- paste(group, "time_1", sep = brm_sep())
      name2 <- paste(group, time, sep = brm_sep())
      expect_equal(
        out$difference_time[[name2]],
        out$response[[name2]] - out$response[[name1]]
      )
    }
  }
})

test_that("brm_marginal_draws() on response without baseline", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = brm_simulate_simple()$data,
    outcome = "response",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1"
  )
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  out <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data
  )
  fields <- c(
    "response",
    "difference_group",
    "effect",
    "sigma"
  )
  expect_equal(sort(names(out)), sort(fields))
})

test_that("brm_marginal_draws() on response, with subgroup", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_subgroup = 2L,
    n_patient = 25L,
    n_time = 4L
  )
  data$response <- rnorm(n = nrow(data))
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  suppressMessages(
    expect_warning(
      brm_marginal_draws(
        model = model,
        formula = formula,
        data = data,
        use_subgroup = TRUE
      ),
      class = "brm_deprecate"
    )
  )
  out <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data
  )
  fields <- c(
    "response",
    "difference_time",
    "difference_group",
    "difference_subgroup",
    "effect",
    "sigma"
  )
  columns_df <- expand.grid(
    group = sort(unique(data$group)),
    subgroup = sort(unique(data$subgroup)),
    time = sort(unique(data$time)),
    stringsAsFactors = FALSE
  )
  columns <- paste(
    columns_df$group,
    columns_df$subgroup,
    columns_df$time,
    sep = brm_sep()
  )
  expect_equal(sort(names(out)), sort(fields))
  for (field in fields) {
    x <- out[[field]]
    expect_true(tibble::is_tibble(x))
    expect_true(all(colnames(x) %in% c(columns, names_mcmc)))
    expect_false(any(unlist(lapply(x, anyNA))))
    expect_equal(nrow(x), 50)
  }
  expect_equal(
    sort(colnames(out$response)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$time != "time_1", ]
  columns <- paste(
    columns_df$group,
    columns_df$subgroup,
    columns_df$time,
    sep = brm_sep()
  )
  expect_equal(
    sort(colnames(out$difference_time)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$group != "group_1", ]
  columns <- paste(
    columns_df$group,
    columns_df$subgroup,
    columns_df$time,
    sep = brm_sep()
  )
  expect_equal(
    sort(colnames(out$difference_group)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$subgroup != "subgroup_1", ]
  columns <- paste(
    columns_df$group,
    columns_df$subgroup,
    columns_df$time,
    sep = brm_sep()
  )
  expect_equal(
    sort(colnames(out$difference_subgroup)),
    sort(c(columns, names_mcmc))
  )
  draws <- tibble::as_tibble(posterior::as_draws_df(model))
  draws <- draws[, grep("^b_sigma_", colnames(draws), value = TRUE)]
  colnames(draws) <- gsub("^b_sigma_", "", colnames(draws))
  colnames(draws) <- gsub(paste0("^time"), "", x = colnames(draws))
  sigma <- exp(draws)
  for (group in setdiff(unique(data$group), "group_1")) {
    for (subgroup in setdiff(unique(data$subgroup), "subgroup_1")) {
      for (time in setdiff(unique(data$time), "time_1")) {
        name1 <- paste(group, "subgroup_1", time, sep = brm_sep())
        name2 <- paste(group, subgroup, time, sep = brm_sep())
        expect_equal(
          out$difference_subgroup[[name2]],
          out$difference_group[[name2]] - out$difference_group[[name1]]
        )
      }
    }
  }
  for (group in setdiff(unique(data$group), "group_1")) {
    for (subgroup in unique(data$subgroup)) {
      for (time in setdiff(unique(data$time), "time_1")) {
        name1 <- paste("group_1", subgroup, time, sep = brm_sep())
        name2 <- paste(group, subgroup, time, sep = brm_sep())
        expect_equal(
          out$difference_group[[name2]],
          out$difference_time[[name2]] - out$difference_time[[name1]]
        )
        expect_equal(
          out$effect[[name2]],
          out$difference_group[[name2]] / sigma[[time]]
        )
        expect_equal(out$sigma[[name2]], sigma[[time]])
      }
    }
  }
  for (group in unique(data$group)) {
    for (subgroup in unique(data$subgroup)) {
      for (time in setdiff(unique(data$time), "time_1")) {
        name1 <- paste(group, subgroup, "time_1", sep = brm_sep())
        name2 <- paste(group, subgroup, time, sep = brm_sep())
        expect_equal(
          out$difference_time[[name2]],
          out$response[[name2]] - out$response[[name1]]
        )
      }
    }
  }
})

test_that("brm_marginal_draws() on change, homogeneous var, no subgroup", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = tibble::as_tibble(brm_simulate_simple()$data),
    outcome = "response",
    group = "group",
    time = "time",
    patient = "patient",
    reference_group = "group_1"
  )
  formula <- brm_formula(
    data = data,
    baseline = FALSE,
    baseline_time = FALSE,
    sigma = brm_formula_sigma(data = data, intercept = TRUE, time = FALSE)
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  out <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data,
    transform = brm_transform_marginal(
      data = data,
      formula = formula,
      average_within_subgroup = FALSE
    )
  )
  fields <- c("response", "difference_group", "effect", "sigma")
  columns_df <- expand.grid(
    group = sort(unique(data$group)),
    time = sort(unique(data$time)),
    stringsAsFactors = FALSE
  )
  columns <- paste(columns_df$group, columns_df$time, sep = brm_sep())
  expect_equal(sort(names(out)), sort(fields))
  for (field in fields) {
    x <- out[[field]]
    expect_true(tibble::is_tibble(x))
    expect_true(all(colnames(x) %in% c(columns, names_mcmc)))
    expect_false(any(unlist(lapply(x, anyNA))))
    expect_equal(nrow(x), 50)
  }
  expect_equal(
    sort(colnames(out$response)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$group != "group_1", ]
  columns <- paste(columns_df$group, columns_df$time, sep = brm_sep())
  expect_equal(
    sort(colnames(out$difference_group)),
    sort(c(columns, names_mcmc))
  )
  draws <- tibble::as_tibble(posterior::as_draws_df(model))
  draws <- draws[, grep("^b_sigma_", colnames(draws), value = TRUE)]
  colnames(draws) <- gsub("^b_sigma_", "", colnames(draws))
  colnames(draws) <- gsub(paste0("^time"), "", x = colnames(draws))
  sigma <- exp(draws)
  for (group in setdiff(unique(data$group), "group_1")) {
    for (time in unique(data$time)) {
      name1 <- paste("group_1", time, sep = brm_sep())
      name2 <- paste(group, time, sep = brm_sep())
      expect_equal(
        out$difference_group[[name2]],
        out$response[[name2]] - out$response[[name1]]
      )
      expect_equal(
        out$effect[[name2]],
        out$difference_group[[name2]] / sigma[[1L]]
      )
      expect_equal(out$sigma[[name2]], sigma[[1L]])
    }
  }
})

test_that("brm_marginal_draws() on change, homogeneous var, with subgroup", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_simulate_outline(
    n_group = 2L,
    n_subgroup = 2L,
    n_patient = 25L,
    n_time = 4L
  )
  data$response <- rnorm(n = nrow(data))
  data <- brm_data_change(
    data,
    name_change = "change",
    name_baseline = "baseline"
  )
  formula <- brm_formula(
    data = data,
    sigma = brm_formula_sigma(data = data, intercept = TRUE, time = FALSE)
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  out <- brm_marginal_draws(
    model = model,
    formula = formula,
    data = data,
    transform = brm_transform_marginal(
      data = data,
      formula = formula,
      average_within_subgroup = FALSE
    )
  )
  fields <- c(
    "response",
    "difference_group",
    "difference_subgroup",
    "effect",
    "sigma"
  )
  expect_equal(sort(names(out)), sort(fields))
  columns_df <- expand.grid(
    group = sort(unique(data$group)),
    subgroup = sort(unique(data$subgroup)),
    time = sort(unique(data$time)),
    stringsAsFactors = FALSE
  )
  columns <- paste(
    columns_df$group,
    columns_df$subgroup,
    columns_df$time,
    sep = brm_sep()
  )
  for (field in fields) {
    x <- out[[field]]
    expect_true(tibble::is_tibble(x))
    expect_true(all(colnames(x) %in% c(columns, names_mcmc)))
    expect_false(any(unlist(lapply(x, anyNA))))
    expect_equal(nrow(x), 50)
  }
  expect_equal(
    sort(colnames(out$response)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$group != "group_1", ]
  columns <- paste(
    columns_df$group,
    columns_df$subgroup,
    columns_df$time,
    sep = brm_sep()
  )
  expect_equal(
    sort(colnames(out$difference_group)),
    sort(c(columns, names_mcmc))
  )
  columns_df <- columns_df[columns_df$subgroup != "subgroup_1", ]
  columns <- paste(
    columns_df$group,
    columns_df$subgroup,
    columns_df$time,
    sep = brm_sep()
  )
  expect_equal(
    sort(colnames(out$difference_subgroup)),
    sort(c(columns, names_mcmc))
  )
  for (group in setdiff(unique(data$group), "group_1")) {
    for (subgroup in unique(data$subgroup)) {
      for (time in unique(data$time)) {
        name1 <- paste("group_1", subgroup, time, sep = brm_sep())
        name2 <- paste(group, subgroup, time, sep = brm_sep())
        expect_equal(
          out$difference_group[[name2]],
          out$response[[name2]] - out$response[[name1]]
        )
      }
    }
  }
  draws <- tibble::as_tibble(posterior::as_draws_df(model))
  draws <- draws[, grep("^b_sigma_", colnames(draws), value = TRUE)]
  colnames(draws) <- gsub("^b_sigma_", "", colnames(draws))
  colnames(draws) <- gsub(paste0("^time"), "", x = colnames(draws))
  sigma <- exp(draws)
  for (group in setdiff(unique(data$group), "group_1")) {
    for (subgroup in setdiff(unique(data$subgroup), "subgroup_1")) {
      for (time in unique(data$time)) {
        name1 <- paste(group, "subgroup_1", time, sep = brm_sep())
        name2 <- paste(group, subgroup, time, sep = brm_sep())
        expect_equal(
          out$difference_subgroup[[name2]],
          out$difference_group[[name2]] - out$difference_group[[name1]]
        )
        expect_equal(
          out$effect[[name2]],
          out$difference_group[[name2]] / sigma[[1L]]
        )
        expect_equal(out$sigma[[name2]], sigma[[1L]])
      }
    }
  }
})
