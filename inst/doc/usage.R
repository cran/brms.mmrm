## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  eval = identical(tolower(Sys.getenv("NOT_CRAN", unset = "")), "true")
)
library(brms.mmrm)
library(dplyr)

## -----------------------------------------------------------------------------
library(brms.mmrm)
library(dplyr)
set.seed(0L)
raw_data <- brm_simulate(
  n_group = 3,
  n_patient = 100,
  n_time = 4
)$data

raw_data

## -----------------------------------------------------------------------------
data <- brm_data(
  data = raw_data,
  outcome = "response",
  role = "response",
  group = "group",
  patient = "patient",
  time = "time"
)

data

class(data)

roles <- attributes(data)
roles$row.names <- NULL
str(roles)

## -----------------------------------------------------------------------------
brm_formula(
  data = data,
  intercept = FALSE,
  effect_base = FALSE,
  effect_group = FALSE,
  effect_time = FALSE,
  interaction_base = FALSE,
  interaction_group = TRUE
)

## -----------------------------------------------------------------------------
formula <- brm_formula(
  data = data,
  intercept = TRUE,
  effect_base = FALSE,
  effect_group = TRUE,
  effect_time = TRUE,
  interaction_base = FALSE,
  interaction_group = TRUE
)

formula

## ---- paged.print = FALSE-----------------------------------------------------
brms::get_prior(data = data, formula = formula)

## ---- eval = FALSE------------------------------------------------------------
#  model <- brm_model(data = data, formula = formula, refresh = 0)

## ---- include = FALSE---------------------------------------------------------
model <- brm_model(data = data, formula = formula, refresh = 0)

## -----------------------------------------------------------------------------
model

## ---- paged.print = FALSE-----------------------------------------------------
draws <- brm_marginal_draws(
  model = model,
  data = data,
  control = "group 1", # automatically cleaned with make.names()
  baseline = "time 1" # also cleaned with make.names()
)

draws

## ---- paged.print = FALSE-----------------------------------------------------
brm_marginal_draws_average(draws = draws, data = data)

## ---- paged.print = FALSE-----------------------------------------------------
summaries <- brm_marginal_summaries(draws, level = 0.95)

summaries

## -----------------------------------------------------------------------------
brm_marginal_probabilities(
  draws = draws,
  threshold = c(-0.1, 0.1),
  direction = c("greater", "less")
)

## ---- paged.print = FALSE-----------------------------------------------------
summaries_data <- brm_marginal_data(data = data, level = 0.95)

summaries_data

## -----------------------------------------------------------------------------
brm_plot_compare(
  data = summaries_data,
  model1 = summaries,
  model2 = summaries
)

## -----------------------------------------------------------------------------
brm_plot_compare(
  model1 = summaries,
  model2 = summaries,
  marginal = "difference" # treatment effect
)

## -----------------------------------------------------------------------------
brm_plot_draws(draws = draws$difference)

