## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = rlang::is_installed("emmeans")
)
library(brms.mmrm)
library(dplyr)
library(tidyr)
library(zoo)

## -----------------------------------------------------------------------------
library(brms.mmrm)
library(dplyr)
library(tidyr)
data(fev_data, package = "mmrm")
data <- fev_data |>
  group_by(USUBJID) |>
  complete(AVISIT) |>
  arrange(AVISIT) |>
  fill(
    any_of(c("ARMCD", "FEV1_BL", "RACE", "SEX", "WEIGHT")),
    .direction = "downup"
  ) |>
  mutate(FEV1 = na.locf(FEV1, na.rm = FALSE)) |>
  mutate(FEV1 = na.locf(FEV1, na.rm = FALSE, fromLast = TRUE)) |>
  ungroup() |>
  filter(!is.na(FEV1)) |>
  mutate(FEV1_CHG = FEV1 - FEV1_BL, USUBJID = as.character(USUBJID)) |>
  select(-FEV1) |>
  as_tibble() |>
  arrange(USUBJID, AVISIT) |>
  brm_data(
    outcome = "FEV1_CHG",
    baseline = "FEV1_BL",
    group = "ARMCD",
    patient = "USUBJID",
    time = "AVISIT",
    covariates = c("RACE", "SEX", "WEIGHT"),
    reference_group = "PBO",
    reference_time = "VIS1"
  )

## -----------------------------------------------------------------------------
data

## -----------------------------------------------------------------------------
reference_grid <- distinct(data, ARMCD, AVISIT)
reference_grid

## -----------------------------------------------------------------------------
brms_mmrm_formula <- brm_formula(data, correlation = "diagonal")
base_formula <- as.formula(brms_mmrm_formula[[1]])
attr(base_formula, "nl") <- NULL
attr(base_formula, "loop") <- NULL

## -----------------------------------------------------------------------------
base_formula

## -----------------------------------------------------------------------------
colnames(model.matrix(object = base_formula, data = data))

## -----------------------------------------------------------------------------
transform <- brm_transform_marginal(data = data, formula = brms_mmrm_formula)

## -----------------------------------------------------------------------------
dim(transform)
transform[, 1:4]

## -----------------------------------------------------------------------------
summary(transform)

## -----------------------------------------------------------------------------
model <- lm(formula = base_formula, data = data)
marginals_custom <- transform %*% coef(model)
marginals_custom

## -----------------------------------------------------------------------------
library(emmeans)
marginals_emmeans <- emmeans(
  object = model,
  specs = ~ARMCD:AVISIT,
  weights = "proportional",
  nuisance = c("USUBJID", "RACE", "SEX")
) |>
  as.data.frame() |>
  as_tibble() |>
  select(ARMCD, AVISIT, emmean) |>
  arrange(ARMCD, AVISIT)

## -----------------------------------------------------------------------------
marginals_emmeans

## -----------------------------------------------------------------------------
marginals_custom - marginals_emmeans$emmean

## -----------------------------------------------------------------------------
summary(transform)

## -----------------------------------------------------------------------------
grid <- data |>
  mutate(FEV1_BL = mean(FEV1_BL), WEIGHT = mean(WEIGHT)) |>
  distinct(ARMCD, AVISIT, FEV1_BL, WEIGHT) |>
  arrange(ARMCD, AVISIT)
grid

## -----------------------------------------------------------------------------
transform <- model.matrix(
  object = ~ FEV1_BL * AVISIT + ARMCD * AVISIT + WEIGHT,
  data = grid
)
rownames(transform) <- paste(grid$ARMCD, grid$AVISIT)
transform

## -----------------------------------------------------------------------------
proportional_factors <- data |>
  model.matrix(object = ~ 0 + SEX + RACE) |>
  colMeans() |>
  t()
proportional_factors

## -----------------------------------------------------------------------------
transform <- transform |>
  bind_cols(proportional_factors) |>
  as.matrix()
transform <- transform[, names(coef(model))]
rownames(transform) <- paste(grid$ARMCD, grid$AVISIT)
transform

## -----------------------------------------------------------------------------
marginals_custom <- transform %*% coef(model)
marginals_custom

## -----------------------------------------------------------------------------
marginals_emmeans |>
  bind_cols(custom = as.numeric(marginals_custom)) |>
  mutate(difference = custom - emmean)

## -----------------------------------------------------------------------------
emmeans(
  object = model,
  specs = ~SEX:ARMCD:AVISIT,
  weights = "proportional",
  nuisance = c("USUBJID", "RACE")
)

## -----------------------------------------------------------------------------
grid <- data |>
  distinct(ARMCD, SEX, AVISIT) |>
  arrange(ARMCD, SEX, AVISIT)
grid

## -----------------------------------------------------------------------------
means <- data |>
  group_by(SEX) |>
  summarize(FEV1_BL = mean(FEV1_BL), WEIGHT = mean(WEIGHT), .groups = "drop")
grid <- left_join(x = grid, y = means, by = "SEX")
grid

## -----------------------------------------------------------------------------
transform <- model.matrix(
  object = ~ FEV1_BL * AVISIT + ARMCD * AVISIT + SEX + WEIGHT,
  data = grid
)

## -----------------------------------------------------------------------------
proportions <- data |>
  model.matrix(object = ~ 0 + RACE) |>
  as.data.frame() |>
  mutate(SEX = data$SEX) |>
  group_by(SEX) |>
  summarize(across(everything(), mean), .groups = "drop")
transform <- transform |>
  as.data.frame() |>
  mutate(SEX = grid$SEX) |>
  left_join(y = proportions, by = "SEX") |>
  select(-SEX) |>
  as.matrix()

## -----------------------------------------------------------------------------
rownames(transform) <- paste(grid$ARMCD, grid$SEX, grid$AVISIT)
transform <- transform[, names(coef(model))]
transform

## -----------------------------------------------------------------------------
transform %*% coef(model)

