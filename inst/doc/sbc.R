## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6
)
library(brms.mmrm)
library(dplyr)
library(fst)
library(ggplot2)
library(tibble)
library(tidyr)

## ----echo = FALSE-------------------------------------------------------------
n_group <- 3L
n_patient <- 100L
n_time <- 4L
outline <- brms.mmrm::brm_simulate_outline(
  n_group = n_group,
  n_patient = n_patient,
  n_time = n_time,
  rate_dropout = 0,
  rate_lapse = 0
)
brms.mmrm::brm_formula(
  data = outline,
  intercept = FALSE,
  baseline = FALSE,
  group = TRUE,
  time = TRUE,
  baseline_time = FALSE,
  group_time = FALSE,
  correlation = "unstructured"
)

## ----paged.print = FALSE------------------------------------------------------
fst::read_fst("sbc/prior_simple.fst")

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)

read_ranks <- function(path) {
  fst::read_fst(path) |>
    tibble::as_tibble() |>
    pivot_longer(
      cols = everything(),
      names_to = "parameter",
      values_to = "rank"
    )
}

plot_ranks <- function(ranks) {
  ggplot(ranks) +
    geom_histogram(
      aes(x = rank),
      breaks = seq(from = 0, to = max(ranks$rank), length.out = 10)
    ) +
    facet_wrap(~parameter)
}

## -----------------------------------------------------------------------------
simple_ranks <- read_ranks("sbc/simple.fst")

## -----------------------------------------------------------------------------
simple_ranks |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
simple_ranks |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
simple_ranks |>
  filter(grepl("cortime_", parameter)) |>
  plot_ranks()

## ----echo = FALSE-------------------------------------------------------------
n_group <- 2L
n_subgroup <- 2L
n_patient <- 150L
n_time <- 3L
outline <- brms.mmrm::brm_simulate_outline(
  n_group = n_group,
  n_subgroup = n_subgroup,
  n_patient = n_patient,
  n_time = n_time,
  rate_dropout = 0.3,
  rate_lapse = 0.08
) |>
  brms.mmrm::brm_simulate_continuous(
    names = c("continuous1", "continuous2")
  ) |>
  brms.mmrm::brm_simulate_categorical(
    names = "balanced",
    levels = c("level1", "level2", "level3")
  ) |>
  brms.mmrm::brm_simulate_categorical(
    names = "unbalanced",
    levels = c("level1", "level2", "level3"),
    probabilities = c(0.64, 0.26, 0.1)
  )
brms.mmrm::brm_formula(
  data = outline,
  correlation = "unstructured"
)

## ----paged.print = FALSE------------------------------------------------------
fst::read_fst("sbc/prior_complex.fst")

## -----------------------------------------------------------------------------
complex_ranks <- read_ranks("sbc/complex.fst")

## -----------------------------------------------------------------------------
complex_ranks |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
complex_ranks |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
complex_ranks |>
  filter(grepl("cortime_", parameter)) |>
  plot_ranks()

