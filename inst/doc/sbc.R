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

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)

source("sbc/R/prior.R")
source("sbc/R/response.R")
source("sbc/R/scenarios.R")

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

## ----echo = FALSE-------------------------------------------------------------
subgroup()$formula

## ----paged.print = FALSE------------------------------------------------------
setup_prior(subgroup) |>
  select(prior, class, coef, dpar) |>
  as.data.frame()

## -----------------------------------------------------------------------------
ranks_subgroup <- read_ranks("sbc/results/subgroup.fst")

## -----------------------------------------------------------------------------
ranks_subgroup |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_subgroup |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_subgroup |>
  filter(grepl("cortime_", parameter)) |>
  plot_ranks()

## ----echo = FALSE-------------------------------------------------------------
unstructured()$formula

## ----paged.print = FALSE------------------------------------------------------
setup_prior(unstructured) |>
  select(prior, class, coef, dpar) |>
  as.data.frame()

## -----------------------------------------------------------------------------
ranks_unstructured <- read_ranks("sbc/results/unstructured.fst")

## -----------------------------------------------------------------------------
ranks_unstructured |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_unstructured |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_unstructured |>
  filter(grepl("cortime_", parameter)) |>
  plot_ranks()

## ----echo = FALSE-------------------------------------------------------------
autoregressive_moving_average()$formula

## ----paged.print = FALSE------------------------------------------------------
setup_prior(autoregressive_moving_average) |>
  select(prior, class, coef, dpar) |>
  as.data.frame()

## -----------------------------------------------------------------------------
ranks_autoregressive_moving_average <- read_ranks(
  "sbc/results/autoregressive_moving_average.fst"
)

## -----------------------------------------------------------------------------
ranks_autoregressive_moving_average |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_autoregressive_moving_average |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_autoregressive_moving_average |>
  filter(parameter %in% c("ar[1]", "ma[1]")) |>
  plot_ranks()

## ----echo = FALSE-------------------------------------------------------------
autoregressive()$formula

## ----paged.print = FALSE------------------------------------------------------
setup_prior(autoregressive) |>
  select(prior, class, coef, dpar) |>
  as.data.frame()

## -----------------------------------------------------------------------------
ranks_autoregressive <- read_ranks("sbc/results/autoregressive.fst")

## -----------------------------------------------------------------------------
ranks_autoregressive |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_autoregressive |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_autoregressive |>
  filter(parameter %in% c("ar[1]", "ar[2]")) |>
  plot_ranks()

## ----echo = FALSE-------------------------------------------------------------
moving_average()$formula

## ----paged.print = FALSE------------------------------------------------------
setup_prior(moving_average) |>
  select(prior, class, coef, dpar) |>
  as.data.frame()

## -----------------------------------------------------------------------------
ranks_moving_average <- read_ranks("sbc/results/moving_average.fst")

## -----------------------------------------------------------------------------
ranks_moving_average |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_moving_average |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_moving_average |>
  filter(parameter %in% c("ma[1]", "ma[2]")) |>
  plot_ranks()

## ----echo = FALSE-------------------------------------------------------------
compound_symmetry()$formula

## ----paged.print = FALSE------------------------------------------------------
setup_prior(compound_symmetry) |>
  select(prior, class, coef, dpar) |>
  as.data.frame()

## -----------------------------------------------------------------------------
ranks_compound_symmetry <- read_ranks("sbc/results/compound_symmetry.fst")

## -----------------------------------------------------------------------------
ranks_compound_symmetry |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_compound_symmetry |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_compound_symmetry |>
  filter(parameter == "cosy") |>
  plot_ranks()

## ----echo = FALSE-------------------------------------------------------------
diagonal()$formula

## ----paged.print = FALSE------------------------------------------------------
setup_prior(diagonal) |>
  select(prior, class, coef, dpar) |>
  as.data.frame()

## -----------------------------------------------------------------------------
ranks_diagonal <- read_ranks("sbc/results/diagonal.fst")

## -----------------------------------------------------------------------------
ranks_diagonal |>
  filter(grepl("^b_", parameter)) |>
  filter(!grepl("^b_sigma", parameter)) |>
  plot_ranks()

## -----------------------------------------------------------------------------
ranks_diagonal |>
  filter(grepl("b_sigma", parameter)) |>
  plot_ranks()

