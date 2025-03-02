## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ----glm, message=FALSE, warning=FALSE----------------------------------------
library(parsnip)
library(offsetreg)
library(broom)
library(recipes)
library(workflows)
library(rsample)
library(tune)

us_deaths$log_pop <- log(us_deaths$population)

poisson_reg() |>
  set_engine("glm") |>
  fit(deaths ~ gender + age_group + year + offset(log_pop), 
      data = us_deaths)

## ----glm-rec-error, error = TRUE----------------------------------------------
try({
mod <- poisson_reg() |> set_engine("glm")
rec <- recipe(deaths ~ gender + age_group + year + offset(log_pop), 
              data = us_deaths)
})

## ----glm-rec-fix--------------------------------------------------------------
rec <- recipe(deaths ~ gender + age_group + year + log_pop, 
              data = us_deaths)

workflow() |> 
  add_model(mod, 
            formula = deaths ~ gender + age_group + year + offset(log_pop)) |> 
  add_recipe(rec) |> 
  fit(us_deaths)

## ----glmnet-------------------------------------------------------------------
poisson_reg(penalty = 1E-5) |>
  set_engine("glmnet", offset = us_deaths$log_pop) |>
  fit(deaths ~ year + gender + age_group, 
      data = us_deaths) |> 
  tidy()


## ----glmnet-offset-problem----------------------------------------------------
mod_glmnet <- poisson_reg(penalty = 1E-5) |> set_engine("glmnet")
rec <- recipe(deaths ~ year + gender + age_group + log_pop, 
              data = us_deaths)

workflow() |> 
  add_model(mod_glmnet, 
            formula = deaths ~ year + gender + age_group + offset(log_pop)) |> 
  add_recipe(rec) |> 
  fit(us_deaths) |> 
  tidy()

## ----mat-drop, fig.cap="The offset term is dropped by model.matrix()"---------
model.matrix(deaths ~ year + gender + age_group + offset(log_pop),
             us_deaths) |> 
  head()

## ----glmnet-offset-fix--------------------------------------------------------
mod_offset <- poisson_reg_offset(penalty = 1E-5) |> 
  set_engine("glmnet_offset", offset_col = "log_pop")
rec <- recipe(deaths ~ year + gender + age_group + log_pop, 
              data = us_deaths) |> 
  step_dummy(all_nominal_predictors())

workflow() |> 
  add_model(mod_offset) |> 
  add_recipe(rec) |> 
  fit(us_deaths) |> 
  tidy()

## ----resamples-glmnet-problem, error=TRUE-------------------------------------
try({
resamples <- bootstraps(us_deaths, times = 5)

mod_glmnet <- poisson_reg(penalty = 1E-5) |> 
  set_engine("glmnet", offset = us_deaths$log_pop)

workflow() |>
  add_recipe(rec) |>
  add_model(mod_glmnet) |>
  fit_resamples(resamples) |>
  collect_metrics()
})

## -----------------------------------------------------------------------------
show_notes(.Last.tune.result)

## ----resamples-glmnet-fix-----------------------------------------------------
workflow() |>
    add_recipe(rec) |>
    add_model(mod_offset) |>
    fit_resamples(resamples) |>
    collect_metrics()

