# identifying the models to pass onto the shiny app
source("R/00_libraries.R")
source("R/04_modelling_utils.R")

# create subset which contain the best performing models by model type
best_models <- readRDS("tests/model_testing/model_summary_information.rds") |> 
  mutate(
    Date = as.Date(Date)
  ) |>
  filter(
    `Tuning objective` == "mape",
    `Number lagged target years` == "0 lagged years"
  ) |> 
  filter(
    `Test set value` == min(`Test set value`),
    .by = c(`Target variable`, `Model type`, `Target variable type`)
  ) |> 
  filter(
    `Number training years` == min(`Number training years`),
    .by = c(`Target variable`, `Model type`, `Target variable type`)
  ) |>
  arrange(
    `Target variable`,
    `Model type`,
    `Target variable type`
  ) |> 
  mutate(
    `Number lagged target years` = as.integer(
      stringr::str_extract(
        `Number lagged target years`,
        "[0-9]"
      )
    )
  )

best_glm_models <- best_models |> 
  filter(
    `Model type` == "logistic_regression"
  )

best_models <- best_models |>
  filter(
    `Test set value` == min(`Test set value`),
    .by = `Target variable`
  )



final_workflows <- list()
for (i in seq_len(nrow(best_glm_models))) {
  
  model_method <- best_glm_models$`Model type`[i]
  predict_year <- 2023
  target_variable <- best_glm_models$`Target variable`[i]
  dc_data <- load_data(
    target_variable,
    value_type = "value",
    incl_numerator_remainder = include_numerator_remainder(model_method),
    binary_covid = FALSE
  ) |> 
    dplyr::filter(
      # retain all rows where target variable is not na
      # retain all rows where we have population age group information
      if_all(
        c(
          all_of(c(target_variable)), 
          contains("age band")
        ), ~ !is.na(.)
      )
    ) |> 
    arrange(
      year, org
    ) |> 
    filter(
      year <= predict_year
    ) |> 
    # make sure target variable is between 0 and 100
    mutate(
      across(
        all_of(target_variable),
        \(x) x / 100
      )
    )
  
  m <- modelling_performance(
    data = dc_data, 
    target_variable, 
    lagged_years = best_glm_models$`Number lagged years`[i], 
    keep_current = TRUE, 
    lag_target = best_glm_models$`Number lagged target years`[i],
    time_series_split = FALSE, 
    training_years = best_glm_models$`Number training years`[i],
    remove_years = NULL,
    shuffle_training_records = FALSE,
    model_type = best_glm_models$`Model type`[i], 
    tuning_grid = "auto",
    target_type = best_glm_models$`Target variable type`[i],
    validation_type = best_glm_models$`Validation method`[i],
    eval_metric = best_glm_models$`Tuning objective`[i],
    include_pi = TRUE,
    seed = 321,
    auto_feature_selection = FALSE
  )
  
  final_workflows[[target_variable]] <- list(
    wf = m |>
      pluck("ft") |> 
      extract_workflow(),
    perm_imp = m |> 
      pluck("permutation_importance")
  )
}

saveRDS(
  final_workflows,
  "outputs/model_objects/live_models.rds"
)
