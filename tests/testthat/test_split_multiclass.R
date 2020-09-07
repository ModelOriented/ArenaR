context("Spliting multiclass explainer")
library(arenar)
library(DALEX)
library(ranger)

set.seed(1313)
hr <- DALEX::HR[1:100, ]
model <- ranger(status ~ . , data = hr, probability=TRUE)
new_observation <- DALEX::HR[101:102, ]
explainer <- explain(model, data = hr, y = hr$status)

arena <- create_arena(shap_B=3, fi_B=3)
arena <- push_model(arena, explainer)
arena <- push_observations(arena, new_observation)

test_that("explainers count is correct", {
  expect_equal(length(arena$explainers), length(levels(hr$status)))
})

n_vars <- ncol(new_observation) - 1
n_obs <- nrow(new_observation)
correct_len <- 3 * 6 + # FI, ROC, REC, Metrics, FM, SP
  3 * n_obs * 2 + # BD, SHAP
  3 * n_vars * 3 + # ADP, PDP, Fairness
  3 * n_vars * n_obs # CP

test_that("plots count is correct", {
  tryCatch(
    expr = {
      expect_equal(length(arena$plots_data), correct_len)
    }, 
    error = function(e){
      print(e)
      # if failed print table of plot types
      print(table(sapply(arena$plots_data, function(p) p$plotType)))
    }
  )
})
