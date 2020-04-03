context("Plots count")
library(arenar)
library(DALEX)

set.seed(1313)
model <- glm(Petal.Length ~ . , data = iris)
new_observation <- iris[1:2,]
explainer_rf <- explain(model, data = iris, y = iris$Petal.Length)

arena <- arena_new()
arena <- arena_push_model(arena, explainer_rf)
arena <- arena_push_observations(arena, new_observation)

n_vars <- ncol(new_observation) - 1
n_obs <- nrow(new_observation)
correct_len <- 1 + # FI
  n_obs * 2 + # BD, SHAP
  n_vars * 2 + # ADP, PDP
  n_vars * n_obs # CP

test_that("plots count is correct", {
  expect_equal(length(arena$plots_data), correct_len)
})
