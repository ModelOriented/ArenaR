context("Plots count")
library(arenar)
library(DALEX)
library(parallel)

set.seed(1313)
model <- glm(Petal.Length ~ . , data = iris)
new_observation <- iris[1:2,]
explainer_rf <- explain(model, data = iris, y = iris$Petal.Length)

arena <- create_arena()
arena <- push_model(arena, explainer_rf)
arena <- push_observations(arena, new_observation)

n_vars <- ncol(new_observation) - 1
n_obs <- nrow(new_observation)
correct_len <- 1 + # FI
  n_obs * 2 + # BD, SHAP
  n_vars * 2 + # ADP, PDP
  n_vars * n_obs # CP

test_that("plots count is correct", {
  expect_equal(length(arena$plots_data), correct_len)
})

cl <- makeCluster(2)
arena_cluster <- create_arena(cl = cl)
arena_cluster <- push_model(arena_cluster, explainer_rf)
arena_cluster <- push_observations(arena_cluster, new_observation)

test_that("plots count is correct using cluster", {
  expect_equal(length(arena_cluster$plots_data), correct_len)
})

stopCluster(cl)
