context("EDA plots count")
library(arenar)
library(DALEX)

set.seed(1313)

arena <- create_arena()
arena <- push_dataset(arena, DALEX::apartments, target="m2.price", label="train")
arena <- push_dataset(arena, DALEX::apartmentsTest, target="m2.price", label="test")

n_vars <- ncol(DALEX::apartments) - 1
correct_len <- 2 * n_vars * 2 # vd, vaa

test_that("eda plots count is correct", {
  expect_equal(length(arena$plots_data), correct_len)
})
