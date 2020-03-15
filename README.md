# ArenaR - Data generator for Arena - interactive XAI dashboard

## Examples
[Apartments from 2009-2010 price per m2](https://piotrpiatyszek.github.io/arena/?data=https://gist.githubusercontent.com/piotrpiatyszek/67fc3d91dab87949683273037ff2b98a/raw/4684a00e103a69e654af9f14128fa312b30cb630/data.json)

## How to use it

# Live version - fast for testing on huge data frames
```
library(arenar)
library(gbm)
library(DALEX)
library(dplyr)

# Create models and DALEX explainers
model_gbm100 <- gbm(m2.price ~ ., data = apartments, n.trees = 100)
expl_gbm100 <- explain(
  model_gbm100,
  data = apartments,
  y = apartments$m2.price,
  label = "gbm [100 trees]"
)

model_gbm500 <- gbm(m2.price ~ ., data = apartments, n.trees = 500)
expl_gbm500 <- explain(
  model_gbm500,
  data = apartments,
  y = apartments$m2.price,
  label = "gbm [500 trees]"
)

new_arena(live = TRUE) %>%
  # Pushing explainers for each models
  arena_push_model(expl_gbm100) %>%
  arena_push_model(expl_gbm500) %>%
  # Push dataframe of observations
  arena_push_observations(apartments) %>%
  # Run server of default port and ip
  arena_run()
```

# Generating static files - easy to share
```
library(arenar)
library(gbm)
library(DALEX)
library(dplyr)

# Create models and DALEX explainers
model_gbm100 <- gbm(m2.price ~ ., data = apartments, n.trees = 100)
expl_gbm100 <- explain(
  model_gbm100,
  data = apartments,
  y = apartments$m2.price,
  label = "gbm [100 trees]"
)

model_gbm500 <- gbm(m2.price ~ ., data = apartments, n.trees = 500)
expl_gbm500 <- explain(
  model_gbm500,
  data = apartments,
  y = apartments$m2.price,
  label = "gbm [500 trees]"
)

# Take only few observations
observations <- apartments %>% filter(construction.year >= 2009)
# Observations' names are taken from rownames
rownames(observations) <- paste0(
  observations$district,
  " ",
  observations$surface,
  "m2 "
)

new_arena() %>%
  # Pushing explainers for each models
  arena_push_model(expl_gbm100) %>%
  arena_push_model(expl_gbm500) %>%
  # Push dataframe of observations
  arena_push_observations(observations) %>%
  # Upload calculated arena files to Gist and open Arena in browser
  arena_upload()
```
