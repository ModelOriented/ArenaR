# ArenaR - Data generator for Arena - interactive XAI dashboard

## Examples
[Apartments from 2009-2010 price per m2](https://arena.drwhy.ai/?data=https://gist.githubusercontent.com/piotrpiatyszek/0129c6dce4c332dccb820b2e709465f3/raw/9644f61d9c977f5c8a4634ad633bb11f65a71efa/data.json)  
[FIFA 20 Players value](https://arena.drwhy.ai/?data=https://gist.githubusercontent.com/piotrpiatyszek/8f93ce2e36adea23a02ceceed0b33882/raw/a1f15984e628c33381b1988e3b7766f1d478d9ab/data.json)  

## How to use it

### Installation
```r
devtools::install_github("ModelOriented/ArenaR")
```

### Live version - fast for testing on huge data frames
```r
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

arena_new(live = TRUE) %>%
  # Pushing explainers for each models
  arena_push_model(expl_gbm100) %>%
  arena_push_model(expl_gbm500) %>%
  # Push dataframe of observations
  arena_push_observations(apartments) %>%
  # Run server of default port and ip
  arena_run()
```

### Generating static files - easy to share
```r
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

arena_new() %>%
  # Pushing explainers for each models
  arena_push_model(expl_gbm100) %>%
  arena_push_model(expl_gbm500) %>%
  # Push dataframe of observations
  arena_push_observations(observations) %>%
  # Upload calculated arena files to Gist and open Arena in browser
  arena_upload()
```
