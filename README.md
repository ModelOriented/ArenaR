# Cross-compare any ML model with the Arena - an interactive XAI dashboard 

[![Codecov test coverage](https://codecov.io/gh/ModelOriented/ArenaR/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/ArenaR?branch=master)
[![R build status](https://github.com/ModelOriented/ArenaR/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/ArenaR/actions)

## Overview

<img src="https://arena.drwhy.ai/img/logo.1a3768b8.png" align="right" width="250"/>

Arena is an interactive tool that allows you to explore and compare any model regardless of its internal structure. 

The arena can be run in two modes - live (R runs in the background and calculates all necessary explanations) and serverless (all necessary explanations are calculated earlier).

Using the Arena is trivially simple. An example with different levels of advancement is available at http://arenar.drwhy.ai/.

<center>
<img src="vignettes/arena03.gif">
</center>

## Installation

Install the `ArenaR` package from GitHub.

```
devtools::install_github("ModelOriented/ArenaR")
```

## How to use it

Examples generated with ArenaR

* [Apartments from 2009-2010 price per m2](https://arena.drwhy.ai/?demo=0)  
* [FIFA 20 Players value](https://arena.drwhy.ai/?demo=1)  
* [HR classification](https://arena.drwhy.ai/?demo=2)  

### Articles

* [Introduction to Arena](https://arenar.drwhy.ai/articles/arena_intro_titanic.html)  
* [Create Live Arena](https://arenar.drwhy.ai/articles/arena_live.html)  
* [Create Static Arena](https://arenar.drwhy.ai/articles/arena_static.html)  
* [Using ArenaR with classificators](https://arenar.drwhy.ai/articles/classification.html)  

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

create_arena(live = TRUE) %>%
  # Pushing explainers for each models
  push_model(expl_gbm100) %>%
  push_model(expl_gbm500) %>%
  # Push dataframe of observations
  push_observations(apartments) %>%
  # Run server of default port and ip
  run_server()
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

create_arena() %>%
  # Pushing explainers for each models
  push_model(expl_gbm100) %>%
  push_model(expl_gbm500) %>%
  # Push dataframe of observations
  push_observations(observations) %>%
  # Upload calculated arena files to Gist and open Arena in browser
  upload_arena()
```

## Acknowledgments

Work on this package was financially supported by the Polish National Science Centre under Opus Grant number 2017/27/B/ST6/0130.
