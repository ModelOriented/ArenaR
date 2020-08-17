# Cross-compare any ML model with the Arena - an interactive XAI dashboard 

[![Codecov test coverage](https://codecov.io/gh/ModelOriented/ArenaR/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/ArenaR?branch=master)
[![R build status](https://github.com/ModelOriented/ArenaR/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/ArenaR/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/arenar)](https://cran.r-project.org/package=arenar)

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
install.packages("arenar")
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

### Guide

![](guide.png)

## Acknowledgments

Work on this package was financially supported by the Polish National Science Centre under Opus Grant number 2017/27/B/ST6/0130.
