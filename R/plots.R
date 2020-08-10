#' Internal function for calculating local plots for all observations
#'
#' Function runs all plot generating methods for given observations
#' 
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param observations Data frame of observations
#' @param params Params from arena object 
#' @return list of generated plots' data
get_local_plots <- function(explainer, observations, params) {
  is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
  vars <- intersect(names(is_y[!is_y]), colnames(observations))

  # observations were validated and have min 1 row
  obs_list <- lapply(1:nrow(observations), function(i) observations[i, vars])

  get_bd <- function(obs) get_break_down(explainer, obs, params)
  get_shap <- function(obs) get_shap_values(explainer, obs, params)
  get_cp <- function(obs) lapply(vars, function(v) {
    get_ceteris_paribus(explainer, obs, v, params)
  })

  if (is.null(params$cl)) { # single thread if cluster was not provided
    bd <- lapply(obs_list, get_bd)
    sp <- lapply(obs_list, get_shap)
    cp <- lapply(obs_list, get_cp)
    cp <- unlist(cp, recursive = FALSE)
  } else {
    # Export variables and functions to cluster
    to_export <- c(
      "explainer",
      "params",
      "vars",
      "get_break_down",
      "get_shap_values",
      "get_ceteris_paribus"
    )
    parallel::clusterExport(params$cl, to_export, envir=environment())
    # Load model's library to access predict function
    parallel::clusterEvalQ(
      params$cl,
      library(explainer$model_info$package, character.only=TRUE)
    )
    bd <- parallel::parLapply(params$cl, obs_list, get_bd)
    sp <- parallel::parLapply(params$cl, obs_list, get_shap)
    cp <- parallel::parLapply(params$cl, obs_list, get_cp)
    cp <- unlist(cp, recursive = FALSE)
  }
  
  # Join results into one list
  c(
    bd[!sapply(bd, is.null)],
    sp[!sapply(sp, is.null)],
    cp[!sapply(cp, is.null)]
  )
}

#' Internal function for calculating global plots
#'
#' Function runs all plot generating methods for given explainer
#' 
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param params Params from arena object 
#' @return list of generated plots' data
get_global_plots <- function(explainer, params) {
  is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
  vars <- names(is_y[!is_y])

  global_plots_names <- c("roc", "rec", "metrics", "subsets_performance", "funnel_measure", "feature_importance")
  global_plots <- lapply(global_plots_names, function(n) get(paste0("get_", n)))

  get_global <- function(f) f(explainer, params)
  get_pd <- function(v) get_partial_dependence(explainer, v, params)
  get_ad <- function(v) get_accumulated_dependence(explainer, v, params)
  get_fr <- function(v) get_fairness(explainer, v, params)

  if (is.null(params$cl)) { # single thread if cluster was not provided
    globals <- lapply(global_plots, get_global)
    pd <- lapply(vars, get_pd)
    ad <- lapply(vars, get_ad)
    fr <- lapply(vars, get_fr)
  } else {
    # Export variables and functions to cluster
    to_export <- c(
      "explainer",
      "params",
      "calculate_subsets_performance",
      "get_partial_dependence",
      "get_accumulated_dependence",
      "get_fairness"
    )
    parallel::clusterExport(params$cl, to_export, envir=environment())
    # Load model's library to access predict function
    parallel::clusterEvalQ(
      params$cl,
      library(explainer$model_info$package, character.only=TRUE)
    )
    globals <- parallel::parLapply(params$cl, global_plots, get_global)
    pd <- parallel::parLapply(params$cl, vars, get_pd)
    ad <- parallel::parLapply(params$cl, vars, get_ad)
    fr <- parallel::parLapply(params$cl, vars, get_fr)
  }

  # Join results into one list
  c(
    pd[!sapply(pd, is.null)],
    ad[!sapply(ad, is.null)],
    fr[!sapply(fr, is.null)],
    globals[!sapply(globals, is.null)]
  )
}

#' Internal function for returning message as plot data
#'
#' This method modify exisiting plot's data in Arena's format
#' to show message instead of chart.
#' @param output existing plot data to be overwritten
#' @param type type of message "info" or "error"
#' @param msg message to be displayed
#' @return Plot data in Arena's format
get_message_output <- function(output, type, msg) {
  output$plotComponent <- "Message"
  output$data <- list(message = msg, type = type)
  output
}

#' Internal function for calculating fairness
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param variable Name of variable
#' @param params Params from arena object 
#' @return Plot data in Arena's format
get_fairness <- function(explainer, variable, params) {
  output <- NULL
  tryCatch({
    output <- list(
      plotType = "Fairness",
      plotCategory = "Dataset Level",
      plotComponent = "Fairness",
      name = "Fairness",
      params = list(model = explainer$label, variable = variable)
    )
    if (explainer$model_info$type != 'classification') {
      return(get_message_output(output, "info", "Fairness plot is only available for classificators"))
    }
    protected <- explainer$data[, variable]
    if (!is.factor(protected)) {
      return(get_message_output(output, "info", "Select categorical variable to check fairness"))
    }
    subgroups <- levels(protected)
    # Get unexported methods from fairmodels
    group_matrices <- utils::getFromNamespace("group_matrices", "fairmodels")
    calculate_group_fairness_metrics <- utils::getFromNamespace("calculate_group_fairness_metrics", "fairmodels")
    # for every cutoff level get group metric matrix
    gmm_list <- lapply(params$fairness_cutoffs, function(cutoff) {
      # make cutoff a const list
      cutoff_list <- as.list(rep(cutoff, length(subgroups)))
      names(cutoff_list) <- subgroups
      # calculate confusion matrices for each subgroup
      stopifnot(is.logical(explainer$y) || is.numeric(explainer$y))
      gm <- group_matrices(
        protected = protected,
        probs = explainer$y_hat,
        preds = as.numeric(explainer$y),
        cutoff = cutoff_list
      )
      # group metric matrix
      gmm <- calculate_group_fairness_metrics(gm)
      data.frame(
        value = as.vector(gmm),
        subgroup = rep(colnames(gmm), each=nrow(gmm)),
        metric = rep(rownames(gmm), ncol(gmm)),
        cutoff = cutoff
      )
    }) 
    gmm <- do.call('rbind', gmm_list)
    # Split data frame by subgroup names
    gmm_transformed <- lapply(split(gmm, gmm$subgroup), function(x) {
      # for each subgroup there is a list for each cutoff
      lapply(split(x, x$cutoff), function(x) {
        # the element for specified subgroup and cutoff
        # is a named list with values for metrics
        structure(as.list(x$value), names=x$metric)
      })
    })
    output$data <- list(subgroups = gmm_transformed)
  }, error = function(e) {
    stop("Failed to calculate fairness\n", e)
  })
  output
}


#' Internal function for calculating subset performance
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param params Params from arena object 
#' @return Plot data in Arena's format
get_subsets_performance <- function(explainer, params) {
  output <- NULL
  tryCatch({
    # get loss function and its name based on model type
    score_functions <- switch(
      explainer$model_info$type,
      regression = list(
        RMSE=auditor::score_rmse,
        MSE=auditor::score_mse,
        R2=auditor::score_r2,
        MAE=auditor::score_mae
      ),
      classification = list(
        Accuracy=auditor::score_acc,
        Recall=auditor::score_recall,
        Precision=auditor::score_precision,
        Specificity=auditor::score_specificity,
        AUC=auditor::score_auc,
        F1=auditor::score_f1
      ),
      stop(explainer$model_info$type, " is not recognized as task name")
    )
    scores <- calculate_subsets_performance(
      explainer,
      score_functions = score_functions,
      nbins = params$fm_nbins,
      cutoff = params$fm_cutoff,
      factor_conversion_threshold = params$fm_factor_threshold
    )
    splited <- split(scores, scores$Variable)
    output_data <- lapply(names(score_functions), function(score_name) {
      list(
        scoreValues = lapply(splited, function(x) structure(as.list(x[, score_name]), names=x$Label)),
        base = score_functions[[score_name]](explainer)$score
      )
    })
    names(output_data) <- names(score_functions)
    output <- list(
      plotComponent = "SubsetsPerformance",
      plotType = "SubsetsPerformance",
      plotCategory = "Model Performance",
      name = "Subsets Performance",
      params = list(
        model = explainer$label
      ),
      data = output_data
    )
  }, error = function(e) {
    stop("Failed to calculate subsets performance\n", e)
  })
  output
}

#' Internal function for calculating funnel measure
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param params Params from arena object 
#' @return Plot data in Arena's format
get_funnel_measure <- function(explainer, params) {
  output <- NULL
  tryCatch({
    # get loss function and its name based on model type
    score_functions <- switch(
      explainer$model_info$type,
      regression = list(MSE=auditor::score_mse),
      classification = list("ONE_MINUS_AUC"=auditor::score_one_minus_auc),
      stop(explainer$model_info$type, " is not recognized as task name")
    )
    scores <- calculate_subsets_performance(
      explainer,
      score_functions = score_functions,
      nbins = params$fm_nbins,
      cutoff = params$fm_cutoff,
      factor_conversion_threshold = params$fm_factor_threshold
    )
    splited <- split(scores, scores$Variable)
    transformed <- lapply(splited, function(x) structure(as.list(x[, names(score_functions)]), names=x$Label))
    output <- list(
      plotComponent = "FunnelMeasure",
      plotType = "FunnelMeasure",
      plotCategory = "Model Performance",
      name = "Funnel Plot",
      params = list(
        model = explainer$label
      ),
      data = list(
        lossValues = transformed,
        lossFunction = gsub("ONE_MINUS_", "1 - ", names(score_functions))
      )
    )
  }, error = function(e) {
    stop("Failed to calculate funnel measure\n", e)
  })
  output
}

#' Internal function for calculating model performance metrics
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param params Params from arena object 
#' @return Plot data in Arena's format
get_metrics <- function(explainer, params) {
  output <- NULL
  tryCatch({
    perf <- DALEX::model_performance(explainer)$measures
    output <- list(
      plotComponent = "Metrics",
      plotType = "Metrics",
      plotCategory = "Model Performance",
      name = "Metrics",
      params = list(
        model = explainer$label
      ),
      data = perf
    )
  }, error = function(e) {
    stop("Failed to calculate Metrics\n", e)
  })
  output
}

#' Internal function for calculating receiver operating curve
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param params Params from arena object 
#' @return Plot data in Arena's format
#' @importFrom stats runif
get_roc <- function(explainer, params) {
  output <- NULL
  tryCatch({
    if (explainer$model_info$type != 'classification') return(NULL)
    eva <- auditor::model_evaluation(explainer)
    if (nrow(eva) > params$roc_grid_points) {
      # take random points
      points <- round(runif(n=params$roc_grid_points, min=1, max=nrow(eva)))
      eva <- eva[points, ]
    }
    eva <- eva[order(eva$`_fpr_`, decreasing = TRUE), ]
    
    output <- list(
      plotComponent = "ROC",
      plotType = "ROC",
      plotCategory = "Model Performance",
      name = "Receiver Operating Characterstic",
      params = list(
        model = explainer$label
      ),
      data = list(
        specifity = 1 - eva$`_fpr_`,
        sensivity = eva$`_tpr_`,
        cutoff = eva$`_cutoffs_`
      )
    )
  }, error = function(e) {
    stop("Failed to calculate ROC\n", e)
  })
  output
}

#' Internal function for calculating regression error characteristic
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param params Params from arena object 
#' @return Plot data in Arena's format
#' @importFrom stats runif
get_rec <- function(explainer, params) {
  output <- NULL
  tryCatch({
    res <- auditor::model_residual(explainer)
    make_dataframe <- utils::getFromNamespace("make_dataframe", "auditor")
    df <- make_dataframe(res, type="rec")
    if (nrow(df) > params$rec_grid_points) {
      # take random points
      points <- round(runif(n=params$rec_grid_points, min=1, max=nrow(df)))
      df <- df[sort(points), ]
    }
    
    output <- list(
      plotComponent = "REC",
      plotType = "REC",
      plotCategory = "Model Performance",
      name = "Regression Error Characteristic",
      params = list(
        model = explainer$label
      ),
      data = list(
        tolerance = df$`_rec_x_`,
        quantity = df$`_rec_y_`
      )
    )
  }, error = function(e) {
    stop("Failed to calculate REC\n", e)
  })
  output
}

#' Internal function for calculating Ceteris Paribus
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param observation One row data frame observation
#' @param variable Name of variable
#' @param params Params from arena object
#' @return Plot data in Arena's format
get_ceteris_paribus <- function(explainer, observation, variable, params) {
  output <- NULL
  tryCatch({
    cp <- ingredients::ceteris_paribus(
      explainer,
      observation, 
      variables = variable,
      grid_points = params$cp_grid_points
    )
    cp <- cp[cp$`_vname_` == variable,]
    is_num <- is.numeric(explainer$data[, variable])
    output <- list(
      plotComponent = ifelse(is_num,
        "NumericalCeterisParibus",
        "CategoricalCeterisParibus"
      ),
      plotType = "CeterisParibus",
      plotCategory = "Observation Level",
      name = "Ceteris Paribus",
      params = list(
        model = explainer$label,
        observation = rownames(observation),
        variable = variable
      ),
      data = list(
        x = cp[, variable],
        y = cp$`_yhat_`,
        min = min(cp$`_yhat`),
        max = max(cp$`_yhat_`),
        variable = variable,
        observation = as.list(attr(cp, "observations"))
      )
    )
  }, error = function(e) {
    stop("Failed to calculate ceteris paribus\n", e)
  })
  output
}

#' Internal function for calculating Break Down
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param observation One row data frame observation
#' @param params Params from arena object
#' @return Plot data in Arena's format
get_break_down <- function(explainer, observation, params) {
  output <- NULL
  tryCatch({
    bd <- iBreakDown::local_attributions(explainer, observation)
    # remove rows: intercept and prediction 
    vars_index <- 2:(length(bd$variable) - 1)
    output <- list(
      plotComponent = "Breakdown",
      plotCategory = "Observation Level",
      plotType = "Breakdown",
      name = "Break Down",
      params = list(
        model = explainer$label,
        observation = rownames(observation)
      ),
      data = list(
        variables = bd$variable_name[vars_index],
        intercept = bd$contribution[1],
        prediction = bd$cumulative[length(bd$cumulative)],
        variables_value = bd$variable_value[vars_index],
        contribution = bd$contribution[vars_index]
      )
    )
  }, error = function(e) {
    stop("Failed to calculate break down\n", e)
  })
  output
}

#' Internal function for calculating Accumulated Dependence
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param variable Name of variable
#' @param params Params from arena object
#' @return Plot data in Arena's format
get_accumulated_dependence <- function(explainer, variable, params) {
  output <- NULL
  tryCatch({
    is_num <- is.numeric(explainer$data[, variable])
    pd <- ingredients::accumulated_dependence(
      explainer,
      variables = variable,
      variable_type = ifelse(is_num, "numerical", "categorical"),
      grid_points = params$ad_grid_points,
      N = params$ad_N
    )
    pd <- pd[pd$`_vname_` == variable, ]
    output <- list(
      data = list(
        x = pd$`_x_`,
        y = pd$`_yhat_`,
        variable = variable,
        base = 0
      ),
      plotType = "AccumulatedDependence",
      plotCategory = "Dataset Level",
      plotComponent = ifelse(is_num,
        "LinearDependence",
        "CategoricalDependence"
      ),
      name = "Accumulated Dependence",
      params = list(model = explainer$label, variable = variable)
    )
  }, error = function(e) {
    stop("Failed to calculate accumulated dependence\n", e)
  })
  output
}

#' Internal function for calculating Partial Dependence
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param variable Name of variable
#' @param params Params from arena object
#' @return Plot data in Arena's format
get_partial_dependence <- function(explainer, variable, params) {
  output <- NULL
  tryCatch({
    is_num <- is.numeric(explainer$data[, variable])
    pd <- ingredients::partial_dependence(
      explainer,
      variables = variable,
      variable_type = ifelse(is_num, "numerical", "categorical"),
      grid_points = params$pd_grid_points,
      N = params$pd_N
    )
    pd <- pd[pd$`_vname_` == variable, ]
    output <- list(
      data = list(
        x = pd$`_x_`,
        y = pd$`_yhat_`,
        variable = variable,
        base = attr(pd, "mean_prediction")
      ),
      plotType = "PartialDependence",
      plotCategory = "Dataset Level",
      plotComponent = ifelse(is_num,
        "LinearDependence",
        "CategoricalDependence"
      ),
      name = "Partial Dependence",
      params = list(model = explainer$label, variable = variable)
    )
  }, error = function(e) {
    stop("Failed to calculate partial dependence\n", e)
  })
  output
}

#' Internal function for calculating feature importance
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param params Params from arena object 
#' @return Plot data in Arena's format
#' @importFrom stats quantile
get_feature_importance <- function(explainer, params) {
  output <- NULL
  tryCatch({
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- names(is_y[!is_y])
    fi <- ingredients::feature_importance(
      explainer,
      variables = vars,
      B = params$fi_B,
      n_sample = params$fi_n_sample
    )
    stats <- data.frame(
      min = tapply(fi$dropout_loss, fi$variable, min, na.rm = TRUE),
      q1 = tapply(fi$dropout_loss, fi$variable, quantile, 0.25, na.rm = TRUE),
      q3 = tapply(fi$dropout_loss, fi$variable, quantile, 0.75, na.rm = TRUE),
      max = tapply(fi$dropout_loss, fi$variable, max, na.rm = TRUE)
    )

    perm0 <- merge(
      fi[fi$permutation == 0,],
      cbind(rownames(stats), stats),
      by.x = "variable",
      by.y = "rownames(stats)"
    )
    # rm permutation column
    perm0 <- subset(perm0, select = setdiff(colnames(perm0), "permutation"))
    # leave only rows for variables, not for full model and baseline
    vars_only <- perm0[!(perm0$variable %in% c("_baseline_", "_full_model_")), ]
    vars_only <- vars_only[order(vars_only$dropout_loss, decreasing = TRUE), ]
    
    output <- list(
      data = list(
        base = perm0$dropout_loss[perm0$variable == "_full_model_"],
        variables = as.character(vars_only$variable),
        dropout_loss = vars_only$dropout_loss,
        min = vars_only$min,
        max = vars_only$max,
        q1 = vars_only$q1,
        q3 = vars_only$q3
      ),
      plotType = "FeatureImportance",
      plotCategory = "Dataset Level",
      plotComponent = "FeatureImportance",
      name = "Variable Importance",
      params = list(model = explainer$label)
    )
  }, error = function(e) {
    stop("Failed to calculate feature importance\n", e)
  })
  output
}

#' Internal function for calculating Shapley Values
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param observation One row data frame observation to calculate Shapley Values
#' @param params Params from arena object
#' @return Plot data in Arena's format
#' @importFrom stats quantile
get_shap_values <- function(explainer, observation, params) {
  output <- NULL
  params
  tryCatch({
    sp <- iBreakDown::shap(
      explainer,
      observation,
      B = params$shap_B
    )
    perm0 <- sp[sp$B == 0, ]
    sp <- sp[sp$B != 0, ]

    stats <- data.frame(
      min = tapply(sp$contribution, sp$variable, min, na.rm = TRUE),
      q1 = tapply(sp$contribution, sp$variable, quantile, 0.25, na.rm = TRUE),
      q3 = tapply(sp$contribution, sp$variable, quantile, 0.75, na.rm = TRUE),
      max = tapply(sp$contribution, sp$variable, max, na.rm = TRUE)
    )

    perm0 <- merge(
      perm0,
      cbind(rownames(stats), stats),
      by.x = "variable",
      by.y = "rownames(stats)"
    )
    # rm permutation column
    perm0 <- subset(perm0, select = setdiff(colnames(perm0), "B"))
    perm0 <- perm0[order(abs(perm0$contribution), decreasing = TRUE), ]

    output <- list(
      data = list(
        intercept = attr(sp, "intercept"),
        variables = as.character(perm0$variable_name),
        variables_value = perm0$variable_value,
        mean = perm0$contribution,
        min = perm0$min,
        max = perm0$max,
        q1 = perm0$q1,
        q3 = perm0$q3
      ),
      plotType = "SHAPValues",
      plotCategory = "Observation Level",
      plotComponent = "SHAPValues",
      name = "Shapley Values",
      params = list(
        model = explainer$label,
        observation = rownames(observation)
      )
    )
  }, error = function(e) {
    stop("Failed to calculate Shapley Values\n", e)
  })
  output
}
