get_local_plots <- function(explainer, observations, params) {
  is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
  vars <- intersect(names(is_y[!is_y]), colnames(observations))
  plots <- list()

  # observations was validated and have min 1 row
  obs_list <- lapply(1:nrow(observations), function(i) observations[i, vars])

  bd <- lapply(obs_list, function(o) get_break_down(explainer, o, params))
  plots <- c(plots, bd[!sapply(bd, is.null)])

  cp <- lapply(obs_list, function(o) {
    lapply(vars, function(v) get_ceteris_paribus(explainer, o, v, params))
  })
  cp <- unlist(cp, recursive = FALSE)
  plots <- c(plots, cp[!sapply(cp, is.null)])

  plots
}

get_global_plots <- function(explainer, params) {
  is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
  vars <- names(is_y[!is_y])
  plots <- list()

  fi <- get_feature_importance(explainer, vars, params)
  if (!is.null(fi)) plots[[length(plots) + 1]] <- fi

  pd <- lapply(vars, function(v) get_partial_dependence(explainer, v, params))
  ad <- lapply(vars, function(v) {
    get_accumulated_dependence(explainer, v, params)
  })
  plots <- c(plots, pd[!sapply(pd, is.null)], ad[!sapply(ad, is.null)])
  
  plots
}

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
      plotCategory = "Local",
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
    warning("Failed to calculate ceteris paribus")
  })
  output
}
get_break_down <- function(explainer, observation, params) {
  output <- NULL
  tryCatch({
    bd <- iBreakDown::local_attributions(explainer, observation)
    # Remove intercept and prediction row
    vars_index <- 2:(length(bd$variable) - 1)
    output <- list(
      plotComponent = "Breakdown",
      plotCategory = "Local",
      plotType = "Breakdown",
      name = "Break down",
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
    warning(e)
    warning("Failed to calculate break down")
  })
  output
}

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
      plotCategory = "Global",
      plotComponent = ifelse(is_num,
        "LinearDependence",
        "CategoricalDependence"
      ),
      name = "Accumulated Dependence",
      params = list(model = explainer$label, variable = variable)
    )
  }, error = function(e) {
    warning("Failed to calculate accumulated dependence")
  })
  output
}

get_partial_dependence <- function(explainer, variable, params) {
  output <- NULL
  params
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
      plotCategory = "Global",
      plotComponent = ifelse(is_num,
        "LinearDependence",
        "CategoricalDependence"
      ),
      name = "Partial Dependence",
      params = list(model = explainer$label, variable = variable)
    )
  }, error = function(e) {
    warning("Failed to calculate partial dependence")
  })
  output
}

get_feature_importance <- function(explainer, vars, params) {
  output <- NULL
  params
  tryCatch({
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
    perm0 <- subset(perm0, select = -permutation)
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
      plotCategory = "Global",
      plotComponent = "FeatureImportance",
      name = "Feature Importance",
      params = list(model = explainer$label)
    )
  }, error = function(e) {
    warning("Failed to calculate feature importance")
  })
  output
}
