#' Splits multiclass explainer into multiple classification explainers
#'
#' @param explainer Multiclass explainer created using \code{DALEX::explain}
#' @return list of explainers
#' @importFrom methods is
split_multiclass_explainer <- function(explainer) {
  if (is.null(explainer) || !is(explainer, "explainer")) {
    stop("Invalid explainer argument")
  }
  type <- explainer$model_info$type
  if (type != "multiclass") {
    stop(paste("Cannot split explainer with type:", type))
  }
  # y column no longer can be inside data
  is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
  data <- explainer$data[, !is_y]
  # prediction columns
  pred_cols <- colnames(explainer$y_hat)
  # create explainers for each y class
  lapply(pred_cols, function(class_name) {
    # modify model info
    model_info <- explainer$model_info
    model_info$type <- "classification"
    # number of column in predict output
    pred_col_number <- which(pred_cols == class_name)
    DALEX::explain(
      explainer$model,
      data = data,
      y = explainer$y == class_name,
      predict_function = function(model, newdata, ...) {
        prediction <- explainer$predict_function(model, newdata, ...)
        if (is.null(dim(prediction))) {
          prediction[pred_col_number]
        } else {
          prediction[, pred_col_number]
        }
      },
      label = paste0(explainer$label, " [", class_name, "]"),
      type = explainer$type,
      model_info = model_info
    )
  })
}

#' Checks if it is safe do add new observations to the arena object
#'
#' Function checks if rownames are not already used and call stop if
#' there is at least one conflict.
#'
#' @param arena live or static arena object
#' @param observations data frame of new observations
#' @return None
#' @importFrom methods is
validate_new_observations <- function(arena, observations) {
  if (is.null(observations) || !is(observations, "data.frame")) {
    stop("Invalid observations argument")
  }
  if (length(intersect(get_observations_list(arena), rownames(observations)))) {
    stop("Observations rownames must be unique")
  }
}

#' Checks if it is safe do add new dataset to the arena object
#'
#' @param arena live or static arena object
#' @param dataset data frame for data analysis
#' @param target name of target variable
#' @param label name of dataset
#' @return None
#' @importFrom methods is
validate_new_dataset <- function(arena, dataset, target, label) {
  if (is.null(dataset) || !is(dataset, "data.frame")) {
    stop("Invalid dataset argument")
  }
  if (is.null(target) || !is.character(target) || length(target) != 1) {
    stop("Invalid target argument")
  }
  if (!(target %in% colnames(dataset))) {
    stop("Target is not a valid column name")
  }
  if (sum(is.na(dataset) > 0)) {
    stop("Dataset cannot contain NAs")
  }
  if (is.null(label) || !is.character(label) || length(label) != 1) {
    stop("Invalid label argument")
  }
  labels <- sapply(arena$datasets, function(x) x$label)
  if (label %in% labels) {
    stop("Dataset must have unique label")
  }
}

#' Checks if it is safe do add a new model to the arena object
#'
#' Function checks if explainer's label is not already used call stop if
#' there is at least one conflict.
#'
#' @param arena live or static arena object
#' @param explainer Explainer created using \code{DALEX::explain}
#' @return None
#' @importFrom methods is
validate_new_model <- function(arena, explainer) {
  if (is.null(explainer) || !is(explainer, "explainer")) {
    stop("Invalid explainer argument")
  }
  labels <- sapply(arena$explainers, function(x) x$label)
  if (explainer$label %in% labels) {
    stop("Explainers must have unique label")
  }
}

#' Generates list of rownames of each observation from each batch
#'
#' @param arena live or static arena object
#' @return list of observations' names
get_observations_list <- function(arena) {
  as.list(unlist(lapply(arena$observations_batches, rownames)))
}

#' Generates list of unique variables(without target) from each explainer and dataset
#'
#' @param arena live or static arena object
#' @return list of variables' names
get_variables_list <- function(arena) {
  from_explainers <- unlist(lapply(arena$explainers, function(expl) {
    is_y <- sapply(expl$data, function(column) { identical(column, expl$y) })
    names(is_y[!is_y])
  }))
  from_datasets <- unlist(lapply(arena$datasets, function(x) x$variables))
  as.list(unique(c(from_explainers, from_datasets)))
}

#' Generates list of datasets' labels
#'
#' @param arena live or static arena object
#' @return list of datasets' labels
get_datasets_list <- function(arena) {
  lapply(arena$datasets, function(dataset) dataset$label)
}

#' Prepare object ready to change into json
#'
#' Function converts object with class \code{arena_live} or \code{arena_static}
#' to object with structure accepted by Arena. See \href{https://github.com/ModelOriented/Arena/tree/master/src/store/schemas}{list of schemas}.
#' @param arena live or static arena object
#' @return Object for direct conversion into json
#' @importFrom methods is
get_json_structure <- function(arena) {
  UseMethod("get_json_structure")
}

#' @importFrom methods is
get_json_structure.arena_static <- function(arena) {
  if (is.null(arena) || !is(arena, "arena_static")) {
    stop("Invalid arena argument")
  }
  list(
    version = "1.1.0",
    availableParams = list(
      observation = get_observations_list(arena),
      variable = get_variables_list(arena),
      model = lapply(arena$explainers, function(x) { x$label }),
      dataset = get_datasets_list(arena)
    ),
    data = arena$plots_data
  )
}

#' @importFrom methods is
get_json_structure.arena_live <- function(arena) {
  if (is.null(arena) || !is(arena, "arena_live")) {
    stop("Invalid arena argument")
  }
  list(
    version = "1.1.0",
    api = "arenar_api",
    timestamp = arena$timestamp*1000,
    availableParams = list(
      observation = get_observations_list(arena),
      variable = get_variables_list(arena),
      model = lapply(arena$explainers, function(x) { x$label }),
      dataset = get_datasets_list(arena)
    ),
    availablePlots = list(
      list(
        name = "Break Down",
        plotType = "Breakdown",
        plotCategory = "Observation Level",
        requiredParams = list("model", "observation")
      ),
      list(
        name = "Ceteris Paribus",
        plotType = "CeterisParibus",
        plotCategory = "Observation Level",
        requiredParams = list("model", "observation", "variable")
      ),
      list(
        name = "Shapley Values",
        plotType = "SHAPValues",
        plotCategory = "Observation Level",
        requiredParams = list("model", "observation")
      ),
      list(
        name = "Partial Dependence",
        plotType = "PartialDependence",
        plotCategory = "Dataset Level",
        requiredParams = list("model", "variable")
      ),
      list(
        name = "Accumulated Dependence",
        plotType = "AccumulatedDependence",
        plotCategory = "Dataset Level",
        requiredParams = list("model", "variable")
      ),
      list(
        name = "Variable Importance",
        plotType = "FeatureImportance",
        plotCategory = "Dataset Level",
        requiredParams = list("model")
      ),
      list(
        name = "Receiver Operating Characterstic",
        plotType = "ROC",
        plotCategory = "Model Performance",
        requiredParams = list("model")
      ),
      list(
        name = "Regression Error Characteristic",
        plotType = "REC",
        plotCategory = "Model Performance",
        requiredParams = list("model")
      ),
      list(
        name = "Funnel Plot",
        plotType = "FunnelMeasure",
        plotCategory = "Model Performance",
        requiredParams = list("model")
      ),
      list(
        name = "Subsets Performance",
        plotType = "SubsetsPerformance",
        plotCategory = "Model Performance",
        requiredParams = list("model")
      ),
      list(
        name = "Metrics",
        plotType = "Metrics",
        plotCategory = "Model Performance",
        requiredParams = list("model")
      ),
      list(
        name = "Fairness",
        plotType = "Fairness",
        plotCategory = "Dataset Level",
        requiredParams = list("model", "variable")
      ),
      list(
        name = "Variable Distribution",
        plotType = "VariableDistribution",
        plotCategory = "EDA",
        requiredParams = list("dataset", "variable")
      ),
      list(
        name = "Variable Against Another",
        plotType = "VariableAgainstAnother",
        plotCategory = "EDA",
        requiredParams = list("dataset", "variable")
      )
    )
  )
}
