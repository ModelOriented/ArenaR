#' Generates list with attributes of a model
#'
#' @param arena live or static arena object
#' @param explainer Explainer created using \code{DALEX::explain}
#' @return simple list with attributes of given model
get_model_attributes <- function(arena, explainer) {
  list()
}

#' Generates list with attributes of a dataset
#'
#' @param arena live or static arena object
#' @param dataset List with following elements 
#' \itemize{
#'   \item{dataset}{ Data frame}
#'   \item{target}{ Name of one column from data frame that is used as target variable}
#'   \item{label}{ Label for dataset to be displayed in Arena}
#'   \item{variables}{ vector of column names from data frame without target}
#' }
#' @return simple list with attributes of given dataset
get_dataset_attributes <- function(arena, dataset) {
  list()
}

#' Generates list with attributes of an observation
#'
#' @param arena live or static arena object
#' @param observation One row data frame observation
#' @return simple list with attributes of given observation
get_observation_attributes <- function(arena, observation) {
  as.list(observation)
}

#' Generates list with attributes of an variable
#'
#' @param arena live or static arena object
#' @param variable Name of variable
#' @return simple list with attributes of given variable
get_variable_attributes <- function(arena, variable) {
  # get all columns for given variable name
  from_explainers <- lapply(arena$explainers, function(expl) expl$data[[variable]])
  from_datasets <- lapply(arena$datasets, function(x) x$dataset[[variable]])
  columns <- c(from_datasets, from_explainers)
  # filter out nulls
  columns <- columns[!sapply(columns, is.null)]
  # return NULL if variable is not present
  if (length(columns) == 0) return(NULL)

  # helper function to return type of column
  get_column_type <- function(col) {
    if (is.logical(col)) return('logical')
    else if (is.numeric(col)) return('numeric')
    else if (is.factor(col) || length(unique(col)) < 20) return('categorical')
    else stop(paste("Unsupported column type for variable:", variable))
  }
  output <- list()
  # all columns should have equal type
  output$type <- unique(sapply(columns, get_column_type))
  if (length(output$type) > 1) {
    stop(paste("Columns of different types are provided for variable:", variable))
  }
  if (output$type == 'numeric') {
    output$min = min(sapply(columns, min), na.rm=TRUE)
    output$max = max(sapply(columns, max), na.rm=TRUE)
    unique_vals <- unique(unlist(lapply(columns, unique)))
    if (length(unique_vals) < 20) output$levels <- unique_vals
  } else if (output$type == 'categorical') {
    output$levels <- unique(unlist(lapply(columns, levels)))
  }
  output
}

#' Returns attributes for all params
#' 
#' When \code{param_type} is not NULL, then function returns list of objects.
#' Each object represents one of available attribute for specified param type.
#' Field \code{name} is attribute name and field \code{values} is mapped list
#' of available params to list of value of this attribute for that param.
#' When \code{param_type} is NULL, then function returns list with keys for
#' each param type and values are lists described above.
#' 
#' @param arena live or static arena object
#' @param param_type Type of param. One of
#' \itemize{
#'   \item{model}
#'   \item{variable}
#'   \item{dataset}
#'   \item{observation}
#' }
#' @return List of attributes or named list of lists of attributes for each param type.
get_attributes <- function(arena, param_type=NULL) {
  if (is.null(param_type)) {
    return(list(
      model = get_attributes(arena, "model"),
      variable = get_attributes(arena, "variable"),
      observation = get_attributes(arena, "observation"),
      dataset = get_attributes(arena, "dataset")
    ))
  }
  # return empty list if attributes are disabled
  if (!arena$params$enable_attributes) return(list())
  available_attrs <- c()
  if (param_type == "model") {
    # We do not have any attribute for models yet
    available_attrs <- c()
    attrs <- lapply(arena$explainers, get_model_attributes, arena=arena)
  } else if (param_type == "variable") {
    available_attrs <- c("min", "max", "type", "levels")
    attrs <- lapply(get_variables_list(arena), get_variable_attributes, arena=arena)
  } else if (param_type == "dataset") {
    # We do not have any attribute for datasets yet
    available_attrs <- c()
    attrs <- lapply(arena$datasets, get_dataset_attributes, arena=arena)
  } else if (param_type == "observation") {
    as.list(unlist(lapply(arena$observations_batches, rownames)))
    # all unique columns names of observations are available attributes
    available_attrs <- unique(unlist(lapply(arena$observations_batches, colnames)))
    # for each batch extract one row observations and then flat the list
    observations <- unlist(lapply(arena$observations_batches, function(observations_batch) {
      obs_list <- lapply(1:nrow(observations_batch), function(i) observations_batch[i, ])
    }), recursive=FALSE)
    attrs <- lapply(observations, get_observation_attributes, arena=arena)
  }

  lapply(available_attrs, function(attribute_name) {
    list(
      name = attribute_name,
      values = lapply(attrs, function(x) x[[attribute_name]])
    )
  })
}
