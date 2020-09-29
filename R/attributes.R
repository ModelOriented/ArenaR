get_model_attributes <- function(arena, explainer) {
  list()
}

get_dataset_attributes <- function(arena, explainer) {
  list()
}

get_observation_attributes <- function(arena, observation) {
  as.list(observation)
}

get_variable_attributes <- function(arena, variable) {
  from_explainers <- lapply(arena$explainers, function(expl) expl$data[[variable]])
  from_datasets <- lapply(arena$datasets, function(x) x$dataset[[variable]])
  columns <- c(from_datasets, from_explainers)
  # filter out nulls
  columns <- columns[!sapply(columns, is.null)]
  # return NULL if variable is not present
  if (length(columns) == 0) return(NULL)
  get_column_type <- function(col) {
    if (is.logical(col)) return('logical')
    else if (is.numeric(col)) return('numeric')
    else if (is.factor(col) || length(unique(col)) < 20) return('categorical')
    else stop(paste("Unsupported column type for variable:", variable))
  }
  output <- list()
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
