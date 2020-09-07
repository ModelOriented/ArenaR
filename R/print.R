#' Internal function for pretty truncationg params list
#'
#' @param vec vector to be truncated
#' @param size elements with index greater than size will be truncated
#' @return string with collapsed and truncated input vector
truncate_vector <- function(vec, size = 6) {
  if (length(vec) > size) {
    paste(
      paste(vec[seq_len(size)], collapse = ", "),
      "and",
      length(vec) - size,
      "more"
    )
  } else {
    paste(vec, collapse = ", ")
  }
}

#' Prints static arena summary
#'
#' @param x \code{arena_static} object
#' @param ... other parameters
#' @export
#' @return None
#' @examples
#' library("DALEX")
#' library("arenar")
#' library("dplyr", quietly=TRUE, warn.conflicts = FALSE)
#' # create a model
#' model <- glm(m2.price ~ ., data=apartments)
#' # create a DALEX explainer
#' explainer <- DALEX::explain(model, data=apartments, y=apartments$m2.price)
#' # prepare observations to be explained
#' observations <- apartments[1:3, ]
#' # rownames are used as labels for each observation
#' rownames(observations) <- paste0(observations$construction.year, "-", observations$surface, "m2")
#' # generate static arena for one model and 3 observations
#' arena <- create_arena(live=FALSE) %>% push_model(explainer) %>% push_observations(observations)
#' # print summary
#' print(arena)
print.arena_static <- function(x, ...) {
  cat("===== Static Arena Summary =====\n")
  jstr <- get_json_structure(x)
  params <- jstr$availableParams
  for (param in names(params)) {
    cat(
      paste0(
        toupper(substr(param, 1, 1)), # capitalize first letter
        substr(param, 2, nchar(param)),
        "s: ",
        truncate_vector(params[[param]]),
        "\n"
      )
    )
  }
  cat(paste("Plots count:", length(jstr$data), "\n"))
}

#' Prints live arena summary
#'
#' @param x \code{arena_live} object
#' @param ... other parameters
#' @export
#' @return None
#' @examples
#' library("DALEX")
#' library("arenar")
#' library("dplyr", quietly=TRUE, warn.conflicts = FALSE)
#' # create a model
#' model <- glm(m2.price ~ ., data=apartments)
#' # create a DALEX explainer
#' explainer <- DALEX::explain(model, data=apartments, y=apartments$m2.price)
#' # prepare observations to be explained
#' observations <- apartments[1:30, ]
#' # rownames are used as labels for each observation
#' rownames(observations) <- paste0(observations$construction.year, "-", observations$surface, "m2")
#' # generate live arena for one model and 30 observations
#' arena <- create_arena(live=TRUE) %>% push_model(explainer) %>% push_observations(observations)
#' # print summary
#' print(arena)
print.arena_live <- function(x, ...) {
  cat("===== Live Arena Summary =====\n")
  jstr <- get_json_structure(x)
  params <- jstr$availableParams
  for (param in names(params)) {
    cat(
      paste0(
        toupper(substr(param, 1, 1)), # capitalize first letter
        substr(param, 2, nchar(param)),
        "s: ",
        truncate_vector(params[[param]]),
        "\n"
      )
    )
  }
  cat("Remember to start server with run_server(arena)\n")
}

