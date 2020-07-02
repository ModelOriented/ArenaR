#' Creates arena object
#'
#' @param live Defines if arena should start live server or generate static json
#' @param N number of observations used to calculate dependence profiles
#' @param fi_B Number of permutation rounds to perform each variable
#' in feature importance
#' @param fi_N number of observations used in feature importance
#' @param grid_points number of points for profile
#' @param shap_B Numer of random paths in SHAP
#' @param cl Cluster used to run parallel computations (Do not work in live Arena)
#' @return Empty \code{arena_static} of \code{arena_live} class object
#' @export
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
#' print(arena)
#' if (interactive()) upload_arena(arena)
create_arena <- function(live = FALSE,
                      N = 500,
                      fi_N = NULL,
                      fi_B = 10,
                      grid_points = 101,
                      shap_B = 10,
                      cl = NULL) {
  if (live) return(
    structure(
      list(
        explainers = list(),
        # batch = one data frame of observations
        observations_batches = list(),
        params = list(
          cp_grid_points = grid_points,
          ad_grid_points = grid_points,
          ad_N = N,
          pd_grid_points = grid_points,
          pd_N = N,
          fi_B = fi_B,
          fi_n_samples = fi_N,
          shap_B = shap_B
        ),
        timestamp = as.numeric(Sys.time())
      ),
      class = "arena_live"
    )
  )
  else return(
    structure(
      list(
        explainers = list(),
        # batch = one data frame of observations
        observations_batches = list(),
        params = list(
          cp_grid_points = grid_points,
          ad_grid_points = grid_points,
          ad_N = N,
          pd_grid_points = grid_points,
          pd_N = N,
          fi_B = fi_B,
          fi_n_samples = fi_N,
          shap_B = shap_B,
          cl = cl
        ),
        plots_data = list()
      ),
      class = "arena_static"
    )
  )
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
  cat(paste("Models:", paste(jstr$models, collapse = ", "), "\n"))
  cat(paste("Observations:", paste(jstr$observations, collapse = ", "), "\n"))
  cat(paste("Variables:", paste(jstr$variables, collapse = ", "), "\n"))
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
  cat(paste("Models:", paste(jstr$models, collapse = ", "), "\n"))
  cat(paste("Observations:", paste(jstr$observations, collapse = ", "), "\n"))
  cat(paste("Variables:", paste(jstr$variables, collapse = ", "), "\n"))
  cat("Remember to start server with arena_run(arena)\n")
}

#' Adds model to arena
#'
#' If arena is static it will start calculations for all already pushed
#' observations and global plots. If arena is live, then plots will be
#' calculated on demand, after calling \code{arena_run}.
#' 
#' @param arena live or static arena object
#' @param explainer Explainer created using \code{DALEX::explain}
#' @return Updated arena object
#' @export
#' @examples
#' library("DALEX")
#' library("arenar")
#' library("dplyr", quietly=TRUE, warn.conflicts = FALSE)
#' # create first model
#' model1 <- glm(m2.price ~ ., data=apartments, family=gaussian)
#' # create a DALEX explainer
#' explainer1 <- DALEX::explain(model1, data=apartments, y=apartments$m2.price, label="GLM gaussian")
#' # create live arena with only one model
#' arena <- create_arena(live=TRUE) %>% push_model(explainer1)
#' print(arena)
#' # create and add next model
#' model2 <- glm(m2.price ~ ., data=apartments, family=Gamma)
#' explainer2 <- DALEX::explain(model2, data=apartments, y=apartments$m2.price, label="GLM gamma")
#' arena <- arena %>% push_model(explainer2)
#' print(arena)
push_model <- function(arena, explainer) {
  UseMethod("push_model")
}

#' @export
#' @importFrom methods is
push_model.arena_static <- function(arena, explainer) {
  if (is.null(arena) || !is(arena, "arena_static")) {
    stop("Invalid arena argument")
  }
  validate_new_model(arena, explainer)
 
  # split expaliner into multiple classification explainers
  if (explainer$model_info$type == "multiclass") {
    cat("Provided explainer is multiclass and will be splited\n")
    splited <- split_multiclass_explainer(explainer)
    for (i in seq_along(splited)) {
      arena <- push_model(arena, splited[[i]])
    }
    return(arena)
  }

  params <- arena$params

  # calculate global plots and append to list
  arena$plots_data <- c(arena$plots_data, get_global_plots(explainer, params))
  # for each observations data frame calculate local plots
  local_plots <- lapply(
    arena$observations_batches,
    function(observations) get_local_plots(explainer, observations, params)
  )
  # flatten result and add to plots
  arena$plots_data <- c(
    arena$plots_data,
    unlist(local_plots, recursive = FALSE)
  )
  # save this explainer
  arena$explainers[[length(arena$explainers) + 1]] <- explainer
  arena
}

#' @export
#' @importFrom methods is
push_model.arena_live <- function(arena, explainer) {
  if (is.null(arena) || !is(arena, "arena_live")) {
    stop("Invalid arena argument")
  }
  validate_new_model(arena, explainer)
  
  # split expaliner into multiple classification explainers
  if (explainer$model_info$type == "multiclass") {
    cat("Provided explainer is multiclass and will be splited\n")
    splited <- split_multiclass_explainer(explainer)
    for (i in seq_along(splited)) {
      arena <- push_model(arena, splited[[i]])
    }
    return(arena)
  }
  
  # save explainer
  arena$explainers[[length(arena$explainers) + 1]] <- explainer
  # update timestamp
  arena$timestamp <- as.numeric(Sys.time())
  arena
}

#' Adds new observations to arena
#' 
#' If arena is static it will start calculations for all already pushed
#' models. If arena is live, then plots will be calculated on demand,
#' after calling \code{arena_run}.
#'
#' @param arena live or static arena object
#' @param observations data frame of new observations
#' @return Updated arena object
#' @export
push_observations <- function(arena, observations) {
  UseMethod("push_observations")
}

#' @export
#' @importFrom methods is
push_observations.arena_static <- function(arena, observations) {
  if (is.null(arena) || !is(arena, "arena_static")) {
    stop("Invalid arena argument")
  }
  validate_new_observations(arena, observations)

  # helper function get local plots for fixed observations
  get_local <- function(expl) get_local_plots(expl, observations, arena$params)

  arena$plots_data <- c(
    arena$plots_data,
    # for each explainer calculate local plots and flatten results into one list
    unlist(lapply(arena$explainers, get_local), recursive = FALSE)
  )
  # save observations batch
  n <- length(arena$observations_batches) + 1
  arena$observations_batches[[n]] <- observations
  arena
}

#' @export
#' @importFrom methods is
push_observations.arena_live <- function(arena, observations) {
  if (is.null(arena) || !is(arena, "arena_live")) {
    stop("Invalid arena argument")
  }
  validate_new_observations(arena, observations)

  # save observations
  n <- length(arena$observations_batches) + 1
  arena$observations_batches[[n]] <- observations
  # update timestamp
  arena$timestamp <- as.numeric(Sys.time())
  arena
}

#' Upload generated json file from static arena
#'
#' By default function opens browser with new arena session. Appending data to
#' already existing session is also possible using argument \code{append_data}
#'
#' @param arena Static arena object
#' @param open_browser Whether to open browser with new session
#' @param append_data Whether to append data to already existing session
#' @param arena_url URL of Arena dashboard instance
#' @param pretty whether to generate pretty and easier to debug JSON
#' @return not modified arena object
#' @export
#' @importFrom methods is
upload_arena <- function (arena, open_browser = TRUE, append_data = FALSE,
                          arena_url = "https://arena.drwhy.ai/", pretty=FALSE) {
  if (is.null(arena) || !is(arena, "arena_static")) {
    stop("Invalid arena argument")
  }
  # generate json string
  json <- jsonlite::toJSON(
    get_json_structure(arena),
    auto_unbox = TRUE,
    pretty = pretty
  )
  # upload json to gist
  gist <- gistr::gist_create(
    public = FALSE,
    browse = FALSE,
    code = json,
    filename = "data.json"
  )
  # url of raw data file
  url <- gist$files$data.json$raw_url
  print(paste("Data url: ", url))
  if (append_data) {
    # append data to already existing session
    utils::browseURL(paste0(arena_url, "?append=", url))
  } else if (open_browser) {
    # open new session
    utils::browseURL(paste0(arena_url, "?data=", url))
  }
  arena
}
