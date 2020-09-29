#' Creates arena object
#'
#' Creates object with class \code{arena_live} or \code{arena_static} depending on the first argument.
#' This method is always first in \code{arenar} workflow and you should specify all plots' parameters there.
#'
#' @param live Defines if arena should start live server or generate static json
#' @param N number of observations used to calculate dependence profiles
#' @param fi_B Number of permutation rounds to perform each variable
#' in feature importance
#' @param fi_N number of observations used in feature importance
#' @param grid_points number of points for profile
#' @param shap_B Numer of random paths in SHAP
#' @param funnel_nbins Number of partitions for numeric columns for funnel plot
#' @param funnel_cutoff Threshold for categorical data. Entries less frequent than specified value will be merged into one category in funnel plot.
#' @param funnel_factor_threshold Numeric columns with lower number of unique values than value of this parameter will be treated as factors in funnel plot.
#' @param fairness_cutoffs vector of available cutoff levels for fairness panel
#' @param max_points_number maximum size of sample to plot scatter plots in variable against another panel
#' @param distribution_bins vector of available bins count for histogram
#' @param enable_attributes Switch for generating attributes of observations and variables. It is required for custom params. Attributes can increase size of static Arena.
#' @param enable_custom_params Switch to allowing user to modify observations and generate plots for them.
#' @param cl Cluster used to run parallel computations (Do not work in live Arena)
#' @return Empty \code{arena_static} or \code{arena_live} class object.\cr
#' \code{arena_static}:
#' \itemize{
#'   \item{explainer}{ List of used explainers}
#'   \item{observations_batches}{ List of data frames added as observations}
#'   \item{params}{ Plots' parameters}
#'   \item{plots_data}{ List of generated data for plots}
#' }
#' \code{arena_live}:
#' \itemize{
#'   \item{explainer}{ List of used explainers}
#'   \item{observations_batches}{ List of data frames added as observations}
#'   \item{params}{ Plots' parameters}
#'   \item{timestamp}{ Timestamp of last modification}
#' }
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
                      funnel_nbins = 5,
                      funnel_cutoff = 0.01,
                      funnel_factor_threshold = 7,
                      fairness_cutoffs = seq(0.05, 0.95, 0.05),
                      max_points_number = 150,
                      distribution_bins = seq(5, 40, 5),
                      enable_attributes = TRUE,
                      enable_custom_params = TRUE,
                      cl = NULL) {
  if (live) return(
    structure(
      list(
        explainers = list(),
        # batch = one data frame of observations
        observations_batches = list(),
        datasets = list(),
        params = list(
          cp_grid_points = grid_points,
          ad_grid_points = grid_points,
          ad_N = N,
          pd_grid_points = grid_points,
          pd_N = N,
          fi_B = fi_B,
          fi_n_samples = fi_N,
          shap_B = shap_B,
          roc_grid_points = grid_points,
          rec_grid_points = grid_points,
          fm_nbins = funnel_nbins,
          fm_cutoff = funnel_cutoff,
          fm_factor_threshold = funnel_factor_threshold,
          fairness_cutoffs = fairness_cutoffs,
          vaa_points_number = max_points_number,
          vd_bins = distribution_bins,
          enable_attributes = enable_attributes,
          enable_custom_params = enable_custom_params
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
        datasets = list(),
        params = list(
          cp_grid_points = grid_points,
          ad_grid_points = grid_points,
          ad_N = N,
          pd_grid_points = grid_points,
          pd_N = N,
          fi_B = fi_B,
          fi_n_samples = fi_N,
          shap_B = shap_B,
          roc_grid_points = grid_points,
          rec_grid_points = grid_points,
          fm_nbins = funnel_nbins,
          fm_cutoff = funnel_cutoff,
          fm_factor_threshold = funnel_factor_threshold,
          fairness_cutoffs = fairness_cutoffs,
          vaa_points_number = max_points_number,
          vd_bins = distribution_bins,
          enable_attributes = enable_attributes,
          cl = cl
        ),
        plots_data = list()
      ),
      class = "arena_static"
    )
  )
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

#' Adds new datasets to Arena
#' 
#' Adds data frame to create exploratory data analysis plots
#' @param arena live or static arena object
#' @param dataset data frame used for EDA plots
#' @param target name of target variable
#' @param label label of dataset
#' @return Updated arena object
#' @export
#' @examples
#' library("DALEX")
#' library("arenar")
#' library("dplyr", quietly=TRUE, warn.conflicts = FALSE)
#' # create live arena with only one dataset
#' apartments <- DALEX::apartments
#' arena <- create_arena(live=TRUE) %>% push_dataset(apartments, "m2.price", "apartment")
#' print(arena)
#' # add another dataset
#' HR <- DALEX::HR
#' arena <- arena %>% push_dataset(HR, "status", "HR")
#' print(arena)
push_dataset <- function(arena, dataset, target, label) {
  UseMethod("push_dataset")
}

#' @export
#' @importFrom methods is
push_dataset.arena_live <- function(arena, dataset, target, label) {
  if (is.null(arena) || !is(arena, "arena_live")) {
    stop("Invalid arena argument")
  }
  validate_new_dataset(arena, dataset, target, label)

  # save dataset
  n <- length(arena$datasets) + 1
  arena$datasets[[n]] <- list(
    dataset = dataset,
    target = target,
    label = label,
    variables = colnames(dataset)[colnames(dataset) != target]
  )
  # update timestamp
  arena$timestamp <- as.numeric(Sys.time())
  arena
}

#' @export
#' @importFrom methods is
push_dataset.arena_static <- function(arena, dataset, target, label) {
  if (is.null(arena) || !is(arena, "arena_static")) {
    stop("Invalid arena argument")
  }
  validate_new_dataset(arena, dataset, target, label)

  # save dataset
  n <- length(arena$datasets) + 1
  arena$datasets[[n]] <- list(
    dataset = dataset,
    target = target,
    label = label,
    variables = colnames(dataset)[colnames(dataset) != target]
  )

  # calculate eda plots and append to list
  arena$plots_data <- c(
    arena$plots_data,
    get_dataset_plots(arena$datasets[[n]], arena$params)
  )
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
    null = "null",
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

#' Save generated json file from static arena
#'
#' @param arena Static arena object
#' @param filename Name of output file
#' @param pretty whether to generate pretty and easier to debug JSON
#' @return not modified arena object
#' @export
#' @importFrom methods is
save_arena <- function (arena, filename="data.json", pretty=FALSE) {
  if (is.null(arena) || !is(arena, "arena_static")) {
    stop("Invalid arena argument")
  }
  # generate json string
  json <- jsonlite::toJSON(
    get_json_structure(arena),
    auto_unbox = TRUE,
    null = "null",
    pretty = pretty
  )
  write(json, file = filename)
  arena
}
