#' Run server providing data for live Arena
#'
#' By default function opens browser with new arena session. Appending data to
#' already existing session is also possible using argument \code{append_data}
#'
#' @param arena Live arena object
#' @param port server port
#' @param host server ip address (hostnames do not work yet)
#' @param open_browser Whether to open browser with new session
#' @param append_data Whether to append data to already existing session
#' @param arena_url URL of Arena dashboard instance
#' @return not modified arena object
#' @export
#' @importFrom methods is
#' @examples
#' library("DALEX")
#' library("arenar")
#' library("dplyr", quietly=TRUE, warn.conflicts = FALSE)
#' # create a model
#' model <- glm(m2.price ~ ., data=apartments)
#' # create a DALEX explainer
#' explainer <- DALEX::explain(model, data=apartments, y=apartments$m2.price)
#' # generate live arena for one model and all data as observations
#' arena <- create_arena(live=TRUE) %>% push_model(explainer) %>% push_observations(apartments)
#' # run the server
#' if (interactive()) run_server(arena, port=1234)
run_server <- function(arena, port = 8181, host = "127.0.0.1",
                      open_browser = TRUE, append_data = FALSE,
                      arena_url = "https://arena.drwhy.ai/") {
  if (is.null(arena) || !is(arena, "arena_live")) {
    stop("Invalid arena argument")
  }
  pr <- plumber::Plumber$new()
  json_structure <- get_json_structure(arena)

  # helper function to find explainer for given name
  get_explainer <- function(model_name) {
    label_equals <- sapply(arena$explainers, function(x) x$label == model_name)
    e_list <- arena$explainers[label_equals]
    if (length(e_list) != 1) return(NULL)
    e_list[[1]]
  }

  # helper function to find observation row for given name
  # when explainer argument is provided, then function can
  # return custom observation saved as JSON in observation_name
  get_observation <- function(observation_name, explainer = NULL) {
    if (!is.character(observation_name) || length(observation_name) != 1) return(NULL)
    # if first char is '{' parse as custom observation
    if (substring(observation_name, 1, 1) == "{" && !is.null(explainer) &&
        arena$params$enable_custom_params) {
      obs <- tryCatch({
        # parse json to row (list)
        obs_object <- jsonlite::fromJSON(observation_name)
        # try to add this row to first row of exisiting data
        # to validate column values
        common_names <- intersect(colnames(explainer$data), names(obs_object))
        df <- rbind(explainer$data[1, common_names], obs_object[common_names])
        # try to predict created custom observation to check
        # if it is valid
        predict(explainer, df[2, ])
        df[2, ]
      }, error = function(cond) {})
      if (!is.null(obs)) return(obs)
    }
    if (length(arena$observations_batches) == 0) return(NULL)
    name_equals <- function(x) x[rownames(x) == observation_name, ]
    obs <- do.call('rbind', lapply(arena$observations_batches, name_equals))
    if (nrow(obs) != 1) return(NULL)
    obs
  }

  # helper function to find dataset for given name
  get_dataset <- function(dataset_name) {
    label_equals <- sapply(arena$datasets, function(x) x$label == dataset_name)
    d_list <- arena$datasets[label_equals]
    if (length(d_list) != 1) return(NULL)
    d_list[[1]]
  }

  pr$handle("GET", "/", function(req, res){
    json_structure
  }, serializer = plumber::serializer_unboxed_json())

  pr$handle("GET", "/timestamp", function(req, res) {
    list(timestamp = arena$timestamp*1000)
  }, serializer = plumber::serializer_unboxed_json())

  pr$handle("GET", "/FeatureImportance", function(req, res, model = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    get_feature_importance(explainer, arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/ROC", function(req, res, model = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    get_roc(explainer, arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/REC", function(req, res, model = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    get_rec(explainer, arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/Metrics", function(req, res, model = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    get_metrics(explainer, arena$params)
  }, serializer = plumber::serializer_unboxed_json())

  pr$handle("GET", "/FunnelMeasure", function(req, res, model = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    get_funnel_measure(explainer, arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/SubsetsPerformance", function(req, res, model = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    get_subsets_performance(explainer, arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/PartialDependence",
            function(req, res, model = "", variable = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- names(is_y[!is_y])
    if (!(variable %in% vars)) return(res$status <- 404)
    get_partial_dependence(explainer, variable, arena$params)
  }, serializer = plumber::serializer_unboxed_json())

  pr$handle("GET", "/Fairness",
            function(req, res, model = "", variable = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- names(is_y[!is_y])
    if (!(variable %in% vars)) return(res$status <- 404)
    get_fairness(explainer, variable, arena$params)
  }, serializer = plumber::serializer_unboxed_json())

  pr$handle("GET", "/AccumulatedDependence",
            function(req, res, model = "", variable = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- names(is_y[!is_y])
    if (!(variable %in% vars)) return(res$status <- 404)
    get_accumulated_dependence(explainer, variable, arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/Breakdown",
            function(req, res, model = "", observation = "") {
    explainer <- get_explainer(model)
    observation <- get_observation(observation, explainer)
    if (is.null(explainer) || is.null(observation)) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- intersect(names(is_y[!is_y]), colnames(observation))
    get_break_down(explainer, observation[, vars], arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/SHAPValues",
            function(req, res, model = "", observation = "") {
    explainer <- get_explainer(model)
    observation <- get_observation(observation, explainer)
    if (is.null(explainer) || is.null(observation)) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- intersect(names(is_y[!is_y]), colnames(observation))
    get_shap_values(explainer, observation[, vars], arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/CeterisParibus",
            function(req, res, model = "", observation = "", variable = "") {
    explainer <- get_explainer(model)
    observation <- get_observation(observation, explainer)
    if (is.null(explainer) || is.null(observation) ||
      !(variable %in% colnames(observation))) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- intersect(names(is_y[!is_y]), colnames(observation))
    get_ceteris_paribus(explainer, observation[, vars], variable, arena$params)
  }, serializer = plumber::serializer_unboxed_json())

  pr$handle("GET", "/VariableDistribution",
            function(req, res, dataset = "", variable = "") {
    dataset <- get_dataset(dataset)
    if (is.null(dataset)) return(res$status <- 404)
    if (!(variable %in% dataset$variables)) return(res$status <- 404)
    get_variable_distribution(dataset, variable, arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/VariableAgainstAnother",
            function(req, res, dataset = "", variable = "") {
    dataset <- get_dataset(dataset)
    if (is.null(dataset)) return(res$status <- 404)
    if (!(variable %in% dataset$variables)) return(res$status <- 404)
    get_variable_against_another(dataset, variable, arena$params)
  }, serializer = plumber::serializer_unboxed_json())

  pr$handle("GET", "/attribute/<param_type>/<param_label>",
            function(req, res, param_type = "", param_label = "") {
    if (!arena$params$enable_attributes) return(res$status <- 404)
    # replace %20 with space etc.
    param_label <- utils::URLdecode(param_label)
    if (param_type == "model") {
      explainer <- get_explainer(param_label)
      if (is.null(explainer)) return(res$status <- 404)
      get_model_attributes(arena, explainer)
    } else if (param_type == "observation") {
      observation <- get_observation(param_label)
      if (is.null(observation)) return(res$status <- 404)
      get_observation_attributes(arena, observation)
    } else if (param_type == "variable") {
      attrs <- get_variable_attributes(arena, param_label)
      if (is.null(attrs)) return(res$status <- 404)
      attrs
    } else if (param_type == "dataset") {
      dataset <- get_dataset(param_label)
      if (is.null(dataset)) return(res$status <- 404)
      get_dataset_attributes(arena, dataset)
    } else {
      return(res$status <- 404)
    }
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$filter("cors", function(req, res){
    res$setHeader("Access-Control-Allow-Origin", "*")
    plumber::forward()
  })

  url <- paste0('http://', host, ':', port, "/")
  if (append_data) {
    utils::browseURL(paste0(arena_url, "?append=", url))
  } else if (open_browser) {
    utils::browseURL(paste0(arena_url, "?data=", url))
  }
  pr$run(port = port, host = host, swagger = FALSE, debug = FALSE)
}
