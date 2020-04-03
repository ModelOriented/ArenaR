#' Run server providing data for live Arena
#'
#' By default function opens browser with new arena session. Appending data to
#' already existing session is also possible usign argument \code{append_data}
#'
#' @param arena Live arena object
#' @param port server port
#' @param host server ip address (hostnames do not work yet)
#' @param open_browser Whether to open browser with new session
#' @param append_data Whether to append data to already existing session
#' @param arena_url URL of Arena dashboard instance
#' @return not modified arena object
#' @export
arena_run <- function(arena, port = 8181, host = "127.0.0.1",
                      open_browser = TRUE, append_data = FALSE,
                      arena_url = "https://arena.drwhy.ai/") {
  if (is.null(arena) || !is(arena, "arena_live")) {
    stop("Invalid arena argument")
  }
  pr <- plumber::plumber$new()
  json_structure <- get_json_structure(arena)

  get_explainer <- function(model_name) {
    label_equals <- sapply(arena$explainers, function(x) x$label == model_name)
    e_list <- arena$explainers[label_equals]
    if (length(e_list) != 1) return(NULL)
    e_list[[1]]
  }

  get_observation <- function(observation_name) {
    if (length(arena$observations_batches) == 0) return(NULL)
    name_equals <- function(x) x[rownames(x) == observation_name, ]
    obs <- do.call('rbind', lapply(arena$observations_batches, name_equals))
    if (nrow(obs) != 1) return(NULL)
    obs
  }

  pr$handle("GET", "/", function(req, res){
    json_structure
  }, serializer = plumber::serializer_unboxed_json())

  pr$handle("GET", "/timestamp", function(req, res) {
    list(timestamp = arena$timestamp*1000)
  }, serializer = plumber::serializer_unboxed_json)

  pr$handle("GET", "/FeatureImportance", function(req, res, model = "") {
    explainer <- get_explainer(model)
    if (is.null(explainer)) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- names(is_y[!is_y])
    get_feature_importance(explainer, vars, arena$params)
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
    observation <- get_observation(observation)
    if (is.null(explainer) || is.null(observation)) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- intersect(names(is_y[!is_y]), colnames(observation))
    get_break_down(explainer, observation[, vars], arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/SHAPValues",
            function(req, res, model = "", observation = "") {
    explainer <- get_explainer(model)
    observation <- get_observation(observation)
    if (is.null(explainer) || is.null(observation)) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- intersect(names(is_y[!is_y]), colnames(observation))
    get_shap_values(explainer, observation[, vars], arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$handle("GET", "/CeterisParibus",
            function(req, res, model = "", observation = "", variable = "") {
    explainer <- get_explainer(model)
    observation <- get_observation(observation)
    if (is.null(explainer) || is.null(observation) ||
      !(variable %in% colnames(observation))) return(res$status <- 404)
    is_y <- sapply(explainer$data, function(v) identical(v, explainer$y))
    vars <- intersect(names(is_y[!is_y]), colnames(observation))
    get_ceteris_paribus(explainer, observation[, vars], variable, arena$params)
  }, serializer = plumber::serializer_unboxed_json())
  
  pr$filter("cors", function(req, res){
    res$setHeader("Access-Control-Allow-Origin", "*")
    plumber::forward()
  })

  url <- paste0('http://', host, ':', port, "/")
  if (append_data) {
    browseURL(paste0(arena_url, "?append=", url))
  } else if (open_browser) {
    browseURL(paste0(arena_url, "?data=", url))
  }
  pr$run(port = port, host = host, swagger = FALSE, debug = FALSE)
}
