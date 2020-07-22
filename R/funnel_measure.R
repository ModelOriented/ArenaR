#' Internal function for calculating data for funnel plot
#'
#' This is simplified version of \code{DALEXtra::funnel_measure}
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param measure_function measure function that calculates performance of model based on true observation and prediction.
#'                         Order of parameters is important and should be (y, y_hat). The measure calculated by the function
#'                         should have the property that lower score value indicates better model. If NULL, RMSE will be used for regression,
#'                           one minus auc for classification and crossentropy for multiclass classification.
#' @param nbins Number of qunatiles (partition points) for numeric columns. In case when more than one qunatile have the same value, there will be less partition points.
#' @param cutoff Threshold for categorical data. Entries less frequent than specified value will be merged into one category.
#' @param cutoff_name Name for new category that arised after merging entries less frequent than \code{cutoff}
#' @param factor_conversion_threshold Numeric columns with lower number of unique values than value of this parameter will be treated as factors
#' @return Data frame with columns
#' \itemize{
#'   \item{Variable}{ Name of splited variable}
#'   \item{Measure}{ Loss value for subset}
#'   \item{Label}{ Label for variable's values subset}
#' }
#' @importFrom methods is
#' @importFrom stats predict
funnel_measure <- function(explainer,
                           measure_function = NULL,
                           nbins = 5,
                           cutoff = 0.01,
                           cutoff_name = "Other",
                           factor_conversion_threshold = 7) {
    if (is.null(explainer) || !is(explainer, "explainer")) {
      stop("Invalid explainer argument")
    }

    data <- explainer$data
    col_index <- 1
    y <- explainer$y
    ret <- data.frame()
    col_names <- colnames(data)

    for (col in data) {
      if (is.character(col)) {
        col <- as.factor(col)
      }
      if (length(unique(col)) < factor_conversion_threshold) {
        col <- as.factor(col)
      }
      if (is.numeric(col)) {
        quantiles <- round(quantile(col, probs = seq(0, 1, length.out = nbins+1)), 2)
        for (i in 1:(length(quantiles) - 1)) {
          # First interval should be closed on both sides and the other ones only on the right side
          rows <- (quantiles[i] < col & col <= quantiles[i + 1]) | (i == 1 & quantiles[i] == col)
          scoring_data <- data[rows,]
          scoring_y <- y[rows]
          #In case of empty compartment
          if (length(scoring_y) == 0) next()
          measure <- measure_function(scoring_y, predict(explainer, scoring_data))
          ret <- rbind(
            ret,
            list(
              "Variable" = col_names[col_index],
              "Measure" = measure,
              "Label" = ifelse(
                quantiles[i] == quantiles[i + 1],
                quantiles[i] ,
                paste(ifelse(i == 1, "[", "("), quantiles[i], ", ", quantiles[i+1], "]", sep = "")
              )
            ),
            stringsAsFactors = FALSE
          )
        }
      } else if (is.factor(col)) {
        col <- as.character(col)
        if (length(unique(col)) > 4) {
          freq <- table(col) / length(col)
          names_to_cut <- names(freq[freq < cutoff])
          for (name in names_to_cut) {
            col[col == name] <- cutoff_name
          }
        }
        for (level in sort(unique(col))) {
          scoring_data <- data[col == level,]
          scoring_y <- y[col == level]
          measure <- measure_function(scoring_y, predict(explainer, scoring_data))
          ret <- rbind(
            ret,
            list(
              "Variable" = col_names[col_index],
              "Measure" = measure,
              "Label" = level
            ),
            stringsAsFactors = FALSE
          )
        }
      } else {
        stop(paste("Not recognizable column type"), col_names[col_index])
      }
      col_index <- col_index + 1
    }
    ret
  }
