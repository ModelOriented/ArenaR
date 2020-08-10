#' Internal function for calculating data for funnel plot
#'
#' This is modified version of \code{DALEXtra::funnel_measure}
#'
#' @param explainer Explainer created using \code{DALEX::explain}
#' @param score_functions Named list of functions named \code{score_*} from \code{auditor} package
#' @param nbins Number of qunatiles (partition points) for numeric columns. In case when more than one qunatile have the same value, there will be less partition points.
#' @param cutoff Threshold for categorical data. Entries less frequent than specified value will be merged into one category.
#' @param cutoff_name Name for new category that arised after merging entries less frequent than \code{cutoff}
#' @param factor_conversion_threshold Numeric columns with lower number of unique values than value of this parameter will be treated as factors
#' @return Data frame with columns
#' \itemize{
#'   \item{Variable}{ Name of splited variable}
#'   \item{Label}{ Label for variable's values subset}
#' }
#' and one column for each score function with returned score
#' @importFrom methods is
#' @importFrom stats predict
calculate_subsets_performance <- function(explainer,
                                          score_functions = list(),
                                          nbins = 5,
                                          cutoff = 0.01,
                                          cutoff_name = "Other",
                                          factor_conversion_threshold = 7) {
    if (is.null(explainer) || !is(explainer, "explainer")) {
      stop("Invalid explainer argument")
    }

    col_index <- 1
    ret <- data.frame()
    col_names <- colnames(explainer$data)

    for (col in explainer$data) {
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
          # create temporary explainer with only selected data
          explainer_tmp <- explainer
          explainer_tmp$data <- explainer$data[rows,]
          explainer_tmp$y <- explainer$y[rows]
          explainer_tmp$y_hat <- explainer$y_hat[rows]
          #In case of empty compartment
          if (length(explainer_tmp$y) == 0) next()
          scores <- lapply(score_functions, function(f) f(explainer_tmp)$score)
          ret <- rbind(
            ret,
            c(
              list(
                "Variable" = col_names[col_index],
                "Label" = ifelse(
                  quantiles[i] == quantiles[i + 1],
                  quantiles[i] ,
                  paste(ifelse(i == 1, "[", "("), quantiles[i], ", ", quantiles[i+1], "]", sep = "")
                )
              ),
              scores
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
          # create temporary explainer with only selected data
          explainer_tmp <- explainer
          explainer_tmp$data <- explainer$data[col == level,]
          explainer_tmp$y <- explainer$y[col == level]
          explainer_tmp$y_hat <- explainer$y_hat[col == level]
          scores <- lapply(score_functions, function(f) f(explainer_tmp)$score)
          ret <- rbind(
            ret,
            c(
              list(
                "Variable" = col_names[col_index],
                "Label" = level
              ),
              scores
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
