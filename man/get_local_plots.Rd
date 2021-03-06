% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{get_local_plots}
\alias{get_local_plots}
\title{Internal function for calculating local plots for all observations}
\usage{
get_local_plots(explainer, observations, params)
}
\arguments{
\item{explainer}{Explainer created using \code{DALEX::explain}}

\item{observations}{Data frame of observations}

\item{params}{Params from arena object}
}
\value{
list of generated plots' data
}
\description{
Function runs all plot generating methods for given observations
}
