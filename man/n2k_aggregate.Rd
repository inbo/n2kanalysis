% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n2k_aggregated.R
\docType{methods}
\name{n2k_aggregate}
\alias{n2k_aggregate}
\alias{n2k_aggregate,ANY-method}
\alias{n2k_aggregate,n2kAggregate-methods}
\title{Create a n2kAggregate object}
\usage{
n2k_aggregate(...)

\S4method{n2k_aggregate}{ANY}(...)
}
\arguments{
\item{...}{other arguments. See below}
}
\description{
A new n2kAggregate model.
}
\details{
\itemize{
\item \code{scheme.id}: a string holding the id of the scheme.
\item \code{species.group.id}: a string identifing the species group.
\item \code{location.group.id}: a string identifing the location group.
\item \code{model.type}: a string identifying the type of model to fit to the data.
\item \code{first.imported.year}: Oldest year considered in the data.
\item \code{last.imported.year}: Most recent year considered in the data.
\item \code{duration}: The width of the moving window.
Defaults to the \code{last.imported.year - first.imported.year + 1}.
\item \code{last.analysed.year}: Most recent year in the window.
Defaults to \code{last.imported.year}.
\item \code{analysis.date}: A POSIXct date indicating the date that the dataset was
imported.
\item \code{seed}: A single integer used as a seed for all calculations.
A random seed will be inserted when missing.
}

\itemize{
\item \code{fun}: The function to apply when aggregating.
\item \code{filter}: An optional list for the \code{filter} argument of
\code{\link[multimput]{aggregate_impute}}.
\item \code{join}: An optional list for the \code{join} argument of
\code{\link[multimput]{aggregate_impute}}.
}
}
