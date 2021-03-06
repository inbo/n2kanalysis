% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n2k_composite.R
\docType{methods}
\name{n2k_composite}
\alias{n2k_composite}
\alias{n2k_composite,data.frame-method}
\alias{n2k_composite,n2kComposite-methods}
\title{Create a n2kComposite object}
\usage{
n2k_composite(parent.status, ...)

\S4method{n2k_composite}{data.frame}(parent.status, status = "waiting",
  result.datasource.id, scheme.id, formula, species.group.id,
  location.group.id, model.type, first.imported.year, last.imported.year,
  duration, last.analysed.year, analysis.date, extractor, ..., seed)
}
\arguments{
\item{parent.status}{A \code{data.frame} with columns
\code{ParentAnalysis} (the file fingerprint of the parent),
\code{ParentStatusFingerprint} (the status fingerprint of the parent),
and \code{ParentStatus} (the status of the parent).}

\item{...}{other arguments}

\item{status}{A single character indicating the status of the model.
Defaults to \code{"waiting"}.}

\item{result.datasource.id}{The id of the results datasource.}

\item{scheme.id}{A single integer holding the id of the scheme.}

\item{formula}{A single character identifying the comparison.}

\item{species.group.id}{A single integer identifing the species group.}

\item{location.group.id}{A single integer identifing the location group.}

\item{model.type}{The type of the models.
Must start with \code{"inla comparison:"}.}

\item{first.imported.year}{Oldest year considered in the data.}

\item{last.imported.year}{Most recent year considered in the data.}

\item{duration}{The width of the moving window.
Defaults to the \code{last.imported.year - first.imported.year + 1}.}

\item{last.analysed.year}{Most recent year in the window.
Defaults to \code{last.imported.year}.}

\item{analysis.date}{A POSIXct date indicating the date that the dataset was
imported.}

\item{extractor}{a function to extract the relevant parameters from the model}

\item{seed}{A single integer used as a seed for all calculations.
A random seed will be inserted when missing.}
}
\description{
A new \code{n2kComposite} model is created when \code{parent} is a
\code{data.frame}.
}
