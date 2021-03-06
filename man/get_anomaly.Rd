% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_anomaly.R, R/get_anomaly_n2kGlmerPoisson.R,
%   R/get_anomaly_n2kInla.R, R/get_anomaly_n2kModel.R
\docType{methods}
\name{get_anomaly}
\alias{get_anomaly}
\alias{get_anomaly,n2kGlmerPoisson-method}
\alias{get_anomaly,n2kGlmerPoisson-methods}
\alias{get_anomaly,n2kInla-method}
\alias{get_anomaly,n2kInla-methods}
\alias{get_anomaly,n2kModel-method}
\title{Get the anomalies from a model}
\usage{
get_anomaly(analysis, ...)

\S4method{get_anomaly}{n2kGlmerPoisson}(analysis, n = 20,
  log.expected.ratio = log(5), log.expected.absent = log(5),
  random.threshold = log(5), verbose = TRUE, ...)

\S4method{get_anomaly}{n2kInla}(analysis, n = 20, expected.ratio = 5,
  expected.absent = 5, random.threshold = 1.05, verbose = TRUE, ...)

\S4method{get_anomaly}{n2kModel}(analysis, verbose = TRUE, ...)
}
\arguments{
\item{analysis}{The model to add}

\item{...}{Extra options. See details}

\item{n}{The maximum number of anomalies per type of anomalies.}

\item{log.expected.ratio}{Observations that have \code{abs(log(observed/fitted))}
above this ratio are potential anomalies.
Defaults to \code{log(5)}, which implies that observed values that are 5 times
higher of lower than the fitted values are potential anomalies.}

\item{log.expected.absent}{Zero observations with \code{log(fitted)} larger than
this threshold are potential anomalies.}

\item{random.threshold}{Random effects with a absolute value above this
threshold are potential anomalies.}

\item{verbose}{Print extra information on the screen}

\item{expected.ratio}{Observations that have
\code{observed / fitted > expected.ratio} or \code{fitted / observed > expected.ratio}
are potential anomalies.
Defaults to \code{5}, which implies that observed values that are 5 times higher
or lower than the fitted values are potential anomalies.}

\item{expected.absent}{Zero observations where \code{fitted > expected.absent} are
potential anomalies.}
}
\description{
Get the anomalies from a model
}
