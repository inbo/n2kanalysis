% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_model.R
\docType{methods}
\name{read_model}
\alias{read_model}
\alias{read_model,ANY,character-method}
\alias{read_model,ANY,s3_bucket-method}
\alias{read_model,ANY,ANY-method}
\title{Read an \code{n2kModel} object}
\usage{
read_model(x, base, project)

\S4method{read_model}{ANY,character}(x, base, project)

\S4method{read_model}{ANY,s3_bucket}(x, base, project)

\S4method{read_model}{ANY,ANY}(x, base, project)
}
\arguments{
\item{x}{the file fingerprint of the \code{n2kModel}}

\item{base}{the base location to read the model}

\item{project}{will be a relative path within the base location}
}
\description{
Read an \code{n2kModel} object
}
