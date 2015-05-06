#' The n2kModel class
#' 
#' A virtual superclass to contain the analysis models
#' @section Slots:
#'   \describe{
#'    \item{\code{Data}}{a data.frame with the data}
#'    \item{\code{Status}}{a single character indicating the status of the model}
#'   }
#' @name n2kModel-class
#' @rdname n2kModel-class
#' @exportClass n2kModel
#' @aliases n2kModel-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kModel",
  representation = representation(
    Data = "data.frame",
    Status = "character",
    "VIRTUAL"
  )
)
