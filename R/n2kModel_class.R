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

#' @importFrom methods setValidity
#' @importFrom n2khelper check_single_character
setValidity(
  "n2kModel",
  function(object){
    check_single_character(object@Status, name = "Status")
    ok.status <- c("new", "error", "converged", "false convergence")
    if(!object@Status %in% ok.status){
      stop(
        "Status must be one of the following: ", 
        paste0("'", ok.status, "'", collapse = ", ")
      )
    }
    return(TRUE)
  }
)
