#' The `n2kSpde` class
#'
#' It hold analysis data based on an INLA model with SPDE.
#' @slot Spde A `list` containing the information for the SPDE.
#' @name n2kSpde-class
#' @rdname n2kSpde-class
#' @exportClass n2kSpde
#' @aliases n2kSpde-class
#' @importFrom methods setClass
#' @docType class
#' @include n2k_inla_class.R
#' @include spde_class.R
setClass(
  "n2kSpde",
  representation = representation(Spde = "Spde"),
  contains = "n2kInla"
)

#' @importFrom methods setValidity
#' @importFrom digest sha1
setValidity(
  "n2kSpde",
  function(object) {
    return(TRUE)
  }
)
