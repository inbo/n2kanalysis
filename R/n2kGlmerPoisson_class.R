#' @importFrom methods setClassUnion
#' @importClassesFrom lme4 glmerMod
setClassUnion("maybeGlmerMod", c("glmerMod", "NULL"))

#' The n2kGlmerPoisson class
#' @name n2kGlmerPoisson-class
#' @rdname n2kGlmerPoisson-class
#' @exportClass n2kGlmerPoisson
#' @aliases n2kGlmerPoisson-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kGlmerPoisson",
  representation = representation(
    Weight = "character",
    Model = "maybeGlmerMod"
  ),
  contains = "n2kModel"
)
