#' The n2kAnalysisMetadata class
#' @name n2kAnalysisMetadata-class
#' @rdname n2kAnalysisMetadata-class
#' @exportClass n2kAnalysisMetadata
#' @aliases n2kAnalysisMetadata-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kAnalysisMetadata",
  contains = "data.frame",
  prototype = prototype(
    LocationGroupID = integer(0),
    SpeciesGroupID = integer(0),
    ModelType = character(0),
    FirstImportedYear = integer(0),
    LastImportedYear = integer(0),
    Duration = integer(0),
    LastAnalysedYear = integer(0),
    Seed = integer(0),
    Fingerprint = character(0),
    AnalysisVersion = character(0),
    Status = character(0)
  )
)

#' @importFrom methods setValidity
setValidity(
  "n2kAnalysisMetadata",
  function(object){
    if(any(object$LocationGroupID <= 0)){
      stop("LocationGroupID must be strictly positive")
    }
    if(any(object$SpeciesGroupID <= 0)){
      stop("SpeciesGroupID must be strictly positive")
    }
    if(any(object$FirstImportedYear <= 0)){
      stop("FirstImportedYear must be strictly positive")
    }
    if(any(object$LastImportedYear <= 0)){
      stop("LastImportedYear must be strictly positive")
    }
    if(any(object$LastAnalysedYear <= 0)){
      stop("LastAnalysedYear must be strictly positive")
    }
    if(any(object$Duration <= 0)){
      stop("Duration must be strictly positive")
    }
    return(TRUE)
  }
)
