#' The n2kComposite class
#' 
#' Calculate composite indices from multiple analysis
#' @section Slots:
#'   \describe{
#'    \item{\code{Parent}}{a vector with the FileFingerprints of the parent analysis}
#'    \item{\code{ParentStatus}}{A data.frame with the FileFingerprint, StatusFingerprint and Status of the parent analysis}
#'    \item{\code{Parameter}}{A data.frame with the relevant parameter estimates of each parent analysis}
#'    \item{\code{Index}}{The composite index based on the parameters}
#'   }
#' @name n2kComposite-class
#' @rdname n2kComposite-class
#' @exportClass n2kComposite
#' @aliases n2kComposite-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kComposite",
  representation = representation(
    Parent = "character",
    ParentStatus = "data.frame",
    Parameter = "data.frame",
    Index = "data.frame"
  ),
  contains = "n2kModel"
)


#' @importFrom methods setValidity
#' @importFrom n2khelper check_single_character check_dataframe_variable
#' @importFrom digest digest
setValidity(
  "n2kComposite",
  function(object){
    check_dataframe_variable(
      df = object@ParentStatus, 
      variable = c("FileFingerprint", "StatusFingerprint", "Status"),
      name = "ParentStatus"
    )
    
    if(!all.equal(
      object@Parent,
      object@ParentStatus$FileFingerprint
    )){
      stop("FileFingerprints in ParentStatus slot don't match Parent slot")
    }
    
    check_dataframe_variable(
      df = object@Parameter,
      name = "Parameter",
      variable = c("Parent", "Value", "Estimate", "Variance")
    )
    check_dataframe_variable(
      df = object@Index,
      name = "Index",
      variable = c("Value", "Estimate", "LowerConfidenceLimit", "UpperConfidenceLimit")
    )
    
    file.fingerprint <- digest(
      list(
        object@SchemeID, object@SpeciesGroupID, object@LocationGroupID, 
        object@ModelType, object@Covariate, object@FirstImportedYear, object@LastImportedYear,
        object@Duration, object@LastAnalysedYear, object@AnalysisDate, object@Seed, 
        object@Parent
      ),
      algo = "sha1"
    )
    if(object@FileFingerprint != file.fingerprint){
      stop("Corrupt FileFingerprint")
    }
    
    status.fingerprint <- digest(
      list(
        object@FileFingerprint, object@Status, object@ParentStatus, object@Parameter, 
        object@Index, object@SessionInfo
      ),
      algo = "sha1"
    )
    if(object@StatusFingerprint != status.fingerprint){
      stop("Corrupt StatusFingerprint")
    }
    
    return(TRUE)
  }
)
