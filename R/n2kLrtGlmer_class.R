#' @importFrom methods setOldClass
setOldClass("anova")

#' @importFrom methods setClassUnion
setClassUnion("maybeAnova", c("anova", "NULL"))

#' The n2kLrtGlmer class
#' 
#' Calculate composite indices from multiple analysis
#' @section Slots:
#'   \describe{
#'    \item{\code{Parent}}{the file fingerprint of the alternative model}
#'    \item{\code{Parent0}}{the file fingerprint of the NULL model}
#'    \item{\code{Model}}{the alternative model}
#'    \item{\code{Model0}}{the NULL model}
#'    \item{\code{ParentStatus}}{the last known status of the parent analysis}
#'    \item{\code{Anova}}{the anova table}
#'   }
#' @name n2kLrtGlmer-class
#' @rdname n2kLrtGlmer-class
#' @exportClass n2kLrtGlmer
#' @aliases n2kLrtGlmer-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R n2kGlmerPoisson_class.R
setClass(
  "n2kLrtGlmer",
  representation = representation(
    Parent = "character",
    Parent0 = "character",
    Model = "maybeGlmerMod",
    Model0 = "maybeGlmerMod",
    ParentStatus = "data.frame",
    Anova = "maybeAnova"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_single_character check_dataframe_variable
#' @importFrom digest digest
setValidity(
  "n2kLrtGlmer",
  function(object){
    check_dataframe_variable(
      df = object@ParentStatus, 
      variable = c("FileFingerprint", "Status", "StatusFingerprint"),
      name = "ParentStatus"
    )
    check_single_character(object@Parent, name = "Parent")
    check_single_character(object@Parent0, name = "Parent0")
    
    if(!all.equal(
      sort(c(object@Parent, object@Parent0)),
      object@ParentStatus$FileFingerprint
    )){
      stop("FileFingerprints in ParentStatus slot don't match Parent and Parent0 slots")
    }
    
    file.fingerprint <- digest(
      list(
        object@SchemeID, object@SpeciesGroupID, object@LocationGroupID, 
        object@ModelType, object@Covariate, object@FirstImportedYear, object@LastImportedYear,
        object@Duration, object@LastAnalysedYear, object@AnalysisDate, object@Seed, 
        object@Parent, object@Parent0
      ),
      algo = "sha1"
    )
    if(object@FileFingerprint != file.fingerprint){
      stop("Corrupt FileFingerprint")
    }
    
    status.fingerprint <- digest(
      list(
        object@FileFingerprint, object@Status, object@ParentStatus, object@Model, 
        object@Model0, object@Anova, object@SessionInfo
      ),
      algo = "sha1"
    )
    if(object@StatusFingerprint != status.fingerprint){
      stop("Corrupt StatusFingerprint")
    }
    
    return(TRUE)
  }
)
