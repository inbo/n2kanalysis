#' Get the parent status of a n2kModel
#' @param x the n2kModel object
#' @return the parent status of the object
#' @name parent_status
#' @rdname parent_status
#' @exportMethod parent_status
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "parent_status",
  def = function(x){
    standard.generic("parent_status")
  }
)

#' @rdname parent_status
#' @aliases parent_status,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod
#' @include n2kAnalysisMetadata_class.R
setMethod(
  f = "parent_status",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x){
    return(x@AnalysisRelation)
  }
)

#' Overwrite the status of a n2kAnalysisMetadata
#' @param x the n2kAnalysisMetadata object
#' @param value the new values for the status
#' @name parent_status<-
#' @rdname parent.status.change
#' @exportMethod parent_status<-
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "parent_status<-",
  def = function(x, value){
    standard.generic("parent_status<-")
  }
)

#' @rdname parent.status.change
#' @importFrom methods setReplaceMethod
#' @include n2kLrtGlmer_class.R
setReplaceMethod(
  "parent_status",
  "n2kLrtGlmer",
  function(x, value){
    x@AnalysisRelation <- value
    x@StatusFingerprint <- get_sha1(
      list(
        x@AnalysisMetadata$FileFingerprint, x@AnalysisMetadata$Status, x@Model,
        x@Model0,  x@Anova, x@AnalysisMetadata$AnalysisVersion,
        x@AnalysisRelation
      )
    )
    validObject(x)
    return(x)
  }
)

#' @rdname parent.status.change
#' @importFrom methods setReplaceMethod
#' @include n2kComposite_class.R
setReplaceMethod(
  "parent_status",
  "n2kComposite",
  function(x, value){
    x@ParentStatus <- value
    x@StatusFingerprint <- get_sha1(
      list(
        x@AnalysisMetadata$FileFingerprint, x@AnalysisMetadata$Status,
        x@Parameter, x@Index, x@AnalysisMetadata$AnalysisVersion,
        x@AnalysisRelation
      )
    )
    validObject(x)
    return(x)
  }
)
