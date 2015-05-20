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
#' @aliases parent_status,n2kLrtGlmer-methods
#' @importFrom methods setMethod
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "parent_status",
  signature = signature(x = "n2kLrtGlmer"),
  definition = function(x){
    return(x@ParentStatus)
  }
)


#' @rdname parent_status
#' @aliases parent_status,n2kComposite-methods
#' @importFrom methods setMethod
#' @include n2kComposite_class.R
setMethod(
  f = "parent_status",
  signature = signature(x = "n2kComposite"),
  definition = function(x){
    return(x@ParentStatus)
  }
)
#' Overwrite the status of a n2kModel
#' @param x the n2kModel object
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
#' @importFrom digest digest
#' @include n2kLrtGlmer_class.R
setReplaceMethod(
  "parent_status",
  "n2kLrtGlmer",
  function(x, value){
    x@ParentStatus <- value
    x@StatusFingerprint <- digest(
      list(
        x@FileFingerprint, x@Status, x@ParentStatus, x@Model, x@Model0, x@SessionInfo
      ),
      algo = "sha1"
    )
    validObject(x)
    return(x)
  }
)

#' @rdname parent.status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest digest
#' @include n2kComposite_class.R
setReplaceMethod(
  "parent_status",
  "n2kComposite",
  function(x, value){
    x@ParentStatus <- value
    x@StatusFingerprint <- digest(
      list(
        x@FileFingerprint, x@Status, x@ParentStatus, x@Model, x@Model0, x@SessionInfo
      ),
      algo = "sha1"
    )
    validObject(x)
    return(x)
  }
)
