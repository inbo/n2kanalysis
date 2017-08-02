#' The n2kManifest class
#' @name n2kManifest-class
#' @rdname n2kManifest-class
#' @exportClass n2kManifest
#' @aliases n2kManifest-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kManifest",
  representation = representation(
    Manifest = "data.frame"
  ),
  prototype = prototype(
    Manifest = data.frame(
      Fingerprint = character(0),
      Parent = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_dataframe_variable
setValidity(
  "n2kManifest",
  function(object){
    check_dataframe_variable(
      df = object@Manifest,
      variable = c("Fingerprint", "Parent"),
      name = "Parameter"
    )
    if (!all(
      na.omit(object@Manifest$Parent) %in% object@Manifest$Fingerprint
    )) {
      stop("Some Parent in 'Manifest' slot not found")
    }
    return(TRUE)
  }
)
