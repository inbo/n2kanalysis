#' The n2kComposite class
#'
#' Calculate composite indices from multiple analysis
#' @section Slots:
#'   \describe{
#'    \item{\code{Extractor}}{A function to extract the relevant parameters from the model}
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
    Extractor = "function",
    Parameter = "data.frame",
    Index = "data.frame"
  ),
  contains = "n2kModel"
)


#' @importFrom methods setValidity
setValidity(
  "n2kComposite",
  function(object){
    if (nrow(object@AnalysisRelation) == 0) {
      stop("'AnalysisRelation' not defined")
    }
    if (anyNA(object@AnalysisRelation$ParentAnalysis)) {
      stop("'ParentAnalysis' in 'AnalysisRelation' slot cannot be missing")
    }

    file.fingerprint <- get_sha1(
      list(
        object@AnalysisMetadata$SchemeID,
        object@AnalysisMetadata$SpeciesGroupID,
        object@AnalysisMetadata$LocationGroupID,
        object@AnalysisMetadata$ModelType, object@AnalysisMetadata$Formula,
        object@AnalysisMetadata$FirstImportedYear,
        object@AnalysisMetadata$LastImportedYear,
        object@AnalysisMetadata$Duration,
        object@AnalysisMetadata$LastAnalysedYear,
        object@AnalysisMetadata$AnalysisDate, object@AnalysisMetadata$Seed,
        object@AnalysisRelation$ParentAnalysis,
        formals(object@Extractor),
        as.character(body(object@Extractor))
      )
    )
    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }
    status.fingerprint <- get_sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@Parameter, object@Index, object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation
      )
    )
    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
