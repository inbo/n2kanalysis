#' The n2kModelImputed class
#'
#' It holds the model of aggregated imputed data
#' @section Slots:
#'   \describe{
#'    \item{\code{Function}}{The object to pass to the \code{model.fun} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{ModelArgs}}{The object to pass to the \code{model.args} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{Extractor}}{The object to pass to the \code{extractor} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{ExtractorArgs}}{The object to pass to the \code{extractor.args} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{Filter}}{The object to pass to the \code{filter} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{Mutate}}{The object to pass to the \code{mutate} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{AggregatedImputed}}{An \code{aggregatedImputed} object with multiple imputations.}
#'   }
#' @name n2kModelImputed-class
#' @rdname n2kModelImputed-class
#' @exportClass n2kModelImputed
#' @aliases n2kModelImputed-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModelImputed_class.R
setClass(
  "n2kModelImputed",
  representation = representation(
    Function = "function",
    ModelArgs = "list",
    Extractor = "function",
    ExtractorArgs = "list",
    Filter = "list",
    Mutate = "list",
    AggregatedImputed = "maybeAggregatedImputed"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom digest sha1
#' @importFrom assertthat assert_that has_name
setValidity(
  "n2kModelImputed",
  function(object){
    file.fingerprint <- sha1(
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
        object@Function, object@Filter, object@Mutate, object@ModelArgs,
        object@Extractor, object@ExtractorArgs
      )
    )

    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status.fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@AnalysisMetadata$AnalysisVersion, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation, object@AggregatedImputed
      ),
      digits = 6L
    )

    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
