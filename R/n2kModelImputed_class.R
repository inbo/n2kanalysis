#' The n2kModelImputed class
#'
#' It holds the model of aggregated imputed data
#' @section Slots:
#'   \describe{
#'    \item{\code{Function}}{The object to pass to the \code{model.fun} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{Package}}{A vector of package names which must be loaded to run the function.}
#'    \item{\code{ModelArgs}}{The object to pass to the \code{model.args} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{PrepareModelArgs}}{An optional list containing a single function that will be applied to the object. The result of the function will be appended to the \code{ModelsArgs}}
#'    \item{\code{Extractor}}{The object to pass to the \code{extractor} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{ExtractorArgs}}{The object to pass to the \code{extractor.args} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{Filter}}{The object to pass to the \code{filter} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{Mutate}}{The object to pass to the \code{mutate} argument of \code{\link[multimput]{model_impute}}}
#'    \item{\code{AggregatedImputed}}{An \code{aggregatedImputed} object with multiple imputations.}
#'    \item{\code{Results}}{The dataframe with the results of \code{\link[multimput]{model_impute}}}
#'   }
#' @name n2kModelImputed-class
#' @rdname n2kModelImputed-class
#' @exportClass n2kModelImputed
#' @aliases n2kModelImputed-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModelImputed_class.R
#' @include n2kInlaComparison_class.R
setClass(
  "n2kModelImputed",
  representation = representation(
    Function = "function",
    Package = "character",
    ModelArgs = "list",
    PrepareModelArgs = "list",
    Extractor = "function",
    ExtractorArgs = "list",
    Filter = "list",
    Mutate = "list",
    AggregatedImputed = "maybeAggregatedImputed",
    Results = "maybeDataFrame"
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
        object@AnalysisMetadata$ResultDatasourceID,
        object@AnalysisMetadata$SchemeID,
        object@AnalysisMetadata$SpeciesGroupID,
        object@AnalysisMetadata$LocationGroupID,
        object@AnalysisMetadata$ModelType, object@AnalysisMetadata$Formula,
        object@AnalysisMetadata$FirstImportedYear,
        object@AnalysisMetadata$LastImportedYear,
        object@AnalysisMetadata$Duration,
        object@AnalysisMetadata$LastAnalysedYear,
        format(object@AnalysisMetadata$AnalysisDate, tz = "UTC"),
        object@AnalysisMetadata$Seed,
        object@AnalysisRelation$ParentAnalysis,
        object@Function, object@Filter, object@Mutate, object@ModelArgs,
        object@PrepareModelArgs, object@Extractor, object@ExtractorArgs,
        object@Package
      ),
      environment = FALSE
    )

    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status.fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@AnalysisMetadata$AnalysisVersion, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation, object@AggregatedImputed, object@Results
      ),
      digits = 6L
    )

    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
