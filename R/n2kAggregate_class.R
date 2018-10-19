#' @importFrom methods setClassUnion
#' @importClassesFrom multimput aggregatedImputed
#' @include n2kInla_class.R
setClassUnion("maybeAggregatedImputed", c("aggregatedImputed", "NULL"))

#' The n2kAggregate class
#'
#' It holds analysis data based on an aggregated imputation
#' @section Slots:
#'   \describe{
#'    \item{\code{RawImputed}}{A \code{rawImputed} object with multiple imputations.}
#'    \item{\code{Function}}{The function to apply on each group.}
#'    \item{\code{Filter}}{The object to pass to the \code{filter} argument of \code{\link[multimput]{aggregate_impute}}}
#'    \item{\code{Join}}{The object to pass to the \code{join} argument of \code{\link[multimput]{aggregate_impute}}}
#'    \item{\code{AggregatedImputed}}{An \code{aggregatedImputed} object with multiple imputations.}
#'   }
#' @name n2kAggregate-class
#' @rdname n2kAggregate-class
#' @exportClass n2kAggregate
#' @aliases n2kAggregate-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kAggregate",
  representation = representation(
    RawImputed = "maybeRawImputed",
    Function = "function",
    Filter = "list",
    Join = "list",
    AggregatedImputed = "maybeAggregatedImputed"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom digest sha1
#' @importFrom assertthat assert_that has_name
setValidity(
  "n2kAggregate",
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
        object@Function, object@Filter, object@Join
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
        object@AnalysisRelation, object@RawImputed, object@AggregatedImputed
      ),
      digits = 6L
    )

    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
