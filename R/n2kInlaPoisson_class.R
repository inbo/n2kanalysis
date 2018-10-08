#' @importFrom methods setClassUnion
#' @importClassesFrom multimput inla rawImputed
setClassUnion("maybeMatrix", c("matrix", "list", "NULL"))
setClassUnion("maybeInla", c("inla", "NULL"))
setClassUnion("maybeRawImputed", c("rawImputed", "aggregatedImputed", "NULL"))

#' The n2kInlaPoisson class
#'
#' It hold analysis data based on an INLA poisson model
#' @section Slots:
#'   \describe{
#'    \item{\code{Data}}{a data.frame with the data}
#'    \item{\code{LinearCombination}}{an optional matrix with the linear
#'        combinations}
#'    \item{\code{ReplicateName}}{an optional list with names of replicates}
#'    \item{\code{Model}}{Either NULL or the resulting INLA model.}
#'    \item{\code{ImputationSize}}{The number of multiple imputations. Defaults to 0, indication no multiple imputation.}
#'    \item{\code{Minimum}}{an optional string containing the name of the variable in \code{Data} holding the minimal values for imputation}
#'    \item{\code{RawImputed}}{A \code{rawImputed} object with multiple imputations.}
#'   }
#' @name n2kInlaPoisson-class
#' @rdname n2kInlaPoisson-class
#' @exportClass n2kInlaPoisson
#' @aliases n2kInlaPoisson-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kInlaPoisson",
  representation = representation(
    Data = "data.frame",
    LinearCombination = "maybeMatrix",
    ReplicateName = "list",
    Model = "maybeInla",
    ImputationSize = "integer",
    Minimum = "character",
    RawImputed = "maybeRawImputed"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom digest sha1
#' @importFrom assertthat assert_that has_name
setValidity(
  "n2kInlaPoisson",
  function(object){
    if (object@ImputationSize < 0) {
      stop("negative ImputationSize")
    }
    check_dataframe_variable(
      df = object@Data[1, ],
      variable = c(
        all.vars(object@AnalysisFormula[[1]]),
        "ObservationID"
      ),
      error = TRUE
    )
    if (any(is.na(object@Data$ObservationID))) {
      stop("ObservationID cannot be NA")
    }
    if (!grepl("^inla Poisson", object@AnalysisMetadata$ModelType)) {
      stop("ModelType should be 'inla Poisson'")
    }
    if (class(object@Model) == "inla") {
      if (object@Model$all.hyper$family[[1]]$label != "poisson") {
        stop("The model must be from the Poisson family")
      }
    }

    assert_that(length(object@Minimum) == 1)
    if (!is.na(object@Minimum) && object@Minimum != "") {
      assert_that(has_name(object@Data, object@Minimum))
    }

    if (is.matrix(object@LinearCombination)) {
      if (is.null(rownames(object@LinearCombination))) {
        stop("A matrix of linear combination must have rownames")
      }
    }
    if (is.list(object@LinearCombination)) {
      if (is.matrix(object@LinearCombination[[1]])) {
        if (is.null(rownames(object@LinearCombination[[1]]))) {
          stop("The first element of linear combination must have rownames")
        }
      } else {
        if (is.null(names(object@LinearCombination[[1]]))) {
          stop("The first element of linear combination must have names")
        }
      }
    }
    file.fingerprint <- sha1(
      list(
        object@Data,
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
        object@ReplicateName, object@LinearCombination, object@ImputationSize,
        object@Minimum
      )
    )
    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status.fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@Model, object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation, object@RawImputed
      ),
      digits = 6L
    )

    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
