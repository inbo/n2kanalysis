#' @importFrom methods setClassUnion
#' @importClassesFrom multimput inla rawImputed
setClassUnion("maybeMatrix", c("matrix", "list", "NULL"))
setClassUnion("maybeInla", c("inla", "NULL"))
setClassUnion("maybeRawImputed", c("rawImputed", "aggregatedImputed", "NULL"))

#' The n2kInla class
#'
#' It hold analysis data based on an INLA poisson model
#' @section Slots:
#'   \describe{
#'    \item{\code{Data}}{A \code{data.frame} with the data.}
#'    \item{\code{LinearCombination}}{An optional matrix with the linear
#'    combinations.}
#'    \item{\code{ReplicateName}}{An optional list with names of replicates.}
#'    \item{\code{Model}}{Either NULL or the resulting INLA model.}
#'    \item{\code{Family}}{The family of the INLA model}
#'    \item{\code{Control}}{
#'    A named list with options passed to the arguments of
#'    \code{\link[INLA]{inla}}.
#'    }
#'    \item{\code{ImputationSize}}{The number of multiple imputations.
#'    Defaults to \code{0}, indication no multiple imputation.}
#'    \item{\code{Minimum}}{An optional string containing the name of the
#'    variable in \code{Data} holding the minimal values for imputation.}
#'    \item{\code{RawImputed}}{A \code{rawImputed} object with multiple
#'    imputations.}
#'   }
#' @name n2kInla-class
#' @rdname n2kInla-class
#' @exportClass n2kInla
#' @aliases n2kInla-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kInla",
  representation = representation(
    Data = "data.frame",
    LinearCombination = "maybeMatrix",
    ReplicateName = "list",
    Model = "maybeInla",
    Family = "character",
    Control = "list",
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
#' @importFrom purrr walk
#' @importFrom INLA inla.models
setValidity(
  "n2kInla",
  function(object) {
    assert_that(object@ImputationSize >= 0, msg = "negative ImputationSize")
    c(
      all.vars(object@AnalysisFormula[[1]]),
      "ObservationID", "DataFieldID"
    ) %>%
      walk(~assert_that(has_name(object@Data, .x)))
    assert_that(
      noNA(object@Data$ObservationID),
      msg = "ObservationID cannot be NA"
    )
    assert_that(noNA(object@Data$DataFieldID), msg = "DataFieldID cannot be NA")

    assert_that(
      all(table(object@Data$ObservationID, object@Data$DataFieldID) <= 1),
      msg = "Duplicated ObservationID"
    )

    assert_that(
      all(object@Family %in% names(inla.models()$likelihood)),
      msg = paste(object@Family, "is not an INLA likelihood")
    )
    rg <- paste("inla", paste(object@Family, collapse = "-"))
    if (!grepl(paste0("^", rg), object@AnalysisMetadata$ModelType)) {
      stop("ModelType should be '", rg, "'")
    }
    assert_that(
      !inherits(object@Model, "inla") ||
        object@Model$.args$family == object@Family,
      msg = "Model of the wrong family"
    )

    assert_that(length(object@Minimum) == 1)
    if (!is.na(object@Minimum) && object@Minimum != "") {
      assert_that(has_name(object@Data, object@Minimum))
    }

    assert_that(
      !is.matrix(object@LinearCombination) ||
        !is.null(rownames(object@LinearCombination)),
      msg = "A matrix of linear combination must have rownames"
    )
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
    assert_that(
      is.list(object@Control),
      msg = "Control must be a list"
    )
    assert_that(
      !has_name(object@Control, "formula"),
      !has_name(object@Control, "family"),
      !has_name(object@Control, "data"),
      !has_name(object@Control, "lincomb")
    )
    file_fingerprint <- sha1(
      list(
        object@Data,
        object@AnalysisMetadata$ResultDatasourceID,
        object@AnalysisMetadata$SchemeID,
        object@AnalysisMetadata$SpeciesGroupID,
        object@AnalysisMetadata$LocationGroupID,
        object@Family,
        object@AnalysisMetadata$ModelType, object@AnalysisMetadata$Formula,
        object@AnalysisMetadata$FirstImportedYear,
        object@AnalysisMetadata$LastImportedYear,
        object@AnalysisMetadata$Duration,
        object@AnalysisMetadata$LastAnalysedYear,
        format(object@AnalysisMetadata$AnalysisDate, tz = "UTC"),
        object@AnalysisMetadata$Seed,
        object@AnalysisRelation$ParentAnalysis,
        object@ReplicateName, object@LinearCombination, object@ImputationSize,
        object@Minimum, object@Control
      )
    )
    assert_that(
      object@AnalysisMetadata$FileFingerprint == file_fingerprint,
      msg = "Corrupt FileFingerprint"
    )

    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@Model, object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation, object@RawImputed
      ),
      digits = 6L
    )

    assert_that(
      object@AnalysisMetadata$StatusFingerprint == status_fingerprint,
      msg = "Corrupt StatusFingerprint"
    )

    return(TRUE)
  }
)
