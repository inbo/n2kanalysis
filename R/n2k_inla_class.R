#' @importFrom methods setClassUnion
#' @importClassesFrom multimput aggregatedImputed rawImputed
setClassUnion("maybeMatrix", c("matrix", "list", "NULL"))
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
#'    A named list with options passed to the arguments of [INLA::inla()].
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
#' @importClassesFrom multimput maybeInla
#' @docType class
#' @include n2k_model_class.R
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
setValidity(
  "n2kInla",
  function(object) {
    assert_that(
      requireNamespace("INLA", quietly = TRUE),
      msg = "INLA package required but not installed."
    )
    assert_that(object@ImputationSize >= 0, msg = "negative ImputationSize")
    c(
      all.vars(object@AnalysisFormula[[1]]),
      "observation_id", "datafield_id"
    ) %>%
      walk(~assert_that(has_name(object@Data, .x)))
    assert_that(
      noNA(object@Data$observation_id), msg = "observation_id cannot be NA"
    )
    assert_that(
      noNA(object@Data$datafield_id), msg = "datafield_id cannot be NA"
    )

    assert_that(
      all(table(object@Data$observation_id, object@Data$datafield_id) <= 1),
      msg = "Duplicated observation_id"
    )

    assert_that(
      all(object@Family %in% names(INLA::inla.models()$likelihood)),
      msg = paste(object@Family, "is not an INLA likelihood")
    )
    rg <- paste("inla", paste(object@Family, collapse = "-"))
    if (!grepl(paste0("^", rg), object@AnalysisMetadata$model_type)) {
      stop("model_type should be '", rg, "'")
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
    assert_that(is.list(object@Control), msg = "Control must be a list")
    assert_that(
      !has_name(object@Control, "formula"), !has_name(object@Control, "family"),
      !has_name(object@Control, "data"), !has_name(object@Control, "lincomb")
    )
    file_fingerprint <- sha1(
      list(
        object@Data,
        object@AnalysisMetadata$result_datasource_id,
        object@AnalysisMetadata$scheme_id,
        object@AnalysisMetadata$species_group_id,
        object@AnalysisMetadata$location_group_id,
        object@Family,
        object@AnalysisMetadata$model_type, object@AnalysisMetadata$formula,
        object@AnalysisMetadata$first_imported_year,
        object@AnalysisMetadata$last_imported_year,
        object@AnalysisMetadata$duration,
        object@AnalysisMetadata$last_analysed_year,
        format(object@AnalysisMetadata$analysis_date, tz = "UTC"),
        object@AnalysisMetadata$seed,
        object@AnalysisRelation$parent_analysis,
        object@ReplicateName, object@LinearCombination, object@ImputationSize,
        object@Minimum, object@Control
      )
    )
    assert_that(
      object@AnalysisMetadata$file_fingerprint == file_fingerprint,
      msg = "Corrupt file_fingerprint"
    )

    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$file_fingerprint,
        object@AnalysisMetadata$status, object@Model,
        object@AnalysisMetadata$analysis_version, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation, object@RawImputed
      ),
      digits = 6L
    )

    assert_that(
      object@AnalysisMetadata$status_fingerprint == status_fingerprint,
      msg = "Corrupt status_fingerprint"
    )

    return(TRUE)
  }
)
