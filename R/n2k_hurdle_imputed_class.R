#' The `n2kHurdleImputed` class
#'
#' It holds the model2 of hurdle imputed data
#' @name n2kHurdleImputed-class
#' @rdname n2kHurdleImputed-class
#' @exportClass n2kHurdleImputed
#' @aliases n2kHurdleImputed-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kHurdleImputed",
  representation = representation(
    Which = "character", Presence = "maybeRawImputed", Count = "maybeRawImputed"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom digest sha1
#' @importFrom assertthat assert_that is.string
setValidity(
  "n2kModelImputed",
  function(object) {
    assert_that(is.string(object@Which))
    assert_that(
      nrow(object@AnalysisRelation) == 2,
      msg = "`n2kHurdleImpute` required exactly two parent analyses"
    )
    assert_that(
      object@Which %in% object@AnalysisRelation$parent_analysis,
      msg = paste(
        c(
          "file fingerprint in `Which` must match with a fingerprint of a",
          "parent analysis"
        )
      )
    )

    file_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$result_datasource_id,
        object@AnalysisMetadata$scheme_id,
        object@AnalysisMetadata$species_group_id,
        object@AnalysisMetadata$location_group_id,
        object@AnalysisMetadata$model_type, object@AnalysisMetadata$formula,
        object@AnalysisMetadata$first_imported_year,
        object@AnalysisMetadata$last_imported_year,
        object@AnalysisMetadata$duration,
        object@AnalysisMetadata$last_analysed_year,
        format(object@AnalysisMetadata$analysis_date, tz = "UTC"),
        object@AnalysisMetadata$seed, object@AnalysisRelation$parent_analysis,
        object@Which
      ),
      environment = FALSE
    )

    assert_that(
      object@AnalysisMetadata$file_fingerprint == file_fingerprint,
      msg = "Corrupt file_fingerprint"
    )

    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$file_fingerprint,
        object@AnalysisMetadata$status,
        object@AnalysisMetadata$analysis_version, object@AnalysisVersion,
        object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation, object@Presence, object@Count
      ),
      digits = 6L
    )

    assert_that(
      object@AnalysisMetadata$status_fingerprint != status_fingerprint,
      msg = "Corrupt status_fingerprint"
    )

    return(TRUE)
  }
)
