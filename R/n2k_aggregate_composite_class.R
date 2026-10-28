#' The `n2kAggregateComposite` class
#'
#' Calculate composite indices from multiple analysis
#' @slot Fun The function that combines the parent imputations.
#' @name n2kAggregateComposite-class
#' @rdname n2kAggregateComposite-class
#' @exportClass n2kAggregateComposite
#' @aliases n2kAggregateComposite-class
#' @importFrom methods setClass
#' @docType class
#' @include n2k_model_class.R
setClass(
  "n2kAggregateComposite",
  representation = representation(
    Fun = "function",
    Input = "list",
    AggregatedImputed = "maybeAggregatedImputed"
  ),
  contains = "n2kModel"
)


#' @importFrom methods setValidity
#' @importFrom digest sha1
setValidity(
  "n2kAggregateComposite",
  function(object) {
    assert_that(
      nrow(object@AnalysisRelation) > 0,
      msg = "'AnalysisRelation' not defined"
    )
    assert_that(
      noNA(object@AnalysisRelation$parent_analysis),
      msg = "'parent_analysis' in 'AnalysisRelation' slot cannot be missing"
    )
    assert_that(
      length(object@Input) == 0 || !is.null(names(object@Input)),
      msg = "`Input` must be a named list"
    )
    assert_that(
      anyDuplicated(names(object@Input)) == 0,
      msg = "All names of `Input` must be unique"
    )
    assert_that(
      length(object@Input) == 0 ||
        all(names(object@Input) %in% object@AnalysisRelation$parent_analysis),
      msg = paste(
        "All names of `Input` must be present in",
        "'AnalysisRelation$parent_analysis'"
      )
    )

    vapply(
      object@Input,
      function(x) {
        inherits(x, "aggregatedImputed")
      },
      logical(1)
    ) |>
      all() |>
      assert_that(
        msg = paste(
          "All elements of `Input` must be of class 'aggregatedImputed'"
        )
      )

    file_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$result_datasource_id,
        object@AnalysisMetadata$scheme_id,
        object@AnalysisMetadata$species_group_id,
        object@AnalysisMetadata$location_group_id,
        object@AnalysisMetadata$model_type,
        object@AnalysisMetadata$formula,
        object@AnalysisMetadata$first_imported_year,
        object@AnalysisMetadata$last_imported_year,
        object@AnalysisMetadata$duration,
        object@AnalysisMetadata$last_analysed_year,
        format(object@AnalysisMetadata$analysis_date, tz = "UTC"),
        object@AnalysisMetadata$seed,
        object@AnalysisRelation$parent_analysis,
        formals(object@Fun),
        as.character(body(object@Fun))
      )
    )
    assert_that(
      object@AnalysisMetadata$file_fingerprint == file_fingerprint,
      msg = "Corrupt file_fingerprint"
    )
    status_fingerprint <- sha1(
      list(
        object@AnalysisMetadata$file_fingerprint,
        object@AnalysisMetadata$status,
        object@AnalysisMetadata$analysis_version,
        object@AnalysisVersion,
        object@RPackage,
        object@AnalysisVersionRPackage,
        object@AnalysisRelation,
        object@Input,
        object@AggregatedImputed
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
