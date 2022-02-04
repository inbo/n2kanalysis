#' The n2kAnalysisMetadata class
#' @name n2kAnalysisMetadata-class
#' @rdname n2kAnalysisMetadata-class
#' @exportClass n2kAnalysisMetadata
#' @aliases n2kAnalysisMetadata-class
#' @importFrom methods setClass
#' @include n2k_analysis_version_class.R
#' @docType class
setClass(
  "n2kAnalysisMetadata",
  representation = representation(
    AnalysisMetadata = "data.frame",
    AnalysisFormula = "list",
    AnalysisRelation = "data.frame"
  ),
  contains = "n2kAnalysisVersion",
  prototype = prototype(
    AnalysisRelation = data.frame(
      analysis = character(0), parent_analysis = character(0),
      parentstatus_fingerprint = character(0), parent_status = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
#' @importFrom n2khelper is_chartor
#' @importFrom stats as.formula
setValidity(
  "n2kAnalysisMetadata",
  function(object) {
    assert_that(has_name(object@AnalysisMetadata, "result_datasource_id"))
    assert_that(has_name(object@AnalysisMetadata, "scheme_id"))
    assert_that(has_name(object@AnalysisMetadata, "species_group_id"))
    assert_that(has_name(object@AnalysisMetadata, "location_group_id"))
    assert_that(has_name(object@AnalysisMetadata, "model_type"))
    assert_that(has_name(object@AnalysisMetadata, "formula"))
    assert_that(has_name(object@AnalysisMetadata, "scheme_id"))
    assert_that(has_name(object@AnalysisMetadata, "first_imported_year"))
    assert_that(has_name(object@AnalysisMetadata, "last_imported_year"))
    assert_that(has_name(object@AnalysisMetadata, "duration"))
    assert_that(has_name(object@AnalysisMetadata, "last_analysed_year"))
    assert_that(has_name(object@AnalysisMetadata, "analysis_date"))
    assert_that(has_name(object@AnalysisMetadata, "seed"))
    assert_that(has_name(object@AnalysisMetadata, "file_fingerprint"))
    assert_that(has_name(object@AnalysisMetadata, "status"))
    assert_that(has_name(object@AnalysisMetadata, "analysis_version"))
    assert_that(has_name(object@AnalysisMetadata, "status_fingerprint"))

    assert_that(is_chartor(object@AnalysisMetadata$result_datasource_id))
    assert_that(is_chartor(object@AnalysisMetadata$scheme_id))
    assert_that(is_chartor(object@AnalysisMetadata$species_group_id))
    assert_that(is_chartor(object@AnalysisMetadata$location_group_id))
    assert_that(is_chartor(object@AnalysisMetadata$model_type))
    assert_that(is_chartor(object@AnalysisMetadata$formula))
    assert_that(is_chartor(object@AnalysisMetadata$file_fingerprint))
    assert_that(is_chartor(object@AnalysisMetadata$status))
    assert_that(is_chartor(object@AnalysisMetadata$analysis_version))
    assert_that(is_chartor(object@AnalysisMetadata$status_fingerprint))
    assert_that(is.integer(object@AnalysisMetadata$first_imported_year))
    assert_that(is.integer(object@AnalysisMetadata$last_imported_year))
    assert_that(is.integer(object@AnalysisMetadata$duration))
    assert_that(is.integer(object@AnalysisMetadata$last_analysed_year))
    assert_that(is.integer(object@AnalysisMetadata$seed))
    assert_that(inherits(object@AnalysisMetadata$analysis_date, "POSIXct"))

    assert_that(all(object@AnalysisMetadata$first_imported_year > 0))
    assert_that(all(object@AnalysisMetadata$last_imported_year > 0))
    assert_that(all(object@AnalysisMetadata$last_analysed_year > 0))
    assert_that(all(object@AnalysisMetadata$seed > 0))

    assert_that(
      length(object@AnalysisFormula) == nrow(object@AnalysisMetadata),
  msg = "Number of 'AnalysisFormula' not equal to number of 'AnalysisMetadata'"
    )
    if (inherits(object@AnalysisMetadata$formula, "character")) {
      assert_that(isTRUE(all.equal(
        lapply(object@AnalysisMetadata$formula, as.formula),
        object@AnalysisFormula
        )),
        msg = "Formulas in 'AnalysisMetadata' don't match 'AnalysisFormula'"
      )
    } else {
      formula_list <- lapply(
        levels(object@AnalysisMetadata$formula),
        as.formula
      )
      assert_that(isTRUE(all.equal(
        formula_list[object@AnalysisMetadata$formula],
        object@AnalysisFormula
      )),
        msg = "Formulas in 'AnalysisMetadata' don't match 'AnalysisFormula'"
      )
    }

    assert_that(has_name(object@AnalysisRelation, "analysis"))
    assert_that(has_name(object@AnalysisRelation, "parent_analysis"))
    assert_that(has_name(object@AnalysisRelation, "parentstatus_fingerprint"))
    assert_that(has_name(object@AnalysisRelation, "parent_status"))

    assert_that(is_chartor(object@AnalysisRelation$analysis))
    assert_that(is_chartor(object@AnalysisRelation$parent_analysis))
    assert_that(is_chartor(object@AnalysisRelation$parentstatus_fingerprint))
    assert_that(is_chartor(object@AnalysisRelation$parent_status))

    this_year <- as.integer(format(Sys.time(), "%Y"))
    if (any(object@AnalysisMetadata$last_imported_year > this_year)) {
      stop("last_imported_year from the future.")
    }
    if (any(
      object@AnalysisMetadata$first_imported_year >
        object@AnalysisMetadata$last_imported_year
    )) {
      stop("first_imported_year cannot exceed last_imported_year")
    }
    if (any(
      object@AnalysisMetadata$last_analysed_year >
        object@AnalysisMetadata$last_imported_year
    )) {
      stop("last_analysed_year cannot exceed last_imported_year")
    }
    if (any(
      object@AnalysisMetadata$duration >
        object@AnalysisMetadata$last_imported_year -
        object@AnalysisMetadata$first_imported_year + 1
    )) {
      stop(
        paste(
          "duration longer than the interval from first_imported_year to",
          "last_imported_year"
        )
      )
    }

    if (any(
      object@AnalysisMetadata$last_analysed_year <
        object@AnalysisMetadata$first_imported_year +
        object@AnalysisMetadata$duration - 1
    )) {
      stop(
"last_analysed_year smaller than first_imported_year + duration - 1. Window
outside imported range."
      )
    }

    ok_status <- c(
      "new", "working", "waiting", "error", "converged", "false_convergence",
      "unstable", "insufficient_data", "time-out"
    )
    if (!all(object@AnalysisMetadata$status %in% ok_status)) {
      stop(
        "status must be one of the following: ",
        paste0("'", ok_status, "'", collapse = ", ")
      )
    }
    if (!all(object@AnalysisRelation$parent_status %in% ok_status)) {
      stop(
        "status must be one of the following: ",
        paste0("'", ok_status, "'", collapse = ", ")
      )
    }
    if (any(object@AnalysisMetadata$analysis_date > Sys.time())) {
      stop("analysis_date must be in the past")
    }

    if (!all(
      object@AnalysisRelation$analysis %in%
        object@AnalysisMetadata$file_fingerprint
    )) {
      stop(
"Some Analysis in 'AnalysisRelation' slot have no matching file_fingerprint in
'Analysis' slot"
      )
    }

    return(TRUE)
  }
)
