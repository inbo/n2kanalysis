#' The n2kAnalysisMetadata class
#' @name n2kAnalysisMetadata-class
#' @rdname n2kAnalysisMetadata-class
#' @exportClass n2kAnalysisMetadata
#' @aliases n2kAnalysisMetadata-class
#' @importFrom methods setClass
#' @include n2kAnalysisVersion_class.R
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
      Analysis = character(0),
      ParentAnalysis = character(0),
      ParentStatusFingerprint = character(0),
      ParentStatus = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that has_name
#' @importFrom n2khelper is.chartor
#' @importFrom stats as.formula
setValidity(
  "n2kAnalysisMetadata",
  function(object){
    assert_that(has_name(object@AnalysisMetadata, "SchemeID"))
    assert_that(has_name(object@AnalysisMetadata, "SpeciesGroupID"))
    assert_that(has_name(object@AnalysisMetadata, "LocationGroupID"))
    assert_that(has_name(object@AnalysisMetadata, "ModelType"))
    assert_that(has_name(object@AnalysisMetadata, "Formula"))
    assert_that(has_name(object@AnalysisMetadata, "SchemeID"))
    assert_that(has_name(object@AnalysisMetadata, "FirstImportedYear"))
    assert_that(has_name(object@AnalysisMetadata, "LastImportedYear"))
    assert_that(has_name(object@AnalysisMetadata, "Duration"))
    assert_that(has_name(object@AnalysisMetadata, "LastAnalysedYear"))
    assert_that(has_name(object@AnalysisMetadata, "AnalysisDate"))
    assert_that(has_name(object@AnalysisMetadata, "Seed"))
    assert_that(has_name(object@AnalysisMetadata, "FileFingerprint"))
    assert_that(has_name(object@AnalysisMetadata, "Status"))
    assert_that(has_name(object@AnalysisMetadata, "AnalysisVersion"))
    assert_that(has_name(object@AnalysisMetadata, "StatusFingerprint"))

    assert_that(is.chartor(object@AnalysisMetadata$SchemeID))
    assert_that(is.chartor(object@AnalysisMetadata$SpeciesGroupID))
    assert_that(is.chartor(object@AnalysisMetadata$LocationGroupID))
    assert_that(is.chartor(object@AnalysisMetadata$ModelType))
    assert_that(is.chartor(object@AnalysisMetadata$Formula))
    assert_that(is.chartor(object@AnalysisMetadata$FileFingerprint))
    assert_that(is.chartor(object@AnalysisMetadata$Status))
    assert_that(is.chartor(object@AnalysisMetadata$AnalysisVersion))
    assert_that(is.chartor(object@AnalysisMetadata$StatusFingerprint))
    assert_that(is.integer(object@AnalysisMetadata$FirstImportedYear))
    assert_that(is.integer(object@AnalysisMetadata$LastImportedYear))
    assert_that(is.integer(object@AnalysisMetadata$Duration))
    assert_that(is.integer(object@AnalysisMetadata$LastAnalysedYear))
    assert_that(is.integer(object@AnalysisMetadata$Seed))
    assert_that(inherits(object@AnalysisMetadata$AnalysisDate, "POSIXct"))

    assert_that(all(object@AnalysisMetadata$FirstImportedYear > 0))
    assert_that(all(object@AnalysisMetadata$LastImportedYear > 0))
    assert_that(all(object@AnalysisMetadata$LastAnalysedYear > 0))
    assert_that(all(object@AnalysisMetadata$Seed > 0))

    if (length(object@AnalysisFormula) != nrow(object@AnalysisMetadata)) {
      stop(
        "Number of 'AnalysisFormula' not equal to number of 'AnalysisMetadata'"
      )
    }
    if (class(object@AnalysisMetadata$Formula) == "character") {
      if (!isTRUE(all.equal(
        lapply(object@AnalysisMetadata$Formula, as.formula),
        object@AnalysisFormula
      ))) {
        stop("Formulas in 'AnalysisMetadata' don't match 'AnalysisFormula'")
      }
    } else {
      formula.list <- lapply(
        levels(object@AnalysisMetadata$Formula),
        as.formula
      )
      if (!isTRUE(all.equal(
        formula.list[object@AnalysisMetadata$Formula],
        object@AnalysisFormula
      ))) {
        stop("Formulas in 'AnalysisMetadata' don't match 'AnalysisFormula'")
      }
    }


    assert_that(has_name(object@AnalysisRelation, "Analysis"))
    assert_that(has_name(object@AnalysisRelation, "ParentAnalysis"))
    assert_that(has_name(object@AnalysisRelation, "ParentStatusFingerprint"))
    assert_that(has_name(object@AnalysisRelation, "ParentStatus"))

    assert_that(is.chartor(object@AnalysisRelation$Analysis))
    assert_that(is.chartor(object@AnalysisRelation$ParentAnalysis))
    assert_that(is.chartor(object@AnalysisRelation$ParentStatusFingerprint))
    assert_that(is.chartor(object@AnalysisRelation$ParentStatus))

    this.year <- as.integer(format(Sys.time(), "%Y"))
    if (any(object@AnalysisMetadata$LastImportedYear > this.year)) {
      stop("LastImportedYear from the future.")
    }
    if (any(
      object@AnalysisMetadata$FirstImportedYear >
        object@AnalysisMetadata$LastImportedYear
    )) {
      stop("FirstImportedYear cannot exceed LastImportedYear")
    }
    if (any(
      object@AnalysisMetadata$LastAnalysedYear >
        object@AnalysisMetadata$LastImportedYear
    )) {
      stop("LastAnalysedYear cannot exceed LastImportedYear")
    }
    if (any(
      object@AnalysisMetadata$Duration >
        object@AnalysisMetadata$LastImportedYear -
        object@AnalysisMetadata$FirstImportedYear + 1
    )) {
      stop(
"Duration longer than the interval from FirstImportedYear to LastImportedYear"
      )
    }

    if (any(
      object@AnalysisMetadata$LastAnalysedYear <
        object@AnalysisMetadata$FirstImportedYear +
        object@AnalysisMetadata$Duration - 1
    )) {
      stop(
"LastAnalysedYear smaller than FirstImportedYear + Duration - 1. Window
outside imported range."
      )
    }

    ok.status <- c(
      "new", "working", "waiting", "error", "converged", "false_convergence",
      "unstable", "insufficient_data"
    )
    if (!all(object@AnalysisMetadata$Status %in% ok.status)) {
      stop(
        "Status must be one of the following: ",
        paste0("'", ok.status, "'", collapse = ", ")
      )
    }
    if (!all(object@AnalysisRelation$ParentStatus %in% ok.status)) {
      stop(
        "Status must be one of the following: ",
        paste0("'", ok.status, "'", collapse = ", ")
      )
    }
    if (any(object@AnalysisMetadata$AnalysisDate > Sys.time())) {
      stop("AnalysisDate must be in the past")
    }

    if (!all(
      object@AnalysisRelation$Analysis %in%
        object@AnalysisMetadata$FileFingerprint
    )) {
      stop(
"Some Analysis in 'AnalysisRelation' slot have no matching FileFingerprint in
'Analysis' slot"
      )
    }

    return(TRUE)
  }
)
