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
#' @importFrom n2khelper check_dataframe_variable
setValidity(
  "n2kAnalysisMetadata",
  function(object){
    required.class <- list(
      SchemeID = "integer",
      SpeciesGroupID = "integer",
      LocationGroupID = "integer",
      ModelType = c("character", "factor"),
      Formula = c("character", "factor"),
      FirstImportedYear = "integer",
      LastImportedYear = "integer",
      Duration = "integer",
      LastAnalysedYear = "integer",
      AnalysisDate = "POSIXct",
      Seed = "integer",
      FileFingerprint = c("character", "factor"),
      Status = c("character", "factor"),
      AnalysisVersion = c("character", "factor"),
      StatusFingerprint = c("character", "factor")
    )
    check_dataframe_variable(
      df = object@AnalysisMetadata,
      variable = required.class,
      name = "AnalysisMetadata"
    )

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

    required.class <- list(
      Analysis = c("character", "factor"),
      ParentAnalysis = c("character", "factor"),
      ParentStatusFingerprint = c("character", "factor"),
      ParentStatus = c("character", "factor")
    )
    check_dataframe_variable(
      df = object@AnalysisRelation,
      variable = required.class,
      name = "AnalysisRelation"
    )

    if (any(object@AnalysisMetadata$LocationGroupID <= 0)) {
      stop("LocationGroupID must be strictly positive")
    }
    if (any(object@AnalysisMetadata$SpeciesGroupID <= 0)) {
      stop("SpeciesGroupID must be strictly positive")
    }
    if (any(object@AnalysisMetadata$FirstImportedYear <= 0)) {
      stop("FirstImportedYear must be strictly positive")
    }
    this.year <- as.integer(format(Sys.time(), "%Y"))
    if (any(object@AnalysisMetadata$FirstImportedYear > this.year)) {
      stop("FirstImportedYear from the future.")
    }
    if (any(object@AnalysisMetadata$LastImportedYear <= 0)) {
      stop("LastImportedYear must be strictly positive")
    }
    if (any(object@AnalysisMetadata$LastImportedYear > this.year)) {
      stop("LastImportedYear from the future.")
    }
    if (any(
      object@AnalysisMetadata$FirstImportedYear >
        object@AnalysisMetadata$LastImportedYear
    )) {
      stop("FirstImportedYear cannot exceed LastImportedYear")
    }
    if (any(object@AnalysisMetadata$LastAnalysedYear <= 0)) {
      stop("LastAnalysedYear must be strictly positive")
    }
    if (any(
      object@AnalysisMetadata$LastAnalysedYear >
        object@AnalysisMetadata$LastImportedYear
    )) {
      stop("LastAnalysedYear cannot exceed LastImportedYear")
    }
    if (any(object@AnalysisMetadata$Duration <= 0)) {
      stop("Duration must be strictly positive")
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

    if (any(object@AnalysisMetadata$Seed <= 0)) {
      stop("Seed must be strictly positive")
    }

    ok.status <- c(
      "new", "working", "waiting", "error", "converged", "false convergence",
      "unstable", "insufficient data"
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
