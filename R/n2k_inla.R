#' Create a n2kInla object
#' @param data a data.frame with the data to analyse
#' @param model_fit The fitted model
#' @param ... other arguments. See below
#' @name n2k_inla
#' @rdname n2k_inla
#' @exportMethod n2k_inla
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_inla",
  def = function(
    data, ..., model_fit
  ) {
    standardGeneric("n2k_inla") # nocov
  }
)

#' @description A new n2kInla model is created when \code{data} is a data.frame.
#' @rdname n2k_inla
#' @aliases n2k_inla,n2kInla-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_inla_class.R
#' @inheritParams n2k_inla_comparison
#' @param family the family to use in the INLA model.
#' @param lin_comb A model matrix to calculate linear combinations.
#' @param replicate_name A list with the names of replicates.
#' Defaults to an empty list.
#' Used in case of `f(X, ..., replicate = Z)`.
#' Should be a named list like e.g. `list(X = c("a", "b", "c"))`.
#' @param control A named list passed to \code{\link[INLA]{inla}} when fitting
#' the model.
#' @param imputation_size The required number of imputations defaults to 0.
#' @param minimum The name of the variable which holds the minimum counts.
#' Only relevant in case of multiple imputation.
#' @param parent The file fingerprint of the optional parent analysis.
#' @param parent_status The status of the parent analysis.
#' @param parent_statusfingerprint The statusfingerprint of the parent analysis.
setMethod(
  f = "n2k_inla",
  signature = signature(data = "data.frame"),
  definition = function(
    data, status = "new", result_datasource_id, scheme_id, family = "poisson",
    formula, species_group_id, location_group_id, model_type,
    first_imported_year, last_imported_year, duration, last_analysed_year,
    analysis_date, lin_comb = NULL, minimum = "", imputation_size,
    parent = character(0), seed, replicate_name = list(), control = list(),
    parent_status = "converged", parent_statusfingerprint, ..., model_fit
  ) {
    assert_that(is.string(status))
    assert_that(is.string(minimum))
    if (missing(seed)) {
      seed <- sample(.Machine$integer.max, 1)
    } else {
      assert_that(is.count(seed))
      seed <- as.integer(seed)
    }
    if (missing(imputation_size)) {
      imputation_size <- 0L
    } else {
      assert_that(is.count(imputation_size))
      imputation_size <- as.integer(imputation_size)
    }
    assert_that(is.string(result_datasource_id))
    assert_that(is.string(scheme_id))
    assert_that(is.string(species_group_id))
    assert_that(is.string(location_group_id))
    assert_that(is.string(model_type))
    assert_that(is.string(formula))
    assert_that(is.count(first_imported_year))
    first_imported_year <- as.integer(first_imported_year)
    assert_that(is.count(last_imported_year))
    last_imported_year <- as.integer(last_imported_year)
    if (missing(duration)) {
      duration <- last_imported_year - first_imported_year + 1L
    } else {
      assert_that(is.count(duration))
      duration <- as.integer(duration)
    }
    if (missing(last_analysed_year)) {
      last_analysed_year <- last_imported_year
    } else {
      assert_that(is.count(last_analysed_year))
      last_analysed_year <- as.integer(last_analysed_year)
    }
    assert_that(is.time(analysis_date))
    if (!is.null(lin_comb)) {
      ok <- inherits(lin_comb, "list") ||
        (inherits(lin_comb, "matrix") && length(dim(lin_comb) == 2))
      if (!ok) {
        stop("lin_comb must be either a list or a matrix")
      }
    }
    assert_that(is.list(replicate_name))
    if (length(replicate_name) > 0) {
      if (is.null(names(replicate_name))) {
        stop("replicate_name must have names")
      }
    }
    assert_that(
      is.character(family),
      length(family) >= 1
    )
    assert_that(is.list(control))
    control$control.compute$dic <- ifelse(
      is.null(control$control.compute$dic),
      TRUE,
      control$control.compute$dic
    )
    control$control.compute$waic <- ifelse(
      is.null(control$control.compute$waic),
      TRUE,
      control$control.compute$waic
    )
    control$control.compute$cpo <- ifelse(
      is.null(control$control.compute$cpo),
      TRUE,
      control$control.compute$cpo
    )
    control$control.compute$config <- ifelse(
      is.null(control$control.compute$config),
      TRUE,
      control$control.compute$config
    )
    control$control.predictor$compute <- ifelse(
      is.null(control$control.predictor$compute),
      TRUE,
      control$control.predictor$compute
    )
    assert_that(
      has_name(data, as.character(as.formula(formula)[[2]])),
      msg = "Response variable is missing from data"
    )
    response <- data[, as.character(as.formula(formula)[[2]])]
    if (is.null(control$control.predictor$link)) {
      control$control.predictor$link <- ifelse(is.na(response), 1, NA)
    }
    control$control.fixed$prec.intercept <- ifelse( # nolint
      is.null(control$control.fixed$prec.intercept),
      1,
      control$control.fixed$prec.intercept
    )

    file_fingerprint <- sha1(
      list(
        data, result_datasource_id, scheme_id, species_group_id,
        location_group_id, family,
        model_type, formula, first_imported_year,
        last_imported_year, duration, last_analysed_year,
        format(analysis_date, tz = "UTC"),
        seed, parent, replicate_name,
        lin_comb, imputation_size, minimum, control
      )
    )

    if (length(parent) == 0) {
      analysis_relation <- data.frame(
        Analysis = character(0),
        ParentAnalysis = character(0),
        ParentStatusFingerprint = character(0),
        ParentStatus = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      assert_that(is.string(parent))
      assert_that(is.string(parent_status))
      if (missing(parent_statusfingerprint)) {
        parent_statusfingerprint <- sha1(parent_status)
      } else {
        assert_that(is.string(parent_statusfingerprint))
      }
      analysis_relation <- data.frame(
        Analysis = file_fingerprint,
        ParentAnalysis = parent,
        ParentStatusFingerprint = parent_statusfingerprint,
        ParentStatus = parent_status,
        stringsAsFactors = FALSE
      )
    }
    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, status, NULL,
        version@AnalysisVersion$Fingerprint, version@AnalysisVersion,
        version@RPackage,  version@AnalysisVersionRPackage, analysis_relation,
        NULL
      ),
      digits = 6L
    )

    new(
      "n2kInla",
      AnalysisVersion = version@AnalysisVersion,
      RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      AnalysisMetadata = data.frame(
        ResultDatasourceID = result_datasource_id,
        SchemeID = scheme_id,
        SpeciesGroupID = species_group_id,
        LocationGroupID = location_group_id,
        ModelType = model_type,
        Formula = formula,
        FirstImportedYear = first_imported_year,
        LastImportedYear = last_imported_year,
        Duration = duration,
        LastAnalysedYear = last_analysed_year,
        AnalysisDate = analysis_date,
        Seed = seed,
        Status = status,
        AnalysisVersion = version@AnalysisVersion$Fingerprint,
        FileFingerprint = file_fingerprint,
        StatusFingerprint = status_fingerprint,
        stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(formula)),
      AnalysisRelation = analysis_relation,
      Data = data,
      ReplicateName = replicate_name,
      LinearCombination = lin_comb,
      Model = NULL,
      Family = family,
      Control = control,
      ImputationSize = imputation_size,
      Minimum = minimum,
      RawImputed = NULL
    )
  }
)

#' @description In case `data` is an `n2kInla` object, then only the model and
#' status are updated.
#' All other slots are unaffected.
#' @rdname n2k_inla
#' @aliases n2k_inla,n2kInla-methods
#' @importFrom methods setMethod validObject new
#' @importFrom digest sha1
#' @importFrom utils sessionInfo
#' @include n2k_inla_class.R
#' @param raw_imputed the optional RawImputed object
setMethod(
  f = "n2k_inla",
  signature = signature(data = "n2kInla", model_fit = "inla"),
  definition = function(
    data, status, raw_imputed = NULL, ..., model_fit
  ) {
    assert_that(is.string(status))
    data@Model <- model_fit
    data@AnalysisMetadata$Status <- status
    version <- get_analysis_version(sessionInfo())
    new_version <- union(data, version)
    data@AnalysisVersion <- new_version$Union@AnalysisVersion
    data@RPackage <- new_version$Union@RPackage
    data@AnalysisVersionRPackage <- new_version$Union@AnalysisVersionRPackage
    data@AnalysisMetadata$AnalysisVersion <- new_version$UnionFingerprint
    data@RawImputed <- raw_imputed
    data@AnalysisMetadata$StatusFingerprint <- sha1(
      list(
        data@AnalysisMetadata$FileFingerprint, data@AnalysisMetadata$Status,
        data@Model, data@AnalysisMetadata$AnalysisVersion,
        data@AnalysisVersion, data@RPackage, data@AnalysisVersionRPackage,
        data@AnalysisRelation, data@RawImputed
      ),
      digits = 6L
    )

    validObject(data)
    return(data)
  }
)
