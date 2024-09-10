#' Create an `n2kSpde` object
#' @param data a `data.frame` with the data to analyse
#' @param model_fit The fitted model
#' @param ... other arguments. See below
#' @name n2k_spde
#' @rdname n2k_spde
#' @exportMethod n2k_spde
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_spde",
  def = function(
    data, ..., model_fit
  ) {
    standardGeneric("n2k_spde") # nocov
  }
)

#' @description A new `n2kSpde` model is created when `data` is a `data.frame`.
#' @rdname n2k_spde
#' @aliases n2k_spde,n2kSpde-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.string is.time
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_spde_class.R
#' @inheritParams n2k_inla_comparison
#' @inheritParams n2k_inla
#' @inheritParams multimput::impute
setMethod(
  f = "n2k_spde",
  signature = signature(data = "data.frame"),
  definition = function(
    data, status = "new", result_datasource_id, scheme_id, family = "poisson",
    formula, species_group_id, location_group_id, model_type, spde,
    first_imported_year, last_imported_year, duration, last_analysed_year,
    analysis_date, lin_comb = NULL, minimum = "", imputation_size,
    parent = character(0), seed, replicate_name = list(), control = list(),
    parent_status = "converged", parent_statusfingerprint, extra, ..., model_fit
  ) {
    assert_that(is.string(status))
    assert_that(is.string(minimum))
    if (missing(seed)) {
      seed <- sample(.Machine$integer.max, 1)
    }
    assert_that(is.count(seed))
    seed <- as.integer(seed)
    if (missing(imputation_size)) {
      imputation_size <- 0L
    } else {
      assert_that(is.count(imputation_size))
      imputation_size <- as.integer(imputation_size)
    }
    assert_that(
      is.string(result_datasource_id), is.string(scheme_id),
      is.string(species_group_id), is.string(location_group_id),
      is.string(model_type), is.string(formula), is.count(first_imported_year),
      is.count(last_imported_year)
    )
    first_imported_year <- as.integer(first_imported_year)
    last_imported_year <- as.integer(last_imported_year)
    if (missing(duration)) {
      duration <- last_imported_year - first_imported_year + 1L
    } else {
      assert_that(is.count(duration))
      duration <- as.integer(duration)
    }
    if (missing(last_analysed_year)) {
      last_analysed_year <- last_imported_year
    }
    assert_that(is.count(last_analysed_year))
    last_analysed_year <- as.integer(last_analysed_year)
    assert_that(is.time(analysis_date))
    assert_that(
      is.null(lin_comb) || inherits(lin_comb, "list") ||
      (inherits(lin_comb, "matrix") && length(dim(lin_comb) == 2)),
      msg = "lin_comb must be either a list or a matrix"
    )
    assert_that(is.list(replicate_name))
    assert_that(
      length(replicate_name) == 0 || !is.null(names(replicate_name)),
      msg = "replicate_name must have names"
    )
    assert_that(is.character(family), length(family) >= 1)
    assert_that(is.list(control))
    control$control.compute$dic <- ifelse(
      is.null(control$control.compute$dic), TRUE, control$control.compute$dic
    )
    control$control.compute$waic <- ifelse(
      is.null(control$control.compute$waic), TRUE, control$control.compute$waic
    )
    control$control.compute$cpo <- ifelse(
      is.null(control$control.compute$cpo), TRUE, control$control.compute$cpo
    )
    control$control.compute$config <- ifelse(
      is.null(control$control.compute$config), TRUE,
      control$control.compute$config
    )
    control$control.predictor$compute <- ifelse(
      is.null(control$control.predictor$compute), TRUE,
      control$control.predictor$compute
    )
    if (is.null(control$control.predictor$link)) {
      control$control.predictor$link <- 1
    }
    control$control.fixed$prec.intercept <- ifelse(
      is.null(control$control.fixed$prec.intercept),
      1, control$control.fixed$prec.intercept
    )
    if (missing(extra)) {
      extra <- data[0, ]
    }

    file_fingerprint <- sha1(
      list(
        data, result_datasource_id, scheme_id, species_group_id,
        location_group_id, family, model_type, formula, first_imported_year,
        last_imported_year, duration, last_analysed_year,
        format(analysis_date, tz = "UTC"), seed, parent, replicate_name,
        lin_comb, imputation_size, minimum, control, extra
      )
    )

    if (length(parent) == 0) {
      analysis_relation <- data.frame(
        analysis = character(0), parent_analysis = character(0),
        parentstatus_fingerprint = character(0), parent_status = character(0),
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
        analysis = file_fingerprint, parent_analysis = parent,
        parentstatus_fingerprint = parent_statusfingerprint,
        parent_status = parent_status, stringsAsFactors = FALSE
      )
    }
    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, status, NULL, version@AnalysisVersion$fingerprint,
        version@AnalysisVersion, version@RPackage,
        version@AnalysisVersionRPackage, analysis_relation, NULL
      ),
      digits = 6L
    )

    new(
      "n2kSpde",
      AnalysisVersion = version@AnalysisVersion, RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      AnalysisMetadata = data.frame(
        result_datasource_id = result_datasource_id, scheme_id = scheme_id,
        species_group_id = species_group_id,
        location_group_id = location_group_id, model_type = model_type,
        formula = formula, first_imported_year = first_imported_year,
        last_imported_year = last_imported_year, duration = duration,
        last_analysed_year = last_analysed_year, analysis_date = analysis_date,
        seed = seed, status = status,
        analysis_version = version@AnalysisVersion$fingerprint,
        file_fingerprint = file_fingerprint,
        status_fingerprint = status_fingerprint, stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(formula)), LinearCombination = lin_comb,
      AnalysisRelation = analysis_relation, Data = data, Model = NULL,
      ReplicateName = replicate_name, Family = family, Control = control,
      ImputationSize = imputation_size, Minimum = minimum, RawImputed = NULL,
      Extra = extra, Spde = spde
    )
  }
)

#' @description In case `data` is an `n2kSpde` object, then only the model and
#' status are updated.
#' All other slots are unaffected.
#' @rdname n2k_spde
#' @aliases n2k_spde,n2kSpde-methods
#' @importFrom methods setMethod validObject new
#' @importFrom digest sha1
#' @importFrom utils sessionInfo
#' @include n2k_spde_class.R
#' @param raw_imputed the optional `rawImputed` object
setMethod(
  f = "n2k_spde",
  signature = signature(data = "n2kSpde", model_fit = "inla"),
  definition = function(
    data, status, raw_imputed = NULL, ..., model_fit
  ) {
    assert_that(is.string(status))
    data@Model <- model_fit
    data@AnalysisMetadata$status <- status
    version <- get_analysis_version(sessionInfo())
    new_version <- union(data, version)
    data@AnalysisVersion <- new_version$Union@AnalysisVersion
    data@RPackage <- new_version$Union@RPackage
    data@AnalysisVersionRPackage <- new_version$Union@AnalysisVersionRPackage
    data@AnalysisMetadata$analysis_version <- new_version$Unionfingerprint
    data@RawImputed <- raw_imputed
    data@AnalysisMetadata$status_fingerprint <- sha1(
      list(
        data@AnalysisMetadata$file_fingerprint, data@AnalysisMetadata$status,
        data@Model, data@AnalysisMetadata$analysis_version,
        data@AnalysisVersion, data@RPackage, data@AnalysisVersionRPackage,
        data@AnalysisRelation, data@RawImputed
      ),
      digits = 6L
    )

    validObject(data)
    return(data)
  }
)
