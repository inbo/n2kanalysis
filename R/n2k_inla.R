#' Create a n2kInla object
#' @inheritParams n2k_glmer_poisson
#' @name n2k_inla
#' @rdname n2k_inla
#' @exportMethod n2k_inla
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_inla",
  def = function(
    data, ..., model.fit
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
#' @include n2kInla_class.R
#' @inheritParams n2k_inla_comparison
#' @param family the family to use in the INLA model.
#' @param lin.comb A model matrix to calculate linear combinations.
#' @param replicate.name A list with the names of replicates.
#' Defaults to an empty list.
#' Used in case of `f(X, ..., replicate = Z)`.
#' Should be a named list like e.g. `list(X = c("a", "b", "c"))`.
#' @param control A named list passed to \code{\link[INLA]{inla}} when fitting
#' the model.
#' @param imputation.size The required number of imputations defaults to 0.
#' @param minimum The name of the variable which holds the minimum counts.
#' Only relevant in case of multiple imputation.
#' @param parent The file fingerprint of the optional parent analysis.
#' @param parent.status The status of the parent analysis.
#' @param parent.statusfingerprint The statusfingerprint of the parent analysis.
setMethod(
  f = "n2k_inla",
  signature = signature(data = "data.frame"),
  definition = function(
    data, status = "new", result.datasource.id, scheme.id, family = "poisson",
    formula, species.group.id, location.group.id, model.type,
    first.imported.year, last.imported.year, duration, last.analysed.year,
    analysis.date, lin.comb = NULL, minimum = "", imputation.size,
    parent = character(0), seed, replicate.name = list(), control = list(),
    parent.status = "converged", parent.statusfingerprint, ..., model.fit
  ) {
    assert_that(is.string(status))
    assert_that(is.string(minimum))
    if (missing(seed)) {
      seed <- sample(.Machine$integer.max, 1)
    } else {
      assert_that(is.count(seed))
      seed <- as.integer(seed)
    }
    if (missing(imputation.size)) {
      imputation.size <- 0L
    } else {
      assert_that(is.count(imputation.size))
      imputation.size <- as.integer(imputation.size)
    }
    assert_that(is.string(result.datasource.id))
    assert_that(is.string(scheme.id))
    assert_that(is.string(species.group.id))
    assert_that(is.string(location.group.id))
    assert_that(is.string(model.type))
    assert_that(is.string(formula))
    assert_that(is.count(first.imported.year))
    first.imported.year <- as.integer(first.imported.year)
    assert_that(is.count(last.imported.year))
    last.imported.year <- as.integer(last.imported.year)
    if (missing(duration)) {
      duration <- last.imported.year - first.imported.year + 1L
    } else {
      assert_that(is.count(duration))
      duration <- as.integer(duration)
    }
    if (missing(last.analysed.year)) {
      last.analysed.year <- last.imported.year
    } else {
      assert_that(is.count(last.analysed.year))
      last.analysed.year <- as.integer(last.analysed.year)
    }
    assert_that(is.time(analysis.date))
    if (!is.null(lin.comb)) {
      ok <- inherits(lin.comb, "list") ||
        (inherits(lin.comb, "matrix") && length(dim(lin.comb) == 2))
      if (!ok) {
        stop("lin.comb must be either a list or a matrix")
      }
    }
    assert_that(is.list(replicate.name))
    if (length(replicate.name) > 0) {
      if (is.null(names(replicate.name))) {
        stop("replicate.name must have names")
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
    control$control.fixed$prec.intercept <- ifelse(
      is.null(control$control.fixed$prec.intercept),
      1,
      control$control.fixed$prec.intercept
    )

    file.fingerprint <- sha1(
      list(
        data, result.datasource.id, scheme.id, species.group.id,
        location.group.id, family,
        model.type, formula, first.imported.year,
        last.imported.year, duration, last.analysed.year,
        format(analysis.date, tz = "UTC"),
        seed, parent, replicate.name,
        lin.comb, imputation.size, minimum, control
      )
    )

    if (length(parent) == 0) {
      analysis.relation <- data.frame(
        Analysis = character(0),
        ParentAnalysis = character(0),
        ParentStatusFingerprint = character(0),
        ParentStatus = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      assert_that(is.string(parent))
      assert_that(is.string(parent.status))
      if (missing(parent.statusfingerprint)) {
        parent.statusfingerprint <- sha1(parent.status)
      } else {
        assert_that(is.string(parent.statusfingerprint))
      }
      analysis.relation <- data.frame(
        Analysis = file.fingerprint,
        ParentAnalysis = parent,
        ParentStatusFingerprint = parent.statusfingerprint,
        ParentStatus = parent.status,
        stringsAsFactors = FALSE
      )
    }
    version <- get_analysis_version(sessionInfo())
    status.fingerprint <- sha1(
      list(
        file.fingerprint, status, NULL,
        version@AnalysisVersion$Fingerprint, version@AnalysisVersion,
        version@RPackage,  version@AnalysisVersionRPackage, analysis.relation,
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
        ResultDatasourceID = result.datasource.id,
        SchemeID = scheme.id,
        SpeciesGroupID = species.group.id,
        LocationGroupID = location.group.id,
        ModelType = model.type,
        Formula = formula,
        FirstImportedYear = first.imported.year,
        LastImportedYear = last.imported.year,
        Duration = duration,
        LastAnalysedYear = last.analysed.year,
        AnalysisDate = analysis.date,
        Seed = seed,
        Status = status,
        AnalysisVersion = version@AnalysisVersion$Fingerprint,
        FileFingerprint = file.fingerprint,
        StatusFingerprint = status.fingerprint,
        stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(formula)),
      AnalysisRelation = analysis.relation,
      Data = data,
      ReplicateName = replicate.name,
      LinearCombination = lin.comb,
      Model = NULL,
      Family = family,
      Control = control,
      ImputationSize = imputation.size,
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
#' @include n2kInla_class.R
#' @param raw.imputed the optional RawImputed object
setMethod(
  f = "n2k_inla",
  signature = signature(data = "n2kInla", model.fit = "inla"),
  definition = function(
    data, status, raw.imputed = NULL, ..., model.fit
  ) {
    assert_that(is.string(status))
    data@Model <- model.fit
    data@AnalysisMetadata$Status <- status
    version <- get_analysis_version(sessionInfo())
    new.version <- union(data, version)
    data@AnalysisVersion <- new.version$Union@AnalysisVersion
    data@RPackage <- new.version$Union@RPackage
    data@AnalysisVersionRPackage <- new.version$Union@AnalysisVersionRPackage
    data@AnalysisMetadata$AnalysisVersion <- new.version$UnionFingerprint
    data@RawImputed <- raw.imputed
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
