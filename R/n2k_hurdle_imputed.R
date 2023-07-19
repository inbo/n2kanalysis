#' Create an `n2kHurdleImputed` object
#' @param presence the `n2kInla` object for the presence model.
#' @param count the `n2kInla` object for the count model.
#' @param verbose display the location group ID and species group ID.
#' Defaults to `FALSE`.
#' @name n2k_hurdle_imputed
#' @rdname n2k_hurdle_imputed
#' @exportMethod n2k_hurdle_imputed
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_hurdle_imputed",
  def = function(
    presence, count, verbose = FALSE
  ) {
    standardGeneric("n2k_hurdle_imputed") # nocov
  }
)

#' @description A new `n2kHurdleImputed` model.
#' @rdname n2k_hurdle_imputed
#' @aliases n2k_hurdle_imputed,n2kHurdleImputed-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.flag is.string is.time noNA
#' @importFrom digest sha1
#' @importFrom stats as.formula
#' @importFrom utils sessionInfo
#' @include n2k_model_imputed_class.R
setMethod(
  f = "n2k_hurdle_imputed",
  signature = signature(presence = "n2kInla"),
  definition = function(
    presence, count, verbose = FALSE
  ) {
    assert_that(inherits(count, "n2kInla"), is.flag(verbose), noNA(verbose))
    sprintf(
      "%s:%s ", count@AnalysisMetadata$location_group_id,
      count@AnalysisMetadata$species_group_id
    ) |>
      display(verbose = verbose, linefeed = FALSE)
    validObject(presence)
    validObject(count)
    status <- ifelse(
      all(c(status(presence), status(count)) == "converged"), "new", "waiting"
    )
    assert_that(
      get_seed(presence) == get_seed(count),
      presence@AnalysisMetadata$result_datasource_id ==
        count@AnalysisMetadata$result_datasource_id,
      get_scheme_id(presence) == get_scheme_id(count),
      presence@AnalysisMetadata$species_group_id ==
        count@AnalysisMetadata$species_group_id,
      presence@AnalysisMetadata$location_group_id ==
        count@AnalysisMetadata$location_group_id,
      grepl("^inla binomial: ", get_model_type(presence)),
      grepl("^inla zeroinflated(nbinomial|poisson)0: ", get_model_type(count)),
      gsub("^inla binomial: ", "", get_model_type(presence)) ==
        gsub(
          "^inla zeroinflated(nbinomial|poisson)0: ", "", get_model_type(count)
        ),
      presence@AnalysisMetadata$first_imported_year ==
        count@AnalysisMetadata$first_imported_year,
      presence@AnalysisMetadata$last_imported_year ==
        count@AnalysisMetadata$last_imported_year,
      presence@AnalysisMetadata$duration == count@AnalysisMetadata$duration,
      presence@AnalysisMetadata$last_analysed_year ==
        count@AnalysisMetadata$last_analysed_year,
      presence@AnalysisMetadata$analysis_date ==
        count@AnalysisMetadata$analysis_date
    )
    model_type <- sprintf(
      "inla binomial + zeroinflated%s0: %s",
      gsub(
        "^inla zeroinflated(nbinomial|poisson)0: .*", "\\1",
        get_model_type(count)
      ),
      gsub("^inla binomial: ", "", get_model_type(presence))
    )
    formula <- "cbind(presence, count) ~ 1"

    file_fingerprint <- sha1(
      list(
        presence@AnalysisMetadata$result_datasource_id,
        presence@AnalysisMetadata$scheme_id,
        presence@AnalysisMetadata$species_group_id,
        presence@AnalysisMetadata$location_group_id,
        model_type, formula,
        presence@AnalysisMetadata$first_imported_year,
        presence@AnalysisMetadata$last_imported_year,
        presence@AnalysisMetadata$duration,
        presence@AnalysisMetadata$last_analysed_year,
        format(presence@AnalysisMetadata$analysis_date, tz = "UTC"),
        presence@AnalysisMetadata$seed,
        c(get_file_fingerprint(presence), get_file_fingerprint(count))
      ),
      environment = FALSE
    )

    analysis_relation <- data.frame(
      analysis = file_fingerprint,
      parent_analysis = c(
        get_file_fingerprint(presence), get_file_fingerprint(count)
      ),
      parentstatus_fingerprint = c(
        get_status_fingerprint(presence), get_status_fingerprint(count)
      ),
      parent_status = c(status(presence), status(count)),
      stringsAsFactors = FALSE
    )
    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        file_fingerprint, status, version@AnalysisVersion$fingerprint,
        version@RPackage, version@AnalysisVersionRPackage, analysis_relation,
        presence@RawImputed, count@RawImputed, NULL
      ),
      digits = 6L
    )
    new(
      "n2kHurdleImputed", AnalysisVersion = version@AnalysisVersion,
      RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      AnalysisMetadata = data.frame(
        result_datasource_id = presence@AnalysisMetadata$result_datasource_id,
        scheme_id = get_scheme_id(presence),
        species_group_id = presence@AnalysisMetadata$species_group_id,
        location_group_id = presence@AnalysisMetadata$location_group_id,
        model_type = model_type, formula = formula,
        first_imported_year = presence@AnalysisMetadata$first_imported_year,
        last_imported_year = presence@AnalysisMetadata$last_imported_year,
        duration = presence@AnalysisMetadata$duration,
        last_analysed_year = presence@AnalysisMetadata$last_analysed_year,
        analysis_date = presence@AnalysisMetadata$analysis_date,
        seed = presence@AnalysisMetadata$seed, status = status,
        analysis_version = version@AnalysisVersion$fingerprint,
        file_fingerprint = file_fingerprint,
        status_fingerprint = status_fingerprint, stringsAsFactors = FALSE
      ),
      AnalysisFormula = list(as.formula(formula)),
      AnalysisRelation = analysis_relation, Presence = presence@RawImputed,
      Count = count@RawImputed, Hurdle = NULL
    )
  }
)
