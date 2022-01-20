context("get the correct status fingerprint")
describe("status fingerprint for n2k_inla", {

  dataset <- test_data()
  this_analysis_date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this_result_datasource_id <- sha1(sample(letters))
  this_scheme_id <- sha1(sample(letters))
  this_species_group_id <- sha1(sample(letters))
  this_location_group_id <- sha1(sample(letters))
  this_seed <- 4L
  this_model_type <- "inla poisson: A"
  this_formula <- "Count ~ A"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2015L
  this_parent <- "abcdef"
  this_duration <- this_last_imported_year - this_first_imported_year + 1

  it("sets the correct fingerprint for an new object", {
    object <- n2k_inla(
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      last_analysed_year = this_last_analysed_year,
      analysis_date = this_analysis_date,
      seed = this_seed,
      data = dataset,
      parent = this_parent,
      this_duration
    )
    version <- get_analysis_version(sessionInfo())
    status_fingerprint <- sha1(
      list(
        get_file_fingerprint(object), status(object), NULL,
        version@AnalysisVersion$Fingerprint,
        version@AnalysisVersion, version@RPackage,
        version@AnalysisVersionRPackage, object@AnalysisRelation
      ),
      digits = 6L
    )
    expect_identical(status_fingerprint, get_status_fingerprint(object))
  })
})
