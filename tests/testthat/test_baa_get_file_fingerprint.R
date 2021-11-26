context("get the correct file fingerprint")
describe("file fingerprint for n2k_glmer_poisson", {

  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DataFieldID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this_analysis_date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this_result_datasource_id <- sha1(sample(letters))
  this_scheme_id <- sha1(sample(letters))
  this_species_group_id <- sha1(sample(letters))
  this_location_group_id <- sha1(sample(letters))
  this_seed <- 4L
  this_model_type <- "glmer poisson: period + herd"
  this_formula <- "incidence ~ offset(log(size)) + period + (1|herd)"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2015L
  this_parent <- "abcdef"
  this_duration <- this_last_imported_year - this_first_imported_year + 1L

  it("sets the correct fingerprint for an new object", {
    object <- n2k_glmer_poisson(
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
      data = cbpp,
      parent = this_parent,
      this_duration
    )
    file_fingerprint <- sha1(
      list(
        cbpp, this_result_datasource_id, this_scheme_id, this_species_group_id,
        this_location_group_id,
        this_model_type, this_formula, this_first_imported_year,
        this_last_imported_year, this_duration, this_last_analysed_year,
        format(this_analysis_date, tz = "UTC"), this_seed, this_parent
      )
    )
    expect_identical(file_fingerprint, get_file_fingerprint(object))
  })
})

describe("get the correct fingerprint from an n2kManifest object", {
  it("gets the correct fingerprint", {
    x <- n2k_manifest(
      data.frame(
        Fingerprint = "1",
        Parent = NA_character_,
        stringsAsFactors = FALSE
      )
    )
    expect_identical(x@Fingerprint, get_file_fingerprint(x))
  })
})
