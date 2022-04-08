context("n2kInla validation")

dataset <- test_data()
lin_comb <- model.matrix(~A, unique(dataset[, "A", drop = FALSE]))
object <- n2k_inla(
  result_datasource_id = sha1(letters),
  scheme_id = sha1(letters),
  species_group_id = sha1(letters),
  location_group_id = sha1(letters),
  model_type = "inla poisson: A",
  formula = "Count ~ A",
  first_imported_year = 1990,
  last_imported_year = 2015,
  duration = 1,
  last_analysed_year = 1995,
  analysis_date = as.POSIXct("2000-01-01 12:13:14", tz = "UTC"),
  data = dataset
)

describe("illegal changes in the file fingerprint", {
  it("detects changes in Data", {
    change_object <- object
    change_object@Data <- head(dataset, 1)
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in scheme_id", {
    change_object <- object
    change_object@AnalysisMetadata$scheme_id <- sha1(Sys.time())
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in species_group_id", {
    change_object <- object
    change_object@AnalysisMetadata$species_group_id <- sha1(Sys.time())
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in location_group_id", {
    change_object <- object
    change_object@AnalysisMetadata$location_group_id <- sha1(Sys.time())
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in analysis_date", {
    change_object <- object
    change_object@AnalysisMetadata$analysis_date <- Sys.time()
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in model_type", {
    change_object <- object
    change_object@AnalysisMetadata$model_type <- "inla poisson: period"
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in formula", {
    change_object <- object
    change_object@AnalysisMetadata$formula <- "Count ~ B"
    expect_that(
      validObject(change_object),
      throws_error(
        "Formulas in 'AnalysisMetadata' don't match 'AnalysisFormula'"
      )
    )
  })

  it("detects changes in first_imported_year", {
    change_object <- object
    change_object@AnalysisMetadata$first_imported_year <- 999L
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in last_imported_year", {
    change_object <- object
    change_object@AnalysisMetadata$last_imported_year <- 2000L
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in duration", {
    change_object <- object
    change_object@AnalysisMetadata$duration <- 2L
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in last_analysed_year", {
    change_object <- object
    change_object@AnalysisMetadata$last_analysed_year <- 2000L
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })

  it("detects changes in LinearCombination", {
    change_object <- object
    change_object@LinearCombination <- lin_comb
    expect_that(
      validObject(change_object),
      throws_error("Corrupt file_fingerprint")
    )
  })
})

describe("illegal changes in the status fingerprint", {
  it("detects changes in status", {
    change_object <- object
    change_object@AnalysisMetadata$status <- "error"
    expect_that(
      validObject(change_object),
      throws_error("Corrupt status_fingerprint")
    )
  })

  it("detects changes in AnalysisVersion", {
    change_object <- object
    change_object@AnalysisMetadata$analysis_version <- ""
    expect_that(
      validObject(change_object),
      throws_error("Corrupt status_fingerprint")
    )
  })


  it("detects changes in Model", {
    model_object <- INLA::inla(
      Count ~ B,
      data = object@Data,
      family = "poisson"
    )
    object_model <- n2k_inla(
      data = object, model_fit = model_object, status = "converged"
    )
    change_model <- INLA::inla(
      Count ~ C,
      data = object@Data,
      family = "poisson"
    )
    object_model@Model <- change_model
    expect_that(
      validObject(object_model),
      throws_error("Corrupt status_fingerprint")
    )
  })
})
