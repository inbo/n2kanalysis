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
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in SchemeID", {
    change_object <- object
    change_object@AnalysisMetadata$SchemeID <- sha1(Sys.time())
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in SpeciesGroupID", {
    change_object <- object
    change_object@AnalysisMetadata$SpeciesGroupID <- sha1(Sys.time())
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LocationGroupID", {
    change_object <- object
    change_object@AnalysisMetadata$LocationGroupID <- sha1(Sys.time())
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in AnalysisDate", {
    change_object <- object
    change_object@AnalysisMetadata$AnalysisDate <- Sys.time()
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in ModelType", {
    change_object <- object
    change_object@AnalysisMetadata$ModelType <- "inla poisson: period"
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in Formula", {
    change_object <- object
    change_object@AnalysisMetadata$Formula <- "Count ~ B"
    expect_that(
      validObject(change_object),
      throws_error(
        "Formulas in 'AnalysisMetadata' don't match 'AnalysisFormula'"
      )
    )
  })

  it("detects changes in FirstImportedYear", {
    change_object <- object
    change_object@AnalysisMetadata$FirstImportedYear <- 999L
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LastImportedYear", {
    change_object <- object
    change_object@AnalysisMetadata$LastImportedYear <- 2000L
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in Duration", {
    change_object <- object
    change_object@AnalysisMetadata$Duration <- 2L
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LastAnalysedYear", {
    change_object <- object
    change_object@AnalysisMetadata$LastAnalysedYear <- 2000L
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LinearCombination", {
    change_object <- object
    change_object@LinearCombination <- lin_comb
    expect_that(
      validObject(change_object),
      throws_error("Corrupt FileFingerprint")
    )
  })
})

describe("illegal changes in the status fingerprint", {
  it("detects changes in Status", {
    change_object <- object
    change_object@AnalysisMetadata$Status <- "error"
    expect_that(
      validObject(change_object),
      throws_error("Corrupt StatusFingerprint")
    )
  })

  it("detects changes in AnalysisVersion", {
    change_object <- object
    change_object@AnalysisMetadata$AnalysisVersion <- ""
    expect_that(
      validObject(change_object),
      throws_error("Corrupt StatusFingerprint")
    )
  })


  it("detects changes in Model", {
    model_object <- inla(
      Count ~ B,
      data = object@Data,
      family = "poisson"
    )
    object_model <- n2k_inla(
      data = object, model_fit = model_object, status = "converged"
    )
    change_model <- inla(
      Count ~ C,
      data = object@Data,
      family = "poisson"
    )
    object_model@Model <- change_model
    expect_that(
      validObject(object_model),
      throws_error("Corrupt StatusFingerprint")
    )
  })
})
