context("n2kGlmerPoisson validation")
data("cbpp", package = "lme4")
cbpp$Weight <- cbpp$size
cbpp$DatasourceID <- 1
cbpp$ObservationID <- seq_len(nrow(cbpp))
object <- n2k_glmer_poisson(
  scheme.id = 1,
  species.group.id = 2,
  location.group.id = 3,
  model.type = "glmer poisson: period + herd",
  formula = "incidence ~ offset(log(size)) + period + (1|herd)",
  first.imported.year = 1990,
  last.imported.year = 2015,
  last.analysed.year = 1995,
  duration = 1,
  analysis.date = as.POSIXct("2000-01-01 12:13:14", tz = "UTC"),
  data = cbpp
)
weighted.object <- n2k_glmer_poisson(
  scheme.id = 1,
  species.group.id = 2,
  location.group.id = 3,
  model.type = "weighted glmer poisson: period + herd",
  formula = "incidence ~ offset(log(size)) + period + (1|herd)",
  first.imported.year = 1990,
  last.imported.year = 2015,
  analysis.date = as.POSIXct("2000-01-01 12:13:14", tz = "UTC"),
  data = cbpp
)

describe("illegal changes in the file fingerprint", {

  it("detects changes in Data", {
    change.object <- object
    change.object@Data <- head(cbpp, 1)
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in SchemeID", {
    change.object <- object
    change.object@AnalysisMetadata$SchemeID <- 999L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in SpeciesGroupID", {
    change.object <- object
    change.object@AnalysisMetadata$SpeciesGroupID <- 999L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LocationGroupID", {
    change.object <- object
    change.object@AnalysisMetadata$LocationGroupID <- 999L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in FirstImportedYear", {
    change.object <- object
    change.object@AnalysisMetadata$FirstImportedYear <- 999L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LastImportedYear", {
    change.object <- object
    change.object@AnalysisMetadata$LastImportedYear <- 2010L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in Duration", {
    change.object <- object
    change.object@AnalysisMetadata$Duration <- 2L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LastAnalysedYear", {
    change.object <- object
    change.object@AnalysisMetadata$LastAnalysedYear <- 1994L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })


  it("detects changes in AnalysisDate", {
    change.object <- object
    change.object@AnalysisMetadata$AnalysisDate <- Sys.time()
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in ModelType", {
    change.object <- object
    change.object@AnalysisMetadata$ModelType <- "glmer poisson: period"
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in Formula", {
    change.object <- object
    change.object@AnalysisMetadata$Formula <- "incidence ~ period"
    expect_that(
      validObject(change.object),
      throws_error(
        "Formulas in 'AnalysisMetadata' don't match 'AnalysisFormula'"
      )
    )
  })

})

describe("illegal changes in the status fingerprint", {

  it("detects changes in Status", {
    change.object <- object
    change.object@AnalysisMetadata$Status <- "error"
    expect_that(
      validObject(change.object),
      throws_error("Corrupt StatusFingerprint")
    )
  })

  it("detects changes in Model", {
    model.object <- lme4::glmer(
      incidence ~ offset(log(size)) + period + (1 | herd),
      data = object@Data,
      family = poisson
    )
    object.model <- n2k_glmer_poisson(
      data = object, model.fit = model.object, status = "converged"
    )
    change.model <- lme4::glmer(
      incidence ~ period + (1 | herd),
      data = object@Data,
      family = poisson
    )
    object.model@Model <- change.model
    expect_that(
      validObject(object.model),
      throws_error("Corrupt StatusFingerprint")
    )
  })

  it("detects changes in AnalysisVersion", {
    change.object <- object
    change.object@AnalysisMetadata$AnalysisVersion <- ""
    expect_that(
      validObject(change.object),
      throws_error("Corrupt StatusFingerprint")
    )
  })
})
