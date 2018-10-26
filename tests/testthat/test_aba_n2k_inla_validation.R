context("n2kInla validation")

data("cbpp", package = "lme4")
cbpp$DataFieldID <- sha1(letters)
cbpp$ObservationID <- seq_len(nrow(cbpp))
lin.comb <- model.matrix(~period, unique(cbpp[, "period", drop = FALSE]))
object <- n2k_inla(
  result.datasource.id = sha1(letters),
  scheme.id = sha1(letters),
  species.group.id = sha1(letters),
  location.group.id = sha1(letters),
  model.type = "inla poisson: period + herd",
  formula = "incidence ~ offset(log(size)) + period + f(herd, model = 'iid')",
  first.imported.year = 1990,
  last.imported.year = 2015,
  duration = 1,
  last.analysed.year = 1995,
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
    change.object@AnalysisMetadata$SchemeID <- sha1(Sys.time())
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in SpeciesGroupID", {
    change.object <- object
    change.object@AnalysisMetadata$SpeciesGroupID <- sha1(Sys.time())
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LocationGroupID", {
    change.object <- object
    change.object@AnalysisMetadata$LocationGroupID <- sha1(Sys.time())
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
    change.object@AnalysisMetadata$ModelType <- "inla poisson: period"
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
    change.object@AnalysisMetadata$LastImportedYear <- 2000L
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
    change.object@AnalysisMetadata$LastAnalysedYear <- 2000L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LinearCombination", {
    change.object <- object
    change.object@LinearCombination <- lin.comb
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
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

  it("detects changes in AnalysisVersion", {
    change.object <- object
    change.object@AnalysisMetadata$AnalysisVersion <- ""
    expect_that(
      validObject(change.object),
      throws_error("Corrupt StatusFingerprint")
    )
  })


  it("detects changes in Model", {
    model.object <- inla(
      incidence ~ offset(log(size)) + period + f(herd, model = "iid"),
      data = object@Data,
      family = "poisson"
    )
    object.model <- n2k_inla(
      data = object, model.fit = model.object, status = "converged"
    )
    change.model <- inla(
      incidence ~ period + f(herd, model = "iid"),
      data = object@Data,
      family = "poisson"
    )
    object.model@Model <- change.model
    expect_that(
      validObject(object.model),
      throws_error("Corrupt StatusFingerprint")
    )
  })
})
