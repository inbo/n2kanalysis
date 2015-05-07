context("n2kGlmerPoisson validation")

context("illegal changes in the file fingerprint")
describe("file fingerprint", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  
  it("detects changes in Data", {
    change.object <- object
    change.object@Data <- head(cbpp, 1)
    expect_that(
      validObject(change.object),
      throws_error("Mismatch between DataFingerprint and Data")
    )
  })
  
  it("detects changes in SchemeID", {
    change.object <- object
    change.object@SchemeID <- 999L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in SpeciesGroupID", {
    change.object <- object
    change.object@SpeciesGroupID <- 999L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in LocationGroupID", {
    change.object <- object
    change.object@LocationGroupID <- 999L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })

  it("detects changes in Weight", {
    change.object <- object
    change.object@Weight <- "size"
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })
  
  it("detects changes in AnalysisDate", {
    change.object <- object
    change.object@AnalysisDate <- Sys.time()
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })
  
  it("ignores changes in Status", {
    change.object <- object
    change.object@Status <- "error"
    expect_that(
      validObject(change.object),
      is_true()
    )
  })

  it("ignores changes in Model", {
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
      is_true()
    )
  })
})
