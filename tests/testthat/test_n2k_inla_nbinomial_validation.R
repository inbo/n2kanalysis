context("n2kInlaNbinomial validation")

data("cbpp", package = "lme4")
cbpp$Count <- cbpp$incidence
object <- n2k_inla_nbinomial(
  scheme.id = 1,
  species.group.id = 2,
  location.group.id = 3,
  model.type = "inla nbinomial: period + herd",
  covariate = "offset(log(size)) + period + f(herd, model = 'iid')",
  first.imported.year = 1990,
  last.imported.year = 2015,
  duration = 1,
  analysis.date = as.POSIXct("2000-01-01"),
  data = cbpp
)

context("illegal changes in the file fingerprint")
describe("file fingerprint", {
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

  it("detects changes in AnalysisDate", {
    change.object <- object
    change.object@AnalysisDate <- Sys.time()
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })
  
  it("detects changes in ModelType", {
    change.object <- object
    change.object@ModelType <- "inla nbinomial: period"
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })
  
  it("detects changes in Covariate", {
    change.object <- object
    change.object@Covariate <- "period"
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })
  
  it("detects changes in FirstImportedYear", {
    change.object <- object
    change.object@FirstImportedYear <- 999L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })
  
  it("detects changes in LastImportedYear", {
    change.object <- object
    change.object@LastImportedYear <- 2000L
    expect_that(
      validObject(change.object),
      throws_error("Corrupt FileFingerprint")
    )
  })
  
  it("detects changes in Duration", {
    change.object <- object
    change.object@Duration <- 2L
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
    require(INLA)
    model.object <- INLA::inla(
      Count ~ offset(log(size)) + period + f(herd, model = "iid"), 
      data = object@Data,
      family = "nbinomial"
    )
    object.model <- n2k_inla_nbinomial(
      data = object, model.fit = model.object, status = "converged"
    )
    change.model <- INLA::inla(
      Count ~ period + f(herd, model = "iid"), 
      data = object@Data,
      family = "nbinomial"
    )
    object.model@Model <- change.model
    expect_that(
      validObject(object.model),
      is_true()
    )
  })
})
