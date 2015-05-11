context("n2kModel validation")

context("illegal changes of the data or the data fingerprint")
describe("data fingerprint", {
  data("cbpp", package = "lme4")
  cbpp$Count <- cbpp$incidence
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
    covariate = "offset(log(size)) + period + (1|herd)",
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  change.object <- object
  change.object@DataFingerprint <- "abc"
  expect_that(
    validObject(change.object),
    throws_error("Mismatch between DataFingerprint and Data")
  )
  change.object <- object
  change.object@Data <- head(cbpp, 1)
  expect_that(
    validObject(change.object),
    throws_error("Mismatch between DataFingerprint and Data")
  )
})
