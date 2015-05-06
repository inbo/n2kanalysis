context("n2kModel validation")

context("illegal changes of the data or the data fingerprint")
describe("data fingerprint", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    data = cbpp
  )
  object@DataFingerprint <- "abc"
  expect_that(
    validObject(object),
    throws_error("Mismatch between DataFingerprint and Data")
  )
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    data = cbpp
  )
  data("sleepstudy", package = "lme4")
  object@Data <- sleepstudy
  expect_that(
    validObject(object),
    throws_error("Mismatch between DataFingerprint and Data")
  )
})
