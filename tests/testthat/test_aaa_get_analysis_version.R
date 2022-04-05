test_that("get_analysis_version", {
  expect_is(
    get_analysis_version(sessionInfo()),
    "n2kAnalysisVersion"
  )
})
