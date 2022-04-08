test_that("gets the correct fingerprint", {
  x <- n2k_manifest(
    data.frame(
      fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  expect_identical(x@Fingerprint, get_file_fingerprint(x))
})
