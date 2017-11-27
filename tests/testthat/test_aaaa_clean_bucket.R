context("clean S3 bucket")
test_that("the unittest bucket is empty", {
  leftovers <- get_bucket("n2kmonitoring", prefix = "unittest")
  expect_true(all(sapply(leftovers, delete_object)))
})
