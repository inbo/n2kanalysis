context("clean S3 bucket")
test_that("the unittest bucket is empty", {
  if (Sys.getenv("GITHUB_ACTION") == "") {
    connect_inbo_s3()
  }
  leftovers <- get_bucket(Sys.getenv("N2KBUCKET"), prefix = "unittest")
  expect_true(all(sapply(leftovers, delete_object)))
})
