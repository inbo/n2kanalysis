context("clean S3 bucket")
test_that("the unittest bucket is empty", {
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "", message = "No AWS access")
  leftovers <- get_bucket(Sys.getenv("N2KBUCKET"), prefix = "unittest")
  expect_true(all(sapply(leftovers, delete_object)))
})
