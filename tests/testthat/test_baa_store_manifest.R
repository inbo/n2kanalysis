context("store_manifest")
test_that("store_manifest stores the manifest on a local file system", {
  temp_dir <- tempfile("store_manifest")
  dir.create(temp_dir)
  object <- n2k_manifest(
    data.frame(
      fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  expect_is(
    filename <- store_manifest(object, temp_dir, "store_manifest"),
    "character"
  )
  file_info <- file.info(filename)
  expect_is(
    filename2 <- store_manifest(object, temp_dir, "store_manifest"),
    "character"
  )
  file_info2 <- file.info(filename2)
  expect_identical(filename, filename2)
  expect_identical(file_info, file_info2)

  file.path(temp_dir, "store_manifest") %>%
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()
})

test_that("store_manifest stores the manifest on an S3 bucket", {
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "", message = "No AWS access")
  project <- "unittest_store_manifest"
  bucket <- get_bucket(Sys.getenv("N2KBUCKET"), prefix = project, max = 1)
  object <- n2k_manifest(
    data.frame(
      fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  expect_type(
    stored <- store_manifest(x = object, base = bucket, project = project),
    "character"
  )
  available <- get_bucket(bucket, prefix = project)
  expect_equivalent(stored, map_chr(available, "Key"))
  expect_type(
    stored2 <- store_manifest(x = object, base = bucket, project = project),
    "character"
  )
  available <- get_bucket(bucket, prefix = "unittest_store_manifest")
  expect_equivalent(stored2, map_chr(available, "Key"))
  expect_equivalent(stored, stored2)
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
})
