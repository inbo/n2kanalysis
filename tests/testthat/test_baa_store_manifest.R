context("store_manifest")
test_that("store_manifest stores the manifest on a local file system", {
  temp_dir <- tempdir()
  object <- n2k_manifest(
    data.frame(
      Fingerprint = "1",
      Parent = NA_character_,
      stringsAsFactors = FALSE
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

  paste0(temp_dir, "/store_manifest") %>% # nolint
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()
})

test_that("store_manifest stores the manifest on an S3 bucket", {
  bucket <- get_bucket("n2kmonitoring")
  object <- n2k_manifest(
    data.frame(
      Fingerprint = "1",
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  expect_is(
    filename <- store_manifest(
      x = object,
      base = bucket,
      project = "unittest_store_manifest"
    ),
    "character"
  )
  available <- get_bucket("n2kmonitoring", prefix = "unittest_store_manifest") %>%
    sapply("[[", "Key")
  expect_true(filename %in% available)
  expect_is(
    filename2 <- store_manifest(
      x = object,
      base = bucket,
      project = "unittest_store_manifest"
    ),
    "character"
  )
  available <- get_bucket("n2kmonitoring", prefix = "unittest_store_manifest") %>%
    sapply("[[", "Key")
  expect_true(filename2 %in% available)
  expect_identical(filename, filename2)
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
})
