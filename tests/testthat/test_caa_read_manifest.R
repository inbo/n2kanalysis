context("read_manifest")
test_that("read_manifest reads the manifest on a local file system", {
  temp_dir <- tempdir()
  object <- n2k_manifest(
    data.frame(
      Fingerprint = "10",
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  object2 <- n2k_manifest(
    data.frame(
      Fingerprint = "2",
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  expect_error(
    read_manifest(temp_dir, "read_manifest"),
    "No manifest files in"
  )
  paste(temp_dir, "read_manifest", "manifest", sep = "/") %>%
    normalizePath(mustWork = FALSE) %>%
    dir.create(recursive = TRUE)
  expect_error(
    read_manifest(temp_dir, "read_manifest"),
    "No manifest files in"
  )
  store_manifest(object, temp_dir, "read_manifest")
  store_manifest(object2, temp_dir, "read_manifest")
  expect_equal(
    read_manifest(temp_dir, "read_manifest", object@Fingerprint),
    object
  )
  expect_equal(
    read_manifest(temp_dir, "read_manifest", object2@Fingerprint),
    object2
  )
  expect_equal(
    read_manifest(temp_dir, "read_manifest"),
    object2
  )
  expect_error(
    read_manifest(temp_dir, "read_manifest", "junk"),
    "No manifest found starting with 'junk'"
  )
  expect_error(
    read_manifest(temp_dir, "read_manifest", "1"),
    "Multiple manifests found starting with '1'"
  )
  sprintf("%s/read_manifest", temp_dir) %>%
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()
})

test_that("read_manifest reads the manifest on an S3 bucket", {
  bucket <- get_bucket("n2kmonitoring")
  expect_error(
    read_manifest(bucket, "unittest_read_manifest"),
    "No manifest files in"
  )
  object <- n2k_manifest(
    data.frame(
      Fingerprint = "10",
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  object2 <- n2k_manifest(
    data.frame(
      Fingerprint = "2",
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  store_manifest(object, bucket, "unittest_read_manifest")
  store_manifest(object2, bucket, "unittest_read_manifest")
  expect_equal(
    read_manifest(bucket, "unittest_read_manifest", object@Fingerprint),
    object
  )
  expect_equal(
    read_manifest(bucket, "unittest_read_manifest", object2@Fingerprint),
    object2
  )
  expect_equal(
    read_manifest(bucket, "unittest_read_manifest"),
    object2
  )
  expect_error(
    read_manifest(bucket, "unittest_read_manifest", "junk"),
    "No manifest found starting with 'junk'"
  )
  expect_error(
    read_manifest(bucket, "unittest_read_manifest", "1"),
    "Multiple manifests found starting with '1'"
  )

  available <- get_bucket(
    "n2kmonitoring",
    prefix = "unittest_read_manifest"
  ) %>%
    sapply("[[", "Key")
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
})
