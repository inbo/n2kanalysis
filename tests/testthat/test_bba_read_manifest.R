test_that("read_manifest reads the manifest on a local file system", {
  temp_dir <- tempfile("read_manifest")
  dir.create(temp_dir)
  object <- n2k_manifest(
    data.frame(
      fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  object2 <- n2k_manifest(
    data.frame(
      fingerprint = "4", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  expect_error(
    read_manifest(temp_dir, "read_manifest"), "No manifest files in"
  )
  file.path(temp_dir, "read_manifest", "manifest") %>%
    normalizePath(mustWork = FALSE) %>%
    dir.create(recursive = TRUE)
  expect_error(
    read_manifest(temp_dir, "read_manifest"), "No manifest files in"
  )
  store_manifest(object, temp_dir, "read_manifest")
  store_manifest(object2, temp_dir, "read_manifest")
  expect_equal(
    read_manifest(temp_dir, "read_manifest", object@Fingerprint), object
  )
  expect_equal(
    read_manifest(temp_dir, "read_manifest", object2@Fingerprint), object2
  )
  Sys.sleep(1)
  expect_equal(read_manifest(temp_dir, "read_manifest"), object2)
  expect_error(
    read_manifest(temp_dir, "read_manifest", "junk"),
    "No manifest found starting with 'junk'"
  )
  expect_error(
    read_manifest(temp_dir, "read_manifest", "5"),
    "Multiple manifests found starting with '5'"
  )
  file.path(temp_dir, "read_manifest") %>%
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()
})

test_that("read_manifest reads the manifest on an S3 bucket", {
  skip_if(Sys.getenv("AWS_SECRET_ACCESS_KEY") == "", message = "No AWS access")
  bucket <- get_bucket("n2kmonitoring")
  project <- "unittest_read_manifest"
  object <- n2k_manifest(
    data.frame(
      fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  object2 <- n2k_manifest(
    data.frame(
      fingerprint = "4", parent = NA_character_, stringsAsFactors = FALSE
    )
  )
  store_manifest(object, bucket, project)
  expect_equal(
    read_manifest(bucket, project, object@Fingerprint),
    object
  )
  Sys.sleep(2)
  stored <- store_manifest(object2, bucket, project)
  expect_equal(read_manifest(bucket, hash = stored$Contents$Key), object2)
  expect_equal(read_manifest(bucket, project, object2@Fingerprint), object2)
  latest <- read_manifest(bucket, project)
  expect_equal(latest, object2)

  expect_error(
    read_manifest(bucket, project, "junk"),
    "No manifest found starting with 'junk'"
  )
  expect_error(
    read_manifest(bucket, project, "5"),
    "Multiple manifests found starting with '5'"
  )

  available <- get_bucket("n2kmonitoring", prefix = project) %>%
    sapply("[[", "Key")
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
  expect_error(read_manifest(bucket, project), "No manifest files in")
})
