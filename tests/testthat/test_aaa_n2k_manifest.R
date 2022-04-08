test_that("n2k_manifest checks the content of Manifest", {
  expect_error(
    new("n2kManifest", Manifest = data.frame(junk = 1)),
    "Variables missing in Manifest: fingerprint, parent"
  )
  expect_error(
    new("n2kManifest", Manifest = data.frame(parent = 1)),
    "Variables missing in Manifest: fingerprint"
  )
  expect_error(
    new("n2kManifest", Manifest = data.frame(fingerprint = 1)),
    "Variables missing in Manifest: parent"
  )
  expect_error(
    new(
      "n2kManifest",
      Manifest = data.frame(
        fingerprint = 1, parent = "1", stringsAsFactors = FALSE
      )
    ),
    "fingerprint: got 'numeric'"
  )
  expect_error(
    new("n2kManifest", Manifest = data.frame(fingerprint = "1", parent = 1)),
    "parent: got 'numeric'"
  )
})

test_that("n2k_manifest checks the fingerprint", {
  manifest <- data.frame(
    fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
  )
  expect_error(new("n2kManifest", Manifest = manifest), "wrong fingerprint")
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = "junk"),
    "wrong fingerprint"
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = letters),
    "Fingerprint is not a string"
  )
  expect_is(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "n2kManifest"
  )
})

test_that(
  "n2k_manifest checks the correct link between parent and fingerprint", {
  manifest <- data.frame(
    fingerprint = "1", parent = "2", stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "All rows have parents"
  )
  manifest <- data.frame(
    fingerprint = c("1", "2"), parent = c(NA, "3"), stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "Some parent in 'Manifest' slot have no matching fingerprint"
  )
  manifest <- data.frame(
    fingerprint = c("1", "2"), parent = c(NA, "2"), stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "Self references between parent and fingerprint"
  )
  manifest <- data.frame(
    fingerprint = c("1", "2", "3"), parent = c(NA, "3", "2"),
    stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "Too many parent - child levels"
  )
  manifest <- data.frame(
    fingerprint = as.character(seq(1, 20)), parent = as.character(c(NA, 1:19)),
    stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "Too many parent - child levels"
  )
})

test_that("n2kManifest generates the object", {
  manifest <- data.frame(
    fingerprint = "1", parent = NA_character_, stringsAsFactors = FALSE
  )
  expect_is(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "n2kManifest"
  )
  manifest <- data.frame(
    fingerprint = c("1", "2"), parent = c(NA, "1"), stringsAsFactors = FALSE
  )
  expect_is(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "n2kManifest"
  )
})

test_that("n2k_manifest checks the content of manifest", {
  expect_error(
    n2k_manifest(data.frame(junk = 1)),
    "manifest does not have .*name.*fingerprint"
  )
  expect_error(
    n2k_manifest(data.frame(parent = 1)),
    "manifest does not have .*name.*fingerprint"
  )
  expect_error(
    n2k_manifest(data.frame(fingerprint = 1)),
    "manifest does not have .*name.*parent"
  )
})

test_that("n2k_manifest ignores extra columns", {
  expect_is(
    x <- n2k_manifest(
      data.frame(
        fingerprint = "1", parent = NA_character_, junk = 1,
        stringsAsFactors = FALSE
      )
    ),
    "n2kManifest"
  )
  expect_identical(
    colnames(x@Manifest), c("fingerprint", "parent")
  )
})

test_that("n2k_manifest handles tbl()", {
  expect_is(
    x <- n2k_manifest(dplyr::tibble(fingerprint = "1", parent = NA_character_)),
    "n2kManifest"
  )
})

test_that("n2k_manifest sorts and compacts", {
  expect_is(
    x <- n2k_manifest(
      dplyr::tibble(
        fingerprint = as.character(c(4, 4, 3, 2, 2, 1)),
        parent = as.character(c(3, 2, 1, 1, 1, NA))
      )
    ),
    "n2kManifest"
  )
  expect_identical(
    x@Manifest,
    dplyr::tibble(
      fingerprint = as.character(c(1, 2, 3, 4, 4)),
      parent = as.character(c(NA, 1, 1, 2, 3))
    ) %>%
      as.data.frame()
  )
})
