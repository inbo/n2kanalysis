test_that("n2k_manifest checks the content of Manifest", {
  expect_error(
    new("n2kManifest", Manifest = data.frame(junk = 1)),
    "Variables missing in Manifest: Fingerprint, Parent"
  )
  expect_error(
    new("n2kManifest", Manifest = data.frame(Parent = 1)),
    "Variables missing in Manifest: Fingerprint"
  )
  expect_error(
    new("n2kManifest", Manifest = data.frame(Fingerprint = 1)),
    "Variables missing in Manifest: Parent"
  )
  expect_error(
    new(
      "n2kManifest",
      Manifest = data.frame(
        Fingerprint = 1,
        Parent = "1",
        stringsAsFactors = FALSE
      )
    ),
    "Fingerprint: got 'numeric'"
  )
  expect_error(
    new(
      "n2kManifest",
      Manifest = data.frame(
        Fingerprint = "1",
        Parent = 1,
        stringsAsFactors = FALSE
      )
    ),
    "Parent: got 'numeric'"
  )
  expect_error(
    new("n2kManifest", Manifest = data.frame(Fingerprint = "1", Parent = 1)),
    "Wrong class for following variable"
  )
})

test_that("n2k_manifest checks the fingerprint", {
  manifest <- data.frame(
    Fingerprint = "1",
    Parent = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest),
    "wrong fingerprint"
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = "junk"),
    "wrong fingerprint"
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = letters),
    "Fingerprint is not a string \\(a length one character vector\\)." # nolint: nonportable_path_linter, line_length_linter.
  )
  expect_is(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "n2kManifest"
  )
})

test_that(
  "n2k_manifest checks the correct link between parent and fingerprint", {
  manifest <- data.frame(
    Fingerprint = "1",
    Parent = "2",
    stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "All rows have parents"
  )
  manifest <- data.frame(
    Fingerprint = c("1", "2"),
    Parent = c(NA, "3"),
    stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "Some Parent in 'Manifest' slot have no matching Fingerprint"
  )
  manifest <- data.frame(
    Fingerprint = c("1", "2"),
    Parent = c(NA, "2"),
    stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "Self references between Parent and Fingerprint"
  )
  manifest <- data.frame(
    Fingerprint = c("1", "2", "3"),
    Parent = c(NA, "3", "2"),
    stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "Too many parent - child levels"
  )
  manifest <- data.frame(
    Fingerprint = as.character(seq(1, 20)),
    Parent = as.character(c(NA, 1:19)),
    stringsAsFactors = FALSE
  )
  expect_error(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "Too many parent - child levels"
  )
})

test_that("n2kManifest generates the object", {
  manifest <- data.frame(
    Fingerprint = "1",
    Parent = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_is(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "n2kManifest"
  )
  manifest <- data.frame(
    Fingerprint = c("1", "2"),
    Parent = c(NA, "1"),
    stringsAsFactors = FALSE
  )
  expect_is(
    new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
    "n2kManifest"
  )
})

test_that("n2k_manifest checks the content of manifest", {
  expect_error(
    n2k_manifest(data.frame(junk = 1)),
    "manifest does not have .*name.*Fingerprint"
  )
  expect_error(
    n2k_manifest(data.frame(Parent = 1)),
    "manifest does not have .*name.*Fingerprint"
  )
  expect_error(
    n2k_manifest(data.frame(Fingerprint = 1)),
    "manifest does not have .*name.*Parent"
  )
})

test_that("n2k_manifest ignores extra columns", {
  expect_is(
    x <- n2k_manifest(
      data.frame(
        Fingerprint = "1",
        Parent = NA_character_,
        junk = 1,
        stringsAsFactors = FALSE
      )
    ),
    "n2kManifest"
  )
  expect_identical(
    colnames(x@Manifest),
    c("Fingerprint", "Parent")
  )
})

test_that("n2k_manifest handles tbl()", {
  expect_is(
    x <- n2k_manifest(
      dplyr::tibble(
        Fingerprint = "1",
        Parent = NA_character_
      )
    ),
    "n2kManifest"
  )
})

test_that("n2k_manifest sorts and compacts", {
  expect_is(
    x <- n2k_manifest(
      dplyr::tibble(
        Fingerprint = as.character(c(4, 4, 3, 2, 2, 1)),
        Parent = as.character(c(3, 2, 1, 1, 1, NA))
      )
    ),
    "n2kManifest"
  )
  expect_identical(
    x@Manifest,
    dplyr::tibble(
      Fingerprint = as.character(c(1, 2, 3, 4, 4)),
      Parent = as.character(c(NA, 1, 1, 2, 3))
    ) %>%
      as.data.frame()
  )
})
