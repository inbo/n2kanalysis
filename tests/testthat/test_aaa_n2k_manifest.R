context("n2k_manifest")

describe("new('n2k_manifest')", {
  it("checks the content of Manifest", {
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
      new("n2kManifest", Manifest = data.frame(Fingerprint = NA, Parent = 1)),
      "Fingerprint contains missing values"
    )
    expect_error(
      new("n2kManifest", Manifest = data.frame(Fingerprint = 1, Parent = "1")),
      "Fingerprint and Parent must be of the same class"
    )
    expect_error(
      new(
        "n2kManifest",
        Manifest = data.frame(
          Fingerprint = factor(1),
          Parent = "1",
          stringsAsFactors = FALSE
        )
      ),
      "Fingerprint and Parent must be of the same class"
    )
    expect_error(
      new(
        "n2kManifest",
        Manifest = data.frame(Fingerprint = "A", Parent = "1")
      ),
      "Fingerprint and Parent must have the same levels"
    )
  })
  it("checks the fingerprint", {
    manifest <- data.frame(Fingerprint = 1, Parent = NA)
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
      "Fingerprint must be a single character"
    )
    expect_is(
      new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
      "n2kManifest"
    )
  })
  it("checks the correct link between parent and fingerprint", {
    manifest <- data.frame(Fingerprint = 1, Parent = 2)
    expect_error(
      new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
      "All rows have parents"
    )
    manifest <- data.frame(Fingerprint = c(1, 2), Parent = c(NA, 3))
    expect_error(
      new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
      "Some Parent in 'Manifest' slot have no matching Fingerprint"
    )
    manifest <- data.frame(Fingerprint = c(1, 2), Parent = c(NA, 2))
    expect_error(
      new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
      "Self references between Parent and Fingerprint"
    )
    manifest <- data.frame(Fingerprint = c(1, 2, 3), Parent = c(NA, 3, 2))
    expect_error(
      new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
      "Too many parent - child levels"
    )
    manifest <- data.frame(Fingerprint = seq(1, 20), Parent = c(NA, 1:19))
    expect_error(
      new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
      "Too many parent - child levels"
    )
  })
})

describe("n2k_manifest", {
  it("generates the object", {
    manifest <- data.frame(Fingerprint = 1, Parent = NA)
    expect_is(
      new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
      "n2kManifest"
    )
    manifest <- data.frame(Fingerprint = c(1, 2), Parent = c(NA, 1))
    expect_is(
      new("n2kManifest", Manifest = manifest, Fingerprint = sha1(manifest)),
      "n2kManifest"
    )
  })
  it("checks the content of manifest", {
    expect_error(
      n2k_manifest(data.frame(junk = 1)),
      "manifest does not have name Fingerprint"
    )
    expect_error(
      n2k_manifest(data.frame(Parent = 1)),
      "manifest does not have name Fingerprint"
    )
    expect_error(
      n2k_manifest(data.frame(Fingerprint = 1)),
      "manifest does not have name Parent"
    )
  })

  it("ignore extra columns", {
    expect_is(
      x <- n2k_manifest(data.frame(Fingerprint = 1, Parent = NA, junk = 1)),
      "n2kManifest"
    )
    expect_identical(
      colnames(x@Manifest),
      c("Fingerprint", "Parent")
    )
  })

  it("handles tbl()", {
    expect_is(
      x <- n2k_manifest(dplyr::tibble(Fingerprint = 1, Parent = NA)),
      "n2kManifest"
    )
  })

  it("sorts and compacts", {
    expect_is(
      x <- n2k_manifest(
        data.frame(
          Fingerprint = c(4, 4, 3, 2, 2, 1),
          Parent = c(3, 2, 1, 1, 1, NA)
        )
      ),
      "n2kManifest"
    )
    expect_identical(
      x@Manifest,
      data.frame(
        Fingerprint = c(1, 2, 3, 4, 4),
        Parent = c(NA, 1, 1, 2, 3)
      )
    )
  })
})
