context("fit_model on n2kManifest")
describe("it handles a manifest", {
  project <- "unit_test_fit_model"
  data("cbpp", package = "lme4")
  cbpp$DatasourceID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  object <- n2k_glmer_poisson(
    result.datasource.id = sha1(sample(letters)),
    scheme.id = sha1(sample(letters)),
    species.group.id = sha1(sample(letters)),
    location.group.id = sha1(sample(letters)),
    model.type = "glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first.imported.year = 1990,
    last.imported.year = 2015,
    analysis.date = Sys.time(),
    data = cbpp
  )
  object2 <- n2k_glmer_poisson(
    result.datasource.id = sha1(sample(letters)),
    scheme.id = sha1(sample(letters)),
    species.group.id = sha1(sample(letters)),
    location.group.id = sha1(sample(letters)),
    model.type = "glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first.imported.year = 1990,
    last.imported.year = 2015,
    analysis.date = Sys.time(),
    data = cbpp
  )
  object3 <- n2k_glmer_poisson(
    result.datasource.id = sha1(sample(letters)),
    scheme.id = sha1(sample(letters)),
    species.group.id = sha1(sample(letters)),
    location.group.id = sha1(sample(letters)),
    model.type = "glmer poisson: period + herd",
    formula = "incidence ~ offset(log(size)) + period + (1|herd)",
    first.imported.year = 1990,
    last.imported.year = 2015,
    analysis.date = Sys.time(),
    data = cbpp
  )

  it("works with local file", {
    base <- tempdir()
    store_model(object, base = base, project = project)
    store_model(object2, base = base, project = project)
    store_model(object3, base = base, project = project)
    x <- data.frame(
      Fingerprint = c(
        get_file_fingerprint(object),
        get_file_fingerprint(object2),
        get_file_fingerprint(object3)
      ),
      Parent = c(
        NA,
        get_file_fingerprint(object),
        get_file_fingerprint(object2)
      ),
      stringsAsFactors = FALSE
    ) %>%
      n2k_manifest()
    expect_identical(
      fit_model(x, base = base, project = project),
      NULL
    )
    x <- store_manifest(x, base, project)
    expect_identical(
      fit_model(x, base = base, project = project),
      NULL
    )
    sprintf("%s/%s", base, project) %>%
      list.files(recursive = TRUE, full.names = TRUE) %>%
      file.remove()
  })

  it("works with an S3 bucket", {
    base <- get_bucket("n2kmonitoring")
    store_model(object, base = base, project = project)
    store_model(object2, base = base, project = project)
    store_model(object3, base = base, project = project)
    x <- data.frame(
      Fingerprint = c(
        get_file_fingerprint(object),
        get_file_fingerprint(object2),
        get_file_fingerprint(object3)
      ),
      Parent = c(
        NA,
        get_file_fingerprint(object),
        get_file_fingerprint(object2)
      ),
      stringsAsFactors = FALSE
    ) %>%
      n2k_manifest()
    expect_identical(fit_model(x, base = base, project = project), NULL)

    available <- get_bucket(
      base,
      prefix = project
    ) %>%
      sapply("[[", "Key")
    expect_true(all(sapply(available, delete_object, bucket = base)))
  })
})
