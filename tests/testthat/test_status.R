context("status")
temp.dir <- tempdir()

describe("n2kGlmerPoisson", {
  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01")
  this.scheme.id <- sha1(letters)
  this.species.group.id <- sha1(letters)
  this.location.group.id <- sha1(letters)
  this.seed <- 4L
  this.model.type <- "glmer poisson: period + herd"
  this.formula <- "incidence ~ offset(log(size)) + period + (1|herd)"
  this.formula.2 <- "incidence ~ offset(log(size)) + (1|herd)"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2015L
  this.parent <- "abcdef"
  this.duration <- this.last.imported.year - this.first.imported.year + 1
  object <- n2k_glmer_poisson(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    last.analysed.year = this.last.analysed.year,
    analysis.date = this.analysis.date,
    seed = this.seed,
    data = cbpp,
    parent = this.parent,
    this.duration
  )
  object.2 <- n2k_glmer_poisson(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula.2,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    last.analysed.year = this.last.analysed.year,
    analysis.date = this.analysis.date,
    seed = this.seed,
    data = cbpp,
    parent = this.parent,
    this.duration
  )
  analysis <- object
  filename <- store_model(analysis, base = temp.dir, root = "", path = "")
  analysis <- object.2
  filename <- store_model(analysis, base = temp.dir, root = "", path = "")
  file.list <- normalizePath(
    list.files(
      temp.dir,
      pattern = "^[0-9a-f]{40}\\.rda$",
      full.names = TRUE
    ),
    winslash = "/"
  )

  it("returns the status in the object", {
    expect_identical(
      status(object),
      object@AnalysisMetadata$Status
    )
  })

  it("returns the status in the object stored in a directory", {
    this.status <- status(temp.dir)
    expect_true(
      all(this.status$Status == "new")
    )
    expect_identical(
      sort(this.status$StatusFingerprint),
      sort(c(get_status_fingerprint(object), get_status_fingerprint(object.2)))
    )
    expect_identical(
      sort(this.status$FileFingerprint),
      sort(c(get_file_fingerprint(object), get_file_fingerprint(object.2)))
    )
    expect_identical(
      sort(this.status$Filename),
      sort(file.list)
    )
  })
  it("works with filenames", {
    expect_identical(
      do.call(
        rbind,
        lapply(file.list, status)
      ),
      status(temp.dir)
    )
  })

  it("sets the correct status fingerprint", {
    status.fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        coef(object@Model), object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation
      ),
      digits = 6L
    )
    expect_identical(
      get_status_fingerprint(object),
      status.fingerprint
    )

    status(object) <- "waiting"
    status.fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        coef(object@Model), object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation
      ),
      digits = 6L
    )
    expect_identical(
      get_status_fingerprint(object),
      status.fingerprint
    )
  })

  it("returns a valid object after changing the status", {
    expect_true(validObject(object))
    status(object) <- "new"
    expect_true(validObject(object))
  })
})

# clean temp files
file.remove(
  list.files(
    temp.dir,
    pattern = "^[0-9a-f]{40}\\.rda$",
    full.names = TRUE
  )
)
