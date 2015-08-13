context("get the correct file fingerprint")
describe("file fingerprint for n2k_glmer_poisson", {

  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- 1
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this.scheme.id <- 1L
  this.species.group.id <- 2L
  this.location.group.id <- 3L
  this.seed <- 4L
  this.model.type <- "glmer poisson: period + herd"
  this.formula <- "incidence ~ offset(log(size)) + period + (1|herd)"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2015L
  this.parent <- "abcdef"
  this.duration <- this.last.imported.year - this.first.imported.year + 1

  it("sets the correct fingerprint for an new object", {
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
    file.fingerprint <- get_sha1(
      list(
        cbpp, this.scheme.id, this.species.group.id, this.location.group.id,
        this.model.type, this.formula, this.first.imported.year,
        this.last.imported.year, this.duration, this.last.analysed.year,
        this.analysis.date, this.seed, this.parent
      )
    )
    expect_identical(file.fingerprint, get_file_fingerprint(object))
  })
})
