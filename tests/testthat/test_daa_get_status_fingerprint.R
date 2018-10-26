context("get the correct status fingerprint")
describe("status fingerprint for n2k_glmer_poisson", {

  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DataFieldID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01 12:13:14", tz = "UTC")
  this.result.datasource.id <- sha1(sample(letters))
  this.scheme.id <- sha1(sample(letters))
  this.species.group.id <- sha1(sample(letters))
  this.location.group.id <- sha1(sample(letters))
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
      result.datasource.id = this.result.datasource.id,
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
    version <- get_analysis_version(sessionInfo())
    status.fingerprint <- sha1(
      list(
        get_file_fingerprint(object), status(object), NULL,
        version@AnalysisVersion$Fingerprint,
        version@AnalysisVersion, version@RPackage,
        version@AnalysisVersionRPackage, object@AnalysisRelation
      ),
      digits = 6L
    )
    expect_identical(status.fingerprint, get_status_fingerprint(object))
  })
})
