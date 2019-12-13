context("model imputed")
test_that("model imputation works", {
  set.seed(20191213)
  this.result.datasource.id <- sha1(letters)
  this.scheme.id <- sha1(letters)
  this.species.group.id <- sha1(letters)
  this.location.group.id <- sha1(letters)
  this.analysis.date <- Sys.time()
  this.model.type <- "inla poisson: A * (B + C) + C:D"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2014L
  this.duration <- 1L
  dataset <- test_data(missing = 0.2)
  base <- tempdir()
  project <- "imputation"

  imputation <- n2k_inla(
    data = dataset, scheme.id = this.scheme.id,
    result.datasource.id = this.result.datasource.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id, model.type = this.model.type,
    first.imported.year = this.first.imported.year, imputation.size = 100,
    last.imported.year = this.last.imported.year, family = "poisson",
    last.analyses.year = this.last.analysed.year, duration = this.duration,
    formula = "Count ~ A * (B + C) + f(E, model = \"iid\")",
    analysis.date = Sys.time(),
  )
  aggregation <- n2k_aggregate(
    scheme.id = this.scheme.id,
    result.datasource.id = this.result.datasource.id, formula = "~ A + B",
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id, model.type = this.model.type,
    first.imported.year = this.first.imported.year, analysis.date = Sys.time(),
    last.imported.year = this.last.imported.year, fun = sum,
    last.analyses.year = this.last.analysed.year, duration = this.duration,
    parent = get_file_fingerprint(imputation)
  )
  aggregation2 <- n2k_aggregate(
    scheme.id = this.scheme.id,
    result.datasource.id = this.result.datasource.id, formula = "~ A",
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id, model.type = this.model.type,
    first.imported.year = this.first.imported.year, analysis.date = Sys.time(),
    last.imported.year = this.last.imported.year, fun = sum,
    last.analyses.year = this.last.analysed.year, duration = this.duration,
    parent = get_file_fingerprint(aggregation)
  )
  extractor <- function(model) {
    model$summary.fixed[, c("mean", "sd")]
  }
  mi <- n2k_model_imputed(
    scheme.id = this.scheme.id, model.args = list(family = "poisson"),
    result.datasource.id = this.result.datasource.id, model.fun = INLA::inla,
    species.group.id = this.species.group.id, extractor = extractor,
    location.group.id = this.location.group.id, model.type = this.model.type,
    first.imported.year = this.first.imported.year, analysis.date = Sys.time(),
    last.imported.year = this.last.imported.year, formula = "~ A",
    last.analyses.year = this.last.analysed.year, duration = this.duration,
    parent = get_file_fingerprint(aggregation)
  )
  pma <- list(
    function(x) {
      return(list(family = "poisson"))
    }
  )
  mi2 <- n2k_model_imputed(
    scheme.id = this.scheme.id, model.args = list(),
    result.datasource.id = this.result.datasource.id, model.fun = INLA::inla,
    species.group.id = this.species.group.id, extractor = extractor,
    location.group.id = this.location.group.id, model.type = this.model.type,
    first.imported.year = this.first.imported.year, analysis.date = Sys.time(),
    last.imported.year = this.last.imported.year, formula = "~ A",
    last.analyses.year = this.last.analysed.year, duration = this.duration,
    parent = get_file_fingerprint(aggregation), prepare.model.args = pma
  )
  store_model(imputation, base, project)
  store_model(aggregation, base, project)
  store_model(mi, base, project)
  store_model(aggregation2, base, project)
  store_model(mi2, base, project)
  fit_model(get_file_fingerprint(imputation), base, project)
  fit_model(get_file_fingerprint(aggregation), base, project)
  fit_model(get_file_fingerprint(mi), base, project)
  fit_model(get_file_fingerprint(aggregation2), base, project)
  fit_model(get_file_fingerprint(mi2), base, project)
})
