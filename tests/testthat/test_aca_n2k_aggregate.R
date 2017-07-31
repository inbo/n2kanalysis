context("prepare a n2kAggregate object")
require(INLA)
this.result.datasource.id <- sha1(sample(letters))
this.scheme.id <- sha1(sample(letters))
this.species.group.id <- sha1(sample(letters))
this.location.group.id <- sha1(sample(letters))
this.analysis.date <- Sys.time()
this.model.type <- "inla nbinomial: A * (B + C) + C:D"
this.formula <-
  "Count ~ A * (B + C) + C:D +
    f(E, model = \"rw1\", replicate = as.integer(A)) +
    f(F, model = \"iid\")"
this.first.imported.year <- 1990L
this.last.imported.year <- 2015L
this.last.analysed.year <- 2014L
this.duration <- 1L
dataset <- test_data()
object <- n2k_inla_nbinomial(
  result.datasource.id = this.result.datasource.id,
  scheme.id = this.scheme.id,
  species.group.id = this.species.group.id,
  location.group.id = this.location.group.id,
  model.type = this.model.type,
  formula = this.formula,
  first.imported.year = this.first.imported.year,
  last.imported.year = this.last.imported.year,
  analysis.date = this.analysis.date,
  data = dataset
)
n2k_aggregate(
  result.datasource.id = this.result.datasource.id,
  scheme.id = this.scheme.id,
  species.group.id = this.species.group.id,
  location.group.id = this.location.group.id,
  model.type = this.model.type,
  formula = this.formula,
  first.imported.year = this.first.imported.year,
  last.imported.year = this.last.imported.year,
  analysis.date = this.analysis.date,
  parent = get_file_fingerprint(object),
  fun = sum
)
