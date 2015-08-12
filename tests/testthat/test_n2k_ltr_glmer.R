context("prepare a n2kLrtGlmer object")
this.seed <- 50L
this.scheme.id <- 1L
this.species.group.id <- 2L
this.location.group.id <- 3L
this.analysis.date <- Sys.time()
this.model.type.parent <- "glmer poisson: period + herd"
this.model.type <- "glmer lrt: cYear / fYear"
this.formula <- "~ period"
this.formula.0 <- "incidence ~ offset(log(size)) + period + (1|herd)"
this.formula.1 <- "incidence ~ offset(log(size)) + (1|herd)"
this.first.imported.year <- 1990L
this.last.imported.year <- 2015L
this.last.analysed.year <- 2014L
this.duration <- 1L
data("cbpp", package = "lme4")
cbpp$DatasourceID <- 1
cbpp$ObservationID <- seq_len(nrow(cbpp))
object.1 <- n2k_glmer_poisson(
  scheme.id = this.scheme.id,
  species.group.id = this.species.group.id,
  location.group.id = this.location.group.id,
  model.type = this.model.type.parent,
  formula = this.formula.1,
  first.imported.year = this.first.imported.year,
  last.imported.year = this.last.imported.year,
  analysis.date = this.analysis.date,
  data = cbpp
)
object.0 <- n2k_glmer_poisson(
  scheme.id = this.scheme.id,
  species.group.id = this.species.group.id,
  location.group.id = this.location.group.id,
  model.type = this.model.type.parent,
  formula = this.formula.0,
  first.imported.year = this.first.imported.year,
  last.imported.year = this.last.imported.year,
  analysis.date = this.analysis.date,
  data = cbpp
)
this.parent.status <- data.frame(
  ParentAnalysis = c(
    get_file_fingerprint(object.1),
    get_file_fingerprint(object.0)
  ),
  ParentStatusFingerprint = c(
    get_status_fingerprint(object.1),
    get_status_fingerprint(object.0)
  ),
  ParentStatus = c(status(object.1), status(object.0))
)

# Check function arguments

## seed

### must be count

expect_error(
  n2k_lrt_glmer(
    parent = get_file_fingerprint(object.1),
    parent.0 = get_file_fingerprint(object.0),
    seed = "a"
  ),
  "dots\\$seed is not a count \\(a single positive integer\\)"
)
expect_error(
  n2k_lrt_glmer(
    parent = get_file_fingerprint(object.1),
    parent.0 = get_file_fingerprint(object.0),
    seed = this.seed + 0.1
  ),
  "dots\\$seed is not a count \\(a single positive integer\\)"
)
expect_is(
  n2k_lrt_glmer(
    parent = get_file_fingerprint(object.1),
    parent.0 = get_file_fingerprint(object.0),
    parent.status = this.parent.status,
    seed = as.numeric(this.seed),
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date
  ),
  "n2kLrtGlmer"
)

### set to a random value when missing
expect_true(
  assertthat::is.count(
    get_seed(
      n2k_lrt_glmer(
        parent = get_file_fingerprint(object.1),
        parent.0 = get_file_fingerprint(object.0),
        parent.status = this.parent.status,
        scheme.id = this.scheme.id,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        formula = this.formula,
        first.imported.year = this.first.imported.year,
        last.imported.year = this.last.imported.year,
        analysis.date = this.analysis.date
      )
    )
  )
)
