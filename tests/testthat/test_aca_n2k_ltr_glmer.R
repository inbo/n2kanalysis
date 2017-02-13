context("prepare a n2kLrtGlmer object")
this.seed <- 50L
this.result.datasource.id <- sha1(sample(letters))
this.scheme.id <- sha1(sample(letters))
this.species.group.id <- sha1(sample(letters))
this.location.group.id <- sha1(sample(letters))
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
cbpp$DatasourceID <- sha1(letters)
cbpp$ObservationID <- seq_len(nrow(cbpp))
object.1 <- n2k_glmer_poisson(
  result.datasource.id = this.result.datasource.id,
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
  result.datasource.id = this.result.datasource.id,
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
  ParentStatus = c(status(object.1), status(object.0)),
  stringsAsFactors = FALSE
)
this.parent.status.factor <- data.frame(
  ParentAnalysis = c(
    get_file_fingerprint(object.1),
    get_file_fingerprint(object.0)
  ),
  ParentStatusFingerprint = c(
    get_status_fingerprint(object.1),
    get_status_fingerprint(object.0)
  ),
  ParentStatus = c(status(object.1), status(object.0)),
  stringsAsFactors = TRUE
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
    result.datasource.id = this.result.datasource.id,
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
        result.datasource.id = this.result.datasource.id,
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


## parent.status

### doesn't contain factors
expect_error(
  n2k_lrt_glmer(
    parent = get_file_fingerprint(object.1),
    parent.0 = get_file_fingerprint(object.0),
    parent.status = this.parent.status.factor,
    result.datasource.id = this.result.datasource.id,
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date
  ),
  "Wrong class for following variable\\(s\\)"
)
