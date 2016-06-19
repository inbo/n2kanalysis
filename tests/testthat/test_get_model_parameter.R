context("get_model_parameter")
test_that(
  "get_model_parameter works with n2kGlmerPoisson", {
  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- 1
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
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
  expect_error(
    get_model_parameter(object, verbose = NA),
    "verbose contains 1 missing values"
  )
  expect_error(
    get_model_parameter(object, verbose = ""),
    "verbose is not a flag"
  )
  expect_message(
    get_model_parameter(object, verbose = TRUE),
    NA
  )
  expect_message(
    get_model_parameter(object, verbose = FALSE),
    NA
  )
  expect_is(
    param <- get_model_parameter(object),
    "n2kParameter"
  )
  expect_identical(nrow(param@Parameter), 0L)
  expect_identical(nrow(param@ParameterEstimate), 0L)

  object <- fit_model(object)
  expect_message(
    get_model_parameter(object, verbose = TRUE),
    "reading model parameters: fixed effects"
  )
  expect_is(
    param <- get_model_parameter(object, verbose = FALSE),
    "n2kParameter"
  )
  expect_true(
    all(
      c(
        "Fixed effect", "Random effect BLUP", "Random effect variance", "Fitted"
      ) %in%
        param@Parameter$Description
    )
  )
  expect_true(
    all(
      lme4::fixef(get_model(object)) %in% param@ParameterEstimate$Estimate
    )
  )
  expect_true(
    all(
      lme4::ranef(get_model(object))$herd[, 1] %in%
        param@ParameterEstimate$Estimate
    )
  )

  object <- n2k_glmer_poisson(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = "incidence ~ offset(log(size)) + (1|herd)",
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    last.analysed.year = this.last.analysed.year,
    analysis.date = this.analysis.date,
    seed = this.seed,
    data = cbpp,
    parent = this.parent,
    this.duration
  )
  object <- fit_model(object)
  expect_is(
    param <- get_model_parameter(object, verbose = FALSE),
    "n2kParameter"
  )

  object <- n2k_glmer_poisson(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = "incidence ~ size:period + (1|herd)",
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    last.analysed.year = this.last.analysed.year,
    analysis.date = this.analysis.date,
    seed = this.seed,
    data = cbpp,
    parent = this.parent,
    this.duration
  )
  object <- fit_model(object)
  expect_is(
    param <- get_model_parameter(object, verbose = FALSE),
    "n2kParameter"
  )
  expect_true("size:period" %in% param@Parameter$Description)
})

test_that("get_model_parameter works with n2kInlaNbinomial", {
  dataset <- n2kanalysis:::test_data()
  this.analysis.date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this.scheme.id <- 1L
  this.species.group.id <- 2L
  this.location.group.id <- 3L
  this.seed <- 4L
  this.model.type <- "inla nbinomial: A * B + E"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2015L
  this.parent <- "abcdef"
  this.duration <- this.last.imported.year - this.first.imported.year + 1
  analysis <- n2k_inla_nbinomial(
    formula = "Count ~ A + C",
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    last.analysed.year = this.last.analysed.year,
    analysis.date = this.analysis.date,
    seed = this.seed,
    data = dataset,
    parent = this.parent,
    this.duration
  )
  expect_is(
    param <- get_model_parameter(analysis),
    "n2kParameter"
  )
  expect_identical(nrow(param@Parameter), 0L)
  expect_identical(nrow(param@ParameterEstimate), 0L)

  analysis <- fit_model(analysis)
  expect_message(
    param <- get_model_parameter(analysis, verbose = TRUE),
    "reading model parameters: fixed effects"
  )
  expect_is(param, "n2kParameter")
  expect_identical(
    param@Parameter %>%
      semi_join(
        data_frame(Description = "Random effect BLUP"),
        by = "Description"
      ) %>%
      inner_join(
        param@Parameter,
        by = c("Fingerprint" = "Parent")
      ) %>%
     nrow(),
    0L
  )
  expect_identical(
    param@Parameter %>%
      semi_join(
        data_frame(Description = "Random effect variance"),
        by = "Description"
      ) %>%
      inner_join(
        param@Parameter,
        by = c("Fingerprint" = "Parent")
      ) %>%
     nrow(),
    0L
  )
  fixed <- param@Parameter %>%
    semi_join(
    param@Parameter %>%
      semi_join(
        data_frame(Description = "Fixed effect"),
        by = "Description"
      ),
    by = c("Parent" = "Fingerprint")
  ) %>%
    select_(~Fingerprint, Main = ~Description) %>%
    left_join(
      param@Parameter,
      by = c("Fingerprint" = "Parent")
    )
  expect_identical(nrow(fixed), 4L)
  expect_identical(
    fixed$Main == "A",
    !is.na(fixed$Description)
  )
  expect_identical(
    fixed$Main == "A",
    !is.na(fixed$Fingerprint.y)
  )
})
