context("get_model_parameter")
test_that(
  "get_model_parameter works with n2kGlmerPoisson", {
  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
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
    result.datasource.id = this.result.datasource.id,
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
    result.datasource.id = this.result.datasource.id,
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

test_that(
  "n2kInlaNbinomial with categorical and numeric fixed effect without random
    effect", {
  dataset <- test_data()
  this.analysis.date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this.result.datasource.id <- sha1(sample(letters))
  this.scheme.id <- sha1(sample(letters))
  this.species.group.id <- sha1(sample(letters))
  this.location.group.id <- sha1(sample(letters))
  this.seed <- 4L
  this.model.type <- "inla nbinomial: A * B + E"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2015L
  this.parent <- "abcdef"
  this.duration <- this.last.imported.year - this.first.imported.year + 1
  analysis <- n2k_inla_nbinomial(
    formula = "Count ~ A + C",
    result.datasource.id = this.result.datasource.id,
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

test_that(
  "n2kInlaNbinomial with single random effect, categorical-categorical
      interaction and categorical numeric interaction", {
  dataset <- test_data()
  this.analysis.date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this.result.datasource.id <- sha1(sample(letters))
  this.scheme.id <- sha1(sample(letters))
  this.species.group.id <- sha1(sample(letters))
  this.location.group.id <- sha1(sample(letters))
  this.seed <- 4L
  this.model.type <- "inla nbinomial: A * B + E"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2015L
  this.parent <- "abcdef"
  this.duration <- this.last.imported.year - this.first.imported.year + 1
  analysis <- n2k_inla_nbinomial(
    formula = "Count ~ 0 + A * C + A * B + f(E, model = \"iid\")",
    result.datasource.id = this.result.datasource.id,
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
  analysis <- fit_model(analysis)
  expect_message(
    param <- get_model_parameter(analysis, verbose = TRUE),
    "reading model parameters: fixed effects"
  )
  expect_is(param, "n2kParameter")
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
    1L
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
  expect_identical(nrow(fixed), 9L)
  expect_identical(
    grepl(":", fixed$Main),
    grepl(":", fixed$Description)
  )
  expect_identical(
    fixed$Main == "C",
    is.na(fixed$Fingerprint.y)
  )
  expect_identical(
    fixed$Main == "A:C",
    grepl(".+:$", fixed$Description)
  )
  expect_identical(
    fixed$Main == "A:B",
    grepl(".+:.+$", fixed$Description)
  )
  random <- param@Parameter %>%
    semi_join(
    param@Parameter %>%
      semi_join(
        data_frame(Description = "Random effect BLUP"),
        by = "Description"
      ),
    by = c("Parent" = "Fingerprint")
  ) %>%
    select_(~Fingerprint, Main = ~Description) %>%
    left_join(
      param@Parameter %>%
        rename_(Finger = ~ Fingerprint, Level = ~Description),
      by = c("Fingerprint" = "Parent")
    ) %>%
    left_join(
      param@Parameter %>%
        select_(~ Parent, Level2 = ~Description),
      by = c("Finger" = "Parent")
    )
  expect_false(any(is.na(random$Level)))
  expect_true(all(is.na(random$Level2)))
  expect_identical(
    dataset$E %>%
      unique() %>%
      sort() %>%
      as.character(),
    random$Level
  )
  expect_identical(
    param@ParameterEstimate %>%
      inner_join(
        random,
        by = c("Parameter" = "Finger")
      ) %>%
      nrow(),
    nrow(random)
  )
})

test_that(
  "n2kInlaNbinomial with numeric-numeric interaction and two random effects of
    which on replicated", {
  dataset <- test_data()
  this.analysis.date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this.result.datasource.id <- sha1(sample(letters))
  this.scheme.id <- sha1(sample(letters))
  this.species.group.id <- sha1(sample(letters))
  this.location.group.id <- sha1(sample(letters))
  this.seed <- 4L
  this.model.type <- "inla nbinomial: A * B + E"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2015L
  this.parent <- "abcdef"
  this.duration <- this.last.imported.year - this.first.imported.year + 1
  analysis <- n2k_inla_nbinomial(
    formula = "Count ~ C * D +
    f(E, model = \"rw1\", replicate = as.integer(A)) +
    f(F, model = \"iid\")",
    result.datasource.id = this.result.datasource.id,
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
  analysis <- fit_model(analysis)
  expect_message(
    param <- get_model_parameter(analysis, verbose = TRUE),
    "reading model parameters: fixed effects"
  )
  expect_is(param, "n2kParameter")
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
    2L
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
  grepl(":", fixed$Description) %>%
    any() %>%
    expect_false()
  grep(":", fixed$Main) %>%
    length() %>%
    expect_identical(1L)
  expect_identical(
    is.na(fixed$Description),
    is.na(fixed$Fingerprint.y)
  )
  fixed$Description %>%
    is.na() %>%
    all() %>%
    expect_true()
  random <- param@Parameter %>%
    semi_join(
    param@Parameter %>%
      semi_join(
        data_frame(Description = "Random effect BLUP"),
        by = "Description"
      ),
    by = c("Parent" = "Fingerprint")
  ) %>%
    select_(~Fingerprint, Main = ~Description) %>%
    left_join(
      param@Parameter %>%
        rename_(Finger = ~ Fingerprint, Level = ~Description),
      by = c("Fingerprint" = "Parent")
    ) %>%
    left_join(
      param@Parameter %>%
        select_(~ Parent, Level2 = ~Description, Finger2 = ~Fingerprint),
      by = c("Finger" = "Parent")
    )
  expect_false(any(is.na(random$Level)))
  expect_equal(
    random %>%
      group_by_(~Main, ~Level) %>%
      summarise_(
        N = ~n(),
        Missing = ~mean(is.na(Level2))
      ) %>%
      group_by_(~Main) %>%
      summarise_(
        N1 = ~n(),
        N2 = ~mean(N),
        Missing = ~mean(Missing)
      ),
    data_frame(
      Main = c("E", "F"),
      N1 = 3L,
      N2 = c(10, 1),
      Missing = c(0, 1)
  )
  )
  expect_identical(
    param@ParameterEstimate %>%
      inner_join(
        random %>%
          mutate_(
            Finger2 = ~ifelse(is.na(Finger2), Finger, Finger2)
          ),
        by = c("Parameter" = "Finger2")
      ) %>%
      nrow(),
    nrow(random)
  )
})
