test_that(
  "n2kInla with categorical and numeric fixed effect without random
    effect", {
  dataset <- test_data()
  this_analysis_date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this_result_datasource_id <- sha1(sample(letters))
  this_scheme_id <- sha1(sample(letters))
  this_species_group_id <- sha1(sample(letters))
  this_location_group_id <- sha1(sample(letters))
  this_seed <- 4L
  this_model_type <- "inla nbinomial: A * B + E"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2015L
  this_parent <- "abcdef"
  this_duration <- this_last_imported_year - this_first_imported_year + 1
  analysis <- n2k_inla(
    data = dataset, formula = "Count ~ A + C",
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, family = "nbinomial",
    model_type = this_model_type,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    last_analysed_year = this_last_analysed_year,
    analysis_date = this_analysis_date, seed = this_seed, parent = this_parent,
    duration = this_duration
  )
  expect_is(param <- get_model_parameter(analysis), "n2kParameter")
  expect_identical(nrow(param@Parameter), 0L)
  expect_identical(nrow(param@ParameterEstimate), 0L)

  analysis <- fit_model(analysis)
  expect_message(
    param <- get_model_parameter(analysis, verbose = TRUE),
    "reading model parameters"
  )
  expect_is(param, "n2kParameter")
  expect_identical(
    param@Parameter %>%
      semi_join(
        tibble(description = "Random effect BLUP"), by = "description"
      ) %>%
      inner_join(param@Parameter, by = c("fingerprint" = "parent")) %>%
     nrow(),
    0L
  )
  expect_identical(
    param@Parameter %>%
      semi_join(
        tibble(description = "Random effect variance"), by = "description"
      ) %>%
      inner_join(param@Parameter, by = c("fingerprint" = "parent")) %>%
     nrow(),
    0L
  )
  fixed <- param@Parameter %>%
    semi_join(
    param@Parameter %>%
      semi_join(tibble(description = "Fixed effect"), by = "description"),
    by = c("parent" = "fingerprint")
  ) %>%
    select("fingerprint", main = "description") %>%
    left_join(param@Parameter, by = c("fingerprint" = "parent"))
  expect_identical(nrow(fixed), 4L)
  expect_identical(fixed$main == "A", !is.na(fixed$description))
  expect_identical(fixed$main == "A", !is.na(fixed$fingerprint.y))
})

test_that(
  "n2kInla with single random effect, categorical-categorical
      interaction and categorical numeric interaction", {
  dataset <- test_data()
  this_analysis_date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this_result_datasource_id <- sha1(sample(letters))
  this_scheme_id <- sha1(sample(letters))
  this_species_group_id <- sha1(sample(letters))
  this_location_group_id <- sha1(sample(letters))
  this_seed <- 4L
  this_model_type <- "inla nbinomial: A * B + E"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2015L
  this_parent <- "abcdef"
  this_duration <- this_last_imported_year - this_first_imported_year + 1
  analysis <- n2k_inla(
    formula = "Count ~ 0 + A * C + A * B + f(E, model = \"iid\")",
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, family = "nbinomial",
    model_type = this_model_type,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    last_analysed_year = this_last_analysed_year,
    analysis_date = this_analysis_date, seed = this_seed, data = dataset,
    parent = this_parent, duration = this_duration
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
        tibble(description = "Random effect variance"), by = "description"
      ) %>%
      inner_join(param@Parameter, by = c("fingerprint" = "parent")) %>%
     nrow(),
    1L
  )
  fixed <- param@Parameter %>%
    semi_join(
    param@Parameter %>%
      semi_join(
        tibble(description = "Fixed effect"),
        by = "description"
      ),
    by = c("parent" = "fingerprint")
  ) %>%
    select("fingerprint", main = "description") %>%
    left_join(
      param@Parameter,
      by = c("fingerprint" = "parent")
    )
  expect_identical(nrow(fixed), 9L)
  expect_identical(grepl(":", fixed$main), grepl(":", fixed$description))
  expect_identical(fixed$main == "C", is.na(fixed$fingerprint.y))
  expect_identical(fixed$main == "A:C", grepl(".+:$", fixed$description))
  expect_identical(fixed$main == "A:B", grepl(".+:.+$", fixed$description))
  random <- param@Parameter %>%
    semi_join(
      param@Parameter %>%
        semi_join(
          tibble(description = "Random effect BLUP"), by = "description"
        ),
    by = c("parent" = "fingerprint")
  ) %>%
    select("fingerprint", Main = "description") %>%
    left_join(
      param@Parameter %>%
        rename(finger = "fingerprint", level = "description"),
      by = c("fingerprint" = "parent")
    ) %>%
    left_join(
      param@Parameter %>%
        select("parent", Level2 = "description"),
      by = c("finger" = "parent")
    )
  expect_false(any(is.na(random$level)))
  expect_true(all(is.na(random$level2)))
  expect_identical(
    dataset$E %>%
      unique() %>%
      sort() %>%
      as.character(),
    random$level
  )
  expect_identical(
    param@ParameterEstimate %>%
      inner_join(
        random,
        by = c("parameter" = "finger")
      ) %>%
      nrow(),
    nrow(random)
  )
})

test_that(
  "n2kInla with numeric-numeric interaction and two random effects of
    which on replicated", {
  dataset <- test_data()
  this_analysis_date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this_result_datasource_id <- sha1(sample(letters))
  this_scheme_id <- sha1(sample(letters))
  this_species_group_id <- sha1(sample(letters))
  this_location_group_id <- sha1(sample(letters))
  this_seed <- 4L
  this_model_type <- "inla nbinomial: A * B + E"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2015L
  this_parent <- "abcdef"
  this_duration <- this_last_imported_year - this_first_imported_year + 1
  analysis <- n2k_inla(
    formula = "Count ~ C * D +
    f(E, model = \"rw1\", replicate = as.integer(A))",
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id, species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, family = "nbinomial",
    model_type = this_model_type,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    last_analysed_year = this_last_analysed_year,
    analysis_date = this_analysis_date, seed = this_seed, data = dataset,
    parent = this_parent, duration = this_duration
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
        tibble(description = "Random effect variance"), by = "description"
      ) %>%
      inner_join(param@Parameter, by = c("fingerprint" = "parent")) %>%
     nrow(),
    1L
  )
  fixed <- param@Parameter %>%
    semi_join(
    param@Parameter %>%
      semi_join(tibble(description = "Fixed effect"), by = "description"),
    by = c("parent" = "fingerprint")
  ) %>%
    select("fingerprint", main = "description") %>%
    left_join(param@Parameter, by = c("fingerprint" = "parent"))
  expect_identical(nrow(fixed), 4L)
  grepl(":", fixed$description) %>%
    any() %>%
    expect_false()
  grep(":", fixed$main) %>%
    length() %>%
    expect_identical(1L)
  expect_identical(is.na(fixed$description), is.na(fixed$fingerprint.y))
  fixed$description %>%
    is.na() %>%
    all() %>%
    expect_true()
  random <- param@Parameter %>%
    semi_join(
    param@Parameter %>%
      semi_join(tibble(description = "Random effect BLUP"), by = "description"),
    by = c("parent" = "fingerprint")
  ) %>%
    select("fingerprint", main = "description") %>%
    left_join(
      param@Parameter %>%
        rename(finger = "fingerprint", level = "description"),
      by = c("fingerprint" = "parent")
    ) %>%
    left_join(
      param@Parameter %>%
        select("parent", level2 = "description", finger2 = "fingerprint"),
      by = c("finger" = "parent")
    )
  expect_false(any(is.na(random$level)))
  expect_equal(
    random %>%
      group_by(.data$main, .data$level) %>%
      summarise(
        n = n(), missing = mean(is.na(.data$level2)), .groups = "drop_last"
      ) %>%
      summarise(
        n1 = n(), n2 = mean(.data$n), missing = mean(.data$missing),
        .groups = "drop"
      ),
    tibble(main = "E", n1 = 3L, n2 = 10, missing = 0)
  )
  expect_identical(
    param@ParameterEstimate %>%
      inner_join(
        random %>%
          mutate(
            finger2 = ifelse(is.na(.data$finger2), .data$finger, .data$finger2)
          ),
        by = c("parameter" = "finger2")
      ) %>%
      nrow(),
    nrow(random)
  )
})

test_that("imputation and aggregation", {
  set.seed(20191213)
  this_result_datasource_id <- sha1(letters)
  this_scheme_id <- sha1(letters)
  this_species_group_id <- sha1(letters)
  this_location_group_id <- sha1(letters)
  this_analysis_date <- Sys.time()
  this_model_type <- "inla poisson: A * (B + C) + C:D"
  this_first_imported_year <- 1990L
  this_last_imported_year <- 2015L
  this_last_analysed_year <- 2014L
  this_duration <- 1L
  dataset <- test_data(missing = 0.2)
  base <- tempfile("imputation")
  dir.create(base)
  project <- "imputation"

  imputation <- n2k_inla(
    data = dataset, scheme_id = this_scheme_id,
    result_datasource_id = this_result_datasource_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    first_imported_year = this_first_imported_year, imputation_size = 3,
    last_imported_year = this_last_imported_year, family = "poisson",
    last_analyses_year = this_last_analysed_year, duration = this_duration,
    formula = "Count ~ A + f(E, model = \"iid\")", analysis_date = Sys.time(),
  )
  aggregation <- n2k_aggregate(
    scheme_id = this_scheme_id,
    result_datasource_id = this_result_datasource_id, formula = "~ A + B",
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id, model_type = this_model_type,
    first_imported_year = this_first_imported_year, analysis_date = Sys.time(),
    last_imported_year = this_last_imported_year, fun = sum,
    last_analyses_year = this_last_analysed_year, duration = this_duration,
    parent = get_file_fingerprint(imputation)
  )
  expect_is(result <- get_model_parameter(imputation), "n2kParameter")
  expect_equal(nrow(result@Parameter), 0L)
  expect_is(result <- get_model_parameter(aggregation), "n2kParameter")
  expect_equal(nrow(result@Parameter), 0L)

  suppressWarnings({
    imputation <- fit_model(imputation, parallel_configs = FALSE)
  })
  store_model(imputation, base = base, project = project)
  aggregation <- fit_model(aggregation, base = base, project = project)
  expect_is(result <- get_model_parameter(imputation), "n2kParameter")
  expect_equal(nrow(result@Parameter), 1956L)
  expect_is(result <- get_model_parameter(aggregation), "n2kParameter")
  expect_equal(nrow(result@Parameter), 14L)

  store_model(aggregation, base = base, project = project)
  extractor <- function(model) {
    model$summary.fixed[, c("mean", "sd")]
  }
  mi <- n2k_model_imputed(
    scheme_id = this_scheme_id, model_args = list(family = "poisson"),
    result_datasource_id = this_result_datasource_id, model_fun = INLA::inla,
    species_group_id = this_species_group_id, extractor = extractor,
    location_group_id = this_location_group_id, model_type = this_model_type,
    first_imported_year = this_first_imported_year, analysis_date = Sys.time(),
    last_imported_year = this_last_imported_year, formula = "~ A",
    last_analyses_year = this_last_analysed_year, duration = this_duration,
    parent = get_file_fingerprint(aggregation)
  )
  expect_is(result <- get_model_parameter(mi), "n2kParameter")
  expect_equal(nrow(result@Parameter), 0L)

  mi <- fit_model(mi, base = base, project = project)
  expect_is(result <- get_model_parameter(mi), "n2kParameter")
  expect_equal(nrow(result@Parameter), 4L)
})
