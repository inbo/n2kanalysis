context("get_model_parameter")
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
    data = dataset,
    formula = "Count ~ A + C",
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    family = "nbinomial",
    model_type = this_model_type,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    last_analysed_year = this_last_analysed_year,
    analysis_date = this_analysis_date,
    seed = this_seed,
    parent = this_parent,
    duration = this_duration
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
    "reading model parameters"
  )
  expect_is(param, "n2kParameter")
  expect_identical(
    param@Parameter %>%
      semi_join(
        tibble(Description = "Random effect BLUP"),
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
        tibble(Description = "Random effect variance"),
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
        tibble(Description = "Fixed effect"),
        by = "Description"
      ),
    by = c("Parent" = "Fingerprint")
  ) %>%
    select("Fingerprint", Main = "Description") %>%
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
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    family = "nbinomial",
    model_type = this_model_type,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    last_analysed_year = this_last_analysed_year,
    analysis_date = this_analysis_date,
    seed = this_seed,
    data = dataset,
    parent = this_parent,
    duration = this_duration
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
        tibble(Description = "Random effect variance"),
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
        tibble(Description = "Fixed effect"),
        by = "Description"
      ),
    by = c("Parent" = "Fingerprint")
  ) %>%
    select("Fingerprint", Main = "Description") %>%
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
          tibble(Description = "Random effect BLUP"),
          by = "Description"
        ),
    by = c("Parent" = "Fingerprint")
  ) %>%
    select("Fingerprint", Main = "Description") %>%
    left_join(
      param@Parameter %>%
        rename(Finger = "Fingerprint", Level = "Description"),
      by = c("Fingerprint" = "Parent")
    ) %>%
    left_join(
      param@Parameter %>%
        select("Parent", Level2 = "Description"),
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
    f(E, model = \"rw1\", replicate = as.integer(A)) +
    f(F, model = \"iid\")",
    result_datasource_id = this_result_datasource_id,
    scheme_id = this_scheme_id,
    species_group_id = this_species_group_id,
    location_group_id = this_location_group_id,
    family = "nbinomial",
    model_type = this_model_type,
    first_imported_year = this_first_imported_year,
    last_imported_year = this_last_imported_year,
    last_analysed_year = this_last_analysed_year,
    analysis_date = this_analysis_date,
    seed = this_seed,
    data = dataset,
    parent = this_parent,
    duration = this_duration
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
        tibble(Description = "Random effect variance"),
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
        tibble(Description = "Fixed effect"),
        by = "Description"
      ),
    by = c("Parent" = "Fingerprint")
  ) %>%
    select("Fingerprint", Main = "Description") %>%
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
        tibble(Description = "Random effect BLUP"),
        by = "Description"
      ),
    by = c("Parent" = "Fingerprint")
  ) %>%
    select("Fingerprint", Main = "Description") %>%
    left_join(
      param@Parameter %>%
        rename(Finger = "Fingerprint", Level = "Description"),
      by = c("Fingerprint" = "Parent")
    ) %>%
    left_join(
      param@Parameter %>%
        select("Parent", Level2 = "Description", Finger2 = "Fingerprint"),
      by = c("Finger" = "Parent")
    )
  expect_false(any(is.na(random$Level)))
  expect_equal(
    random %>%
      group_by(.data$Main, .data$Level) %>%
      summarise(
        N = n(),
        Missing = mean(is.na(.data$Level2))
      ) %>%
      summarise(
        N1 = n(),
        N2 = mean(.data$N),
        Missing = mean(.data$Missing)
      ),
    tibble(
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
          mutate(
            Finger2 = ifelse(is.na(.data$Finger2), .data$Finger, .data$Finger2)
          ),
        by = c("Parameter" = "Finger2")
      ) %>%
      nrow(),
    nrow(random)
  )
})
