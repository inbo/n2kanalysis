this_result_datasource_id <- sha1(sample(letters))
this_scheme_id <- sha1(sample(letters))
this_species_group_id <- sha1(sample(letters))
this_location_group_id <- sha1(sample(letters))
this_analysis_date <- Sys.time()
this_model_type <- "inla poisson: A * (B + C) + C:D"
this_formula <-
  "Count ~ A * (B + C) + C:D +
    f(E, model = \"rw1\", replicate = as.integer(A)) +
    f(G, model = \"iid\")"
this_first_imported_year <- 1990L
this_last_imported_year <- 2015L
this_last_analysed_year <- 2014L
this_duration <- 1L
dataset <- test_data()
this_lc <- dataset %>%
  select("A", "B", "C", "D") %>%
  filter(.data$C == max(.data$C), .data$D == max(.data$D)) %>%
  distinct() %>%
  model.matrix(object = ~A * (B + C) + C:D)
object <- n2k_inla(
  result_datasource_id = this_result_datasource_id,
  scheme_id = this_scheme_id,
  species_group_id = this_species_group_id,
  location_group_id = this_location_group_id,
  model_type = this_model_type,
  formula = this_formula,
  first_imported_year = this_first_imported_year,
  last_imported_year = this_last_imported_year,
  analysis_date = this_analysis_date,
  data = dataset
)
object_model <- fit_model(object)

test_that("status() returns the status of n2kModels", {
  expect_that(
    status(object), is_identical_to(object@AnalysisMetadata$status)
  )
  expect_that(
    status(object_model), is_identical_to(object_model@AnalysisMetadata$status)
  )
})
test_that("status() updates the status of n2kModels", {
  status(object) <- "error"
  expect_that(status(object), is_identical_to("error"))
  expect_that(
    status(object) <- "junk",
    throws_error("status must be one of the following")
  )
})

test_that("get_data() returns the Data slot", {
  expect_that(get_data(object), is_identical_to(dataset))
  expect_that(get_data(object_model), is_identical_to(dataset))
})

test_that("get_seed() returns the seed slot", {
  expect_that(get_seed(object), is_identical_to(object@AnalysisMetadata$seed))
})

test_that("get_scheme_id() returns the scheme_id slot", {
  expect_that(
    get_scheme_id(object), is_identical_to(object@AnalysisMetadata$scheme_id)
  )
})

test_that("get_species_group_id() returns the species_group_id slot", {
  expect_that(
    get_species_group_id(object),
    is_identical_to(object@AnalysisMetadata$species_group_id)
  )
})

test_that("get_location_group_id() returns the location_group_id slot", {
  expect_that(
    get_location_group_id(object),
    is_identical_to(object@AnalysisMetadata$location_group_id)
  )
})

test_that("get_analysis_date() returns the analysis_date slot", {
  expect_that(
    get_analysis_date(object),
    is_identical_to(object@AnalysisMetadata$analysis_date)
  )
})

test_that("get_model_type() returns the model_type slot", {
  expect_that(
    get_model_type(object),
    is_identical_to(object@AnalysisMetadata$model_type)
  )
})

test_that("get_formula() returns the AnalysisFormula slot", {
  expect_that(get_formula(object), is_identical_to(object@AnalysisFormula))
})

test_that("get_file_fingerprint() returns the file_fingerprint slot", {
  expect_that(
    get_file_fingerprint(object),
    is_identical_to(object@AnalysisMetadata$file_fingerprint)
  )
})

test_that("get_status_fingerprint() returns the status_fingerprint slot", {
  expect_that(
    get_status_fingerprint(object),
    is_identical_to(object@AnalysisMetadata$status_fingerprint)
  )
})
