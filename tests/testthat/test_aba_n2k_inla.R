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
  select(.data$A, .data$B, .data$C, .data$D) %>%
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
model_object <- inla(
  Count ~ A * (B + C) + C:D +
    f(E, model = "rw1", replicate = as.integer(A)) +
    f(G, model = "iid"),
  data = object@Data,
  family = "poisson"
)
model_truth <- inla(
  Count ~ A * (B + C) + C:D +
    f(E, model = "rw1", replicate = as.integer(A)) +
    f(G, model = "iid"),
  data = dataset,
  family = "poisson"
)
test_that("n2k_inla() adds the data as a data.frame", {
  expect_that(
    object@Data,
    is_identical_to(dataset)
  )
  expect_equal(
    model_object$summary.fixed,
    model_truth$summary.fixed,
    tolerance = 1e-5
  )
  expect_equal(
    model_object$summary.random,
    model_truth$summary.random,
    tolerance = 1e-3
  )
})
test_that("n2k_inla(), uses 'new' as default status", {
  expect_that(
    object@AnalysisMetadata$Status,
    is_identical_to("new")
  )
})
test_that("n2k_inla() requires a correct status", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      status = "junk"
    ),
    throws_error("Status must be one of the following")
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      status = NA_character_
    ),
    throws_error("Status must be one of the following")
  )
})
test_that("n2k_inla() checks the model type", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = "junk",
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date
    ),
    throws_error("ModelType should be 'inla poisson'")
  )
})
test_that("n2k_inla() sets the correct seed", {
  this_seed <- 12345L
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      seed = this_seed
    )@AnalysisMetadata$Seed,
    is_identical_to(this_seed)
  )
})
test_that("n2k_inla() converts numeric seed, when possible", {
  this_seed <- 12345
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      seed = this_seed
    )@AnalysisMetadata$Seed,
    is_identical_to(as.integer(this_seed))
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      seed = this_seed + 0.1
    ),
    throws_error("seed is not a count \\(a single positive integer\\)") # nolint: nonportable_path_linter, line_length_linter.
  )
})
test_that("n2k_inla() sets a random seed when not provided", {
  expect_that(
    object@AnalysisMetadata$Seed,
    is_a("integer")
  )
})

test_that("n2k_inla() sets the correct SchemeID", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$SchemeID,
    is_identical_to(this_scheme_id)
  )
})

test_that("n2k_inla() sets the correct SpeciesGroupID", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      formula = this_formula,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$SpeciesGroupID,
    is_identical_to(this_species_group_id)
  )
})

test_that("n2k_inla() sets the correct LocationGroupID", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$LocationGroupID,
    is_identical_to(this_location_group_id)
  )
})

test_that("n2k_inla() sets the correct FirstImportedYear", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$FirstImportedYear,
    is_identical_to(this_first_imported_year)
  )
})
test_that("n2k_inla() checks that FirstImportedYear is from the past", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = as.integer(format(Sys.time(), "%Y")) + 1,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("FirstImportedYear cannot exceed LastImportedYear")
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = as.integer(format(Sys.time(), "%Y")),
      last_imported_year = as.integer(format(Sys.time(), "%Y")),
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    is_a("n2kInla")
  )
})

test_that("n2k_inla() sets the correct LastImportedYear", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$LastImportedYear,
    is_identical_to(this_last_imported_year)
  )
})
test_that("n2k_inla() converts numeric last_imported_year, when possible", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = as.numeric(this_last_imported_year),
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$LastImportedYear,
    is_identical_to(this_last_imported_year)
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year + 0.1,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error(
      "last_imported_year is not a count \\(a single positive integer\\)" # nolint: nonportable_path_linter, line_length_linter.
    )
  )
})
test_that("n2k_inla() checks that LastImportedYear is from the past", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = as.integer(format(Sys.time(), "%Y")) + 1,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("LastImportedYear from the future")
  )
})
test_that(
  "n2k_inla() checks that LastImportedYear is not earlier that FirstImportedYear
  ", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = 2000,
      last_imported_year = 1999,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("FirstImportedYear cannot exceed LastImportedYear")
  )
})

test_that("n2k_inla() sets the correct Duration", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      duration = this_duration,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$Duration,
    is_identical_to(this_duration)
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$Duration,
    is_identical_to(this_last_imported_year - this_first_imported_year + 1L)
  )
})
test_that("n2k_inla() converts numeric duration, when possible", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      last_analysed_year = this_last_analysed_year,
      duration = as.numeric(this_duration),
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$Duration,
    is_identical_to(this_duration)
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      duration = this_duration + 0.1,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("duration is not a count \\(a single positive integer\\)") # nolint: nonportable_path_linter, line_length_linter.
  )
})
test_that(
"n2k_inla() checks that Duration is not outside the FirstImportYear -
LastImportedYear ranges", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = 1999,
      last_imported_year = 1999,
      duration = 2,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error(
"Duration longer than the interval from FirstImportedYear to LastImportedYear"
    )
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = 1999,
      last_imported_year = 1999,
      duration = 0,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error(
      "duration is not a count \\(a single positive integer\\)" # nolint: nonportable_path_linter, line_length_linter.
    )
  )
})

test_that("sets the correct LastAnalysedYear", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      last_analysed_year = this_last_analysed_year,
      duration = 1L,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$LastAnalysedYear,
    is_identical_to(this_last_analysed_year)
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$LastAnalysedYear,
    is_identical_to(this_last_imported_year)
  )
})
test_that("n2k_inla() converts numeric last_analysed_year, when possible", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      last_analysed_year = as.numeric(this_last_analysed_year),
      duration = 1,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    )@AnalysisMetadata$LastAnalysedYear,
    is_identical_to(this_last_analysed_year)
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      duration = 1,
      last_analysed_year = this_last_analysed_year + 0.1,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error(
      "last_analysed_year is not a count \\(a single positive integer\\)" # nolint: nonportable_path_linter, line_length_linter.
    )
  )
})
test_that("n2k_inla() checks that LastAnalysedYear is within the range", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      duration = 1,
      last_analysed_year = this_last_imported_year + 1,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("LastAnalysedYear cannot exceed LastImportedYear")
  )
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      last_analysed_year = this_first_imported_year + this_duration - 2,
      duration = this_duration,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error(
"LastAnalysedYear smaller than FirstImportedYear \\+ Duration \\- 1\\. Window
outside imported range\\."
    )
  )
})

test_that("n2k_inla() checks if analysis date is from the past", {
  expect_that(
    n2k_inla(
      data = dataset,
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = Sys.time() + 24 * 60 * 60,
      scheme_id = this_scheme_id
    ),
    throws_error("AnalysisDate must be in the past")
  )
})
test_that(
  "n2k_inla() checks if all variable in formula are available in the data", {
  expect_that(
    n2k_inla(
      data = dataset[, c("A", "B", "C", "D", "E", "G")],
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("Response variable is missing from data")
  )
  expect_that(
    n2k_inla(
      data = dataset[, c("Count", "B", "C", "D", "E", "G")],
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("object@Data does not have .*name.*A")
  )
  expect_that(
    n2k_inla(
      data = dataset[, c("Count", "A", "B", "C", "D", "E")],
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("object@Data does not have .*name.*G")
  )
  expect_that(
    n2k_inla(
      data = dataset[, c("A", "B", "C", "Count", "E", "G")],
      result_datasource_id = this_result_datasource_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      scheme_id = this_scheme_id
    ),
    throws_error("object@Data does not have .*name.*D")
  )
})

object_model <- n2k_inla(
  data = object, model_fit = model_object, status = "converged"
)
test_that("n2k_inla() keeps the objects", {
  expect_that(
    object_model@Data,
    is_identical_to(dataset)
  )
  expect_equal(
    object_model@Model$summary.fixed,
    model_truth$summary.fixed,
    tolerance = 1e-5
  )
  expect_equal(
    object_model@Model$summary.random,
    model_truth$summary.random,
    tolerance = 1e-3
  )
  expect_that(
    object_model@Model,
    is_identical_to(model_object)
  )
  expect_that(
    object_model@AnalysisMetadata$Seed,
    is_identical_to(object@AnalysisMetadata$Seed)
  )
  expect_that(
    n2k_inla(
      data = object, model_fit = model_object, status = "converged", seed = 1
    )@AnalysisMetadata$Seed,
    is_identical_to(object@AnalysisMetadata$Seed)
  )
  expect_that(
    object_model@AnalysisMetadata$SchemeID,
    is_identical_to(object@AnalysisMetadata$SchemeID)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      scheme_id = 999
    )@AnalysisMetadata$SchemeID,
    is_identical_to(object@AnalysisMetadata$SchemeID)
  )
  expect_that(
    object_model@AnalysisMetadata$SpeciesGroupID,
    is_identical_to(object@AnalysisMetadata$SpeciesGroupID)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      species_group_id = sha1(LETTERS)
    )@AnalysisMetadata$SpeciesGroupID,
    is_identical_to(object@AnalysisMetadata$SpeciesGroupID)
  )
  expect_that(
    object_model@AnalysisMetadata$LocationGroupID,
    is_identical_to(object@AnalysisMetadata$LocationGroupID)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      location_group_id = sha1(LETTERS)
    )@AnalysisMetadata$LocationGroupID,
    is_identical_to(object@AnalysisMetadata$LocationGroupID)
  )
  expect_that(
    object_model@AnalysisMetadata$ModelType,
    is_identical_to(object@AnalysisMetadata$ModelType)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      model_type = 999
    )@AnalysisMetadata$ModelType,
    is_identical_to(object@AnalysisMetadata$ModelType)
  )
  expect_that(
    object_model@AnalysisMetadata$Covariate,
    is_identical_to(object@AnalysisMetadata$Covariate)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      covariate = 999
    )@AnalysisMetadata$Covariate,
    is_identical_to(object@AnalysisMetadata$Covariate)
  )
  expect_that(
    object_model@AnalysisMetadata$FirstImportedYear,
    is_identical_to(object@AnalysisMetadata$FirstImportedYear)
  )
  expect_that(
    object_model@AnalysisMetadata$Duration,
    is_identical_to(object@AnalysisMetadata$Duration)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      duration = 999
    )@AnalysisMetadata$Duration,
    is_identical_to(object@AnalysisMetadata$Duration)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      first_imported_year = 999
    )@AnalysisMetadata$FirstImportedYear,
    is_identical_to(object@AnalysisMetadata$FirstImportedYear)
  )
  expect_that(
    object_model@AnalysisMetadata$LastImportedYear,
    is_identical_to(object@AnalysisMetadata$LastImportedYear)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      last_imported_year = 999
    )@AnalysisMetadata$LastImportedYear,
    is_identical_to(object@AnalysisMetadata$LastImportedYear)
  )
  expect_that(
    object_model@AnalysisMetadata$LastAnalysedYear,
    is_identical_to(object@AnalysisMetadata$LastAnalysedYear)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      last_analysed_year = 3000
    )@AnalysisMetadata$LastAnalysedYear,
    is_identical_to(object@AnalysisMetadata$LastAnalysedYear)
  )
  expect_that(
    object_model@AnalysisMetadata$AnalysisDate,
    is_identical_to(object@AnalysisMetadata$AnalysisDate)
  )
  expect_that(
    n2k_inla(
      data = object,
      model_fit = model_object,
      status = "converged",
      analysis.data = 999
    )@AnalysisMetadata$AnalysisDate,
    is_identical_to(object@AnalysisMetadata$AnalysisDate)
  )
})
test_that("n2k_inla() stores the new status", {
  expect_that(
    object_model@AnalysisMetadata$Status,
    is_identical_to("converged")
  )
  expect_that(
    n2k_inla(
      data = object, model_fit = model_object, status = "junk"
    ),
    throws_error("Status must be one of the following")
  )
})
model_other <- inla(
  Count ~ A * (B + C) + C:D +
    f(E, model = "rw1", replicate = as.integer(A)) +
    f(G, model = "iid"),
  data = object@Data,
  family = "nbinomial"
)
test_that("n2k_inla() checks if the family matches", {
  expect_that(
    n2k_inla(
      data = object, model_fit = model_other, status = "converged"
    ),
    throws_error("Model of the wrong family")
  )
})
test_that("n2kInla handles linear combinations", {
  expect_error(
    n2k_inla(
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      data = dataset,
      lin_comb = "junk"
    ),
    "lin_comb must be either a list or a matrix"
  )
  expect_is(
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
      data = dataset,
      lin_comb = this_lc
    ),
    "n2kInla"
  )
  expect_identical(
    object@LinearCombination,
    this_lc
  )

  expect_error(
    n2k_inla(
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      data = dataset,
      lin_comb = this_lc,
      replicate_name = "junk"
    ),
    "replicate\\_name is not a list" # nolint: nonportable_path_linter.
  )
  expect_error(
    n2k_inla(
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id,
      species_group_id = this_species_group_id,
      location_group_id = this_location_group_id,
      model_type = this_model_type,
      formula = this_formula,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date,
      data = dataset,
      lin_comb = this_lc,
      replicate_name = list("junk")
    ),
    "replicate\\_name must have names" # nolint: nonportable_path_linter.
  )
})
