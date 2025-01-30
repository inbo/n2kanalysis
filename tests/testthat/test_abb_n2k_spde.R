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
test_that("n2k_spde() creates the object", {
  expect_s4_class(
    spde_model <- spde(
      dataset[, c("C", "D")], range = c(0.5, 0.05), sigma = c(0.5, 0.05)
    ),
    "Spde"
  )
  expect_s4_class(
    object <- n2k_spde(
      result_datasource_id = this_result_datasource_id,
      scheme_id = this_scheme_id, species_group_id = this_species_group_id,
      location_group_id = this_location_group_id, model_type = this_model_type,
      formula = this_formula, spde = spde_model, data = dataset,
      first_imported_year = this_first_imported_year,
      last_imported_year = this_last_imported_year,
      analysis_date = this_analysis_date
    ),
    "n2kSpde"
  )
})
