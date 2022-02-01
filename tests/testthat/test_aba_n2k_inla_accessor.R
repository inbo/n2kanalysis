test_that("get_model() handles n2kInla objects", {
  dataset <- test_data()
  object <- n2k_inla(
    result_datasource_id = sha1(letters), scheme_id = sha1(letters),
    species_group_id = sha1(letters), location_group_id = sha1(letters),
    model_type = "inla poisson: A", formula = "Count ~ A",
    first_imported_year = 1990, last_imported_year = 2015,
    analysis_date = as.POSIXct("2000-01-01 12:13:14", tz = "UTC"),
    data = dataset
  )
  model_object <- inla( Count ~ A, data = object@Data, family = "poisson")
  object_model <- n2k_inla(
    data = object, model_fit = model_object, status = "converged"
  )
  expect_that(get_model(object), is_identical_to(object@Model))
  expect_that(get_model(object_model), is_identical_to(object_model@Model))
})
