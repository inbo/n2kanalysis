test_that("elaborate comparison of sha1() on 32-bit and 64 bit", {
  this_analysis_date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this_result_datasource_id <- sha1(letters[1:5])
  this_scheme_id <- sha1(letters[6:10])
  this_species_group_id <- sha1(letters[11:15])
  this_location_group_id <- sha1(letters[16:20])
  this_seed <- 4L
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
  this_parent <- "abcded"

  test_element <- list(
    dataset, this_result_datasource_id,
    this_scheme_id, this_species_group_id, this_location_group_id,
    this_model_type, this_formula, this_first_imported_year,
    this_last_imported_year, this_duration, this_last_analysed_year,
    this_analysis_date, this_seed, this_parent,
    -0.005754 # gives error when using signif(x, 4)
  )
  current_sha1 <- sapply(test_element, sha1)
  # generate the correct values
  cat("\ncorrect <- c(\n")
  cat(
    sprintf("  \"%s\"", current_sha1),
    sep = ",\n"
  )
  cat(")\n")
  # 64-bit Ubuntu 20.04
  correct <- c(
    "9ceeae85decb5c480a115feac0686f39493f9110",
    "70095252e47ccdbabc7bcd32ea13c5057a9776dc",
    "49c22e6e1d9b2f48a4478975f0b14f9911c8499a",
    "b17832f39416892d8b44d09e4b14994fa603b5dd",
    "313bbe15527b560c326d6a523ad82c7e6c0c7375",
    "ad580171e5f9c24498f91a68a537974e31dc59d9",
    "620db157ffa7129b0b8dd8e2d0aa635753ab1ee1",
    "24f565badff755cba798cb8f5a54754b62b5f2c4",
    "0dd2f2e8108df7072f22c65c7319b43a9839ac5c",
    "b48c17a2ac82601ff38df374f87d76005fb61cbd",
    "2f6f17026058eaead59fda7cc776ccdd2e11f52d",
    "5b07e8722eec5f09726a2296099583bf0a98058b",
    "80eb128686c6c7a105b76a869e3d1542bc9cb375",
    "9410ee62d3b1b5d4b69c86859b9313fac7a6a2e2",
    "6b4b2c93a3c40ebb82d66150a136e2c6bed60733"
  )
  for (i in seq_along(test_element)) {
    expect_identical(
      current_sha1[i],
      correct[i],
      label = paste0("test_element[[", i, "]]")
    )
  }
})
