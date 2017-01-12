context("store_model")
test_that("store_model stores the model on a local file system", {
  temp_dir <- tempdir()
  this.scheme.id <- sha1(letters)
  this.species.group.id <- sha1(letters)
  this.location.group.id <- sha1(letters)
  this.analysis.date <- Sys.time()
  this.model.type <- "inla nbinomial: A * (B + C) + C:D"
  this.formula <-
    "Count ~ A * (B + C) + C:D +
      f(E, model = \"rw1\", replicate = as.integer(A)) +
      f(F, model = \"iid\")"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2014L
  this.duration <- 1L
  dataset <- test_data()
  object <- n2k_inla_nbinomial(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset
  )
  expect_is(
    filename <- store_model(object, temp_dir, "store_model"),
    "character"
  )
  file_info <- file.info(filename)
  expect_is(
    filename2 <- store_model(object, temp_dir, "store_model"),
    "character"
  )
  file_info2 <- file.info(filename2)
  expect_identical(filename, filename2)
  expect_identical(file_info, file_info2)

  paste0(temp_dir, "/store_model") %>% # nolint
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()
})

test_that("store_model stores the model on an S3 bucket", {
  bucket <- get_bucket("n2kmonitoring")
  this.scheme.id <- sha1(letters)
  this.species.group.id <- sha1(letters)
  this.location.group.id <- sha1(letters)
  this.analysis.date <- Sys.time()
  this.model.type <- "inla nbinomial: A * (B + C) + C:D"
  this.formula <-
    "Count ~ A * (B + C) + C:D +
      f(E, model = \"rw1\", replicate = as.integer(A)) +
      f(F, model = \"iid\")"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2014L
  this.duration <- 1L
  dataset <- test_data()
  object <- n2k_inla_nbinomial(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = dataset
  )
  expect_is(
    filename <- store_model(
      x = object,
      base = bucket,
      project = "unittest_store_model"
    ),
    "character"
  )
  available <- get_bucket("n2kmonitoring", prefix = "unittest_store_model") %>%
    sapply("[[", "Key")
  expect_true(filename %in% available)
  expect_is(
    filename2 <- store_model(
      x = object,
      base = bucket,
      project = "unittest_store_model"
    ),
    "character"
  )
  available <- get_bucket("n2kmonitoring", prefix = "unittest_store_model") %>%
    sapply("[[", "Key")
  expect_true(filename2 %in% available)
  expect_identical(filename, filename2)
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
})
