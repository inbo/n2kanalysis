context("store_model")
test_that("store_model stores the model on a local file system", {
  base <- tempdir()
  project <- "store_model"
  this.result.datasource.id <- sha1(sample(letters))
  this.scheme.id <- sha1(sample(letters))
  this.species.group.id <- sha1(sample(letters))
  this.location.group.id <- sha1(sample(letters))
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
    result.datasource.id = this.result.datasource.id,
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
    filename <- store_model(object, base, project),
    "character"
  )
  file_info <- file.info(filename)
  expect_is(
    filename2 <- store_model(object, base, project, overwrite = FALSE),
    "character"
  )
  file_info2 <- file.info(filename2)
  expect_identical(filename, filename2)
  expect_identical(file_info, file_info2)

  expect_identical(get_model(filename), NULL)

  fitted <- fit_model(filename, base, project)
  expect_identical(
    sprintf("%s/%s", base, project) %>%
      list.files(recursive = TRUE, full.names = TRUE),
    gsub("new", "converged", filename)
  )
  expect_is(
    filename2 <- store_model(object, base, project, overwrite = FALSE),
    "character"
  )
  expect_identical(
    filename2,
    gsub("new", "converged", filename)
  )
  expect_is(
    filename2 <- store_model(object, base, project),
    "character"
  )
  expect_identical(
    filename2,
    filename
  )

  sprintf("%s/%s", base, project) %>%
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()
})

test_that("store_model stores the model on an S3 bucket", {
  if (Sys.getenv("AWS_SECRET_ACCESS_KEY") == "") {
    return(NULL)
  }
  bucket <- get_bucket("n2kmonitoring")
  project <- "unittest_store_model"
  this.result.datasource.id <- sha1(sample(letters))
  this.scheme.id <- sha1(sample(letters))
  this.species.group.id <- sha1(sample(letters))
  this.location.group.id <- sha1(sample(letters))
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
    result.datasource.id = this.result.datasource.id,
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
      project = project
    ),
    "character"
  )
  available <- get_bucket("n2kmonitoring", prefix = project) %>%
    sapply("[[", "Key")
  expect_true(filename %in% available)
  expect_is(
    filename2 <- store_model(
      x = object,
      base = bucket,
      project = project
    ),
    "character"
  )
  available <- get_bucket("n2kmonitoring", prefix = project) %>%
    sapply("[[", "Key")
  expect_true(filename2 %in% available)
  expect_identical(filename, filename2)
  expect_true(all(sapply(available, delete_object, bucket = bucket)))
})
