test_that("read_model() handles exceptions on characters", {
  base <- tempdir()
  project <- "read_model"
  dir.create(file.path(base, project, "test"), recursive = TRUE)
  writeLines("junk", file.path(base, project, "test", "test1.rds"))
  writeLines("junk", file.path(base, project, "test", "test2.rds"))
  expect_error(
    suppressWarnings(read_model("junk", base, project)),
    "no matching object in directory"
  )
  expect_error(
    read_model("test", base, project),
    "multiple matching objects in directory"
  )
  file.path(base, project) %>%
    list.files(recursive = TRUE, full.names = TRUE) %>%
    file.remove()
})

test_that("read_model() works with S3 buckets", {
  base <- get_bucket("n2kmonitoring", max = 1)
  project <- "unittest_read_model"
  s3saveRDS(
    project,
    bucket = base,
    object = file.path(project, "test", "test1.rds")
  )
  s3saveRDS(
    project,
    bucket = base,
    object = file.path(project, "test", "test2.rds")
  )
  expect_identical(
    read_model("test1", base, project),
    project
  )
  expect_error(
    read_model("junk", base, project),
    "no matching object in bucket"
  )
  expect_error(
    read_model("test", base, project),
    "multiple matching objects in bucket"
  )
  get_bucket("n2kmonitoring", prefix = project) %>%
    map_chr("Key") %>%
    basename() %>%
    gsub(pattern = "\\.rds", replacement = "") %>%
    unique() -> available
  expect_true(
    all(unlist(sapply(available, delete_model, base = base, project = project)))
  )
})
