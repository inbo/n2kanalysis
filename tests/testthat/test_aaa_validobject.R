test_that("only affects character objects that contains filenames or paths", {
  dataset <- test_data()
  expect_identical(
    n2kanalysis::validObject(dataset),
    methods::validObject(dataset)
  )
  expect_identical(
    n2kanalysis::validObject("abc"),
    methods::validObject("abc")
  )
})

test_that("n2kanalysis objects", {
  dataset <- test_data()
  # example code from ?methods::validOject
  setClass("track", representation(x = "numeric", y = "numeric"))
  t1 <- new("track", x = 1:10, y = sort(stats::rnorm(10)))
  ## A valid "track" object has the same number of x, y values
  valid_track_object <- function(object) {
    if (length(object@x) == length(object@y)) {
      TRUE
    } else {
      paste(
        "Unequal x,y lengths: ", length(object@x), ", ", length(object@y),
        sep = ""
      )
    }
  }
  ## assign the function as the validity method for the class
  setValidity("track", valid_track_object)
  ## t1 should be a valid "track" object
  expect_true(validObject(t1))

  ## Now we do something bad
  t2 <- t1
  t2@x <- 1:20
  expect_error(validObject(t2))

  temp_dir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  dir.create(paste(temp_dir, "sub", sep = "/"))
  good_file <- paste(temp_dir, "good.rda", sep = "/")
  bad_file <- paste(temp_dir, "sub", "bad.rda", sep = "/")
  save(t1, dataset, file = good_file)
  save(t2, dataset, file = bad_file)

  good <- validObject(good_file)
  bad <- validObject(bad_file)
  expect_is(good, "data.frame")
  expect_is(bad, "data.frame")
  expect_identical(
    colnames(good),
    c("Filename", "Object", "Valid")
  )
  expect_true(all(good$Valid))
  expect_false(all(bad$Valid))
  expect_identical(
    unique(good$Filename),
    good_file
  )
  expect_identical(
    unique(bad$Filename),
    bad_file
  )
  expect_identical(
    good$Object,
    c("dataset", "t1")
  )
  expect_identical(
    bad$Object,
    c("dataset", "t2")
  )

  good_file2 <- paste(temp_dir, "good.rds", sep = "/")
  bad_file2 <- paste(temp_dir, "sub", "bad.rds", sep = "/")
  saveRDS(t1, file = good_file2)
  saveRDS(t2, file = bad_file2)
  good2 <- validObject(good_file2)
  bad2 <- validObject(bad_file2)
  expect_is(good2, "data.frame")
  expect_is(bad2, "data.frame")
  expect_identical(
    colnames(good2),
    c("Filename", "Object", "Valid")
  )
  expect_true(all(good2$Valid))
  expect_false(all(bad2$Valid))
  expect_identical(
    unique(good2$Filename),
    good_file2
  )
  expect_identical(
    unique(bad2$Filename),
    bad_file2
  )
  expect_identical(
    good2$Object,
    good2$Filename
  )
  expect_identical(
    bad2$Object,
    bad2$Filename
  )

  valid_dir <- validObject(temp_dir)
  expect_is(valid_dir, "data.frame")
  expect_identical(colnames(valid_dir), colnames(good))
  expect_identical(valid_dir, rbind(good, good2, bad, bad2))

  expect_identical(
    unique(valid_dir$Filename),
    c(good_file, good_file2, bad_file, bad_file2)
  )

  bad_extension <- paste(temp_dir, "bad.txt", sep = "/")
  save(t2, dataset, file = bad_extension)
  expect_is(validObject(bad_extension), "data.frame")

  bad_csv <- paste(temp_dir, "bad.csv", sep = "/")
  write.csv(dataset, file = bad_csv)
  expect_error(suppressWarning(validObject(bad_csv)))

  #clean up temp files
  file.remove(list.files(temp_dir, recursive = TRUE, full.names = TRUE))
})
