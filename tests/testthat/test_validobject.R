context("validObject")
describe("n2kanalysis::validObject", {
  it("only affects character objects that contains filenames or paths", {
    expect_identical(
      n2kanalysis::validObject(cbpp),
      methods::validObject(cbpp)
    )
    expect_identical(
      n2kanalysis::validObject("abc"),
      methods::validObject("abc")
    )
  })

  # example code from ?methods::validOject
  setClass("track", representation(x = "numeric", y = "numeric"))
  t1 <- new("track", x = 1:10, y = sort(stats::rnorm(10)))
  ## A valid "track" object has the same number of x, y values
  validTrackObject <- function(object) {
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
  setValidity("track", validTrackObject)
  ## t1 should be a valid "track" object
  expect_true(validObject(t1))

  ## Now we do something bad
  t2 <- t1
  t2@x <- 1:20
  expect_error(validObject(t2))

  temp.dir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  dir.create(paste(temp.dir, "sub", sep = "/"))
  expect_error(
    validObject(temp.dir),
    "no matching files found"
  )
  good.file <- paste(temp.dir, "good.rda", sep = "/")
  bad.file <- paste(temp.dir, "sub", "bad.rda", sep = "/")
  save(t1, cbpp, file = good.file)
  save(t2, cbpp, file = bad.file)

  good <- validObject(good.file)
  bad <- validObject(bad.file)
  it("handles all objects in the rda files", {
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
      good.file
    )
    expect_identical(
      unique(bad$Filename),
      bad.file
    )
    expect_identical(
      good$Object,
      c("cbpp", "t1")
    )
    expect_identical(
      bad$Object,
      c("cbpp", "t2")
    )
  })

  it("handles empty files", {
    save(file = paste(temp.dir, "empty.rda", sep = "/"))
    empty <- validObject(paste(temp.dir, "empty.rda", sep = "/"))
    expect_is(empty, "data.frame")
    expect_true(nrow(empty) == 0)
    expect_identical(colnames(empty), colnames(good))
  })

  it("handles single rda files regardless the extension", {
    bad.extension <- paste(temp.dir, "bad.txt", sep = "/")
    save(t2, cbpp, file = bad.extension)
    expect_is(validObject(bad.extension), "data.frame")
  })

  it("handles single non-rda files", {
    bad.csv <- paste(temp.dir, "bad.csv", sep = "/")
    write.csv(cbpp, file = bad.csv)
    expect_error(validObject(bad.csv))
  })

  valid.dir <- validObject(temp.dir)
  it("works on all rda files recursively", {
    expect_is(valid.dir, "data.frame")
    expect_identical(colnames(valid.dir), colnames(good))
    expect_identical(valid.dir, rbind(good, bad))
  })

  it("ignores non-rda extensions when a path is given", {
    expect_identical(
      unique(valid.dir$Filename),
      c(good.file, bad.file)
    )
  })

  #clean up temp files
  file.remove(list.files(temp.dir, recursive = TRUE, full.names = TRUE))
})
