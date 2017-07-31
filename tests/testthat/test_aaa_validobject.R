context("validObject")
data("cbpp", package = "lme4")
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

  good.file2 <- paste(temp.dir, "good.rds", sep = "/")
  bad.file2 <- paste(temp.dir, "sub", "bad.rds", sep = "/")
  saveRDS(t1, file = good.file2)
  saveRDS(t2, file = bad.file2)
  good2 <- validObject(good.file2)
  bad2 <- validObject(bad.file2)
  it("handles all objects in the rda files", {
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
      good.file2
    )
    expect_identical(
      unique(bad2$Filename),
      bad.file2
    )
    expect_identical(
      good2$Object,
      good2$Filename
    )
    expect_identical(
      bad2$Object,
      bad2$Filename
    )
  })

  valid.dir <- validObject(temp.dir)
  it("works on all rds files recursively", {
    expect_is(valid.dir, "data.frame")
    expect_identical(colnames(valid.dir), colnames(good))
    expect_identical(valid.dir, rbind(good, good2, bad, bad2))
  })

  it("ignores non-rda extensions when a path is given", {
    expect_identical(
      unique(valid.dir$Filename),
      c(good.file, good.file2, bad.file, bad.file2)
    )
  })

  it("handles single rda files regardless the extension", {
    bad.extension <- paste(temp.dir, "bad.txt", sep = "/")
    save(t2, cbpp, file = bad.extension)
    expect_is(validObject(bad.extension), "data.frame")
  })

  it("handles single non-rda, non-rds files", {
    bad.csv <- paste(temp.dir, "bad.csv", sep = "/")
    write.csv(cbpp, file = bad.csv)
    expect_error(validObject(bad.csv))
  })

  #clean up temp files
  file.remove(list.files(temp.dir, recursive = TRUE, full.names = TRUE))
})
