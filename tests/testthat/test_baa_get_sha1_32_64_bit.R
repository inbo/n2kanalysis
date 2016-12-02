context("elaborate comparison of sha1() on 32-bit and 64 bit")
describe("file fingerprint for n2k_glmer_poisson", {

  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this.scheme.id <- sha1(letters)
  this.species.group.id <- sha1(letters)
  this.location.group.id <- sha1(letters)
  this.seed <- 4L
  this.model.type <- "glmer poisson: period + herd"
  this.formula <- "incidence ~ offset(log(size)) + period + (1|herd)"
  this.first.imported.year <- 1990L
  this.last.imported.year <- 2015L
  this.last.analysed.year <- 2015L
  this.parent <- "abcdef"
  this.duration <- this.last.imported.year - this.first.imported.year + 1
  object <- n2k_glmer_poisson(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    last.analysed.year = this.last.analysed.year,
    analysis.date = this.analysis.date,
    seed = this.seed,
    data = cbpp,
    parent = this.parent,
    this.duration
  )
  gm1 <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial
  )


  test.element <- list(
    cbpp, this.scheme.id, this.species.group.id, this.location.group.id,
    this.model.type, this.formula, this.first.imported.year,
    this.last.imported.year, this.duration, this.last.analysed.year,
    this.analysis.date, this.seed, this.parent, gm1,
    -0.005754 # gives error when using signif(x, 4)
  )
  # generate the correct values
  cat("\ncorrect <- c(\n")
  cat(
    sprintf("  \"%s\"", sapply(test.element, sha1)),
    sep = ",\n"
  )
  cat(")\n")
  # 32-bit windows 7
  correct <- c(
    "15a3dfe9aab0afff4b2a44dc3008fbf472f3ab43",
    "2bd213d075fa7bdf9d71186fd0fd2a84eb3fe3f6",
    "2bd213d075fa7bdf9d71186fd0fd2a84eb3fe3f6",
    "2bd213d075fa7bdf9d71186fd0fd2a84eb3fe3f6",
    "e096044ac3b55cc74e03c6085619e4e3850d91af",
    "5ff0998ae6ef94d64f63dc7c1c89954f2e6f3f81",
    "24f565badff755cba798cb8f5a54754b62b5f2c4",
    "0dd2f2e8108df7072f22c65c7319b43a9839ac5c",
    "0393bba20a2d8fafc02b7d42c0dfdc038ab5fe45",
    "0dd2f2e8108df7072f22c65c7319b43a9839ac5c",
    "81433e7fcb79752888dfd4900fa9c38a78170ec4",
    "80eb128686c6c7a105b76a869e3d1542bc9cb375",
    "0bb8190d1ec01e38b077c7c728a6a631735b3e24",
    "9f00651e112f3ac9cbe715e6ef890c4da15b0cac",
    "6b4b2c93a3c40ebb82d66150a136e2c6bed60733"
  )
  it("return the same SHA1 on both 32-bit and 64-bit OS", {
    for (i in seq_along(test.element)) {
      expect_identical(
        sha1(test.element[[i]]),
        correct[i],
        label = paste0("test.element[[", i, "]]")
      )
    }
  })
})
