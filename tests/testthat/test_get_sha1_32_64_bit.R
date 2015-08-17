context("elaborate comparison of get_sha1() on 32-bit and 64 bit")
describe("file fingerprint for n2k_glmer_poisson", {

  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- 1
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01 04:05:06.12", tz = "UTC")
  this.scheme.id <- 1L
  this.species.group.id <- 2L
  this.location.group.id <- 3L
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
    this.analysis.date, this.seed, this.parent, gm1
  )
  # generate the correct values
  cat("\ncorrect <- c(\n")
  cat(
    sprintf("  \"%s\"", sapply(test.element, get_sha1)),
    sep = ",\n"
  )
  cat(")\n")
  # 32-bit windows 7
  correct <- c(
    "310bbf8f6f68f1112a2103e633178ee5bb1f4e0a",
    "6c30934a0ea2c0473d37b6d8bb5b955b435a8bc1",
    "315a5aa84aa6cfa4f3fb4b652a596770be0365e8",
    "a05091ea911bb9665d685c99b42f20e08c8a1927",
    "092dfcc3af5141bd836da53309a1bdae437594c5",
    "42961f9c6bf0d14db87ed7c87ce286417b1d9b3a",
    "7d7c9bfc4ef9092bff67f4e2b381f55eb7662db9",
    "d571e1cfe37d51537693902580bfe07573131acd",
    "a97053267d374e75ae832e541ece558ef2a5cebc",
    "d571e1cfe37d51537693902580bfe07573131acd",
    "32558a12c667699e9ee985f0f98a7e27308c4c81",
    "f4477038cc95efbea855596fcc42fa28bc7dc9da",
    "a89ee68a22ad35e374650960b21c6ffaf0561ff5",
    "a3b3d8932261f04bea4921e7a4c50865ad5c402e"
  )
  it("return the same SHA1 on both 32-bit and 64-bit OS", {
    for (i in seq_along(test.element)) {
      expect_identical(
        get_sha1(test.element[[i]]),
        correct[i],
        label = paste0("test.element[[", i, "]]")
      )
    }
  })
})
