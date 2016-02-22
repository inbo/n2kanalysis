context("elaborate comparison of sha1() on 32-bit and 64 bit")
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
    "e14c9825a12b51ca89cd9d431e72dc34ffaaa175",
    "49731da30df853ee8959035e3e309df129ad5348",
    "d1fafdecb299e9f16ba224ecb4a0601fd31859f5",
    "cac51e723748aae54d21b79b40b48f9000f5e90e",
    "092dfcc3af5141bd836da53309a1bdae437594c5",
    "42961f9c6bf0d14db87ed7c87ce286417b1d9b3a",
    "a88bee33ab1d3bc100f4182d27051b9986851665",
    "60795657ffb9d3e0de4be99f3185c1519d794d10",
    "d5bafa2a9c4bf01bc354128329eddce6eb3e713c",
    "60795657ffb9d3e0de4be99f3185c1519d794d10",
    "32558a12c667699e9ee985f0f98a7e27308c4c81",
    "91619e26fbae8cd58c353ee18689728c0c87d002",
    "a89ee68a22ad35e374650960b21c6ffaf0561ff5",
    "cd5521cfbe6fff3e187a1e003510f253ac5f927e",
    "12a3be85ecf18d166df28e042af1e7ead03bfd35"
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
