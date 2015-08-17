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
    this.analysis.date, this.seed, this.parent, gm1,
    -0.005754 # gives error when using signif(x, 4)
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
    "546aaf082716b09663c29d3bab818a50bd5ed5ce",
    "1ff7ec513ca937d95071b47e30f851941a92bb1a",
    "4d1e599d09ad910c1d489011fcd0a323504d891e",
    "b68b33f773c469ad4545f178bb008eaae1f3f364",
    "092dfcc3af5141bd836da53309a1bdae437594c5",
    "42961f9c6bf0d14db87ed7c87ce286417b1d9b3a",
    "61337ecca7b22e35acfd49f0573a36acfaeab230",
    "82941a5f3ace6a8f5e00abc052b390d445e96ce4",
    "e7bebd7a85541cd5ccb386e178b2c9a32ec98c23",
    "82941a5f3ace6a8f5e00abc052b390d445e96ce4",
    "32558a12c667699e9ee985f0f98a7e27308c4c81",
    "d219c0181a373e300b3105f725c38f2c671ad8e4",
    "a89ee68a22ad35e374650960b21c6ffaf0561ff5",
    "42a47033e825407d8da4f4398a35f341c3ef0fbd",
    "476f639d51b78eca9bb60aaf9b095e58e3a8d3f9"
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
