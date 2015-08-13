context("elaborate comparison of get_sha1() on 32-bit and 64 bit")
describe("file fingerprint for n2k_glmer_poisson", {

  data(cbpp, package = "lme4")
  cbpp$Weight <- cbpp$size
  cbpp$DatasourceID <- 1
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  this.analysis.date <- as.POSIXct("2015-01-01")
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

  file.fingerprint <- list(
    cbpp, this.scheme.id, this.species.group.id, this.location.group.id,
    this.model.type, this.formula, this.first.imported.year,
    this.last.imported.year, this.duration, this.last.analysed.year,
    this.analysis.date, this.seed, this.parent
  )
  cat(
    "\nfile.sha1 <- \"", get_sha1(file.fingerprint), "\"\n",
    sep = ""
  )
  file.sha1 <- "66917be07650f5faa52f1a4ffe38a2edb50858ec"
  expect_identical(file.sha1, get_sha1(file.fingerprint))

  cat("\ntarget <- c(\n")
  cat(
    sprintf(
      "  \"%s\"",
      sapply(
        seq_along(file.fingerprint),
        function(i){
          get_sha1(file.fingerprint[-i])
        }
      )
    ),
    sep = ",\n"
  )
  cat(")\n")
  target <- c(
    "83697b60657a8d8093d006f0c9e6b32a6d5b83d1",
    "3969746842445cb6b5801c0c72dc97effe1fbe46",
    "a7ee7eaa4c3472ca7488e92a9a5b0c3243b39afb",
    "6cb76ec92667eccb6878395fd8080f6c596c1c84",
    "94c314e7cdb0420bb167cf4b4248af102c08a17c",
    "87511f2d251e183b6c2e1dac28fdd6478038f117",
    "8a0724dba1b5bdfa7675eddbf71d91b48d53c0b3",
    "44d49b2ea267a2f832b83236f34b27dda6c3926d",
    "6c76659264b1cf9929bcc1194121fb5ef9f64067",
    "73c5cec3ff977e8f35c5dbe0d1e64e273ec15c88",
    "d8d8934de5f25031da05ee13b1f3d35f43e58769",
    "84a1d9f9a371b583f2f9174b688dfce4429fa078",
    "0d1a0e2dd018da17a89d3ca51353874d3a355ff2"
  )
  sapply(
    seq_along(file.fingerprint),
    function(i){
      expect_identical(
        target[i],
        get_sha1(file.fingerprint[-i])
      )
    }
  )
})
