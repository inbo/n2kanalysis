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

  test.element <- list(
    cbpp, this.scheme.id, this.species.group.id, this.location.group.id,
    this.model.type, this.formula, this.first.imported.year,
    this.last.imported.year, this.duration, this.last.analysed.year,
    this.analysis.date, this.seed, this.parent
  )
  # add list with 2 elements
  test.element <- c(
    test.element,
    combn(test.element, 2, simplify = FALSE)
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
    "d76d8dabc44cbac47c61f054652f723585c0b7b9",
    "6c30934a0ea2c0473d37b6d8bb5b955b435a8bc1",
    "315a5aa84aa6cfa4f3fb4b652a596770be0365e8",
    "a05091ea911bb9665d685c99b42f20e08c8a1927",
    "092dfcc3af5141bd836da53309a1bdae437594c5",
    "42961f9c6bf0d14db87ed7c87ce286417b1d9b3a",
    "7d7c9bfc4ef9092bff67f4e2b381f55eb7662db9",
    "d571e1cfe37d51537693902580bfe07573131acd",
    "a97053267d374e75ae832e541ece558ef2a5cebc",
    "d571e1cfe37d51537693902580bfe07573131acd",
    "e6f700c38215a9dfb3ba54d8a7daac08f27230e4",
    "f4477038cc95efbea855596fcc42fa28bc7dc9da",
    "a89ee68a22ad35e374650960b21c6ffaf0561ff5",
    "44660a703bca8584079f0173b95cd44029a865e3",
    "b9a84d0f84be54e0ef2bd3728155be418a8643f9",
    "1e80a319e956e17da6d263cfbb2dd0e4f01817a6",
    "7ea27157e61c250cf12f29418bac441ed4a927bd",
    "555eb0c3772d7123a2ca91fc756fad92e0df7e22",
    "a6fddd7401cad5bd7cf526b8be58816d1b53ccc1",
    "cd7a80415f0af2d52672cc81bb7e6ad6d46994f0",
    "557a636bfed9035a8e393797175094353a3eadf0",
    "cd7a80415f0af2d52672cc81bb7e6ad6d46994f0",
    "a656067d9022a1ced0cacd4edf99853e19da3e79",
    "3efed7f73286431e1b002fdb4764adb40b3249f8",
    "e45280032df6ab1cf7c898690f99b02d2024735e",
    "6b3d5ee13814051a87b46ad243a802bedd0f7def",
    "7126334caa9647732ddb6f3cba8e496965a8ca1b",
    "9dcdff451f757d40e57fe9fb0840f09930ba7cf9",
    "8a6c37153138302cef65514b0341fd1e5c26a6c9",
    "e548930fb89f3c2b2442f77deada6a19039691dd",
    "f43824dbd8c4ab6283c653d12c659d7a81accb47",
    "83d0438aa6a66b92ea6ece8ed57769b89922d631",
    "f43824dbd8c4ab6283c653d12c659d7a81accb47",
    "e617b7b15809b39a3554fbfca4d09a333f5ae6ad",
    "f8d652debec15268cfcd3ad2f417fc2fb8b39e89",
    "31e337371aea0e4342b4e7ae40db3fd1dbabe81d",
    "b6ee71207e72e4ba6ac804aaa7346cb0a7727e30",
    "f6dd63cae9ed4315134a77be8fcb47812bc3b23f",
    "00ef9745ed5224c84ce36f645f66d78e71eb52f4",
    "9fff95932097bf3ba2091c896fdd6e8c62e83ea1",
    "6596c4758224e2f31f5477602831dbaf31c1a96b",
    "3a5cfb87faebf2858b73754a9183b0f7c21c1a1f",
    "6596c4758224e2f31f5477602831dbaf31c1a96b",
    "7f8ece77dcec78934ea8534092e69126070dc0ce",
    "63343ab97460132098148698a4042ba67e182924",
    "7625bf5455a4bda8a6c3afb8eeaa16c04e2532fe",
    "aae95182b07afc57c759fb0a8e326995611c897f",
    "c203437384f7572322c020d305e5a85194d03b60",
    "6ecc2679e9687144bbdf8cf988f1d65c23ee384a",
    "254a9943471c7890d08e3d1be72d4da9a269e301",
    "77e3b21e84650a3d595b3024d11f605388259656",
    "254a9943471c7890d08e3d1be72d4da9a269e301",
    "88fae5acd4ba23f3fa2e4cec8e4ae320e97943d4",
    "fdf4e7e72c04eb148ac0d9ca7d3acceb25ce7c06",
    "437968b88accf60a713c4d4ed27d480a493585d2",
    "82fe40b69aedbc8dc699646fd0d012fea69ac03e",
    "7216f2934a95b3e42145d8688a11633abd7f581b",
    "895ef9b52f1de58b23db73dbd1306e07596ac709",
    "4e74e6de63dc327589a14838cc6cf7e851e4b2e7",
    "895ef9b52f1de58b23db73dbd1306e07596ac709",
    "ac061513c43d3772bdfd0637c65bc5b5e34c0e54",
    "4e53b8f7a5e7ba889de5b3631a167a6038dd378c",
    "59e98eca2a9fb944728ed9d850e1adbb0a444678",
    "6fd1a672f035e368b1b7a86d0c3325bcd30e24b3",
    "13a1fba5ff77527025a44b1c18a5eb656e38273d",
    "275a66b6994e8e6569d051fcca47a1b91e2d932b",
    "13a1fba5ff77527025a44b1c18a5eb656e38273d",
    "23584e04a51f3d7728aacf7adc213cab889e9cb8",
    "66df1535378905c25cf51663cf8b503908af654d",
    "591aa933d290064dd5bbcd6644c016c8f5d9e163",
    "d1be4ed1f9536980cb141f2f1fcd9c78f2c9f1e2",
    "091931f2807bd996c4b557e3e7c70adaac6d82db",
    "d1be4ed1f9536980cb141f2f1fcd9c78f2c9f1e2",
    "9172bc6a85d82d47ab7581cf5f17584e79c97bb3",
    "7afee3c35d90acfdf37d4405a77e641c9ad3f581",
    "8928c597ce77e2db9b9f788a1f1ac5e73304b4ab",
    "bf214b9296badfa814c03e40904d403afdaeb16c",
    "189c72598309c5d1b601ebbc5776e93f3caffae8",
    "b5d1c2f41c067836070df04f280189192001dd5c",
    "63a4408d825faa428d27b4a83315d783749ab305",
    "d432814be35f0301fe82be5e8eef2ade3c9a5a62",
    "13f16363d878ed5618ab88b3cd9606bda64d471a",
    "5ed85de6de1e82ab454fa718e3d4ebd124485550",
    "650549b5336bbee37e67b01752bc74b564e080d4",
    "c518a53561498a188e310a919e84bc32fe85dc18",
    "b5d1c2f41c067836070df04f280189192001dd5c",
    "63a4408d825faa428d27b4a83315d783749ab305",
    "d432814be35f0301fe82be5e8eef2ade3c9a5a62",
    "7ae88f50e10c31a5b9186a3168a97d7d104a2436",
    "e4f95c5e5a09a4884258ffa0428c189a17cdb1f5",
    "37390e843e6add69bd9cedba0c1255eb933f5076"
  )
  it("returns the same SHA1 on both 32-bit and 64-bit OS", {
    expect_identical(
      correct,
      unname(sapply(test.element, get_sha1))
    )
  })

})
