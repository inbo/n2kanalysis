context("n2import")
status <- "new"
result.datasource.id <- "result.datasource.id"
scheme.id <- "scheme.id"
species.group.id <- "species.group.id"
location.group.id <- "location.group.id"
model.type <- "model.type"
formula <- "count ~ 1"
first.imported.year <- 1
last.imported.year <- 10
analysis.date <- Sys.time()
dataset <- data.frame(
  filename = "filename",
  fingerprint = "fingerprint",
  import_date = Sys.time(),
  stringsAsFactors = TRUE
)
expect_is(
  junk <- n2k_import(
    status = status,
    result.datasource.id = result.datasource.id,
    scheme.id = scheme.id,
    species.group.id = species.group.id,
    location.group.id = location.group.id,
    model.type = model.type,
    formula = formula,
    first.imported.year = first.imported.year,
    last.imported.year = last.imported.year,
    analysis.date = analysis.date,
    dataset = dataset
  ),
  "n2kImport"
)
