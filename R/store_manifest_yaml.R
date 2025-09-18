#' Store a Docker configuration
#' @inheritParams store_manifest
#' @inheritParams store_model
#' @param docker the docker image to use
#' @param dependencies extra GitHub packages to install
#' @name store_manifest_yaml
#' @rdname store_manifest_yaml
#' @exportMethod store_manifest_yaml
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "store_manifest_yaml",
  def = function(x, base, project, docker, dependencies, overwrite = FALSE) {
    standardGeneric("store_manifest_yaml") # nocov
  }
)

#' @export
#' @rdname store_manifest_yaml
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom purrr map_chr
#' @importFrom yaml write_yaml
setMethod(
  f = "store_manifest_yaml",
  signature = signature(base = "s3_bucket"),
  definition = function(
    x,
    base,
    project,
    docker,
    dependencies,
    overwrite = FALSE
  ) {
    assert_that(
      is.string(docker),
      is.character(dependencies),
      noNA(dependencies),
      noNA(docker),
      is.flag(overwrite),
      noNA(overwrite)
    )

    stored <- store_manifest(
      x = x,
      base = base,
      project = project,
      overwrite = overwrite
    )
    list(
      github = dependencies,
      docker = docker,
      bucket = attr(base, "Name"),
      project = project,
      hash = basename(stored) |>
        gsub(pattern = "\\.manifest", replacement = "")
    ) -> yaml
    filename <- sprintf("%s/yaml/%s.yaml", project, sha1(yaml))

    write_s3_fun(
      object = yaml,
      bucket = base,
      key = filename,
      overwrite = overwrite,
      fun = write_yaml
    )
  }
)

#' @export
#' @rdname store_manifest_yaml
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.dir
#' @importFrom dplyr %>%
#' @importFrom yaml write_yaml
setMethod(
  f = "store_manifest_yaml",
  signature = signature(base = "character"),
  definition = function(
    x,
    base,
    project,
    docker,
    dependencies,
    overwrite = FALSE
  ) {
    assert_that(
      is.dir(base),
      is.string(docker),
      is.character(dependencies),
      is.flag(overwrite),
      noNA(overwrite)
    )

    stored <- store_manifest(
      x = x,
      base = base,
      project = project,
      overwrite = overwrite
    )
    list(
      github = dependencies,
      docker = docker,
      bucket = base,
      project = project,
      hash = basename(stored) |>
        gsub(pattern = "\\.manifest", replacement = "")
    ) -> yaml
    sprintf("%s/%s/yaml/%s.yaml", base, project, sha1(yaml)) |>
      normalizePath(winslash = "/", mustWork = FALSE) -> filename
    if (!overwrite && file.exists(filename)) {
      return(filename)
    }
    dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
    write_yaml(yaml, filename)
    return(filename)
  }
)
