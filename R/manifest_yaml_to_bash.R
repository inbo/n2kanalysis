#' Convert a manifest yaml file into a bash script
#' @inheritParams store_manifest
#' @param hash Fingerprint of the manifest `yaml`file.
#' @param shutdown Append a shutdown command at the end of the script.
#' Defaults to `FALSE`.
#' @param split Number of scripts over which to splits the analyses.
#' Default to 1.
#' @param status A vector with status levels naming the levels which should be
#' calculated.
#' Defaults to `c("new", "waiting")`.
#' @name manifest_yaml_to_bash
#' @rdname manifest_yaml_to_bash
#' @exportMethod manifest_yaml_to_bash
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "manifest_yaml_to_bash",
  def = function(
    base, project, hash, shutdown = FALSE, split = 1,
    status = c("new", "waiting")
  ) {
    standardGeneric("manifest_yaml_to_bash") # nocov
  }
)

#' @export
#' @rdname manifest_yaml_to_bash
#' @importFrom assertthat assert_that is.count is.flag is.string noNA
#' @importFrom aws.s3 get_bucket get_bucketname s3read_using
#' @importFrom methods setMethod new
#' @importFrom purrr map_chr
#' @importFrom yaml read_yaml
setMethod(
  f = "manifest_yaml_to_bash",
  signature = signature(base = "s3_bucket"),
  definition = function(
    base, project, hash, shutdown = FALSE, split = 1,
    status = c("new", "waiting")
  ) {
    assert_that(
      is.string(project), noNA(project), is.flag(shutdown), noNA(shutdown),
      is.count(split)
    )
    if (missing(hash)) {
      paste(project, "yaml", sep = "/") |>
        get_bucket(bucket = base, max = Inf) -> available
      stopifnot("No manifest files in this project" = length(available) > 0)
      map_chr(available, "LastModified") |>
        as.POSIXct(tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS") |>
        which.max() -> latest
      yaml_object <- available[[latest]]
    } else {
      assert_that(is.string(hash), noNA(hash))
      sprintf("%s/yaml/%s", project, hash) |>
        get_bucket(bucket = base, max = 1) -> yaml_object
      assert_that(
        length(yaml_object) == 1,
        msg = sprintf("no object found with hash %s", hash)
      )
      yaml_object <- yaml_object[[1]]
    }
    yaml <- s3read_using(read_yaml, object = yaml_object)
    gsub("\\.manifest$", "", yaml$hash) |>
      read_manifest(base = base, project = project) -> manifest
    docker_hash <- get_file_fingerprint(manifest)
    sprintf(
      "Rscript -e 'remotes::install_github(\\\"%s\\\"%s)'", yaml$github,
      ", dependencies = TRUE, upgrade = \\\"never\\\", keep_source = FALSE"
    ) -> deps
    sprintf(
      "#!/bin/bash
echo \"FROM %s
RUN %s\" > Dockerfile
docker build --pull --tag rn2k:%s .
rm Dockerfile",
      yaml$docker, paste(deps, collapse = " \\\n&&  "), docker_hash
    ) -> init
    volume <- "/n2kanalysis:/n2kanalysis:rw"
    models <- order_manifest(manifest = manifest)
    to_do <- object_status(base = base, project = project, status = status)
    models <- models[models %in% to_do]
    sprintf(
      "echo \"model %i of %i\"
date
docker run %s --name=%s -v %s rn2k:%s ./fit_model_aws.sh -b %s -p %s -m %s",
      seq_along(models), length(models),
      paste(
        c(
          "--rm", "--env AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID",
          "--env AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY",
          "--env AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION",
          "--cap-add NET_ADMIN", "--cpu-shares=512"
        ),
        collapse = " "
      ), models, volume, docker_hash, get_bucketname(base), project, models
    ) -> model_scripts
    vapply(
      seq_len(split), FUN.VALUE = character(1), project = project, init = init,
      split = split, shutdown = shutdown, base = base,
      FUN = function(i, project, split, init, shutdown, base) {
        script <- path(
          project, sprintf("bash/%s_%i.sh", docker_hash, i)
        )
        c(
          init, model_scripts[seq_along(model_scripts) %% split == (i - 1)],
          "shutdown -h now"[shutdown]
        ) |>
          s3write_using(writeLines, object = script, bucket = base)
        return(script)
      }
    )
  }
)

#' @export
#' @rdname manifest_yaml_to_bash
#' @importFrom assertthat assert_that is.count is.string noNA
#' @importFrom dplyr slice_max
#' @importFrom fs dir_create dir_info file_chmod path
#' @importFrom methods setMethod new
#' @importFrom utils file_test
#' @importFrom yaml read_yaml
setMethod(
  f = "manifest_yaml_to_bash",
  signature = signature(base = "character"),
  definition = function(
    base, project, hash, shutdown = FALSE, split = 1,
    status = c("new", "waiting")
  ) {
    assert_that(
      is.string(base), noNA(base), file_test("-d", base), is.string(project),
      noNA(project), is.flag(shutdown), noNA(shutdown), is.count(split)
    )
    assert_that(split == 1, msg = "`split > 1` to do on local file systems.")
    assert_that(
      file_test("-d", path(base, project)),
      msg = sprintf("`%s` is not a subdirectory of `%s`", project, base)
    )
    assert_that(
      file_test("-d", path(base, project, "yaml")),
      msg = sprintf("`yaml` is not a subdirectory of `%s/%s`", base, project)
    )
    if (missing(hash)) {
      path(base, project, "yaml") |>
        dir_info(type = "file", regexp = "\\.yaml$") |>
        slice_max(.data$modification_time, n = 1) -> yaml
    } else {
      assert_that(is.string(hash), noNA(hash))
      path(base, project, "yaml") |>
        dir_info(type = "file", regexp = hash) -> yaml
    }
    assert_that(
      nrow(yaml) > 0,
      msg = sprintf("No manifests found at `%s/%s/yaml`", base, project)
    )
    assert_that(
      nrow(yaml) == 1,
      msg = sprintf("Multiple manifests found at `%s/%s/yaml`", base, project)
    )
    yaml <- read_yaml(yaml$path)
    gsub("\\.manifest$", "", yaml$hash) |>
      read_manifest(base = base, project = project) -> manifest
    docker_hash <- get_file_fingerprint(manifest)
    sprintf(
      "Rscript -e 'remotes::install_github(\\\"%s\\\"%s)'", yaml$github,
      ", dependencies = TRUE, upgrade = \\\"never\\\", keep_source = FALSE"
    ) -> deps
    sprintf(
      "#!/bin/bash
echo \"FROM %s
RUN %s\" > Dockerfile
docker build --pull --tag rn2k:%s .
rm Dockerfile",
      yaml$docker, paste(deps, collapse = " \\\n&&  "), docker_hash
    ) -> init
    base <- normalizePath(base, winslash = "/")
    volume <- paste(base, base, "rw", sep = ":")
    models <- order_manifest(manifest = manifest)
    sprintf(
      "echo \"model %i of %i\"
docker run %s --name=%s -v %s rn2k:%s ./fit_model_file.sh -b %s -p %s -m %s
date
docker stop --time 14400 %s
date",
      seq_along(models), length(models), "--rm -d", models, volume, docker_hash,
      base, project, models, models
    ) -> model_scripts
    path(base, project, "bash") |>
      dir_create()
    script <- path(base, project, sprintf("bash/%s.sh", docker_hash))
    c(init, model_scripts, "shutdown -h now"[shutdown]) |>
      writeLines(con = script)
    file_chmod(script, "711")
    return(script)
  }
)

#' @importFrom assertthat assert_that
order_manifest <- function(manifest) {
  assert_that(inherits(manifest, "n2kManifest"))
  full <- slot(manifest, "Manifest")
  to_do <- nrow(full)
  full$fingerprint[is.na(full$parent)] |>
    unique() |>
    sort() -> final_order
  full <- full[!full$fingerprint %in% final_order, ]
  while (nrow(full) < to_do && nrow(full) > 0) {
    c(
      final_order,
      full$fingerprint[full$parent %in% final_order] |>
        unique() |>
        sort()
    ) -> final_order
    to_do <- nrow(full)
    full <- full[!full$fingerprint %in% final_order, ]
  }
  assert_that(nrow(full) == 0)
  return(final_order)
}

#' @importFrom assertthat assert_that
#' @importFrom aws.s3 get_bucket
#' @importFrom purrr map_chr
object_status <- function(base, project, status = c("new", "waiting"), hash) {
  assert_that(
    inherits(base, "s3_bucket"), is.character(status), length(status) > 0,
    is.string(project)
  )
  if (missing(hash)) {
    get_bucket(base, project, max = Inf) |>
      map_chr("Key") -> available
    sprintf("^%s/[[:xdigit:]]{4}/.+/[[:xdigit:]]{40}", project) |>
      grepl(available) -> relevant
  } else {
    assert_that(is.string(hash), grepl("^[[:xdigit:]]{40}$", hash))
    substr(hash, 1, 4) |>
      sprintf(fmt = "%2$s/%1$s", project) |>
      get_bucket(bucket = base, max = Inf) |>
      map_chr("Key") -> available
    substr(hash, 1, 4) |>
      sprintf(fmt = "^%2$s/%1$s/.+/%3$s", project, hash) |>
      grepl(available) -> relevant
  }
  available[relevant] |>
    basename() |>
    gsub(pattern = "\\.rds$", replacement = "") -> hash
  available[relevant] |>
    dirname() |>
    basename() -> current_status
  return(hash[current_status %in% status])
}
