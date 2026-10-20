#' Determine the status of objects in a project
#'
#' A faster way to determine the status of objects in a project.
#' Because this function determines the status of objects by looking at the file
#' system,
#' it is much faster than `status()` which requires to read the object first.
#' @inheritParams fit_model
#' @param hash the optional fingerprint of the object.
#' If missing, then all objects will be returned.
#' @return a vector of hashes of existing files with matching status
#' Returns `character(0)` when no file exists or no file matches `status`.
#' @export
object_status <- function(base, project, status = c("new", "waiting"), hash) {
  UseMethod("object_status", base)
}

#' @importFrom assertthat assert_that
#' @export
object_status.character <- function(
  base, project, status = c("new", "waiting"), hash
) {
  assert_that(
    is.string(base),
    is.character(status),
    length(status) > 0,
    is.string(project)
  )
  if (missing(hash)) {
    file.path(base, project) |>
      list.files(
        recursive = TRUE, pattern = "[[:xdigit:]]{40}\\.rds$"
      ) -> available
  } else {
    assert_that(is.string(hash), grepl("^[[:xdigit:]]{40}$", hash))
    file.path(base, project) |>
      list.files(recursive = TRUE, pattern = hash) -> available

  }
  available |>
    grepv(pattern = paste(status, collapse = "|") |>
    sprintf(fmt = "(%s)")) |>
    basename() |>
    gsub(pattern = "\\.rds$", replacement = "")
}

#' @importFrom assertthat assert_that
#' @importFrom aws.s3 get_bucket
#' @importFrom purrr map_chr
#' @export
object_status.s3_bucket <- function(base, project, status = c("new", "waiting"), hash) {
  assert_that(
    inherits(base, "s3_bucket"),
    is.character(status),
    length(status) > 0,
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
