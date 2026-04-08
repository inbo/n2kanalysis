#' Set environment variables for INBO S3 bucket
#'
#' Before running this function you must have an `.aws` folder in your home
#' directory with a `credentials` file containing the credentials for the INBO
#' shared infrastructure.
#' Run the `aws assume role` command to get the credentials for the INBO shared
#' infrastructure before running this function.
#'
#' @export
#' @importFrom fs dir_exists path path_home
#' @importFrom stats setNames
#' @importFrom utils head tail
connect_inbo_s3 <- function() {
  aws_dir <- path_home(".aws")
  stopifnot("no `.aws` folder found" = dir_exists(aws_dir))
  # Read the credentials file
  path(aws_dir, "credentials") |>
    readLines() -> creds
  # keep credentials related to the role
  role <- grep("\\[inbo-shared-infra", creds)
  stopifnot("no role found" = length(role) == 1)
  tail(creds, -role) -> creds
  grep("\\[", creds) |>
    c(length(creds) + 1) |>
    min() -> other
  head(creds, other - 1) -> creds
  # set environment variables
  gsub("(.*) = (.*)", "\\1", creds) |>
    toupper() |>
    setNames(object = creds) |>
    gsub(pattern = "(.*) = (.*)", replacement = "\\2", x = _) |>
    c(AWS_DEFAULT_REGION = "eu-west-1") |>
    as.list() |>
    do.call(what = Sys.setenv)
  return(invisible(NULL))
}
