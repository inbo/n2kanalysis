context("status")
temp_dir <- tempdir()
on.exit(
  file.remove(list.files(temp_dir, recursive = TRUE, full.names = TRUE)),
  add = TRUE, after = TRUE
)
