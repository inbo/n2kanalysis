context("status")
temp.dir <- tempdir()
on.exit(
  file.remove(list.files(temp.dir, recursive = TRUE, full.names = TRUE)),
  add = TRUE, after = TRUE
)
