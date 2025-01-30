test_that("get_datafield_id", {
  root <- tempfile("get_datafield_id")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))

  expect_is(
    get_datafield_id(
      table = "test", field = "id", datasource = "database", root = root
    ),
    "integer"
  )
  expect_is(
    get_datafield_id(
      table = "test", field = "id", datasource = "database", root = root
    ),
    "integer"
  )
  expect_is(
    get_datafield_id(
      table = "test2", field = "id", datasource = "database", root = root
    ),
    "integer"
  )
})
