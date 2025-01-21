#' Get the data field id
#' @param table The table name
#' @param field The field name
#' @param datasource The data source name
#' @inheritParams git2rdata::write_vc
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom git2rdata is_git2rmeta update_metadata verify_vc write_vc
get_datafield_id <- function(table, field, datasource, root, stage = FALSE) {
  assert_that(
    is.string(table), is.string(field), is.string(datasource), noNA(table),
    noNA(field), noNA(datasource)
  )
  if (!is_git2rmeta(file = "datafield", root = root)) {
    data.frame(id = 1L, table = table, field = field, source = datasource) |>
      write_vc(file = "datafield", root = root, sorting = "id", stage = stage)
    update_metadata(
      file = "datafield", root = root, stage = stage, name = "datafield",
      title = "Pointer to external code identifiers",
      description =
        "This dataset describes the external code identifiers used in the data.
It points to the original source of the external code: which datasource, which
table in that datasource and which field in that table.",
      field_description = c(
        id = "unique identifier of the datafield",
        table = "table name of the identifier",
        field = "field name of the identifier",
        source = "data source which stores the data"
      )
    )
    return(1)
  }
  datafield <- verify_vc(
    file = "datafield", root = root,
    variables = c("id", "table", "field", "source")
  )
  which(
    datafield$table == table & datafield$field == field &
      datafield$source == datasource
  ) -> relevant
  stopifnot("multiple matching datafield id found" = length(relevant) <= 1)
  if (length(relevant) == 1) {
    return(datafield$id[relevant])
  }
  new_id <- max(datafield$id) + 1L
  data.frame(id = new_id, table = table, field = field, source = datasource) |>
    write_vc(file = "datafield", root = root, stage = stage, append = TRUE)
  return(new_id)
}
