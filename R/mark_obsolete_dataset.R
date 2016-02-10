#' Find and mark obsolete datasets
#'
#' A dataset is obsolete when a newer version is available
#' @export
#' @param channel An open ODBC channl
#' @importFrom RODBC sqlQuery
#' @importFrom n2khelper check_dbtable_variable
mark_obsolete_dataset <- function(channel){
  check_dbtable_variable(
    table = "Dataset",
    variable = c("Obsolete", "FileName", "PathName", "ImportDate"),
    channel = channel
  )
  sql <- "
    UPDATE
      Dataset
    SET
      Dataset.Obsolete = 1
    FROM
        Dataset
      INNER JOIN
        (
          SELECT
            FileName,
            PathName,
            Max(ImportDate) AS MostRecent
          FROM
            Dataset
          GROUP BY
            FileName,
            PathName
        ) AS Recent
      ON
        Dataset.FileName = Recent.FileName AND
        Dataset.PathName = Recent.PathName
    WHERE
      Obsolete = 0 AND
      ImportDate < MostRecent
  "
  sql.status <- sqlQuery(channel = channel, query = sql)
  return(sql.status)
}
