#' @details
#' - `scheme.id`: a string holding the id of the scheme.
#' - `species.group.id`: a string identifing the species group.
#' - `location.group.id`: a string identifing the location group.
#' - `model.type`: a string identifying the type of model to fit to the data.
#' - `first.imported.year`: Oldest year considered in the data.
#' - `last.imported.year`: Most recent year considered in the data.
#' - `duration`: The width of the moving window.
#' Defaults to the `last.imported.year - first.imported.year + 1`.
#' - `last.analysed.year`: Most recent year in the window.
#' Defaults to `last.imported.year`.
#' - `analysis.date`: A POSIXct date indicating the date that the dataset was
#' imported.
#' - `seed`: A single integer used as a seed for all calculations.
#' A random seed will be inserted when missing.
