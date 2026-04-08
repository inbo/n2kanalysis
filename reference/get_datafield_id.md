# Get the data field id

Get the data field id

## Usage

``` r
get_datafield_id(table, field, datasource, root, stage = FALSE)
```

## Arguments

- table:

  The table name

- field:

  The field name

- datasource:

  The data source name

- root:

  The root of a project. Can be a file path or a `git-repository`.
  Defaults to the current working directory (`"."`).

- stage:

  Logical value indicating whether to stage the changes after writing
  the data. Defaults to `FALSE`.
