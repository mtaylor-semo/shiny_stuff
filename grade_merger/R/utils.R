# Global functions --------------------------------------------------------
# for the grade_merger shiny app.

get_grade_file <- function(filename) {
  ext <- tools::file_ext(filename$name)
  switch(
    ext,
    csv = read_csv(filename$datapath),
    tsv = read_tsv(filename$datapath),
    validate("Invalid file; Please upload a .csv file")
  )
}

trim_fields <- function(.data) {
  select(
    .data,
    `Student`, # Select the fields required by Canvas. See above.
    `ID`,
    `SIS User ID`,
    `SIS Login ID`,
    `Section`
  ) %>%
    slice(-1) # Remove first row after the column names.
}

lab_fields <- function(.data, oldName, newName) {
  select(
    .data,
    `Student`, # Select the fields required by Canvas. See above.
    `newName` = `oldName`
  )
}
