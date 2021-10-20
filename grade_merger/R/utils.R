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

trim_lecture_cols <- function(.data) {
  select(
    .data,
    `Student`, # Select the fields required by Canvas. See above.
    `ID`,
    `SIS User ID`,
    `SIS Login ID`,
    `Section`
  ) %>%
    slice(-c(1:2)) # Remove first row after the column names.
}

# Probably should have more of the Canvas Columns to avoid duplicate
# names such as Smith, John. SIS User ID, eg, would avoid dupes.
# Could then merge off of them.
trim_lab_cols <- function(.data) {
  select(
    .data,
    `Student`, 
    `Current Score`,
    everything()
  )
}
lab_fields <- function(.data, oldName, newName) {
  select(
    .data,
    `Student`, # Select the fields required by Canvas. See above.
    `newName` = `oldName`
  )
}
