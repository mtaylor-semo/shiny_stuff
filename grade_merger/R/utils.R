# Global functions --------------------------------------------------------
# for the grade_merger shiny app.

get_grade_file <- function(filename, columnTypes = NULL) {
  cat(file = stderr(), str(filename))
  ext <- tools::file_ext(filename)
  switch(
    ext,
    csv = read_csv(filename, col_types = columnTypes),
    tsv = read_tsv(filename, col_types = columnTypes),
    validate("Invalid file; Please upload a .csv file")
  )
}


# Keep only the columns from the lecture grades required 
# by Canvas for importing.
trim_lecture_cols <- function(.data) {
  select(
    .data,
    `Student`, 
    `ID`,
    `SIS User ID`,
    `SIS Login ID`,
    `Section`
  ) 
}

# Arrange the lab columns so that Current Score, the default
# lab grade column, is displayed after the required columns.
trim_lab_cols <- function(.data) {
  select(
    .data,
    `Student`, 
    `ID`,
    `SIS User ID`,
    `SIS Login ID`,
    `Section`,
    `Current Score`,
    everything()
  )
}

