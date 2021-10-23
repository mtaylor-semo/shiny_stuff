# Global functions --------------------------------------------------------
# for the grade_merger shiny app.


#####
## Ignoring this function for now. May try to adapt for use with
## purrr.
####
get_grade_file <- function(filename, columnTypes = NULL) {
  cat(file = stderr(), str(filename))
  ext <- tools::file_ext(filename)
  switch(
    ext,
    csv = read_csv(filename, col_types = columnTypes),
#                   col_types = list(
#                    .default = col_double(),
#                     Student = col_character(),
#                     `SIS Login ID` = col_character(),
#                     Section = col_character()
#                   ),     #),
    tsv = read_tsv(filename, col_types = columnTypes),
    validate("Invalid file; Please upload a .csv file")
  )
}

# get_grade_file <- function(filename) {
#   ext <- tools::file_ext(filename$name)
#   switch(
#     ext,
#     csv = read_csv(filename$datapath),
#     tsv = read_tsv(filename$datapath),
#     validate("Invalid file; Please upload a .csv file")
#   )
# }

# Keep only the columns from the lecture grades required 
# by Canvas for importing.
trim_lecture_cols <- function(.data) {
  select(
    .data,
    `Student`, # Select the fields required by Canvas. See above.
    `ID`,
    `SIS User ID`,
    `SIS Login ID`,
    `Section`
  ) #%>%
    ## My data has two rows to remove because I have a manual posting.
    ## Not all files will have that. Need to adjust to accommodate.
   # slice(-c(1:2)) # Remove first row after the column names.
}

# Arrange the lab columns so that Current Score, the default
# lab grade column, is displayed after the required columns.
trim_lab_cols <- function(.data) {
  select(
    .data,
    `Student`, # Select the fields required by Canvas. See above.
    `ID`,
    `SIS User ID`,
    `SIS Login ID`,
    `Section`,
    `Current Score`,
    everything()
  )
}

