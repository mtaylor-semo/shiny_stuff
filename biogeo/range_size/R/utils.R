# Global functions --------------------------------------------------------
# for the geographic range shiny app.

# Open the data set. Can probably generalize these 
# to open csv and tsv files.

open_file <- function(tx, st = NULL) {
  if (is.null(st)) {
    file_to_open <- paste0("na_data/na_", tx, ".csv")
  } else {
    switch(st,
           "California" = {
             file_to_open <- "marine/california_marine_fishes.csv"
           },
           {
             the_state <- str_replace_all(st, " ", "_") 
             file_to_open <- paste0("state_data/", the_state, "_", tx, ".csv")
           }
           )
  }
  read.csv(file_to_open, row.names = 1)
}

# open_state_file <- function(st, tx) {
#   # State and taxon
#   the_state <- str_replace_all(st, " ", "_") # For two word states
#   file_to_open <- paste0("state_data/", the_state, "_", tx, ".csv")
#   read.csv(file_to_open, row.names = 1)
# }
# 
# open_na_file <- function(natx) {
#   # North American taxon
#   file_to_open <- paste0("na_data/na_", natx, ".csv")
#   read.csv(file_to_open, row.names = 1)
# }
# 

## Prediction check. Move requirement check for predictions here.
## sn = student_name, ps = pred_state, pn = pred_na, pc = pred_ca
pred_check <- function(sn = NULL, ps = NULL, pn = NULL, pc = NULL) {
  req(sn, ps, pn, pc)
}