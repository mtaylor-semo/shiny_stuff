file_list <- list.files("state_data/")
file_list_no_ext <- tools::file_path_sans_ext(file_list)

states <- file_list_no_ext %>%
  word(start = 1, end = -2, sep = "_") %>%
  str_replace("_", " ") %>%
  str_to_title()

taxa <-
  word(file_list_no_ext,
    start = -1,
    sep = "_"
  ) %>%
  str_to_title()

state_taxa <- tibble(states, taxa)
state_choices <- unique(states)


# Define States tab -------------------------------------------------------

states_tab <- tabPanel(
  "State",
  fluidRow(
    column(1),
    column(
      2,
      wellPanel(
        p("Choose your state and then taxon
                 to see the histogram."),
        selectInput(
          inputId = "state",
          label = "Choose a state",
          choices = state_choices,
          selected = "Georgia",
          multiple = FALSE
        ),
        uiOutput("dynamic_radio_buttons"),
        hr(),
      )
    ),
    column(5, plotOutput("state_histogram")),
    column(
      2,
      uiOutput("state_numbers"),
      hr(),
      actionButton(inputId = "btn_next_state", label = "Next", width = "35%")
    )
  )
)



# Define North America Tab ------------------------------------------------

na_tab <- tabPanel(
  "North America",
  fluidRow(
    column(1),
    column(
      2,
      wellPanel(
        p("Range size for North America."),
        radioButtons("na_taxon",
          label = "Choose taxon:",
          choices = c("Fishes", "Mussels"),
          selected = "Fishes"
        )
      )
    ),
    column(5, plotOutput("na_histogram")),
    column(
      2,
      uiOutput("na_numbers"),
      hr(),
      actionButton(inputId = "btn_next_na", label = "Next", width = "35%")
    )
  )
)


# Define California Marine Tab --------------------------------------------


ca_tab <- tabPanel(
  "California Marine Fishes",
  fluidRow(
    column(
      2,
      offset = 1,
      wellPanel(radioButtons(
        inputId = "ca_marine",
        label = "Choose plot type",
        choices = c("Range size", "Range extent")
      ))
    ),
    column(5, plotOutput("ca_marine_plot")),
    column(
      4,
      p("This data set has 516 species."),
      uiOutput("ca_info"),
      downloadButton("downloadReport")
      # img(src = "california.png", width = "320px")
    )
  )
)
