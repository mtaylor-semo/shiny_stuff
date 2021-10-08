library(dplyr)
library(stringr)

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


# Define North America Tab ------------------------------------------------

na_tab <- tabPanel(
  "North America",
  fluidRow(
    column(#style="padding-left:3%", # Same for paddingg-top, etc.
      3,
      wellPanel(
        p("Range size for North America."),
        radioButtons("na_taxon",
          label = "Choose taxon:",
          choices = c("Fishes", "Mussels"),
          selected = "Fishes"
        )
      ),
      hr(),
      p(strong("You predicted:")),
      uiOutput("prediction_na")
    ),
    column(6, plotOutput("na_histogram"),
           p(),
           hr(),
           p(),
           uiOutput("na_numbers")),
    column(
      3,
      p("Do the results agree with your prediction? Explain below, 
      then press the Next button."),
      textAreaInput(inputId = "na_result",
                    label = NULL,
                    rows = 5),
      p(),
      hr(),
      p(),
      actionButton(inputId = "btn_next_na", label = "Next", width = "35%")
    )
  )
)


# Define States tab -------------------------------------------------------

states_tab <- tabPanel(
  "State",
  fluidRow(
    column(
      3,
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
        uiOutput("dynamic_radio_buttons")
      ),
      hr(),
      p(strong("You predicted:")),
      uiOutput("prediction_state")
    ),
    column(6, plotOutput("state_histogram"),
           p(),
           hr(),
           p(),
           uiOutput("state_numbers")),
    column(
      3,
      p("Do the results agree with your prediction? Explain below, 
      then press the Next button."),
      textAreaInput(inputId = "state_result",
                    label = NULL,
                    rows = 5),
      p(),
      hr(),
      p(),
      actionButton(inputId = "btn_next_state", label = "Next", width = "35%")
    )
  )
)



# Define California Marine Tab --------------------------------------------


ca_tab <- tabPanel(
  "California Marine Fishes",
  fluidRow(
    column(
      3,
      wellPanel(radioButtons(
        inputId = "ca_marine",
        label = "Choose plot type",
        choices = c("Range size", "Range extent")
      )),
      hr(),
      p(strong("You predicted:")),
      uiOutput("prediction_ca")
    ),
    column(6, plotOutput("ca_marine_plot"),
           p(),
           hr(),
           p(),
           p("This data set has 516 species.")),
    column(
      3,
      uiOutput("ca_info"),
      p(),
      hr(),
      p(),
      p("Do the results agree with your prediction? Explain below, 
      then press the Next button."),
      textAreaInput(inputId = "ca_result",
                    label = NULL,
                    rows = 5),
      p(),
      hr(),
      p(),
      
      downloadButton("downloadReport")
      # img(src = "california.png", width = "320px")
    )
  )
)
