##
## Show range size histograms for fishes, crayfishes, or mussels
## at state and North American levels.

library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# Global variables --------------------------------------------------------

# Define global variables here.

file_list <- list.files("data/")
file_list_no_ext <- tools::file_path_sans_ext(file_list)

states <- file_list_no_ext %>%
  word(start = 1, end = -2, sep = "_") %>%
  str_replace("_", " ") %>%
  str_to_title()

taxa <-
  word(file_list_no_ext,
       start = -1,
       sep = "_") %>%
  str_to_title()

state_taxa <- tibble(states, taxa)

# Global functions --------------------------------------------------------

# Open the data set.

open_file <- function(st, tx) {
  # State and taxon
  the_state <- str_replace_all(st, " ", "_") # For two word states
  file_to_open <- paste0("data/", the_state, "_", tx, ".csv")
  read.csv(file_to_open, row.names = 1)
}

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  theme = "semo_mods.css",
  windowTitle = "Biogeograpy: Range Size",
  title = div(img(src = "semo_logo.png", height = "70px"),
              "Range size"),
  
  # Overview tab ------------------------------------------------------------
  
  tabPanel(
    "Overview",
    mainPanel(
      p("This app allows you to explore range sizes for three 
        aquatic groups (crayfishes, fishes, and mussels) for
        several states and for North America (primarily U.S.)."),
      p("Choose the State tab to begin. Choose the state and the
        taxon that was assigned to you."),
      p("NOTE TO MST: Rework the assignment to have students 
        explore latitudal gradient, compare taxa within state, etc..."),
      p(strong("Consider adding California marine taxa here."))
    )
  ),
  

  # State tab ---------------------------------------------------------------

  tabPanel("State",
           sidebarLayout(
             sidebarPanel(
               p("Choose your state and then taxon to see the histogram."
               ),
               selectInput(inputId = "state",
                           label = "Choose a state", 
                           choices = unique(states),
                           selected = "Georgia",
                           multiple = FALSE),
               uiOutput("dynamic_radio_buttons"),
               uiOutput("state_numbers")
             ),
             
             mainPanel(
               plotOutput("the_histogram"),
               p(), # A global variable was called here.
               hr(),
               htmlOutput("question"),
               hr(),
               htmlOutput("show_hist_result")
             )
           )),
  
  # North America tab -------------------------------------------------------
  
  tabPanel("North America",
           sidebarLayout(
             sidebarPanel(
               p(
                 "Press \"New problem\" for a new problem to solve. Press
                 \"Show answer\" to see the solution. Refer to your notes
                 for details of the steps."
               ),
               actionButton("button_three",
                            "Button 3"),
               actionButton("button_four",
                            "Button 4")
             ),
             mainPanel(
               p(
                 "Use the methods learned in class to calculate whether
                 this population is in Hardy-Weinberg equilibrium.
                 Use the observed number of each genotype to calculate
                 genotype frequencies and observed allele frequencies."
               ),
               p(strong(
                 "Round each step to 3 digits after the decimal point."
               )),
               p(),
               # Global variable was called here.
               tags$hr(),
               textOutput("intro_counting"),
               htmlOutput("question_counting"),
               tags$hr(),
               htmlOutput("answer_counting"),
               uiOutput("check")
             )
           )),
  # Chi-square problems  ---------------------------------------------------
  tabPanel("Tab 3",
           sidebarLayout(
             sidebarPanel(
               p("A third tab if you need it."),
               actionButton("new_chi_problem",
                            "New problem"),
               actionButton("show_chi_answer",
                            "Show answer")
             ),
             mainPanel("Text for Tab 3 Main Panel")
           )) # End chi tabPanel
) # end UI

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  state <- reactive({
    filter(state_taxa,
           states == input$state)
  })
  
  
  output$dynamic_radio_buttons <- renderUI({
    choices <- unique(state()$taxa)
    freezeReactiveValue(input, "taxon")
    radioButtons(inputId = "taxon",
                 "Choose a taxon",
                 choices = choices) #c("Crayfishes", "Fishes", "Mussels"))
  })
  
  spp <- reactive({
    open_file(input$state, input$taxon)
  })
  
  output$state_numbers <- renderUI({
    dims <- dim(spp())
    sprintf("This data set has %d watersheds and %d species.", dims[1], dims[2])
  })
  
  # Output -----------------------------------------------------------
  
  output$the_histogram <- renderPlot({
    numWatersheds <- colSums(spp())
    numSpecies <- rowSums(spp())
    bins <-
      seq(min(numWatersheds), max(numWatersheds))#, length.out = input$bins + 1)
    dat <- tibble(numWatersheds)
    
    nws <- nrow(spp())
    #    highSp <- ceiling(max(numSpecies)/10)*10
    
    ggplot(dat, aes(x = numWatersheds)) +
      geom_histogram(
        binwidth = 1,
        closed = "right",
        breaks = seq(0, nws, 1),
        color = "white"
      ) +
      xlab("Number of Watersheds") +
      ylab("Number of Species") +
      xlim(0, nws) +
      theme_minimal()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
