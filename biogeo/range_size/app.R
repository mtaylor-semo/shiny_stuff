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

file_list <- list.files("state_data/")
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

open_state_file <- function(st, tx) {
  # State and taxon
  the_state <- str_replace_all(st, " ", "_") # For two word states
  file_to_open <- paste0("state_data/", the_state, "_", tx, ".csv")
  read.csv(file_to_open, row.names = 1)
}

open_na_file <- function(natx) {
  # North American taxon
  file_to_open <- paste0("na_data/na_", natx, ".csv")
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
               p("Choose your state and then taxon 
                 to see the histogram."
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
               plotOutput("state_histogram")
             )
           )),
  
  # North America tab -------------------------------------------------------
  
  tabPanel("North America",
           sidebarLayout(
             sidebarPanel(
               p("Range size for North America."
               ),
               radioButtons("na_taxon", 
                            label = "Choose taxon:",
                            choices = c("Fishes", "Mussels"),
                            selected = "Fishes"),
               uiOutput("na_numbers")
               ),
             
             
             mainPanel(
               plotOutput("na_histogram")
             )
           )),
  # Chi-square problems  ---------------------------------------------------
  tabPanel("California Marine Fishes",
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
    open_state_file(input$state, input$taxon)
  })
  
  spp_na <- reactive({
    open_na_file(input$na_taxon)
  })
  
  output$state_numbers <- renderUI({
    dims <- dim(spp())
    sprintf("This data set has %d watersheds and %d species.", dims[1], dims[2])
  })
  
  output$na_numbers <- renderUI({
    dims <- dim(spp_na())
    sprintf("This data set has %d watersheds and %d species.", dims[1], dims[2])
  })
  
    # Output -----------------------------------------------------------
  
  output$state_histogram <- renderPlot({
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
  
  output$na_histogram <- renderPlot({
    numWatersheds <- colSums(spp_na())
    numSpecies <- rowSums(spp_na())
    bins <-
      seq(min(numWatersheds), max(numWatersheds))#, length.out = input$bins + 1)
    dat <- tibble(numWatersheds)
    
    nws <- nrow(spp_na())
    #    highSp <- ceiling(max(numSpecies)/10)*10
    
    ggplot(dat, aes(x = numWatersheds)) +
      geom_histogram(
        binwidth = 5,
        closed = "right",
        breaks = seq(0, nws, 5),
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
