#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(stringr)
#library(magrittr)

# Load data

#spp <- read.csv("data/georgia_fishes.csv", 
#                col_names = TRUE, #read_csv only
#                row.names = 1)

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

state_taxa <- tibble(states, taxa, file_list)

    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("List of Files"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("state",
                        "Choose a state", 
                        unique(state_taxa$states)),
            radioButtons("taxon", 
                         "Choose a taxon", 
                         choices = c("Crayfishes", "Fishes", "Mussels"), 
                         selected = character(0))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("state_taxa_table"),
           verbatimTextOutput("states"),
           verbatimTextOutput("taxa")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    state <- reactive({
        filter(state_taxa, 
               states == input$state)
    })
    
    observeEvent(state(), {
        choices <- unique(state()$taxa)
        freezeReactiveValue(input, "taxon")
        updateRadioButtons(inputId = "taxon", 
                           choices = choices, 
                           selected = character(0))
    })
    output$state_taxa_table <- renderDataTable({
        state_taxa
    })
    
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        numWatersheds <- colSums(spp)
        numSpecies <- rowSums(spp)
        bins <- seq(min(numWatersheds), max(numWatersheds), length.out = input$bins + 1)

        nws <- nrow(spp)
        highSp <- ceiling(max(numSpecies)/10)*10
        
        # draw the histogram with the specified number of bins
        hist(numWatersheds, breaks=seq(0,nws,1), xlim = c(0,nws), xlab = 'Number of Watersheds', ylab = 'Number of Species', las=1, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
