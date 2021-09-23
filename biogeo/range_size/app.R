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
library(ggplot2)
library(ggthemes)

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

    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("List of Files"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "state",
                        label = "Choose a state", 
                        choices = unique(states),
                        selected = "Georgia",
                        multiple = FALSE),
            # radioButtons("taxon", 
            #              "Choose a taxon", 
            #              choices = c("Crayfishes", "Fishes", "Mussels"),
            #              selected = "Fishes"),
            uiOutput("dynamic_radio_buttons")),

        # Show the histogram
        mainPanel(
            plotOutput("the_histogram")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    state <- reactive({
        filter(state_taxa, 
               states == input$state)
    })
    
    # observeEvent(state(), {
    #     choices <- unique(state()$taxa)
    #     freezeReactiveValue(input, "taxon")
    #     updateRadioButtons(inputId = "taxon", 
    #                        choices = choices)
    # })
    
    output$dynamic_radio_buttons <- renderUI({
        choices <- unique(state()$taxa)
        freezeReactiveValue(input, "taxon1")
        radioButtons(inputId = "taxon1", 
                     "Choose a taxon", 
                     choices = choices) #c("Crayfishes", "Fishes", "Mussels"))
    })
    
    output$the_histogram <- renderPlot({
        file_to_open <- paste0("data/",input$state, "_", input$taxon1,".csv")
        spp <- read.csv(file_to_open, row.names = 1)

        numWatersheds <- colSums(spp)
        numSpecies <- rowSums(spp)
        bins <- seq(min(numWatersheds), max(numWatersheds))#, length.out = input$bins + 1)
        dat <- tibble(numWatersheds)
     
        #paste(numWatersheds)   
        nws <- nrow(spp)
    #    highSp <- ceiling(max(numSpecies)/10)*10
        
        # draw the histogram with the specified number of bins
        #hist(numWatersheds, breaks=seq(0,nws,1), xlim = c(0,nws), xlab = 'Number of Watersheds', ylab = 'Number of Species', las=1, col = 'darkgray', border = 'white')
        ggplot(dat, aes(x=numWatersheds)) + 
            geom_histogram(binwidth = 1,
                           closed = "right", 
                           breaks=seq(0,nws,1),
                           color = "white") +
            xlab("Number of Watersheds") +
            ylab("Number of Species") +
            xlim(0,nws) +
            theme_minimal() 
    })
    
 }

# Run the application 
shinyApp(ui = ui, server = server)
