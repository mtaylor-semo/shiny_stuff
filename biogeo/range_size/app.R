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


## UI ----------------------------------------------------------------------

ui <- navbarPage(
  theme = "semo_mods.css",
  windowTitle = "Biogeograpy: Geographic Range Size",
  title = div(img(src = "semo_logo.png", height = "70px"),
              "Georgraphic range size"),
  
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
        explore latitudal gradient, compare taxa within state, etc...")
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
               uiOutput("state_numbers"),
               hr(),
               downloadButton('downloadReport')
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
  

# California Marine Fishes ------------------------------------------------

  tabPanel("California Marine Fishes",
           sidebarLayout(
             sidebarPanel(
               radioButtons(inputId = "ca_marine",
                            label = "Choose plot type",
                            choices = c("Range size", "Range extent")
                            ),
               p("This data set has 516 species."),
               hr(),
               img(src = "california.png", width = "320px")
             ),
             mainPanel(
               plotOutput("ca_marine_plot")
             )
           )) # End chi tabPanel
) # end UI

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
## Reactive values ---------------------------------------------------------
  
  state <- reactive({
    filter(state_taxa,
           states == input$state)
  })

  spp <- reactive({
    open_state_file(input$state, input$taxon)
  })
  
  spp_na <- reactive({
    open_na_file(input$na_taxon)
  })
  
  plots <- reactiveValues(state = NULL, na = NULL, ca = NULL)
  
## Outputs -------------------------------------------------------------

  output$dynamic_radio_buttons <- renderUI({
    choices <- unique(state()$taxa)
    freezeReactiveValue(input, "taxon")
    radioButtons(inputId = "taxon",
                 "Choose a taxon",
                 choices = choices) #c("Crayfishes", "Fishes", "Mussels"))
  })
  
  output$state_numbers <- renderUI({
    dims <- dim(spp())
    sprintf("This data set has %d watersheds and %d species.", dims[1], dims[2])
  })
  
  output$na_numbers <- renderUI({
    dims <- dim(spp_na())
    sprintf("This data set has %d watersheds and %d species.", dims[1], dims[2])
  })
  
  
## State histograms ------------------------------------------------------
  
  output$state_histogram <- renderPlot({
    numWatersheds <- colSums(spp())
    numSpecies <- rowSums(spp())
    
    nws <- nrow(spp())
    
    #dat <- tibble(numWatersheds) # Need tibble for ggplot.
    
    plots$state <- ggplot(tibble(numWatersheds), aes(x = numWatersheds)) +
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
    
    plots$state
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', 'pdf'
      )
    },
    
    content = function(file) {
      src <- normalizePath('range_report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'range_report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('range_report.Rmd', pdf_document()
      )
      file.rename(out, file)
    }
  )
## North America histogram -------------------------------------------------
  
  output$na_histogram <- renderPlot({
    numWatersheds <- colSums(spp_na())
    numSpecies <- rowSums(spp_na())
    bins <-
      seq(min(numWatersheds), max(numWatersheds))
    dat <- tibble(numWatersheds)
    
    nws <- nrow(spp_na()) # Number of watersheds for x-axis

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
  

## California Marine plots -------------------------------------------------

  output$ca_marine_plot <- renderPlot({
    cafish <- read.csv('marine/california_marine_fishes.csv', header=TRUE, row.names=1)
    
    if (input$ca_marine == "Range size") {
      rangeSize <- rowSums(cafish)
      numSpecies <- colSums(cafish)
      
      highSp <- ceiling(max(numSpecies)/10)*10
      
      max(rangeSize)	# maximum number of degrees latitude occupied
      min(rangeSize)	# minimum number of degrees latitude occupied
      mean(rangeSize)	# mean number of degrees latitude occupied
      
      dat <- tibble(rangeSize) 
      ggplot(dat, aes(x = rangeSize)) +
        geom_histogram(
          closed = "right",
          breaks = seq(0,100,5),
          color = "white"
        ) +
        scale_x_continuous(breaks = seq(0,100,20)) +
        xlab("Latitude (°N)") +
        ylab("Number of Species") +
        theme_minimal()
      
      
    } else { # plot 2.  Need better checks for the if/else
      
      mycolors <- c("#E69F00", "#56B4E9")
      numRows <- nrow(cafish) ## number of species
      numCols <- ncol(cafish) ## Number of 1° latitude cells
      
      meanCut <- 34.4481  ## Point Conception latitude as cutoff for northern and southern species.
      
      meanLat <- rep(NA,numRows) ## Create a vector same length as number of species.
      
      minLat <- vector('numeric')
      maxLat <- vector('numeric')
      
      for (i in 1:numRows) {
        x <- data.frame(cafish)[i,]
        y <- colnames(x)[x==1]
        
        colNames <- gsub('N','',y)
        colNames <- gsub('S','-',colNames)
        
        minLat[i] <- as.numeric(colNames[1])
        maxLat[i] <- as.numeric(colNames[length(colNames)])
        meanLat[i] <- mean(as.numeric(colNames))
      }
      
      cafish$minLat <- minLat
      cafish$maxLat <- maxLat
      cafish$meanLat <- meanLat
      
      latCol <- vector('character')
      for (i in 1:numRows) {
        if (cafish$meanLat[i] > meanCut){latCol[i] = mycolors[1]}
        else { latCol[i] = mycolors[2]}	
      } 
      
      cafish$latCol <- latCol
      
      cafish <- cafish[order(-cafish$minLat,-cafish$meanLat),]
      
      
      plot(nrow(cafish),99, type='n', xlim=c(1,516), ylim=c(-30,68), ylab='Latitude (°S — °N)', xlab='Species Index', main='Latitudinal Range for\nCalifornia Coastal Marine Fishes')
      for (i in 1:numRows){
        segments(x0 = i, y0 = cafish$minLat[i], x1 = i, y1 <- cafish$maxLat[i], col=cafish$latCol[i])
        
      }
      abline(h=36,col='gray')
      abline(h=meanCut,col='gray3')
      abline(h=32,col='gray')
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
