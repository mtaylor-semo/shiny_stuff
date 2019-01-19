#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(broom)

stdev <- 3
short_mean <- 66.5

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("T-test"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("tall_mean",
                     "Choose Mean:",
                     min = 65,
                     max = 75,
                     value = 70,
                     step = 0.5),
         sliderInput("sample_size",
                     "Sample size",
                     min = 10,
                     max = 40,
                     value = 25,
                     step = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("densityPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_set <- reactive({
    df <- tibble(short = rnorm(input$sample_size, 
                               mean = short_mean, 
                               sd = stdev),
                 tall = rnorm(input$sample_size, 
                              mean = input$tall_mean, 
                              sd = stdev))
    df <- gather(df, key = "group", 
                 value = "height")
  })
  
#  t_test <- reactive({
#  })

    output$densityPlot <- renderPlot({
      
    t_test <- tidy(t.test(height ~ group, 
                          data = data_set(), 
                          var.equal = TRUE))$statistic
    t_test <- round(abs(t_test), 2)
    
    
    ggplot(data_set()) + 
        geom_density(aes(x = height, 
                         fill = group),
                     alpha = 0.5) +
        geom_text(aes(x = 60, y = 0.12), label = t_test)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

