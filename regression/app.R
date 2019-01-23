# Code modified from # http://michaelminn.net/tutorials/r-simulating-data/
# Modified below for iris data.
# Will show change in scatter by changing the standard deviation
# of the error, which will simulate change in R2.

# TO add lines from actual to predicted points to show residuals.

library(shiny)
library(tidyverse)

set.seed(42)
irs <- iris %>%
  filter(Species == "versicolor")

mean_petal_length <- mean(irs$Petal.Length)
sd_petal_length <- sd(irs$Petal.Length)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Iris Regression"),
   
   # Choose standard error, default is actual sd
   # of iris versicolor petal length
   sidebarLayout(
      sidebarPanel(
         sliderInput("stdev",
                     "Amount of error:",
                     min = 0,
                     max = 0.8,
                     value = 0.34),
         helpText("More error = more scatter")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("regrPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$regrPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     pl <- rnorm(50, mean = mean_petal_length, sd = sd_petal_length)
     error <- rnorm(length(pl), 0, input$stdev)
     sl <- 2.41 + (0.828 * pl) + error
     
     irs <- tibble(pl, sl)
     
     #model <- summary(lm(sl ~ pl, data = irs)) %>% glance()
     model <- summary(lm(sl ~ pl, data = irs)) %>% glance()
     r2 <- round(model$r.squared, 2)
     
      # draw the histogram with the specified number of bins
      ggplot(data = irs, aes(x = pl, y = sl)) + 
        geom_point() +
        geom_smooth(method = "lm",
                    se = FALSE) +
        labs(title = paste("R^2 =", r2),
             x = "Petal Length (cm)",
             y = "Sepal Length (cm)")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

