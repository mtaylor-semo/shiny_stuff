# Code modified from # http://michaelminn.net/tutorials/r-simulating-data/
# Modified below for iris data.
# Will show change in scatter by changing the standard deviation
# of the error, which will simulate change in R2.

# TO add lines from actual to predicted points to show residuals.

library(shiny)
library(tidyverse)
library(broom)

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
      sliderInput("slope",
                  "Value of beta:",
                  min = -1,
                  max = 1,
                  value = 0.8,
                  step = 0.05),
      sliderInput("stdev",
                  "Amount of error:",
                  min = 0,
                  max = 0.8,
                  value = 0.34),
      helpText("More error = more scatter"),
      checkboxInput("showLM", "Show regression line?",
                    value = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("regrPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  regrData <- reactive({
    pl <- rnorm(50, mean = mean_petal_length, sd = sd_petal_length)
    error <- rnorm(length(pl), 0, input$stdev)
    sl <- 2.41 + (input$slope * pl) + error
    
    tibble(pl, sl)
    
  })
  
  fittedData <- reactive({
    df <- lm(sl ~ pl, regrData()) %>% augment()
  })
  
  output$regrPlot <- renderPlot({
    
    model <- summary(lm(sl ~ pl, data = regrData())) %>% glance()
    r2 <- round(model$r.squared, 2)
    
    cor_r <- round(cor(regrData()$sl, regrData()$pl), 2)
    
    p1 <- ggplot(data = regrData(), aes(x = pl, y = sl)) + 
      geom_point() +
      labs(title = paste("R^2 =", r2,". Correlation =", cor_r),
           x = "Petal Length (cm)",
           y = "Sepal Length (cm)") +
      geom_segment(data = fittedData(),
                   aes(xend = pl, yend = .fitted), alpha = .2) +
      # Add a "Show fitted points" checkbox.
      geom_point(data = fittedData(),
                 aes(y = .fitted), shape = 1)
    
    p2 <- {if (input$showLM) p1 + geom_smooth(method = "lm",
                                              se = FALSE) else p1}
    print(p2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

