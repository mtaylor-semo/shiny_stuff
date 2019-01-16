library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Simple HWE example"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      print("Slide the bar to change\nthe A1 frequency."),
      print("\n\n"),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "freq",
                  label = "Allele A1 frequency",
                  min = 0,
                  max = 1,
                  value = 0.5),
      

    print("Choose the relative fitness of each genotype.\n"),
    print("Leave all three at 1 for no selection."),
    sliderInput(inputId = "w11_fitness",
                label = "Fitness A1A1 genotype",
                min = 0,
                max = 1,
                value = 1),
    
    sliderInput(inputId = "w12_fitness",
                label = "Fitness A1A2 genotype",
                min = 0,
                max = 1,
                value = 1),
    
    sliderInput(inputId = "w22_fitness",
                label = "Fitness A2A2 genotype",
                min = 0,
                max = 1,
                value = 1)
    ),
  
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- input$freq
    w11 <- input$w11_fitness
    w12 <- input$w12_fitness
    w22 <- input$w22_fitness
    
    freqs <- c(x^2*w11, 2*x*(1-x)*w12, (1-x)^2*w22)
    
    barplot(height = freqs, 
            xlab = "Genotype", 
            ylab = "Frequency", 
            names.arg = c("A1A1", "A1A2", "A2A2"),
            ylim = c(0,1))
    
  })
  
}

shinyApp(ui = ui, server = server)
