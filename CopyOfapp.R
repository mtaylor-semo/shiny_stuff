library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Population Growth"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      print("Slide the bar to set population size"),
      print("\n\n"),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "capacity",
                  label = "Starting Population Size (N)",
                  min = 100,
                  max = 2000,
                  value = 1000),
      

    print("Slide the bar to set intrinsic \n"),
    print("rate of population growth."),
    sliderInput(inputId = "pop_r",
                label = "Intrinsic Rate (r)",
                min = 0,
                max = 1,
                value = 0.5)
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
    
    K <- input$capacity
    r <- input$pop_r
    gens <- 1:30
    N0 = 2
#    pop_vec <- vector(mode="numeric", length = gens)
#    pop_vec <- N0

#    for (i in 1:(gens-1)) {
#      pop_vec[i+1] <- (r*pop_vec[i]*(1-(pop_vec[i]/K))) + pop_vec[i]
#    }
    
#    print(pop_vec)

    lambda = exp(r)  #e^r
    N = N0 * lambda ^ gens
    
    print(N)
#    N = N0 * lambda^gens
    round(N, 0)
    plot(x = gens,
         y = N, 
            xlab = "Generation", 
            ylab = "Pop Size")
    lines(x = gens, y = N)
    
  })
  
}

shinyApp(ui = ui, server = server)
