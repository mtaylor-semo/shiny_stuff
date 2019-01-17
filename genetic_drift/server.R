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
