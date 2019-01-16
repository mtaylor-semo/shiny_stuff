library(shiny)
library(miniUI)

# Define UI for app that draws a histogram ----
ui <- miniPage(
  gadgetTitleBar("Population Growth"),
    miniTabstripPanel("Intrinsic Rate", icon = icon("sliders"),
                      fillCol(
                        miniContentPanel(
                          plotOutput("distPlot", 
                                     height = "100%")),
                        miniContentPanel(
                          sliderInput("pop_r", 
                                      "Intrinsic Growth Rate (r)", 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.5)
                        ), height = "100%")
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    K <- 100 # input$capacity
    r <- input$pop_r
#    N = 500
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
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

shinyApp(ui = ui, server = server)
