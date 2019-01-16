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
                      fillRow(miniContentPanel(
                        sliderInput("pop_b", 
                                    "Birth Rate (b)", 
                                    min = 0, 
                                    max = 1, 
                                    value = 0.5)
                      ), miniContentPanel(
                        sliderInput("pop_d", 
                                    "Death Rate (d)", 
                                    min = 0, 
                                    max = 1, 
                                    value = 0.5)
                      ), width = "100%"), height = "100%")
  )
)

server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    
    K <- 100 # input$capacity
    b <- input$pop_b
    d <- input$pop_d
    r <- b - d
    #N = 500
    gens <- 1:30
    N0 = 100
    #    pop_vec <- vector(mode="numeric", length = gens)
    #    pop_vec <- N0
    
    #    for (i in 1:(gens-1)) {
    #      pop_vec[i+1] <- (r*pop_vec[i]*(1-(pop_vec[i]/K))) + pop_vec[i]
    #    }
    
    #    print(pop_vec)
    
    lambda = exp(r)  #e^r
    N = N0 * lambda ^ gens
    
  #  print(N)
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
