library(tidyverse)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Genetic Drift"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput(
        "num_pops",
        label = "Number of populations",
        choices = 1:5,
        selected = 1
      ),
      br(),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "num_gens",
                  label = "Number of generations",
                  min = 1000,
                  max = 6000,
                  value = 3000,
                  step = 100),
      br(),
      sliderInput(inputId = "pop_size",
                  label = "Starting population size",
                  min = 100,
                  max = 5000,
                  value = 500,
                  step = 100),
      br(),
      sliderInput(inputId = "start_freq",
                  label = "Initial A1 Frequency",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.05),
      br(),
      checkboxInput(inputId = "show_q",
                    label = "Show A2 allele?")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      helpText("Simulation of genetic drift. Choose the number of
               populations, generations, starting populations size,
               and the starting frequency of the A1
               allele. 'Show A2 allele' will show the frequencies of
               the A1 and A2 
               alleles, but only when one population is simulated."),
      
      helpText("The horizontal gray line shows the A1 frequency
               over time when all Hardy-Weinberg assumptions are met."),
      # Output: Histogram ----
      plotOutput(outputId = "driftPlot")
      
    )
  )
)

server <- function(input, output) {
  
    output$driftPlot <- renderPlot({
    N <- input$pop_size # number of diploid individuals
    num_chromosomes <- 2 * N # number of chromosomes
    p <- input$start_freq
    
    
    X <- array(0, dim = c(
      input$num_gens,
      input$num_pops
    ))
    X[1, ] <- rep(
      num_chromosomes * p,
      input$num_pops
    )
    
    for (j in 1:input$num_pops) {
      for (i in 2:input$num_gens) {
        X[i, j] <- rbinom(1,
                          num_chromosomes,
                          prob = X[i - 1, j] / num_chromosomes
        )
      }
    }
    
    X <- data.frame(X)
    
    # Gather the data and plot the 5 simulations
    sim_data <- X %>%
      gather(
        key = "population",
        value = "freq"
      ) %>% 
      mutate(freq = freq / num_chromosomes,
             population = str_remove(population, "X"))
  
    ggplot(sim_data,
           aes(x = rep(c(1:input$num_gens), input$num_pops), 
               y = freq,
               color = population)) + 
      {if (input$show_q & 
           input$num_pops == 1) geom_line(aes(y = 1-freq,
                                       group = "population"), 
                                   color = "gray85")} +
      geom_hline(yintercept = input$start_freq, 
                 color = "grey80") +
      geom_line() +
      labs(
        x = "Generation",
        y = "A1 Frequency",
        color = "Population") +
      coord_cartesian(ylim = c(0, 1)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      theme_classic()
  })

  }


# Run app ----
shinyApp(ui, server)
