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
