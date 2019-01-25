## Based on a script written by Corey Chivers, the Bayesian Biologist.
## https://bayesianbiologist.com/2011/06/13/using-simulation-to-demonstrate-theory-hardy-weinberg-equilibrium/

## Original script available from
## https://gist.github.com/cjbayesian/468725f4bd7d6f3f027d#file-hw_sim-r

## Adapted for Shiny by MST, 24 Jan 2019

library(shiny)
library(ggplot2)
library(dplyr)

cols <-RColorBrewer::brewer.pal(3,"Dark2")
names(cols) <- c("p2", "2pq", "q2")

alleles <- c('A', 'a')
genos <- c("p2", "pq2", "q2")




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("HWE Sampling"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("pop_size",
                    "Population Size",
                    min = 50,
                    max = 450,
                    value = 200,
                    step = 50),
        sliderInput("p_freq",
                    "Choose frequency of p:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.05)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(align = "center",
         plotOutput("hwe_plot"),
         plotOutput("freq_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
# Functions ---------------------------------------------------------------

  cross <- function(parents) {
    offspring <- c('d', 'd') #initiate a child object
    offspring[1] <- sample(parents[1, ], 1)
    offspring[2] <- sample(parents[2, ], 1)
    return(offspring)
  }
  
  random_mating <- function() {
    tmp_pop <- pop
    for (n in 1:N)
    {
      parents <- sample(1:N, 2)
      tmp_pop[n, ] <- cross(pop[parents, ])
    }
    pop <- tmp_pop
  }
  
  theoretical_curves <- 
    list(function(x) x^2, 
         function(x) 2 * x * (1 - x),
         function(x) (1 - x)^2)


# Reactives ---------------------------------------------------------------

  sample_pop <- reactive({
      p <- 0.5
      q <- 1 - p
      N = input$pop_size
      a_freq <- c(p, q)
      
      pop <- array(sample(alleles, 2 * N, 
                          p = a_freq, 
                          replace = T), 
                   dim = c(N, 2))
      
      
      #     I <- N
      num_generations = 1
      g_freq <- array(dim = c(N, 3))
      p_vec <- array(dim = N)
      for (i in 1:N)
      {
        p <- runif(1, 0, 1)
        q <- 1 - p
        a_freq <- c(p, q)
        
        pop[, 1] <- sample(alleles, N, p = a_freq, replace = T)
        pop[, 2] <- sample(alleles, N, p = a_freq, replace = T)
        
        for (g in 1:num_generations)
          random_mating()
        
        f_aa <- 0
        f_Aa <- 0
        f_AA <- 0
        
        for (n in 1:N)
        {
          if (identical(pop[n, ], c('A', 'A')))
            f_AA = f_AA + 1
          if (identical(pop[n, ], c('A', 'a')) ||
              identical(pop[n, ], c('a', 'A')))
            f_Aa = f_Aa + 1
          if (identical(pop[n, ], c('a', 'a')))
            f_aa = f_aa + 1
          
        }
        f_aa <- f_aa / N
        f_Aa <- f_Aa / N
        f_AA <- f_AA / N
        
        g_freq[i, ] <- c(f_AA, f_Aa, f_aa)
        p_vec[i] <- p
      }
      
      data_frame(p = p_vec, 
                 p2 = g_freq[, 1],
                 pq2 = g_freq[, 2],
                 q2 = g_freq[, 3])
    })


# Plots -------------------------------------------------------------------


  output$hwe_plot <- renderPlot(height = 300,{

     hw <- sample_pop()
     
     p <-ggplot(data = hw) +
       xlim(c(0, 1)) +
       ylim(c(0, 1))
     
     for(i in 1:length(theoretical_curves))
       p <- p + stat_function(aes(y = 0),
                              fun = theoretical_curves[[i]], 
                              colour = cols[i])
     p <- p + 
       geom_point(aes(x = p, y = p2),
                  color = cols["p2"]) +
       geom_point(aes(x = p, y = pq2),
                  color = cols["2pq"],
                  shape = 15) +
       geom_point(aes(x = p,
                      y = q2),
                  color = cols["q2"],
                  shape = 17)
     
     p <- p + geom_vline(aes(xintercept = input$p_freq))
     
     print(p)
   })
  
  output$freq_plot <- renderPlot(height = 300, {
    
    freq <- input$p_freq
    N <- input$pop_size
    
    df <- data_frame(genotype = factor(genos, levels = genos, ordered = TRUE),
                    freqs = c(freq^2, 2 * freq * (1-freq), (1 - freq)^2))

    sampled <- sample_pop() %>% 
      filter(p >= freq - 0.05 & p <= freq + 0.05) %>% 
      select(contains("2")) %>% 
      gather(key = genotype,
             value = freqs)
    
    ggplot(df, aes(x = genotype,
                   y = freqs)) + ylim(0,1) +
      geom_point(data = sampled,
                  aes(x = genotype,
                      y = freqs,
                      color = genotype),
                  alpha = 0.5) +
      geom_point(aes(fill = genotype),
                 size = 3,
                 shape = 21) +
      scale_fill_brewer(type = "qual",
                        palette = "Dark2",
                        guide = FALSE) +
      scale_color_brewer(type = "qual",
                        palette = "Dark2",
                        guide = FALSE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

