#
# 
# 
#
# 
#
# 
#


# Libraries ---------------------------------------------------------------


library(shiny)
library(tidyverse)
library(broom)


# Global vars -------------------------------------------------------------


mean_colors <- RColorBrewer::brewer.pal(3, "Dark2")

stdev <- 3
short_mean <- 66.5

y_point <- 0.45
alpha <- 0.05
negative_t <- NULL
min_scale = -4
max_scale = 4
t_range <- tibble(t = c(min_scale, max_scale))


# Shiny UI ----------------------------------------------------------------

ui <- fluidPage(
   
   # Application title
   titlePanel("T-test"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
         sliderInput("tall_mean",
                     "Choose Mean:",
                     min = 65,
                     max = 75,
                     value = 70,
                     step = 0.5),
         sliderInput("sample_size",
                     "Sample size (each population)",
                     min = 10,
                     max = 40,
                     value = 25,
                     step = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("densityPlot"),
         plotOutput("tPlot")
      )
   )
)


# Shiny server ------------------------------------------------------------

server <- function(input, output) {
  
  short_set <- reactive({
    short = rnorm(input$sample_size, 
                  mean = short_mean, 
                  sd = stdev)
  })
  
  data_set <- reactive({
    df <- tibble(short = short_set(),
                 tall = rnorm(input$sample_size, 
                              mean = input$tall_mean, 
                              sd = stdev))
    df <- gather(df, key = "group", 
                 value = "height")
  })
  
  output$densityPlot <- renderPlot({
    
    t_result <- tidy(t.test(height ~ group, 
                            data = data_set(), 
                            var.equal = TRUE))
    
    t_value <- round(abs(t_result$statistic), 2)
    degrees_freedom <- round(t_result$parameter, 2)  
    
    ggplot(data_set()) + 
      geom_density(aes(x = height, 
                       fill = group),
                   alpha = 0.5) +
      scale_x_continuous(limits = c(55,85), breaks = seq(55, 85, by = 5)) +
      scale_y_continuous(limits= c(0, 0.2)) +
      geom_vline(xintercept = t_result$estimate1,
                 color = mean_colors[1]) +
      geom_vline(xintercept = t_result$estimate2,
                 color = mean_colors[2]) +
      scale_fill_brewer(palette = "Dark2") +
      ggtitle(paste("t =", t_test, "with", degrees_freedom, "degrees of freedom"))
  })

  output$tPlot <- renderPlot({
    ggplot(data = data.frame(x = 1:10, y = 1:10)) + geom_point(aes(x, y))
  })  
  
}


# Run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

