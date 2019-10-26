#
# Shiny app to show how student's t relates to the differerence
# between the mean of two groups. 
#
# One group has a fixed mean. The other mean is chosen by the user.
# Sample size can also be chosen. 
# 
#

# TO DO
# Add instructions
# Remove some redundancy? I think some rounding is redundant.

# Libraries ---------------------------------------------------------------


library(shiny)
library(tidyverse)
library(broom)
library(RColorBrewer)

# Global vars -------------------------------------------------------------


mean_colors <- brewer.pal(3, "Dark2")

stdev <- 3
short_mean <- 68
tall_stdev <- 5


y_point <- 0.45
alpha <- 0.05
negative_t <- NULL
min_scale = -4
max_scale = 4
t_range <- tibble(t = c(min_scale, max_scale))


# Shiny UI ----------------------------------------------------------------

ui <- fluidPage(theme = "bootstrap2015.css",
  
  # Application title
  titlePanel(title=div(img(src="semo_logo.png", height="50px"), "T-test")),
  # First Row
  fluidRow(
    column(3, wellPanel(
    sliderInput(
      "tall_mean",
      "Mean of variable population:",
      min = 62,
      max = 74,
      value = 68,
      step = 1
    ),
    sliderInput(
      "sample_size",
      "Sample size (each population)",
      min = 10,
      max = 40,
      value = 25,
      step = 1
    ),
    p("One sample has a mean of about 68. Change the slider
       to set the mean for the second (variable) sample. Change the slider
       to change the same size for each sample"
    ),
    p("TO DO: Add a Resample button.")
  )),
  column(7, offset = 1,
         plotOutput("densityPlot")
  )),
  hr(),
  # Second row
  fluidRow(
    column(3, wellPanel(
           p("TO DO: Add info on how to interpret the arrow and the range encompassing zero.")
    )),
    column(7, offset = 1,
           plotOutput("tPlot")
    )
  )
)
  


# Shiny server ------------------------------------------------------------

server <- function(input, output) {
  
# Reactives ---------------------------------------------------------------
  short_set <- reactive({
    short = rnorm(input$sample_size, 
                  mean = short_mean, 
                  sd = stdev)
  })

  data_set <- reactive({
    df <- tibble(Fixed = short_set(),
                 Variable = rnorm(input$sample_size, 
                              mean = input$tall_mean, 
                              sd = stdev))
    df <- gather(df, key = "group", 
                 value = "height")
  })
  
  t_test <- reactive({
    tidy(t.test(height ~ group, 
                data = data_set(), 
                var.equal = TRUE))
  })
  

# Upper plot: two normal distribs -----------------------------------------

  
  output$densityPlot <- renderPlot({
    
    t_value <- round(t_test()$statistic, 2)
    degrees_freedom <- round(t_test()$parameter, 2)  
    
    ggplot(data_set()) + 
      geom_density(aes(x = height, 
                       fill = group),
                   alpha = 0.5) +
      scale_x_continuous(limits = c(55,85), breaks = seq(55, 85, by = 5)) +
      scale_y_continuous(limits= c(0, 0.2)) +
      geom_vline(xintercept = t_test()$estimate1,
                 color = mean_colors[1]) +
      geom_vline(xintercept = t_test()$estimate2,
                 color = mean_colors[2]) +
      scale_fill_brewer(palette = "Dark2") +
      labs(x = "Mean", 
           y = "Probability",
           fill = "Sample",
           title = paste("t =", t_value, "with", degrees_freedom, "degrees of freedom"))
#      ggtitle(paste("t =", t_value, "with", degrees_freedom, "degrees of freedom"))
  })


# Lower plot: t distrib ---------------------------------------------------

  
  output$tPlot <- renderPlot({
    
    crit_t <- round(qt(1-alpha/2, t_test()$parameter), 2)

    if(t_test()$statistic < 0) {
      shaded_area <- c(min_scale, -crit_t)
    } else {
      shaded_area <- c(crit_t, max_scale)
    }
    
    crit_y <- round(dt(crit_t, t_test()$parameter), 2)
    calc_y <- round(dt(t_test()$statistic, t_test()$parameter), 2)
    
    arrows_df <- tibble(crit_xstart = crit_t, 
                        crit_xend = crit_t,
                        crit_ystart = crit_y,
                        crit_yend = crit_y + 0.05,
                        calc_xstart = t_test()$statistic, 
                        calc_xend = t_test()$statistic,
                        calc_ystart = calc_y,
                        calc_yend = calc_y + 0.05)
    
      ggplot(t_test(), aes(y = y_point)) +
        geom_vline(aes(xintercept = 0), color = "gray75") +
        stat_function(data = t_range,
                      aes(x = t),
                      fun = dt, args = list(df = t_test()$parameter),
                      xlim = shaded_area,
                      geom = "area",
                      fill = "gray") +
        stat_function(data = t_range, 
                    aes(x = t),
                    fun = dt, args = list(df = t_test()$parameter)) +
      geom_segment(aes(x = conf.low, 
                       xend = conf.high, 
                       yend = y_point),
                   color = mean_colors[3],
                   size = 1) + 
        geom_point(aes(x = conf.low), size = 1, color = mean_colors[3]) +
        geom_point(aes(x = conf.high), size = 1, color = mean_colors[3]) +
        geom_point(aes(x = estimate1 - estimate2),
                   shape = 17,
                   color = mean_colors[3],
                   size = 3) +
      geom_segment(data = arrows_df,
                     aes(x = calc_xstart, 
                         xend = calc_xend,
                         y = 0, 
                         yend = 0.05),
                     arrow = arrow(ends = "first",
                                   type = "closed",
                                   length = unit(2, "mm"))) +
        geom_text(data = arrows_df,
                  aes(x = calc_xstart,
                      y = 0.05),
                  label = paste("t =", round(t_test()$statistic, 2)),
                  vjust = 0,
                  position = position_nudge(y = 0.01)) +
        labs(x = expression("Student's"~italic(t)),
             y = "Probability")
  })  
  
}


# Run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

