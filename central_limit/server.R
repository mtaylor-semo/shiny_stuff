library(ggplot2)
library(shiny)


# Global vars -------------------------------------------------------------

pop_mean = 10 # Population mean
pop_sd = 1.5  # Population std dev.

sample_count_str <- "Sample"
sample_mean_str <- "Mean:"
std_dev_str <- "Standard deviation:"

# Idea for using stat_function to plot the normal curve
# https://sebastiansauer.github.io/normal_curve_ggplot2/

base_plot <- ggplot(data = data.frame(x = c(7, 13)), aes(x)) +
  labs(x = "Mean",
       y = NULL)  +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = 7:13) +
  theme_minimal() +
  theme(
    title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 16)
  ) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(color = "gray"))

curve_layer <-
  stat_function(fun = dnorm,
                n = 101,
                args = list(mean = pop_mean, sd = pop_sd))

p1 <- base_plot + curve_layer

samp_num <- 0
grand_mean <- 0

server <- function(input, output, session) {
  output$normal_plot <- renderPlot(p1)
  
  output$many_plot <- renderPlot(base_plot)
  
  output$sample_count <- renderText(sample_count_str)
  output$sample_mean <- renderText(sample_mean_str)
  output$standard_deviation <- renderText(std_dev_str)
  
  reset_count <- eventReactive(input$clear_data, {
    output$sample_count <- renderText(sample_count_str)
    output$sample_mean <- renderText(sample_mean_str)
    output$standard_deviation <- renderText(std_dev_str)
    output$sample_means <- renderText("")
  })
  
  # Single Sample -----------------------------------------------------------
  
  observeEvent(input$clear_data, {
    output$normal_plot <- renderPlot(p1)
    p1 <<- base_plot + curve_layer
    samp_num <<- 0
    grand_mean <<- 0
    reset_count()
  })
  
  
  observeEvent(input$sample_data, {
    samp <- rnorm(input$samp_size, mean = pop_mean, sd = pop_sd)
    samp_mean <- mean(samp)
    samp_sd <- round(sd(samp), 2)
    
    samp_num <<- samp_num + 1
    grand_mean <<- grand_mean + samp_mean
    
    p1 <<- p1 +
      geom_vline(aes(xintercept = samp_mean),
                 color = "grey50",
                 size = 0.5)
    
    p1 <- p1 +
      geom_vline(aes(xintercept = samp_mean),
                 color = "#9D2235",
                 size = 1.1)
    
    output$normal_plot <- renderPlot(p1)
    
    output$sample_count <-
      renderText(paste(sample_count_str, sprintf("%i", samp_num)))
    output$sample_mean <-
      renderText(paste(sample_mean_str, sprintf("%.2f", samp_mean)))
    output$standard_deviation <-
      renderText(paste(std_dev_str, sprintf("%.2f", samp_sd)))
    
    output$sample_means <-
      renderText(sprintf(
        "Mean of %i sample means: %.2f",
        samp_num,
        grand_mean / samp_num
      ))
  })
  
  
  # Many samples ------------------------------------------------------------
  
  bin_width = 0.2
  
  observeEvent(input$sample_population, {
    samples <- matrix(rnorm(
      input$number_samples * input$size_sample,
      mean = pop_mean,
      sd = pop_sd
    ))
    sample_means <- data.frame(means = apply(samples, 1, mean))
    
    output$many_plot <- renderPlot(
      base_plot +
        geom_histogram(
          data = sample_means,
          aes(x = means),
          color = "grey",
          binwidth = bin_width
        ) +
        stat_function(
          fun = function(x)
            dnorm(x, mean = pop_mean, sd = sd(samples)) * input$size_sample * input$number_samples * bin_width
        )
    )
    
    output$mean_of_means <-
      renderText(sprintf(
        "Mean of %i sample means: %.2f",
        input$number_samples,
        mean(samples)
      ))
  })
  
} # End server()
