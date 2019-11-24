library(ggplot2)
library(shiny)


# Global vars -------------------------------------------------------------

pop_mean = 10 # Population mean
pop_sd = 1.5

# Idea for using stat_function to plot the normal curve
# https://sebastiansauer.github.io/normal_curve_ggplot2/

base_plot <- ggplot(data = data.frame(x = c(7, 13)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = pop_mean, sd = pop_sd)) + 
  labs(x = "Mean",
       y = NULL,
       title = "Number:\nMean:\nStd Dev:")  +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = 7:13) +
  theme_minimal() +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16)) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(color = "gray"))

p1 <- base_plot
samp_num <- 0
grand_mean <- 0

server <- function(input, output, session){

  output$normal_plot <- renderPlot(base_plot)
  
  output$many_plot <- renderPlot(base_plot)
  
  observeEvent(input$clear_data, {
    output$normal_plot <- renderPlot(base_plot)
    p1 <<- base_plot
    samp_num <<- 0
    grand_mean <<- 0
#    output$sample_means <- renderText(sprintf("Mean of means:")) 
  })
  
observeEvent(input$sample_data, {
  samp <- rnorm(input$samp_size, mean = pop_mean, sd = pop_sd)
  samp_mean <- mean(samp)
  samp_sd <- round(sd(samp), 2)

  samp_num <<- samp_num + 1 
  grand_mean <<- grand_mean + samp_mean

    plot_title <- sprintf("Number: %i.\nMean: %.2f.\nStd Dev: %.2f.", 
                          samp_num, samp_mean, samp_sd)
  
  p1 <<- p1 + 
    geom_vline(aes(xintercept = samp_mean),
               color = "grey50",
               size = 0.5) 
  
  p1 <- p1 + 
    geom_vline(aes(xintercept = samp_mean), 
               color = "#9D2235",
               size = 1.1) +
    labs(title = plot_title)

  output$normal_plot <- renderPlot(p1)
  
  output$sample_means <- renderText(sprintf("Mean of %i sample means: %.2f", samp_num, grand_mean/samp_num))
})


# Many samples ------------------------------------------------------------

bin_width = 0.5

observeEvent(input$sample_population,{
  samples <- matrix(rnorm(input$number_samples*input$size_sample, 
                          mean = pop_mean, 
                          sd = pop_sd))  
  sample_means <- data.frame(means = apply(samples, 1, mean))
  
  # output$many_plot <- renderPlot(
  #   p1 +
  #     geom_histogram(data = sample_means,
  #                    aes(x = means),
  #                    alpha = 0.1,
  #                    binwidth = 0.5) +
  #     stat_function(data = data.frame(x = c(7, 13)), 
  #                   aes(x),
  #                   fun = dnorm, n = 101, args = list(mean = pop_mean, sd = pop_sd))
  #   )
  output$many_plot <- renderPlot(
    ggplot(data = sample_means, aes(x = means)) +
      geom_histogram(alpha = 0.5,
                     binwidth = bin_width) +
      stat_function(fun = function(x) dnorm(x, mean = pop_mean, sd = pop_sd) * input$size_sample*input$number_samples * bin_width)
  )
  
})



#the_samples <- matrix(rnorm(input$number_samples*input$size_sample,
#                            mean = 10,
#                            sd = 1.5))


}
