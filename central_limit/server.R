library(ggplot2)
library(shiny)


# Global vars -------------------------------------------------------------

pop_mean = 10 # Population mean
pop_sd = 1.5  # Population std dev.

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

# Begin Server ------------------------------------------------------------
#
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  
  ## Reactive variables
  samp_num <- reactiveVal(0)
  grand_mean <- reactiveVal(0)
  mean_str <- reactiveVal("Mean:")
  sd_str <- reactiveVal("Standard deviation:")
  
  past_means <- reactiveValues()
  reset_means <- function() {
    past_means$df <- data.frame(means = numeric(0))
  }
  past_means$df <- reset_means()
  
  
  clear_single_data <- function() {
    samp_num(0)
    grand_mean(0)
    mean_str("Mean:")
    sd_str("Standard deviation:")
    output$sample_mean <- renderText(mean_str())
    output$standard_deviation <- renderText(sd_str())
    past_means <- reset_means()
  }
  
  # Single Sample -----------------------------------------------------------
  
  one_sample <- eventReactive(input$sample_data, {
    rnorm(as.numeric(input$sample_size_single),
          mean = pop_mean,
          sd = pop_sd)
  })
  
  
  samp_mean <- eventReactive(input$sample_data, {
    mean(one_sample())
  })
  
  samp_sd <- eventReactive(input$sample_data, {
    sd(one_sample())
  })
  
  observeEvent(input$clear_data, {
    clear_single_data()
  })
  
  observeEvent(input$sample_size_single, {
    clear_single_data()
  })
  
  observeEvent(input$sample_data, {
    samp_num(samp_num() + 1)
    grand_mean(grand_mean() + samp_mean())
    mean_str(sprintf("Mean: %.2f", samp_mean()))
    sd_str(sprintf("Standard deviation: %.2f", samp_sd()))
    past_means$df[samp_num(), ] <- samp_mean()
  })
  
  output$normal_plot <- renderPlot({
    if (samp_num() == 0) {
      base_plot + curve_layer
    } else {
      base_plot + curve_layer +
        geom_vline(
          data = past_means$df,
          aes(xintercept = means),
          color = "grey50",
          size = 0.5
        ) +
        geom_vline(aes(xintercept = samp_mean()),
                   color = "#9D2235",
                   size = 1.1)
    }
  })
  
  
  output$sample_count <-
    renderText(sprintf("Sample %i", samp_num()))
  output$sample_mean <-
    renderText(mean_str())
  output$standard_deviation <-
    renderText(sd_str())
  
  observe(output$sample_means <-
            if (samp_num() > 0) {
              renderText(sprintf(
                "Mean of %i sample means: %.2f",
                samp_num(),
                grand_mean() / samp_num()
              ))
            } else {
              renderText("")
            })
  
  
  
  # Many samples ------------------------------------------------------------
  # 

  bin_width = 0.2
  
  samples <- eventReactive(input$sample_population, {
    matrix(rnorm(
      as.numeric(input$number_samples) * as.numeric(input$sample_size_many),
      mean = pop_mean,
      sd = pop_sd
    ))
  })
  
  
  sample_means <- eventReactive(input$sample_population, {
    data.frame(means = apply(samples(), 1, mean))
  })
  
  output$many_plot <-
    renderPlot(
      base_plot +
        geom_histogram(
          data = sample_means(),
          aes(x = means),
          color = "grey",
          fill = "grey60",
          binwidth = bin_width
        ) +
        stat_function(
          fun = function(x)
            dnorm(x, mean = pop_mean, sd = pop_sd) * as.numeric(input$sample_size_many) * as.numeric(input$number_samples) * bin_width
        )
    )
  
  output$mean_of_means <-
    renderText(sprintf(
      "Mean of %i sample means: %.2f",
      as.numeric(input$number_samples),
      mean(samples())
    ))
  output$sd_of_means <-
    renderText(sprintf(
      "Variance of %i sample means: %.4f",
      as.numeric(input$number_samples),
      var(samples()) / as.numeric(input$number_samples)
    ))

} # End server()
