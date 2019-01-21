# Shows confidence intervals for 100 random sampling events
# from a population, with 24 individuals sampled per event.
# Idea based on https://www.stat.wisc.edu/~yandell/st571/R/append7.pdf
# 
# TO DO

library(shiny)
library(tidyverse)

dat <- c(6.2, 6.6, 7.1, 7.4, 7.6, 7.9, 
         8.0, 8.3, 8.4, 8.5, 8.6, 8.8, 
         8.8, 9.1, 9.2, 9.4, 9.4, 9.7, 
         9.9, 10.2, 10.4, 10.8, 11.3, 11.9)


n.draw = 100
mu = 9
n = 24
SD = round(sd(dat), 2)

colors_has_mean <- c("Yes" = "gray80",
                     "No" = "black")
# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("Confidence Intervals"),
   
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
        helpText("This app shows that confidence intervals are based on
                 the sample mean and may not always contain the true
                 population mean. The larger the interval, the greater
                 confidence that you have that your sample contains
                 the true population mean."),
        helpText("A population of lizards has a true mean tail length
                 of 9.0 and standard deviation of 1.43 cm. Choose the 
                 sample size and the confidence interval, and then press 
                 the sample button." ),
        helpText("Confidence intervals that do not include the true mean
                 are highlighted."),
        br(),
        helpText("Run several samples."),
        sliderInput("size", "Sample size",
                    min = 20,
                    max = 100,
                    value = 30,
                    step = 10),
        selectInput("interval", "Confidence interval",
                    choices = list(0.80, 
                                   0.90, 
                                   0.95, 
                                   0.99),
                    selected = 0.95),
         actionButton("sample",
                     "Sample population")
      ),
      
      # Show a plot
      mainPanel(
         plotOutput("intervalPlot")
      )
   )
)

# Define server logic
server <- function(input, output) {
   
  resample <- eventReactive(input$sample, {
    draws = as_tibble(
      matrix(
        rnorm(n.draw * input$size, 
              mu, SD), input$size))
    
    conf.int <- draws %>% 
      map_df(~ t.test(., conf.level = as.numeric(input$interval))$conf.int) %>% 
      rownames_to_column() %>% 
      gather(key = "run", value = "interval", -rowname) %>% 
      spread(rowname, interval) %>% 
      rename("low" = `1`,
             "high" =`2`) %>% 
      mutate(run = str_remove_all(run, pattern = "V"),
             run = as.numeric(run)) %>% 
      mutate(includes_zero = ifelse(low > mu | high < mu, "No", "Yes")) %>% 
      arrange(run)
    
  })
    
   output$intervalPlot <- renderPlot({
     
       ggplot(resample(), aes(y = run)) +
       geom_vline(xintercept = mu,
                  color = "gray60",
                  lty = 2) +
       geom_segment(aes(x = low, xend = high,
                        y = run, yend = run,
                        color = includes_zero)) +
       theme_classic() +
       scale_color_manual(values = colors_has_mean,
                          name = "Includes Mean?") +
       labs(x = "Confidence Interval",
            y = "Sample\nnumber") +
       theme(legend.position = "bottom",
             axis.title.y = element_text(angle = 0)) +
       scale_x_continuous(breaks = seq(7,11, by = 0.2)) +
       scale_y_continuous(breaks = seq(0, 100, by = 20))
   }, height = 600, unit = "px")
}

# Run the application 
shinyApp(ui = ui, server = server)

