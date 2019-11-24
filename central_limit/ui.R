library(ggplot2)
library(shiny)

ui <- navbarPage(theme = "semo_mods.css",
                 windowTitle = "BI 163: Central Limit Theorem",
                 title=div(img(src="semo_logo.png", height="70px"), 
                           "Central Limit Theorem"),
  tabPanel("Overview",
           mainPanel(
             p("Introduction to Central Limit Theorem")
           )),
  tabPanel("Single Samples",
           sidebarLayout(
             sidebarPanel(
               sliderInput("samp_size",
                           "Sample Size",
                           min = 10,
                           max = 50,
                           value = 30,
                           step = 5),
               actionButton("sample_data",
                            "Sample Data"),
               actionButton("clear_data",
                            "Clear Means")
             ),
           mainPanel(h4("Sample"),
                     plotOutput("normal_plot"),
                     tags$hr(),
                     textOutput("sample_means"))
)))
