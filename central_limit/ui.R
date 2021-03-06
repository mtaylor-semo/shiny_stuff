library(ggplot2)
library(shiny)

ui <- navbarPage(
  theme = "semo_mods.css",
  windowTitle = "BI 163: Central Limit Theorem",
  title = div(
    img(src = "semo_logo.png", height = "70px"),
    "Central Limit Theorem"
  ),

# Overview Tab ------------------------------------------------------------
#
  tabPanel("Overview",
           mainPanel(
             h2("Introduction to the Central Limit Theorem")
           )),
  
# Single Samples tab ------------------------------------------------------
#
  tabPanel("Single Samples",
           sidebarLayout(
             sidebarPanel(
               sliderInput(
                 "samp_size",
                 "Sample Size",
                 min = 10,
                 max = 50,
                 value = 10,
                 step = 5
               ),
               actionButton("sample_data",
                            "Sample Data"),
               actionButton("clear_data",
                            "Clear Means")
             ),
             mainPanel(
               plotOutput("normal_plot"),
               tags$hr(),
               tags$h5(textOutput("sample_count")),
#               textOutput("sample_count"),
               textOutput("sample_mean"),
               textOutput("standard_deviation"),
               tags$hr(),
               textOutput("sample_means")
             )
           )),

# Many samples tab --------------------------------------------------------
#
   tabPanel("Many Samples",
            sidebarLayout(
              sidebarPanel(
                sliderInput("size_sample",
                            "Sample Size",
                            min = 10,
                            max = 50,
                            value = 10,
                            step = 5),
                sliderInput("number_samples",
                                       "Number of Samples",
                                       min = 100,
                                       max = 1000,
                                       value = 250,
                                       step = 50),
                           actionButton("sample_population",
                                        "Sample Data")),
              mainPanel(plotOutput("many_plot"),
                        tags$hr(),
                        textOutput("mean_of_means"))
            )
))
