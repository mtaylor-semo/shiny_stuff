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
               radioButtons("sample_choice",
                            "Sample Size",
                            choices = c("10" = "10",
                                        "30" = "30",
                                        "100" = "100")),
               actionButton("sample_data",
                            "Sample Data"),
               actionButton("clear_data",
                            "Clear Means")
             ),
             mainPanel(
               plotOutput("normal_plot"),
               tags$hr(),
               tags$h5(textOutput("sample_count")),
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
                radioButtons("sample_size_many",
                             "Sample size",
                             choices = c("10" = "10",
                                         "30" = "30",
                                         "100" = "100")),

                radioButtons("number_samples",
                              "Number of Samples",
                               choices = c("100" = "100",
                                           "250" = "250",
                                           "1000" = "1000")),
                           actionButton("sample_population",
                                        "Sample Data")),
              mainPanel(plotOutput("many_plot"),
                        tags$hr(),
                        textOutput("mean_of_means"),
                        textOutput("sd_of_means"))
            )
))
