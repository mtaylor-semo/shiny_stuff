#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(reactable)


# UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  tags$body(style = "font-size:12px;"), # override semo_mods.css
  fluidPage(
    title = "BI 163 Grade Merger",
    fluidRow(
      column(
        3,
        offset = 9,
        div(style = "padding: 5px; display: inline-block;vertical-align:bottom; width: 200px;", p(id = "ptitle", "BI 163 Grade Merger")),
        div(style = "padding: 5px; display: inline-block;vertical-align:bottom; width: 50px;", img(src = "semo_logo.png", height = "40px")),
        tags$style(HTML("#ptitle{font-size: 20px;}"))
      )
    ),
    fluidRow(
      p(),
      column(
        3,
        fileInput(
          inputId = "choose163",
          label = "Choose 163 grades file",
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            ".csv",
            "text/tsv",
            "text/tab-separated-values",
            ".tsv"
          )
        ),
        hr(),
        p(),
        uiOutput("columns163")
      ),
      column(
        8,
        span(textOutput("filename163"), style = "color:#9D2235"),
        p(),
        reactableOutput("data163")
      )
    ),
    hr(),
    fluidRow(
      column(
        3,
        fileInput(
          inputId = "choose063",
          label = "Choose 063 grades file",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            ".csv",
            "text/tsv",
            "text/tab-separated-values",
            ".tsv"
          )
        ),
        hr(),
        p(),
        uiOutput("columns063")
      ),
      column(
        8,
        span(textOutput("filename063"), style = "color:#9D2235"),
        p(),
        reactableOutput("data063")
      )
    ),
    hr(),
    fluidRow(
      column(
        3,
        fluidRow(
          column(4, uiOutput("merge_btn")),
          column(5, uiOutput("dl_btn"))
        )
      ),
      column(
        8,
        reactableOutput("data_merged")
      )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  # Open 163 data -----------------------------------------------------------

  file163 <- reactive({
    req(input$choose163)
    input$choose163$datapath %>%
      purrr::map_dfr(~ get_grade_file(.x)) %>% 
    #read_csv(input$choose163$datapath) %>% 
      filter(Student != "Points Possible")
  })

  output$columns163 <- renderUI({
    columns <- colnames(file163())
    columns <- c("No column yet", columns[6:length(columns)])
    selectInput(
      inputId = "cols163",
      label = "Choose column to receive lab grades:",
      choices = columns,
      selected = NULL
    )
  })

  output$filename163 <- renderText(input$choose163$name)

  output$data163 <- renderReactable({
    reactable(file163() %>% trim_lecture_cols(),
      pagination = FALSE,
      highlight = FALSE,
      height = 150,
      width = 700,
      wrap = FALSE
    )
  })


  # Open 063 data -----------------------------------------------------------

  file063 <- reactive({
    req(input$choose063)
    
    column_types <- list(
            .default = col_double(),
             Student = col_character(),
             `SIS Login ID` = col_character(),
             Section = col_character()
    )

    input$choose063$datapath %>%
      purrr::map_dfr(
        ~ get_grade_file(
          .x, 
          columnTypes = column_types)) %>% 
      # purrr::map_dfr(
      #   ~ read_csv(
      #     .x,
      #     col_types = list(
      #       .default = col_double(),
      #       Student = col_character(),
      #       `SIS Login ID` = col_character(),
      #       Section = col_character()
      #     )
      #   )
      # ) %>%
      filter(
        Student != "Points Possible"
      ) %>%
      arrange(Student)
  })

  output$columns063 <- renderUI({
    columns <- colnames(file063())
    selectInput(
      inputId = "cols063",
      label = "Choose column with lab grades:",
      choices = columns,
      selected = "Current Score"
    )
  })

  output$filename063 <- renderText(input$choose063$name)

  output$data063 <- renderReactable({
    reactable(file063() %>% trim_lab_cols(),
      pagination = FALSE,
      highlight = FALSE,
      height = 150,
      wrap = FALSE,
      width = 700
    )
  })

  output$merge_btn <- renderUI({
    if (is.null(input$choose163) | is.null(input$choose063)) {
      return()
    } else {
      tagList(
        actionButton(
          inputId = "merge_button",
          label = "Merge files"
        ),
        p()
      )
    }
  })

  merged <- reactiveValues(dat = NULL)

  output$dl_btn <- renderUI({
    if (is.null(merged$dat)) {
      return()
    } else {
      tagList(
        downloadButton(
          outputId = "downloadGrades",
          label = "Download grades"
        ),
        p()
      )
    }
  })

  #
  observeEvent(input$merge_button, {
    merged$dat <- left_join(
      file163() %>% trim_lecture_cols() %>%
        add_row(Student = "Points Possible", .before = 1),
      file063() %>% select(Student, `SIS Login ID`, input$cols063)
    )

    # Total percentage points for the lab grade
    merged$dat[1, 6] <- 100

    # Did column exist in BI 163 grades?
    if (input$cols163 == "No column yet") {
      colName <- "BI063 Grade"
    } else {
      colName <- input$cols163
    }
    colnames(merged$dat)[colnames(merged$dat) == input$cols063] <- colName # input$cols163
    # }
  })

  output$data_merged <- renderReactable({
    if (is.null(merged$dat)) {
      return(NULL)
    } else {
      reactable(merged$dat,
        pagination = FALSE,
        highlight = FALSE,
        height = 150,
        width = 700,
        wrap = FALSE
      )
    }
  })


  # Download ----------------------------------------------------------------

  # Download CSV file of grades.
  # File name follows recommendation of Canvas
  output$downloadGrades <- downloadHandler(
    filename = "grades-bi_163.csv",
    content = function(file) {
      write_csv(merged$dat, file, na = "")
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
