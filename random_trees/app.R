#
## Application that generates random character matrices.
## Students draw phylogenetic trees from the matrix.
## Students can show the tree.

## Options to show / hide trees from 
## https://stackoverflow.com/q/54393592/3832941

library(shiny)
library(ape)
library(phangorn)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Draw Phylogenetic Trees"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_taxa",
                  "Number of species:",
                  min = 6,
                  max = 12,
                  value = 8,
                  step = 1),
      helpText("Choose the number of species and then draw the tree from
               the character matrix. Each row represents one species, 
               indicated by letters. Each column repesents a character, 
               indicated by C1, C2, etc.
               The number of characters is always one less than the number
               of species."),
      actionButton("new_matrix",
                   "New dataset"),
      actionButton("show_tree",
                   "Show answer")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("char_table"),
      
      plotOutput("phylo_tree")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
#   showTree <- reactiveVal(FALSE)
  
   tree_and_matrix <- function(){

    ntaxa <- input$num_taxa
    nchar <- ntaxa - 1

    char_mat <- array(0L, dim = c(ntaxa, ntaxa - 1))
    
    for (i in 1:nchar) {
      char_mat[,i] <- replace(char_mat[,i], seq(1, (ntaxa+1)-i), 1)
    }
    
    char_mat <- char_mat[sample.int(nrow(char_mat)), # Shuffle rows
                         sample.int(ncol(char_mat))] # and cols
    
    col_names <- paste0("C",seq(1,nchar))
    colnames(char_mat) <- col_names
    rownames(char_mat) <- LETTERS[1:ntaxa]
    
    mode(char_mat) <- "integer"

    upgma_tree <- upgma(dist.gene(char_mat))
    upgma_tree <- compute.brlen(upgma_tree, power = 1)
    
    # Make list item with data and tree
    list("char_mat" = char_mat,
         "phylo_tree" = upgma_tree)
   }
   
   
  get_tree <- eventReactive(input$new_matrix, {
    list <- tree_and_matrix()
  })
  
  
   output$char_table <- renderTable(
     rownames = TRUE,
     striped = TRUE, {
       
       tree_mat <- get_tree()[[1]]
       tree_mat
       
     })
   
   observeEvent(input$num_taxa, {
#     showTree(FALSE) 
     hide("phylo_tree")
   })
   
   observeEvent(input$new_matrix, {
#     showTree(FALSE) 
     hide("phylo_tree")
   })
   observeEvent(input$show_tree, {
#     showTree(TRUE)
     show("phylo_tree")
   })
   
   #p <- eventReactive(input$show_tree, {
  #   plot(get_tree()[[2]], "phylo")
   #})
   
   output$phylo_tree <- renderPlot({
      #if(showTree())
        plot(get_tree()[[2]], "phylo")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

