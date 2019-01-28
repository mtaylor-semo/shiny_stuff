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
                  min = 5,
                  max = 12,
                  value = 7,
                  step = 1),
      helpText("Choose the number of species and then press the `New Dataset` button.
                Draw the tree from resulting character matrix. Each row represents one taxon, 
               indicated by T1, T2, etc. Each column repesents a character, 
               indicated by C1, C2, etc. Click on `Show answer` to reveal the correct tree.
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

    tree <- rtree(ntaxa, br = NULL)
    
    # Gets descendants, but removes the first ntaxa elements,
    # which are the individual tips
    desc <- phangorn::Descendants(tree)[-seq(1, ntaxa)]
    
    char_mat <- array(0, dim = c(ntaxa, nchar))
    
    for (i in 1:nchar) {
      char_mat[,i] <- replace(char_mat[,i], y <- desc[[i]], 1)
    }
    
    char_names <- paste0("C",seq(1,nchar))

    char_names <- sample(char_names, nchar, replace = FALSE)

    tree$node.label <- char_names
    
    colnames(char_mat) <- char_names
    rownames(char_mat) <- toupper(tree$tip.label)
    tree$tip.label <- toupper(tree$tip.label)
    
    char_mat <- char_mat[order(rownames(char_mat)),
                         order(colnames(char_mat))]
    
    mode(char_mat) <- "integer"
    
    # Make list item with data and tree
    list("char_mat" = data.frame(char_mat),
         "phylo_tree" = tree)
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
   
   output$phylo_tree <- renderPlot({
    plot(the_tree <- get_tree()[[2]], type = "phylo", direction = "upward")
    nodelabels(text = the_tree$node.label,
               bg = "white")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

