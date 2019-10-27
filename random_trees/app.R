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
library(gtools)
# Define UI for application that draws a histogram
ui <- navbarPage(theme = "semo_mods.css",
                 windowTitle = "BI 163: Tree-building practice",
                 title=div(img(src="semo_logo.png", height="70px"), 
                           "Tree-building practice"),
  
  # Application title
#  "BI 163 Tree-building practice",
  
  tabPanel(
    "Overview",
    mainPanel(
      p(
        "This site presents two types of phylogenetic tree problems so you
        can practice your tree-building skills, for 5 to 12 taxa.", strong("Remember:"), "A taxon (plural: taxa) can represent a species, a family, or 
        any other taxonomic level. "
      ),
      p(
        "The \"Data First\" tab starts with a presence-absence (0/1) character matrix
        that you use as data to build a tree."
      ),
      p(
        "The \"Tree First\" tab starts with a phylogenetic tree that you use to 
        make the presence-absence (0/1) character matrix. This will help you think 
        about phylogenetic trees in a different way."
      ),
      p(
        strong("The phylogenetic trees will not show the root but you should draw the
               root on your trees.")
      )
      )),

#Sidebar with a slider input for number of bins
    tabPanel("Data First",
             sidebarLayout(
    sidebarPanel(shinyjs::useShinyjs(),
      sliderInput("num_taxa",
                  "Number of species:",
                  min = 5,
                  max = 12,
                  value = 7,
                  step = 1),
      p("Choose the number of taxa and then press the `New dataset` button.
                Draw the tree from resulting character matrix. Each row represents one taxon,
               indicated by T1, T2, etc. Each column repesents a character,
               indicated by C1, C2, etc. Click on `Show answer` to reveal the correct tree.
               The number of characters is always one less than the number
               of taxa."),
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
),
tabPanel("Tree First",
         sidebarPanel(
           sliderInput("num_spp",
                       "Number of species:",
                       min = 5,
                       max = 12,
                       value = 7,
                       step = 1),
           p("Choose the number of taxa and then press the `New tree` button.
                    Build the character matrix from the resulting tree. Make each row 
                    represent one taxon, using T1, T2, etc. Make each column repesents 
                    one character, using C1, C2, etc. Click on `Show answer` to reveal the 
                    correct matrix. The number of characters is always one less than the 
                    number of taxa."),
           actionButton("new_tree",
                        "New tree"),
           actionButton("show_matrix",
                        "Show answer")
           ),
         # Show a plot of the generated distribution
         mainPanel(
           plotOutput("my_tree"),
           tableOutput("char_matrix")
         )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
#   showTree <- reactiveVal(FALSE)
  
   tree_and_matrix <- function(num_species){

    ntaxa <- num_species
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
    
    char_mat <- char_mat[mixedorder(rownames(char_mat)),
                         mixedorder(colnames(char_mat))]
    
    mode(char_mat) <- "integer"
    
    # Make list item with data and tree
    list("char_mat" = data.frame(char_mat),
         "phylo_tree" = tree)
   }
   
   
  get_tree <- eventReactive(input$new_matrix, {
    list <- tree_and_matrix(input$num_taxa)
  })
  
  
  get_matrix <- eventReactive(input$new_tree, {
    list <- tree_and_matrix(input$num_spp)
  })
  
   output$char_table <- renderTable(
     rownames = TRUE,
     striped = TRUE, {
       
       tree_mat <- get_tree()[[1]]
       tree_mat
       
     })

   output$char_matrix <- renderTable(
     rownames = TRUE,
     striped = TRUE, {
       
       char_mat <- get_matrix()[[1]]
       char_mat
       
     })
   
   observeEvent(input$num_taxa, {
#     showTree(FALSE) 
     hide("phylo_tree")
   })
   
   observeEvent(input$num_spp, {
     #     showTree(FALSE) 
     hide("char_matrix")
   })
   
   observeEvent(input$new_matrix, {
#     showTree(FALSE) 
     hide("phylo_tree")
   })
   observeEvent(input$show_tree, {
     #     showTree(TRUE)
     show("phylo_tree")
   })
   
   observeEvent(input$new_tree, {
     #     showTree(TRUE)
     show("my_tree")
   })
   
   observeEvent(input$new_tree, {
     #     showTree(TRUE)
     hide("char_matrix")
   })
   
   observeEvent(input$show_matrix, {
     #     showTree(TRUE)
     show("char_matrix")
   })
   
   
   output$phylo_tree <- renderPlot({
    plot(the_tree <- get_tree()[[2]], type = "phylo")
    nodelabels(text = the_tree$node.label,
               bg = "white")
    })

   
   
   output$my_tree <- renderPlot({
     plot(the_tree <- get_matrix()[[2]], type = "phylo")
     nodelabels(text = the_tree$node.label,
                bg = "white")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

