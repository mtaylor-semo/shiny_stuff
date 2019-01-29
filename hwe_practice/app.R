
library(shiny)

ui <- fluidPage(
   
   titlePanel("Practice HW: Counting "),
   
   sidebarLayout(
      sidebarPanel(
        withMathJax(),
        helpText("Press `New problem` for a new problem to solve. Press
                 `Show answer` to see the solution."),
        actionButton("new_problem",
                     "New problem"),
        actionButton("show_answer",
                     "Show answer")
      ),
      
      mainPanel(
        textOutput("intro"),
        htmlOutput("question_hom1"),
        helpText("Use the methods learned in class to calculate whether
                 this population is in Hardy-Weinberg equilibrium.
                 Use the observed number of each genotype to calculate
                 genotype frequencies and observed allele frequencies."),
        hr(),
         htmlOutput("answer"),
        uiOutput("check")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  show_ans <- reactiveVal(FALSE)
  
  values <- reactiveValues()
  
  observeEvent(input$new_problem, {
    show_ans(FALSE)
    LETTER <- sample(LETTERS, 1)
    values$a1 <- paste0(LETTER, "<sub>1</sub>")
    values$a2 <- paste0(LETTER, "<sub>2</sub>")

    hom1 <- paste0(values$a1, values$a1)
    het <- paste0(values$a1, values$a2)
    hom2 <- paste0(values$a2, values$a2)
    
    values$genotypes <- c(hom1, het, hom2)
    
    values$genotype_nums <- floor(runif(3, 4, 101))
    values$N <- sum(values$genotype_nums)
    
    values$geno_freqs <- round(values$genotype_nums/values$N, 3)
    
    values$allele1 <- values$genotype_nums[1]*2 + values$genotype_nums[2]
    values$allele2 <- values$genotype_nums[3]*2 + values$genotype_nums[2]
    values$allele1_freq <- round(values$allele1/(values$N*2), 3)
    values$allele2_freq <- round(values$allele2/(values$N*2), 3)
    
  }, ignoreNULL = FALSE)
   
  output$intro <- renderText({
    sprintf("A sample of %d individuals contained:",
            values$N)
  })
   output$question_hom1 <- renderText({
  
    paste(values$genotype_nums[1], 
              values$genotypes[1], "individuals,</br>",
              values$genotype_nums[2], 
              values$genotypes[2], "individuals, and </br>",
              values$genotype_nums[3],
              values$genotypes[3], "individuals")
   })
   
   observeEvent(input$show_answer, {
     show_ans(TRUE)
   })
   
   output$answer <- renderText({
     if (show_ans()) {
       sprintf("The sample has %d \u00D7 2 = %d total alleles.</br></br>
               The sample has %d \u00D7 2 + %d = %d total %s alleles.</br>
               The sample has %d \u00D7 2 + %d = %d  total %s alleles.</br></br>
               The frequency of the %s allele is %d/%d = %0.3f.</br>
               The frequency of the %s allele is %d/%d = %0.3f.</br></br>
               The frequency of the %s genotype is %d/%d = %0.3f</br> 
               The frequency of the %s genotype is %d/%d = %0.3f</br> 
               The frequency of the %s genotype is %d/%d = %0.3f</br>", 
               # First sentence
               values$N, 
               values$N*2, 
               # second sentence
               values$genotype_nums[1],
               values$genotype_nums[2], 
               values$allele1,
               values$a1, 
               # third sentence
               values$genotype_nums[3],
               values$genotype_nums[2], 
               values$allele2,
               values$a2,
               # fourth sentence
               values$a1,
               values$allele1,
               values$N*2,
               values$allele1_freq,
               # fifth sentence
               values$a2,
               values$allele2,
               values$N*2,
               values$allele2_freq,
               # sixth sentence
               values$genotypes[1],
               values$genotype_nums[1],
               values$N,
               values$geno_freqs[1],
               # sixth sentence
               values$genotypes[2],
               values$genotype_nums[2],
               values$N,
               values$geno_freqs[2],
               # sixth sentence
               values$genotypes[3],
               values$genotype_nums[3],
               values$N,
               values$geno_freqs[3])
     }
   })
   output$check <- renderUI({
     if (show_ans()) {
      withMathJax(
        HTML(sprintf("<hr><b>Check:</b></br>\\(\\sqrt{%0.3f}  = %0.3f ≠ %0.3f \\)</br>
                \\(\\sqrt{%0.3f}  = %0.3f ≠ %0.3f\\)</br>",
                values$geno_freqs[1], 
                sqrt(values$geno_freqs[1]), 
                values$allele1_freq,
                values$geno_freqs[3], 
                sqrt(values$geno_freqs[3]), 
                values$allele2_freq))
        )
       
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

