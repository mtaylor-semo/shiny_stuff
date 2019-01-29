##
## Calculate observed allele and genotypes for a diploid population.
## Number of individuals are generated randomly.

## MST 28 January 2019

library(shiny)

ui <- fluidPage(
  
  titlePanel("BI 163 Hardy-Weinberg practice: simple problems"),
  
  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      helpText("Press \"New problem\" for a new problem to solve. Press
                 \"Show answer\" to see the solution. Refer to your notes 
                 for details of the steps."),
      actionButton("new_problem",
                   "New problem"),
      actionButton("show_answer",
                   "Show answer"),
      uiOutput("equations")
    ),
    
    mainPanel(
      helpText("The frequency of one allele or one homozygous genotype is provided. 
                Calculate the four remaining frequencies using the two Hardy-Weinberg 
                equations, shown below left. The population is in Hardy-Weinberg equilibrium. 
                Each gene has only two alleles."),
      helpText("The final frequencies of the genotypes might sum to 0.999, 1.000,
               or 1.001, due to rounding. If you do not obtain the values shown in the
               answer, then double-check your work."),
      hr(),
      textOutput("intro"),
      htmlOutput("question"),
      hr(),
      htmlOutput("answer")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$equations <- renderUI({
    withMathJax("$$p + q = 1$$ $$p^2 + 2pq + q^2 = 1$$")
  })
  
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

    values$allele1 <- round(runif(1, 0.1, 0.9), 3)
    values$allele2 <- 1 - values$allele1
    
    values$AA_freq <- round(values$allele1^2, 3)
    values$Aa_freq <- round(2 * values$allele1 * values$allele2, 3)
    values$aa_freq <- round(values$allele2^2, 3)
    
   }, ignoreNULL = FALSE)
  
  output$question <- renderText({
    
    allele_or_geno <- rbinom(n = 1, size = 1, prob = 0.5)
    one_or_two <- rbinom(n = 1, size = 1, prob = 0.5)
    
    if (allele_or_geno == 0) { # Start with allele freq
      if (one_or_two == 0){ # use p
        sprintf("The frequency of allele %s is %0.3f.", values$a1, values$allele1)
      } else { #use q
        sprintf("The frequency of allele %s is %0.3f.", values$a2, values$allele2)
      }
    } else { # Genotype freq
      if (one_or_two == 0){ # use p2
        sprintf("The frequency of genotype %s is %0.3f.", values$genotypes[1], values$AA_freq)
      } else {# use q2
        sprintf("The frequency of genotype %s is %0.3f.", values$genotypes[3], values$aa_freq)
      }
    }
    
    
        # 
        # paste(values$genotype_nums[1], 
        #   values$genotypes[1], "individuals,</br>",
        #   values$genotype_nums[2], 
        #   values$genotypes[2], "individuals, and </br>",
        #   values$genotype_nums[3],
        #   values$genotypes[3], "individuals")
  })
  
  observeEvent(input$show_answer, {
    show_ans(TRUE)
  })
  
  output$answer <- renderText({
    if (show_ans()) {
      sprintf("The frequency of allele %s is %0.3f. </br>
               The frequency of allele %s is %0.3f. </br></br>
               The frequency of genotype %s is %0.3f.</br>
               The frequency of genotype %s is %0.3f.</br>
               The frequency of genotype %s is %0.3f.</br></br>
              The three genotype frequencies sum to %1.3f", 
              # allele frequency sentences
              values$a1, values$allele1,
              values$a2, values$allele2,
              # genotype sentences
              values$genotypes[1], values$AA_freq,
              values$genotypes[2], values$Aa_freq,
              values$genotypes[3], values$aa_freq,
              sum(values$AA_freq, values$Aa_freq, values$aa_freq))
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

