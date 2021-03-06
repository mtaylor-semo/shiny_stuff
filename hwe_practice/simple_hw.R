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
      actionButton("new_simple_problem",
                   "New problem"),
      actionButton("show_simple_answer",
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
  
  show_simple_ans <- reactiveVal(FALSE)
  
  simple <- reactiveValues()
  
  observeEvent(input$new_simple_problem, {
    show_simple_ans(FALSE)
    
    LETTER <- sample(LETTERS, 1)
    simple$a1 <- paste0(LETTER, "<sub>1</sub>")
    simple$a2 <- paste0(LETTER, "<sub>2</sub>")
    
    hom1 <- paste0(simple$a1, simple$a1)
    het <- paste0(simple$a1, simple$a2)
    hom2 <- paste0(simple$a2, simple$a2)
    
    simple$genotypes <- c(hom1, het, hom2)
    
    simple$allele1 <- round(runif(1, 0.1, 0.9), 3)
    simple$allele2 <- 1 - simple$allele1
    
    simple$AA_freq <- round(simple$allele1^2, 3)
    simple$Aa_freq <- round(2 * simple$allele1 * simple$allele2, 3)
    simple$aa_freq <- round(simple$allele2^2, 3)
    
  }, ignoreNULL = FALSE)
  
  output$question <- renderText({
    
    allele_or_geno <- rbinom(n = 1, size = 1, prob = 0.5)
    one_or_two <- rbinom(n = 1, size = 1, prob = 0.5)
    
    if (allele_or_geno == 0) { # Start with allele freq
      if (one_or_two == 0){ # use p
        sprintf("The frequency of allele %s is %0.3f.", simple$a1, simple$allele1)
      } else { #use q
        sprintf("The frequency of allele %s is %0.3f.", simple$a2, simple$allele2)
      }
    } else { # Genotype freq
      if (one_or_two == 0){ # use p2
        sprintf("The frequency of genotype %s is %0.3f.", simple$genotypes[1], simple$AA_freq)
      } else {# use q2
        sprintf("The frequency of genotype %s is %0.3f.", simple$genotypes[3], simple$aa_freq)
      }
    }
    
  })
  
  observeEvent(input$show_simple_answer, {
    show_simple_ans(TRUE)
  })
  
  output$answer <- renderText({
    if (show_simple_ans()) {
      sprintf(
        "The frequency of allele %s is %0.3f. </br>
        The frequency of allele %s is %0.3f. </br></br>
        The frequency of genotype %s is %0.3f.</br>
        The frequency of genotype %s is %0.3f.</br>
        The frequency of genotype %s is %0.3f.</br></br>
        The three genotype frequencies sum to %1.3f",
        # allele frequency sentences
        simple$a1,
        simple$allele1,
        simple$a2,
        simple$allele2,
        # genotype sentences
        simple$genotypes[1],
        simple$AA_freq,
        simple$genotypes[2],
        simple$Aa_freq,
        simple$genotypes[3],
        simple$aa_freq,
        sum(simple$AA_freq, simple$Aa_freq, simple$aa_freq)
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

