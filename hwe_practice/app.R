##
## Calculate observed allele and genotypes for a diploid population.
## Number of individuals are generated randomly.

## MST 28 January 2019
## MST 30 January 2019: Merged simple and counting with navBar.

library(shiny)


# Global variables --------------------------------------------------------


final_freq_warning <-
  "The final frequencies of the genotypes might sum to 0.999, 1.000,
or 1.001, due to rounding. If you do not obtain the values shown in the
answer, then double-check your work."


# Global functions --------------------------------------------------------

# Sample a random letter to use for allele and genotype names.
get_names <- function(react_list) {
  LETTER <- sample(LETTERS, 1)
  react_list$a1 <- paste0(LETTER, "<sub>1</sub>")
  react_list$a2 <- paste0(LETTER, "<sub>2</sub>")
  
  hom1 <- paste0(react_list$a1, react_list$a1)
  het <- paste0(react_list$a1, react_list$a2)
  hom2 <- paste0(react_list$a2, react_list$a2)
  
  react_list$genotypes <- c(hom1, het, hom2)
  react_list
}

sample_genotypes <- function(react_list) {
  react_list$genotype_nums <- floor(runif(3, 4, 101))
  react_list$N <- sum(react_list$genotype_nums)
  
  react_list$geno_freqs <-
    round(react_list$genotype_nums / react_list$N, 3)
  
  react_list$allele1 <-
    react_list$genotype_nums[1] * 2 + react_list$genotype_nums[2]
  react_list$allele2 <-
    react_list$genotype_nums[3] * 2 + react_list$genotype_nums[2]
  react_list$allele1_freq <-
    round(react_list$allele1 / (react_list$N * 2), 3)
  react_list$allele2_freq <-
    round(react_list$allele2 / (react_list$N * 2), 3)
  react_list
}
# UI ----------------------------------------------------------------------


ui <- navbarPage(
  "BI 163 Hardy-Weinberg practice",
  
  # Overview tab ------------------------------------------------------------
  
  tabPanel(
    "Overview",
    mainPanel(
      helpText(
        "This site presents Hardy-Weinberg problems so you
        can practice and develop your problem-solving skills."
      ),
      helpText(
        "The \"Simple problems\" tab presents problems that
        assumes a population",
        em("is"),
        "in
        in Hardy-Weinberg equilibrium. You will be presented with
        the frequency of one allele or one homozygous genotype.
        You then calculate the four remaining frequencies using
        the two Hardy-Weinberg equations shown below."
      ),
      helpText(
        "The \"Counting problems\" tab presents problems for you
        to determine",
        em("if"),
        "a population is in
        equibilibrium. These are the types of problems we
        practiced in class, where you calculate the
        allele and genotype frequencies independently and
        then compare the results to determine if the
        population is in equilibrium."
      ),
      helpText(strong(
        "All problems assume a single gene with only
        two alleles."
      )),
      hr(),
      h5("Equations and things to remember"),
      br(),
      uiOutput("equations")
      )
      ),
  
  # Simple problems tab -----------------------------------------------------
  
  tabPanel("Simple problems",
           sidebarLayout(
             sidebarPanel(
               withMathJax(),
               helpText(
                 "Press \"New problem\" for a new problem to solve. Press
                 \"Show answer\" to see the solution. Refer to your notes
                 for details of the steps."
               ),
               actionButton("new_simple_problem",
                            "New problem"),
               actionButton("show_simple_answer",
                            "Show answer")
               ),
             
             mainPanel(
               helpText(
                 "The frequency of one allele or one homozygous genotype is provided.
                 Calculate the four remaining frequencies using the two Hardy-Weinberg
                 equations, shown below left. The population is in Hardy-Weinberg equilibrium.
                 Each gene has only two alleles."
               ),
               helpText(final_freq_warning),
               hr(),
               htmlOutput("question"),
               hr(),
               htmlOutput("answer")
               )
             )),
  
  # Counting problems -------------------------------------------------------
  
  tabPanel("Counting problems",
           sidebarLayout(
             sidebarPanel(
               helpText(
                 "Press \"New problem\" for a new problem to solve. Press
                 \"Show answer\" to see the solution. Refer to your notes
                 for details of the steps."
               ),
               actionButton("new_counting_problem",
                            "New problem"),
               actionButton("show_answer",
                            "Show answer")
               ),
             mainPanel(
               helpText(
                 "Use the methods learned in class to calculate whether
                 this population is in Hardy-Weinberg equilibrium.
                 Use the observed number of each genotype to calculate
                 genotype frequencies and observed allele frequencies."
               ),
               helpText(strong(
                 "Round each step to 3 digits after the decimal point."
               )),
               helpText(final_freq_warning),
               hr(),
               textOutput("intro_counting"),
               htmlOutput("question_counting"),
               hr(),
               htmlOutput("answer_counting"),
               uiOutput("check")
               )
)),
  # Chi-square problems  ---------------------------------------------------
tabPanel(withMathJax("\\(\\chi^2\\)"),
         sidebarLayout(
           sidebarPanel(
             helpText(
               "Press \"New problem\" for a new problem to solve. Press
               \"Show answer\" to see the solution. Refer to your notes
               for details of the steps."
             ),
             actionButton("new_chi_problem",
                          "New problem"),
             actionButton("show_chi_answer",
                          "Show answer")
             ),
           mainPanel(
             "Are apparent deviations from Hardy-Weinberg equilibrium
             biologically meaningful or an artifact of sampling? A",
             withMathJax("\\(\\chi^2\\)"),
             "test will help you.",
             strong("Coming soon.")
           )
)) # End chi tabPanel
) # end UI

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  # Overview output ---------------------------------------------------------
  
  output$equations <- renderUI({
    withMathJax(
      HTML(
        "Allele frequencies: \\(p + q = 1\\)</br></br>
        Genotype frequencies: \\(p^2 + 2pq + q^2 = 1\\)</br></br>
        Remember: \\(1 - p = q\\)</br></br>
        Remember: \\(\\sqrt{p^2} = p\\)
        "
      )
      )
  })
  
  
  # Reactive values ---------------------------------------------------------
  
  show_simple_ans <- reactiveVal(FALSE)
  show_ans <- reactiveVal(FALSE)
  
  simple <- reactiveValues()#
  counting <- reactiveValues()
  chi <- reactiveValues()
  
  # Simple output -----------------------------------------------------------
  
  observeEvent(input$new_simple_problem, {
    show_simple_ans(FALSE)
    
    # Get allele and genotype names
    simple <- get_names(simple)
    
    simple$allele1 <- round(runif(1, 0.1, 0.9), 3)
    simple$allele2 <- 1 - simple$allele1
    
    simple$AA_freq <- round(simple$allele1 ^ 2, 3)
    simple$Aa_freq <- round(2 * simple$allele1 * simple$allele2, 3)
    simple$aa_freq <- round(simple$allele2 ^ 2, 3)
    
  }, ignoreNULL = FALSE)
  
  output$question <- renderText({
    allele_or_geno <- rbinom(n = 1, size = 1, prob = 0.5)
    one_or_two <- rbinom(n = 1, size = 1, prob = 0.5)
    
    if (allele_or_geno == 0) {
      # Start with allele freq
      if (one_or_two == 0) {
        # use p
        sprintf("The frequency of allele %s is %0.3f.",
                simple$a1,
                simple$allele1)
      } else {
        #use q
        sprintf("The frequency of allele %s is %0.3f.",
                simple$a2,
                simple$allele2)
      }
    } else {
      # Genotype freq
      if (one_or_two == 0) {
        # use p2
        sprintf("The frequency of genotype %s is %0.3f.",
                simple$genotypes[1],
                simple$AA_freq)
      } else {
        # use q2
        sprintf("The frequency of genotype %s is %0.3f.",
                simple$genotypes[3],
                simple$aa_freq)
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
  
  # Counting output ---------------------------------------------------------
  
  observeEvent(input$new_counting_problem, {
    show_ans(FALSE)
    
    # Get allele and genotype names
    counting <- get_names(counting)
    counting <- sample_genotypes(counting)
    # counting$genotype_nums <- floor(runif(3, 4, 101))
    # counting$N <- sum(counting$genotype_nums)
    # 
    # counting$geno_freqs <-
    #   round(counting$genotype_nums / counting$N, 3)
    # 
    # counting$allele1 <-
    #   counting$genotype_nums[1] * 2 + counting$genotype_nums[2]
    # counting$allele2 <-
    #   counting$genotype_nums[3] * 2 + counting$genotype_nums[2]
    # counting$allele1_freq <-
    #   round(counting$allele1 / (counting$N * 2), 3)
    # counting$allele2_freq <-
    #   round(counting$allele2 / (counting$N * 2), 3)
    
  }, ignoreNULL = FALSE)
  
  output$intro_counting <- renderText({
    sprintf("A sample of %d individuals contained:",
            counting$N)
  })
  output$question_counting <- renderText({
    paste(
      counting$genotype_nums[1],
      counting$genotypes[1],
      "individuals,</br>",
      counting$genotype_nums[2],
      counting$genotypes[2],
      "individuals, and </br>",
      counting$genotype_nums[3],
      counting$genotypes[3],
      "individuals"
    )
  })
  
  observeEvent(input$show_answer, {
    show_ans(TRUE)
  })
  
  output$answer_counting <- renderText({
    if (show_ans()) {
      sprintf(
        "The sample has %d \u00D7 2 = %d total alleles.</br></br>
        The sample has %d \u00D7 2 + %d = %d total %s alleles.</br>
        The sample has %d \u00D7 2 + %d = %d  total %s alleles.</br></br>
        The frequency of the %s allele is %d/%d = %0.3f.</br>
        The frequency of the %s allele is %d/%d = %0.3f.</br></br>
        The frequency of the %s genotype is %d/%d = %0.3f</br>
        The frequency of the %s genotype is %d/%d = %0.3f</br>
        The frequency of the %s genotype is %d/%d = %0.3f</br></br>
        The three genotype frequencies sum to %1.3f",
        # First sentence
        counting$N,
        counting$N * 2,
        # second sentence
        counting$genotype_nums[1],
        counting$genotype_nums[2],
        counting$allele1,
        counting$a1,
        # third sentence
        counting$genotype_nums[3],
        counting$genotype_nums[2],
        counting$allele2,
        counting$a2,
        # fourth sentence
        counting$a1,
        counting$allele1,
        counting$N * 2,
        counting$allele1_freq,
        # fifth sentence
        counting$a2,
        counting$allele2,
        counting$N * 2,
        counting$allele2_freq,
        # sixth sentence
        counting$genotypes[1],
        counting$genotype_nums[1],
        counting$N,
        counting$geno_freqs[1],
        # sixth sentence
        counting$genotypes[2],
        counting$genotype_nums[2],
        counting$N,
        counting$geno_freqs[2],
        # sixth sentence
        counting$genotypes[3],
        counting$genotype_nums[3],
        counting$N,
        counting$geno_freqs[3],
        sum(counting$geno_freqs)
      )
    }
  })
  output$check <- renderUI({
    if (show_ans()) {
      withMathJax(HTML(
        sprintf(
          "<hr><b>Check:</b></br>\\(\\sqrt{%0.3f}  = %0.3f ≠ %0.3f \\)</br>
          \\(\\sqrt{%0.3f}  = %0.3f ≠ %0.3f\\)</br>",
          counting$geno_freqs[1],
          sqrt(counting$geno_freqs[1]),
          counting$allele1_freq,
          counting$geno_freqs[3],
          sqrt(counting$geno_freqs[3]),
          counting$allele2_freq
        )
      ))
      
    }
  })
  

# Chi-square output -------------------------------------------------------

  
  }

# Run the application
shinyApp(ui = ui, server = server)
