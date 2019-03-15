# QUIZ
# PRODUCT OF MATRICES

library(shiny)
library(RCurl)

# Initial values for variables
start_time <- Sys.time()
n1 <- sample(1:3, 1)
m1 <- sample(2:4, 1)
m2 <- sample(1:4, 1)
A <- matrix(sample(-10:10, n1*m1), nrow = n1)
colnames(A) <- paste("C", 1:m1, sep = "")
rownames(A) <- paste("R", 1:n1, sep = "")
B <- matrix(sample(-10:10, m1*m2), nrow = m1)
colnames(B) <- paste("C", 1:m2, sep = "")
rownames(B) <- paste("R", 1:m1, sep = "")
pans <- paste(rep(0, n1*m2), collapse = ", ")
C <- A %*% B

# Define UI for application that posed an exercise
ui <- fluidPage(
   
   # Application title
   titlePanel("Quiz: Product of Matrices"),
   
   # Capture answer for the exercise
   sidebarLayout(
      sidebarPanel(
        h4(textOutput("elapsedTime")),
        hr(),
        textAreaInput("prodab", "Answer problem:", pans),
        actionButton("answer", "Verify answer"),
        conditionalPanel(condition = "input.answer >= 1",
            br(),
            actionButton("submit", "Submit to Google Form")
        )
     ),
      
      # Show the problem
      mainPanel(
        h4("Problem."),
        p("Compute the product [A]*[B]. 
          Report your answer in the box."),
        p("List your resulting matrix by columns, separated by commas."),
        p("MATRIX A"),
        tableOutput("matA"),
        p("MATRIX B"),
        tableOutput("matB"),
        hr(),
        h3(textOutput("result"))
      )
   )
)

# Define server logic required to evaluate results
server <- function(input, output, session) {
  
  # Count clicks to verify answer
  vans <- reactiveVal(0)
  # Record quiz time
  submissions <- reactiveVal(0)
  
  # Control clock display
  output$elapsedTime <- renderText({
    qt <- round(difftime(Sys.time(), start_time, units = "mins"), digits = 2)
    if (input$submit < 1) {
      invalidateLater(1000, session)
      paste("Elapsed time = ", qt, "min")
    } else 
        if (vans() == 1) {
          paste("Quiz time = ", qt, "mins")
        }
  })
  
  output$matA <- renderTable({
    return(A)
  }, rownames = TRUE, digits = 0)
  
  output$matB <- renderTable({
    return(B)
  }, rownames = TRUE, digits = 0)
  
  # Function sends results to a Google Form  
  rsubmit <- function(fres, qt) {
    
    print("Sending results ...")
    # Link for the Google Form
    pre_fill_link <- 
      "https://docs.google.com/forms/d/e/1FAIpQLScjWdLBSYHvLdtgLivYc05qG0SexvGpaT1Wzvh9fRY8-oooMQ/viewform?usp=pp_url&entry.1963454833="
    cptr <- "No response"
    try(cptr <- system("WMIC CSPRODUCT GET NAME", intern = TRUE)[2])
    uresults <- paste(fres - runif(1), qt, cptr, sep=",")
    uresults <- paste(uresults, paste(Sys.info(), collapse = ","), sep = ",")
    encoded_log <- base64(uresults)[[1]]
    browseURL(paste0(pre_fill_link, encoded_log))
  }
  
  # Detect button for the answer
  observeEvent(input$answer, {
    # Detect click on answe button
    vans(vans() + 1)  
    
    output$result <- reactive({
      if (vans() == 0) {
        paste("Waiting for your answer ...")
      } else 
        if (vans() == 1) {
          CR <- matrix(as.numeric(strsplit(input$prodab,",")[[1]]), nrow = n1)
          fres <- (sum(abs(C-CR)) < 0.0001*n1*m2)
          paste(ifelse(fres, "Your answer is CORRECT. The clock will stop 
            until you submit your answer!", "Your answer is INCORRECT. Check 
            out your response and verify your answer again!"), sep = "")
        } else if (vans() == 2) {
          CR <- matrix(as.numeric(strsplit(input$prodab,",")[[1]]), nrow = n1)
          fres <- (sum(abs(C-CR)) < 0.0001*n1*m2)
          paste("Your answer is ", 
                ifelse(fres, "NOW CORRECT. The clock will stop until
                you submit your answer!", "STILL INCORRECT. Check out your response 
                and verify your answer again!"), sep = "")
        } else {
          paste("Sorry. Close this window and try again!")
        }
    })
    
  })
  
  # Detect a submission
  observeEvent(input$submit, {
    if (submissions() == 0) {
      qt <- round(difftime(Sys.time(), start_time, units = "mins"), 
                  digits = 6)
      CR <- matrix(as.numeric(strsplit(input$prodab,",")[[1]]), nrow = n1)
      fres <- (sum(abs(C-CR)) < 0.0001*n1*m2)
      rsubmit(fres, qt)
    } else {
      output$result <- reactive({ 
        paste("Already sent the Google Form. Close this window and try the quiz again!")
      })
    }
    submissions(submissions() + 1)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)