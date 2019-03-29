# QUIZ
# INVERSE WITH ADJOINT MATRIX

library(shiny)
library(RCurl)
library(MASS)

# Initial values for variables
start_time <- Sys.time()
B <- matrix(sample(c(-7:7, rep(0, 4)), 9), nrow=3)
colnames(B) <- paste("C", 1:3, sep = "")
rownames(B) <- paste("R", 1:3, sep = "")
if (det(B) == 0) {
  B <- matrix(c(1,2,3,6,5,4,-9,-8,-1), nrow = 3)
}

# Define UI for application that shows the exercise
ui <- fluidPage(
   
   # Application title
   titlePanel("Quiz: Inverse with the adjoint matrix"),
   
   # Capture answer for the exercise
   sidebarLayout(
      sidebarPanel(
        h4(textOutput("elapsedTime")),
        hr(),
        textInput("detb", "Report determinant of Matrix:", "0"),
        textAreaInput("adjb", "Report Adjoint Matrix:", 
                      "0, 0, 0, 0, 0, 0, 0, 0, 0"),
        actionButton("answer", "Verify answer"),
        conditionalPanel(condition = "input.answer <= 2",
            br(),
            actionButton("submit", "Submit to Google Form")
        )
     ),
      
      # Show the problem
      mainPanel(
        h4("Problem."),
        p("Compute the inverse of the following matrix by using the method of the adjoint."), 
        p("Report your solution in the proper box."),
        tableOutput("matb"),
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
  qtime <- reactiveVal(0)
  # Count clicks for submission
  submissions <- reactiveVal(0)
  
  # Control clock display
  output$elapsedTime <- renderText({
    qt <- round(difftime(Sys.time(), start_time, units = "mins"), digits = 2)
    if (input$answer < 1) {
      invalidateLater(1000, session)
      paste("Elapsed time = ", qt, "min")
    } else 
        if (vans() == 1) {
          paste("Quiz time = ", qt, "mins")
        }
  })
  
  output$matb <- renderTable({
    return(B)
  }, rownames = TRUE, digits = 0)
  
  # Function sends results to a Google Form  
  rsubmit <- function(fres, qt) {
    
    print("Sending results ...")
    # Link for the Google Form
    pre_fill_link <- 
      "https://docs.google.com/forms/d/e/1FAIpQLSdPDyN0cq9Yzd2LBtTJKjEpRZiJIKNKYgahAM043lzvFTeiwg/viewform?usp=pp_url&entry.1963454833="
    cptr <- "No response"
    try(cptr <- system("WMIC CSPRODUCT GET NAME", intern = TRUE)[2])
    uresults <- paste(fres - runif(1), qt, cptr, sep=",")
    uresults <- paste(uresults, paste(Sys.info(), collapse = ","), sep = ",")
    encoded_log <- base64(uresults)[[1]]
    browseURL(paste0(pre_fill_link, encoded_log))
  }
  
  output$result <- reactive({
    if (vans() == 0) {
      paste("Waiting for your answer ...")
    } else 
        if (vans() <= 2) {
          qt <- round(difftime(Sys.time(), start_time, units = "mins"), 
                         digits = 6)
          qtime(qt)
          detb <- as.numeric(input$detb)
          AdjB <- matrix(as.numeric(strsplit(input$adjb,",")[[1]]), nrow = 3)
          fres <- sum(abs(solve(B)-AdjB/detb)) < 0.0009
          paste("Your answer is ", 
            ifelse(fres, "CORRECT.", "INCORRECT."), sep = "")
        } else {
          paste("Close this window and try again!")
        }
  })
  
  # Detect button for the answer
  observeEvent(input$answer, {
    # Detect click on answe button
    vans(vans() + 1)  
  })
  
  # Detect a submission
  observeEvent(input$submit, {
    if (submissions() == 0) {
      detb <- as.numeric(input$detb)
      AdjB <- matrix(as.numeric(strsplit(input$adjb,",")[[1]]), nrow = 3)
      fres <- sum(abs(solve(B)-AdjB/detb)) < 0.0009
      rsubmit(fres, qtime())
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