# QUIZ
# LINEAR FIT (LEAST SQUARES)

library(shiny)
library(RCurl)

# Initial values for variables
start_time <- Sys.time()
x <- sort(c(1, 10, sample(2:9, 3)))
beta <- runif(2, min = -5, max = 5)
y <- round(beta[1] + beta[2]*x + 5*rnorm(length(x)), digits = 4)

# Define UI for application that shows the exercise
ui <- fluidPage(
   
   # Application title
   titlePanel("Quiz: Fit of a line to data"),
   
   # Capture answer for the exercise
   sidebarLayout(
      sidebarPanel(
        h4(textOutput("elapsedTime")),
        hr(),
        textInput("bhat0", "betahat_0 =", "0"),
        textInput("bhat1", "betahat_1 =", "0"),
        actionButton("answer", "Verify answer"),
        conditionalPanel(condition = "input.answer == 1",
            br(),
            actionButton("submit", "Submit to Google Form")
        )
     ),
      
      # Show the problem
      mainPanel(
        h4("Problem."),
        p("Find the equation of line that best fit the data."), 
        p("Report your estimates in the proper box."),
        tableOutput("tablaxy"),
        hr(),
        plotOutput("scatter"),
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
  
  output$tablaxy <- renderTable({
    return(data.frame(x, y))
  }, rownames = TRUE, digits = 2)
  
  output$scatter <- renderPlot({
    xt <- c(min(x), max(x))
    flin <- lm(y ~ x)
    plot(xt, flin$coefficients[1] + flin$coefficients[2]*xt,
         type = "l", xlab = "x", ylab = "y", 
         ylim = c(min(y), max(y)))
    points(x, y)
  })
  
  # Function sends results to a Google Form  
  rsubmit <- function(fres, qt) {
    
    print("Sending results ...")
    # Link for the Google Form
    pre_fill_link <- 
      "https://docs.google.com/forms/d/e/1FAIpQLSfNQ0Fn0AzFlO_-hU4fldPiBh7PMVMRp5oQgB0ABXtRaaCSMQ/viewform?usp=pp_url&entry.1963454833="
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
        if (vans() == 1) {
          qt <- round(difftime(Sys.time(), start_time, units = "mins"), 
                         digits = 6)
          qtime(qt)
          lfit <- lm(y ~ x)
          fres <- abs(lfit$coefficients[1] - as.numeric(input$bhat0)) +
            abs(lfit$coefficients[2] - as.numeric(input$bhat1)) < .02
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
      lfit <- lm(y ~ x)
      fres <- abs(lfit$coefficients[1] - as.numeric(input$bhat0)) +
        abs(lfit$coefficients[2] - as.numeric(input$bhat1)) < .02
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