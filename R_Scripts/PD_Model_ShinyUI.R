library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$style(HTML("
      #submit_btn {
        margin-top: 20px;
      }
      #info_btn {
        margin-top: 20px;
        margin-left: 10px;
      }
    "))
  ),
  headerPanel(title = "PD Model"),
  sidebarLayout(
    sidebarPanel(
      h3("Enter Details"),
      radioButtons("Gender", "Select Your Gender", list("Male", "Female"), ""),
      numericInput("Age", "Your Age", ""),
      selectInput(
        "Feature", "Select Your Feature",
        choices = list(
          "Feature 1",
          "Feature 2",
          "Feature 3",
          "Feature 4",
          "Feature 5"
        ),
        multiple = TRUE
      ),
      uiOutput("score_range"),
      actionButton("submit_btn", "Submit"),
      # Add "i" button to open modal with overview
      actionButton("info_btn", icon("info-circle"))
    ),
    mainPanel(
      h3("User Information"),
      textOutput("mygender"),
      textOutput("myage"),
      textOutput("myfeature"),
      textOutput("myscore")
    )
  )
)

server <- function(input, output) {
  # Create reactiveValues object to store feature scores
  feature_scores <- reactiveValues(features = character(), scores = numeric())
  
  output$mygender <- renderText(input$Gender)
  output$myage <- renderText(input$Age)
  output$myfeature <- renderText(paste(input$Feature, collapse = ", "))
  output$myscore <- renderText(paste0(
    "Feature: ", paste(input$Feature, collapse = ", "), "\n",
    "Score(s): ", paste(sapply(input$Feature, function(f) input[[paste0("Score_", f)]]), collapse = ", ")
  ))
  
  output$score_range <- renderUI({
    feature <- input$Feature
    score_ranges <- lapply(feature, function(feat) {
      switch(
        feat,
        'Feature 1' = sliderInput(paste0('Score_', feat), 'Score of the Feature', min = 1, max = 5, value = 1),
        'Feature 2' = sliderInput(paste0('Score_', feat), 'Score of the Feature', min = 3.6, max = 43.8, value = 3.9),
        'Feature 3' = sliderInput(paste0('Score_', feat), 'Score of the Feature', min = 7, max = 21, value = 7),
        'Feature 4' = sliderInput(paste0('Score_', feat), 'Score of the Feature', min = 11, max = 17, value = 11),
        'Feature 5' = sliderInput(paste0('Score_', feat), 'Score of the Feature', min = 22, max = 143, value = 22)
      )
    })
    do.call(tagList, score_ranges)
  })
  
  # Add modal with overview
  observeEvent(input$info_btn, {
    showModal(
      modalDialog(
        title = "Overview",
        "Parkinson's disease is a progressive disorder that affects the nervous system and the 
        parts of the body controlled by the nerves.
        In the early stages of Parkinson's disease.",
        easyClose = TRUE,
        footer = NULL
      ))
  })
  
  observeEvent(input$submit_btn, {
    # Update feature_scores reactiveValues object
    feature_scores$features <- input$Feature
    feature_scores$scores <- sapply(input$Feature, function(f) input[[paste0("Score_", f)]])
    output$myscore <- renderText({
      paste("Selected Features: ", paste(feature_scores$features, collapse = ", "), "\n",
            "Scores: ", paste(feature_scores$scores, collapse = ", "))
    })
    # Check if at least two features are selected
    if(length(input$Feature) < 2) {
      showModal(modalDialog(
        title = "Error",
        "Please select at least two features.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      # Perform calculations or save data
    }
  })
}


shinyApp(ui = ui, server = server)
