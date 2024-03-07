#setwd("C:\\Users\\system 4\\Py_Command")

library(shiny)
library(shinyalert)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "PD Model"),
  dashboardSidebar(
    h3("Enter Details"), width = 333,
    radioButtons("Gender", "Select Your Gender", list("Male", "Female"), ""),
    numericInput("Age", "Your Age", ""),
    selectInput(
      "Feature", "Select Your Feature",
      choices = list(
        "Anxiety","Apathy","Benton","Clock","Cognition","COGSTATE","Constipate","Depress","DopaDefic","Education",
        "Epworth","Fatigue","Geriatric_Depression","Hallucination","Hand","Hopkins","Hopkins_Recog",
        "Impulsive_CompulsiveBehavior","Impulsive_ICD","LetterNumber","Lexical_Fluency","LightHead","Modif_Boston",
        "Montreal_Cognitive","Pain","REM_AwakeDream","REM_AwakeProblem","REM_Dream","REM_Movement","SCOPA_Cardio",
        "SCOPA_Eye","SCOPA_Gastro","SCOPA_Sex","SCOPA_Thermo","SCOPA_Urine","Semantic","SleepDay","SleepNight",
        "STAIA","STAIS","Symbol_Digit","Trail_Making_A","Trail_Making_B","UPSIT","Urine"
      ),
      multiple = TRUE
    ),
    uiOutput("score_range"),
    actionButton("submit_btn", "Submit"),
    # Add "i" button to open modal with overview
    actionButton("info_btn", icon("info-circle"))
  ),
  dashboardBody(
    h3("User Information"),
    fluidRow(
      box(
        width = 4,
        textOutput("mygender"),
      ),
      box(
        width = 4,
        textOutput("myage"),
      ),
      box(
        width = 4,
        textOutput("myfeature"),
      ),
      box(
        width = 12,
        textOutput("myscore")
      )
    )
  )
)


server <- function(input, output) {
  feature_scores <- reactiveValues(features = character(), scores = numeric())
  output$mygender <- renderText(input$Gender)
  output$myage <- renderText(input$Age)
  output$myfeature <- renderText({
    paste("Selected features: ", paste(input$Feature, collapse = ", "))
  })
  output$myscore <- renderText({
    scores <- lapply(input$Feature, function(f) {
      paste(f, ": ", input[[paste0("Score_", f)]])
    })
    paste("Scores: ", paste(scores, collapse = "; "))
  })
  
  output$score_range <- renderUI({
    feature <- input$Feature
    score_ranges <- lapply(feature, function(feat) {
      switch(
        feat,
        'Anxiety' = sliderInput(paste0('Score_', feat), 'Score of Anxiety', min = 0, max = 3, value = 1),
        'Apathy' = sliderInput(paste0('Score_', feat), 'Score of Apathy', min = 0, max = 2, value = 1),
        'Benton' = sliderInput(paste0('Score_', feat), 'Score of Benton', min = 5.1, max = 16.38, value = 10),
        'Clock' = sliderInput(paste0('Score_', feat), 'Score of Clock', min = 1, max = 7, value = 4),
        'Cognition' = sliderInput(paste0('Score_', feat), 'Score of Cognition', min = 0, max = 2, value = 1),
        'COGSTATE' = sliderInput(paste0('Score_', feat), 'Score of COGSTATE', min = 1, max = 2, value = 1, step = 1),
        'Constipate' = sliderInput(paste0('Score_', feat), 'Score of Constipate', min = 0, max = 3, value = 1),
        'Depress' = sliderInput(paste0('Score_', feat), 'Score of Depress', min = 0, max = 4, value = 1),
        'DopaDefic' = sliderInput(paste0('Score_', feat), 'Score of DopaDefic', min = 0, max = 2, value = 1),
        'Education' = sliderInput(paste0('Score_', feat), 'Score of Education', min = 8, max = 26, value = 12),
        'Epworth' = sliderInput(paste0('Score_', feat), 'Score of Epworth', min = 0, max = 11, value = 5),
        'Fatigue' = sliderInput(paste0('Score_', feat), 'Score of Fatigue', min = 0, max = 4, value = 2),
        'Geriatric_Depression' = sliderInput(paste0('Score_', feat), 'Score of Geriatric_Depression', min = 1, max = 15, value = 6),
        'Hallucination' = sliderInput(paste0('Score_', feat), 'Score of Hallucination', min = 0, max = 1, value = 0, step = 1),
        'Hand' = sliderInput(paste0('Score_', feat), 'Score of Hand', min = 1, max = 3, value = 1),
        'Hopkins' = sliderInput(paste0('Score_', feat), 'Score of Hopkins', min = 20, max = 64, value = 30),
        'Hopkins_Recog' = sliderInput(paste0('Score_', feat), 'Score of Hopkins_Recog', min = 20, max = 61, value = 30),
        'Impulsive_CompulsiveBehavior' = sliderInput(paste0('Score_', feat), 'Score of Impulsive_CompulsiveBehavior', min = 0, max = 1, value = 1, step = 1),
        'Impulsive_ICD' = sliderInput(paste0('Score_', feat), 'Score of Impulsive_ICD', min = 0, max = 4, value = 0),
        'LetterNumber' = sliderInput(paste0('Score_', feat), 'Score of LetterNumber', min = 4, max = 19, value = 10),
        'Lexical_Fluency' = sliderInput(paste0('Score_', feat), 'Score of Lexical_Fluency', min = 11, max = 101, value = 50),
        'LightHead' = sliderInput(paste0('Score_', feat), 'Score of LightHead', min = 0, max = 3, value = 1),
        'Modif_Boston' = sliderInput(paste0('Score_', feat), 'Score of Modif_Boston', min = 8, max = 60, value = 30),
        'Montreal_Cognitive' = sliderInput(paste0('Score_', feat), 'Score of Montreal_Cognitive', min = 17, max = 30, value = 20),
        'Pain' = sliderInput(paste0('Score_', feat), 'Score of Pain', min = 0, max = 3, value = 1),
        'REM_AwakeDream' = sliderInput(paste0('Score_', feat), 'Score of REM_AwakeDream', min = 0, max = 1, value = 0, step = 1),
        'REM_AwakeProblem' = sliderInput(paste0('Score_', feat), 'Score of REM_AwakeProblem', min = 0, max = 2, value = 1),
        'REM_Dream' = sliderInput(paste0('Score_', feat), 'Score of REM_Dream', min = 0, max = 3, value = 1),
        'REM_Movement' = sliderInput(paste0('Score_', feat), 'Score of REM_Movement', min = 0, max = 6, value = 1),
        'SCOPA_Cardio' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Cardio', min = 0, max = 6, value = 1),
        'SCOPA_Eye' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Eye', min = 0, max = 3, value = 1),
        'SCOPA_Gastro' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Gastro', min = 0, max = 9, value = 1),
        'SCOPA_Sex' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Sex', min = 0, max = 6, value = 1),
        'SCOPA_Thermo' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Thermo', min = 0, max = 6, value = 1),
        'SCOPA_Urine' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Urine', min = 0, max = 17, value = 1),
        'Semantic' = sliderInput(paste0('Score_', feat), 'Score of Semantic', min = 25, max = 81, value = 60),
        'SleepDay' = sliderInput(paste0('Score_', feat), 'Score of SleepDay', min = 0, max = 2, value = 1),
        'SleepNight' = sliderInput(paste0('Score_', feat), 'Score of SleepNight', min = 0, max = 4, value = 1),
        'STAIA' = sliderInput(paste0('Score_', feat), 'Score of STAIA', min = 31, max = 54, value = 40),
        'STAIS' = sliderInput(paste0('Score_', feat), 'Score of STAIS', min = 19, max = 59, value = 40),
        'Symbol_Digit' = sliderInput(paste0('Score_', feat), 'Score of Symbol_Digit', min = 20, max = 80, value = 60),
        'Trail_Making_A' = sliderInput(paste0('Score_', feat), 'Score of Trail_Making_A', min = 13, max = 150, value = 100),
        'Trail_Making_B' = sliderInput(paste0('Score_', feat), 'Score of Trail_Making_B', min = 20, max = 294, value = 100),
        'UPSIT' = sliderInput(paste0('Score_', feat), 'Score of UPSIT', min = 0, max = 40, value = 30),
        'Urine' = sliderInput(paste0('Score_', feat), 'Score of Urine', min = 0, max = 4, value = 2)
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
    # Define the 10 texts to display
    texts <- c("Parkinson's Disease in second most common neurodegeneartive disease",
               "Non-motor symptoms appear early in Parkinson's Disease", 
               "Dopamine levels decreased in Parkinson's patient",
               "Parkinson's Disease progression can be slowed down")
    # Choose a random text
    random_text <- sample(texts, 1)
    # Show the alert with the random text
    shinyalert(random_text, type = "info", timer = 10000, title = 'Do you know ?', closeOnClickOutside = T)
  })
  
  observeEvent(input$submit_btn, {
    # saveRDS(list(input$Gender, input$Age, input$Feature), "user_data.RDS")
    
    feature_scores$features <- input$Feature
    feature_scores$age <- input$Age
    feature_scores$gender <- input$Gender
    feature_scores$scores <- sapply(input$Feature, function(f) input[[paste0("Score_", f)]])
    n = length(input$Feature)
    age <- feature_scores$age
    output$myscore <- renderText({
      paste("Selected Features: ", paste(feature_scores$features, collapse = ", "), "\n",
            "Scores: ", paste(feature_scores$scores, collapse = ", "))
    })
    vec=c()
    for (i in 1:length(feature_scores$features)){
        vec=c(vec,noquote(feature_scores$features[i]))
        vec=c(vec,as.vector(feature_scores$scores[i]))
    }
    
    # Check if at least three features are selected
    if(length(feature_scores$features) < 3) {
      showModal(modalDialog(
        title = "Error",
        "Please select at least three features.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      inputVec = paste(vec,  collapse =" ")
      command = sprintf("python Z:\\PPMI_Data\\Excels\\CollaborativeFiltering\\ThreeModelPrediction.py %s %s", age, n)
      command = paste(command, inputVec, " ")
      out = shell(command,intern = TRUE)
      #out[9]
      print(out[1])
    }
    
  })
}

shinyApp(ui = ui, server = server)