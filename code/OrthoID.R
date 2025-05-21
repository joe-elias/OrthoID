library(shiny)
library(dplyr)

# Load metadata from CSV
metadata <- read.csv('audio.csv', stringsAsFactors = FALSE)

# UI Section (I just added color)
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e7f1dc;
        font-family: 'Segoe UI', sans-serif;
      }
      .btn {
        background-color: #a3c585;
        color: white;
      }
    "))
  ),
  
  titlePanel('Ortho ID'), 
  sidebarLayout(
    sidebarPanel(
      actionButton('new_test', 'Orthopterate!'), 
      selectInput('answer', "Choose the correct species:", choices = NULL), 
      actionButton('submit', 'Submit Answer'), 
      textOutput('feedback')
    ), 
    mainPanel(
      uiOutput('audio_player')
    )
  )
)

# Server Section
server <- function(input, output, session) {
  quiz_data <- reactiveValues(file = NULL, species = NULL, common = NULL)
  
  # Select a random file and generate options
  observeEvent(input$new_test, {
    selected <- metadata[sample(nrow(metadata), 1), ]
    quiz_data$file <- selected$filename
    quiz_data$species <- selected$species
    quiz_data$common <- selected$common
    
    # Generate random incorrect species
    incorrect_options <- sample(metadata$species[metadata$species != quiz_data$species], 4)
    all_options <- sample(c(quiz_data$species, incorrect_options)) # Shuffle options
    
    # Update the selectInput choices
    updateSelectInput(session, 'answer', choices = all_options)
    output$feedback <- renderText("") #just resets feedback so it isn't on for next question
  })
  
  # Display audio player
  output$audio_player <- renderUI({
    req(quiz_data$file)
    tags$audio(src = paste0('https://raw.githubusercontent.com/joe-elias/OrthoID/main/audio/', 
                            quiz_data$file), type = 'audio/mp3', controls = NA)
  })
  
  # Check answer
  observeEvent(input$submit, {
    req(input$answer, quiz_data$species)
    if (tolower(input$answer) == tolower(quiz_data$species)) {
      output$feedback <- renderText(paste("Correct!", quiz_data$species, ';', quiz_data$common))
    } else {
      output$feedback <- renderText(paste("Wrong! Correct answer: ", 
                                          quiz_data$species, ';', quiz_data$common))
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)
