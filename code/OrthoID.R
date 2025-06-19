library(shiny)
library(dplyr)

# Load metadata from CSV (I decided to make 2 seperate csv's for crickets and katydids)
cricket <- read.csv("Crickets.csv", stringsAsFactors = FALSE)
katydid <- read.csv("Katydid.csv", stringsAsFactors = FALSE)

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
  ")),    
  ),
  
  titlePanel('Ortho ID'), 
  sidebarLayout(
    sidebarPanel(

#Adding 2 more buttons (I may change their style later)
      actionButton("cricket", "Cricket"),
      actionButton("katydid", "Katydid"),
      
      actionButton("new_test", "Orthopterate!"),
      selectInput('answer', "Choose the correct species:", choices = NULL), 
      actionButton('submit', 'Submit'), 
      textOutput('feedback')
    ), 
    mainPanel(
      uiOutput('audio_player')
    )
  )
)

# Server Section
server <- function(input, output, session) {
  quiz_data <- reactiveValues(file = NULL, species = NULL, common = NULL, group=NULL)

  # Cricket button
  observeEvent(input$cricket, {
    quiz_data$group <- "cricket"
    updateSelectInput(session, 'answer', choices = unique(cricket$species))#Quite literally took me FOREVER to do
    output$audio_player <- renderUI(NULL)  # Clear audio
  })

  # Katydid button
  observeEvent(input$katydid, {
    quiz_data$group <- "katydid"
    updateSelectInput(session, 'answer', choices = unique(katydid$species))
    output$audio_player <- renderUI(NULL)  # Clear audio
  })

  # Select a random file and generate options
  observeEvent(input$new_test, {
    req(quiz_data$group) #makes sure cricket or katydid is selected before continuing
    metadata<-if(quiz_data$group=="cricket")cricket else katydid #if else statment so determine which one is being used
      
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
    
  # Display audio player
    output$audio_player <- renderUI({
      req(quiz_data$file)

    #we can change this back later
      tags$audio(src = paste0('https://raw.githubusercontent.com/JenniferSlater/OrthoID/main/Audio.20/', 
                              quiz_data$file), type = 'audio/mp3', controls = NA) 
    })
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
