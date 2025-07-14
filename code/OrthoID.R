# shiny app to learn and reference songs of Orthopterans in Eastern Texas
library(shiny)
library(dplyr)

# Load metadata from CSV (I decided to make 2 seperate csv's for crickets and katydids)
cricket <- read.csv("Crickets.csv", stringsAsFactors = FALSE)
katydid <- read.csv("Katydid.csv", stringsAsFactors = FALSE)

# UI ---------------------------------------------------------------------------
ui <- navbarPage(
  title = 'Ortho ID',
  tabPanel("Quiz",
           fluidPage(
             tags$head(
               tags$style(HTML("
          body {
        background-color: #e7f1dc;
        font-family: 'Segoe UI', sans-serif;
     }
       #cricket, #katydid {
      background-color: #6c9a8b;
      color: white;
    }
       #cricket:hover, #katydid:hover {
      background-color: #55877c;
    }
      .btn {
        background-color: #a3c585;
        color: white;
      }
      
      .btn:hover{
        background-color: #99b878;
        color: white;
      }
      #hint {
        background-color: #f39c12;
        color: white;
      }
      #hint:hover {
        background-color: #e67e22;
        color: white;
      }
      #submit {
        background-color: #acd8a7;
        color: white;
      }
      #submit:hover {
        background-color: #8bca84;
        color: white;
      }
        "))
             ),
             
             ## side bar layout----
             sidebarLayout(
               sidebarPanel(
                 actionButton("cricket", "Cricket"),
                 actionButton("katydid", "Katydid"),
                 actionButton("new_test", "Orthopterate!"),
                 selectInput('species_answer', "Choose the correct scientific name:", choices = NULL), 
                 selectInput('common_answer', "Choose the correct common name:", choices = NULL),
                 actionButton('submit', 'Submit'), 
                 actionButton('hint','Hint'),
                 textOutput('feedback'),
                 tags$h4(tags$strong(style = "color: #4c6e63", "Points:")),
                 textOutput("Points")
               ), 
               #main panel ----
               mainPanel(
                 uiOutput('audio_player'),
                 uiOutput('wave_displayer'),
                 uiOutput('image_displayer')
               )
             )
           )
  ),
  ## about panel ----
  tabPanel("About", 
           fluidPage(
             img(src="https://cdn.vectorstock.com/i/500p/78/79/bush-katydid-north-spiders-2-vector-53807879.jpg", 
                 height="300px", width="400px"),
             tags$h1(tags$strong( style = "color: #4c6e63","Welcome to Ortho ID!")),
             tags$h4("This app helps users identify Orthoptera species using sound and image cues."),
             tags$h4("Recordings and pictures are specific to ", tags$strong(style="color: #6c9a8b","Eastern Texas"), "!"),
             
             tags$h3("How to Play", style = "color: #4c6e63"),
             tags$h5("To play the game, simply press either the", tags$strong(style="color: #6c9a8b","Cricket"),"or", tags$strong(style="color: #6c9a8b","Katydid"), "button then click", tags$strong(style="color: #6c9a8b","Orthopterate!")),
             tags$h5("The ", tags$strong(style="color: #6c9a8b","audio recording"), "should show up with the ", tags$strong(style="color: #6c9a8b","wavelength"), "below."),
             tags$h5("Guess both the ", tags$strong(style="color: #6c9a8b","scientific"), "and ", tags$strong(style="color: #6c9a8b","common names"), "from the dropdowns, when you're done click ", tags$strong(style="color: #6c9a8b","Submit"),"to check your answer."),
             tags$h5("If you need help, click on", tags$strong(style="color: #6c9a8b","Hint")," to see a related image."),
             tags$h5(tags$strong(style="color: #6c9a8b", "For more information"), "click on", tags$strong(style="color: #6c9a8b","Guide")," and it will show you more about that orthopterate!."),
             tags$h3("Enjoy!", style = "color: #4c6e63")
      )
  ),
  # Orthopteran Guide ----
  tabPanel("Guide", 
           fluidPage(
             h3("A Guide to Common Crickets and Katydids of Eastern Texas"), 
             uiOutput("Guide"), 
             fluidRow(
               column(6, h4("crickets"), tableOutput("crickets")), 
               column(6, h4("katydids"), tableOutput("katydids"))
             )
           )), 
)


# Server Section ---------------------------------------------------------------
server <- function(input, output, session) {
  ## QUIZ ----
  quiz_data <- reactiveValues(file = NULL, species = NULL, common = NULL, group=NULL, points=0)
  
  # Cricket button
  observeEvent(input$cricket, {
    quiz_data$group <- "cricket"
    updateSelectInput(session, 'species_answer', choices = unique(cricket$species))
    output$audio_player <- renderUI(NULL)  # Clear audio
    output$wave_displayer <- renderUI(NULL)
    output$image_displayer <- renderUI(NULL)
    updateActionButton(session, "new_test", disabled = FALSE)  #bug fix
  })
  
  # Katydid button
  observeEvent(input$katydid, {
    quiz_data$group <- "katydid"
    updateSelectInput(session, 'species_answer', choices = unique(katydid$species))
    output$audio_player <- renderUI(NULL)  # Clear audio
    output$wave_displayer <- renderUI(NULL)
    output$image_displayer <- renderUI(NULL)
    updateActionButton(session, "new_test", disabled = FALSE)
  })
  
  # Select a random file and generate options
  observeEvent(input$new_test, {
    req(quiz_data$group) #makes sure cricket or katydid is selected before continuing
    metadata<-if(quiz_data$group=="cricket")cricket else katydid #if else statment so determine which one is being used
    
    selected <- metadata[sample(nrow(metadata), 1), ]
    quiz_data$file <- selected$filename
    quiz_data$species <- selected$species
    quiz_data$common <- selected$common
    quiz_data$wave <- selected$Wavelength
    quiz_data$hint <- selected$Images
    
    # Generate random incorrect species
    incorrect_options <- sample(metadata$species[metadata$species != quiz_data$species], 4)
    all_options <- sample(c(quiz_data$species, incorrect_options)) # Shuffle options
    
    incorrect_common <- sample(metadata$common[metadata$common != quiz_data$common], 4)
    all_common <- sample(c(quiz_data$common, incorrect_common)) # Shuffle options
    
    # Update the selectInput choices
    updateSelectInput(session, 'species_answer', choices = all_options)
    updateSelectInput(session, 'common_answer', choices = all_common)
    output$feedback <- renderText("") #just resets feedback so it isn't on for next question
    output$image_displayer <- renderUI("")
    
    # Display audio player
    output$audio_player <- renderUI({
      req(quiz_data$file)
      
      #we can change this back later
      tags$audio(src = paste0('https://raw.githubusercontent.com/JenniferSlater/OrthoID/main/Audio.20/', 
                              quiz_data$file), type = 'audio/mp3', controls = NA) 
    })
    # Wavelength
    output$wave_displayer <- renderUI({
      req(quiz_data$wave)
      
      #we can change this back later
      tags$img(src = paste0('https://raw.githubusercontent.com/JenniferSlater/OrthoID/main/Audio.20/', 
                            quiz_data$wave), type = 'wave/png', height="100px", width="300px") 
    })
  #Points
    output$Points <- renderText({paste(quiz_data$points)})
    
  })
  # Check answer
  observeEvent(input$submit, {
    req(input$species_answer, quiz_data$species,input$common_answer, quiz_data$common)
    
    if (tolower(input$species_answer) == tolower(quiz_data$species) && 
        tolower(input$common_answer) == tolower(quiz_data$common)) {
      output$feedback <- renderText(paste("Correct!", quiz_data$species, ';', quiz_data$common))
      quiz_data$points <- quiz_data$points + 2 #Adding points
    } 
    else if(tolower(input$species_answer) == tolower(quiz_data$species) || 
            tolower(input$common_answer) == tolower(quiz_data$common) ){
      output$feedback <- renderText(paste("Almost! Correct answer: ", quiz_data$species, ';', quiz_data$common))
      quiz_data$points <- quiz_data$points + 1 #Half Credit
    }
    else {
      output$feedback <- renderText(paste("Wrong! Correct answer: ", quiz_data$species, ';', quiz_data$common))
      
    }
  })
  #HINT BUTTON :)
  observeEvent(input$hint, {
    req(quiz_data$hint)
    output$image_displayer <- renderUI({
      tags$img(src = paste0('https://raw.githubusercontent.com/JenniferSlater/OrthoID/main/Audio.20/', 
                            quiz_data$hint), type = 'img/jpg', height="200px", width="300px") 
    })
  })
  ## Guide Panel ----
  renderPrint({
    quiz_data
  })
}

# Run the App
shinyApp(ui = ui, server = server)
