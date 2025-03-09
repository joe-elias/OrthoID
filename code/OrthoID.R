library(shiny)
library(dplyr)


metadata<-read.csv('audio.csv', stringsAsFactors = FALSE)

random_row<-metadata[sample(row(metadata), 1), ]
random_file<-random_row$filename
correct_species<-random_row$species

ui<-fluidPage(
  titlePanel('Ortho ID'), 
  
  sidebarLayout(
    sidebarPanel(
      actionButton('new_test', 'Orthopterate!'), 
      textInput('guess', 'enter species name'), 
      actionButton('submit', 'submit answer'), 
      textOutput('feedback')
    ), 
    mainPanel(
      uiOutput('audio_player')
    )
  )
)

server<-function(input, output, session){
  
  addResourcePath("audio", file.path(getwd(), "audio"))  # Absolute path
  quiz_data<-reactiveValues(file=NULL, species=NULL)
  
  # select a random file 
  observeEvent(input$new_test, {
    selected<-metadata[sample(nrow(metadata), 1), ]
    quiz_data$file<-selected$filename
    quiz_data$species<-selected$species
  })
  
  # display audio player
  output$audio_player<-renderUI({
    req(quiz_data$file)
    tags$audio(src=paste0('audio/', quiz_data$file), type='audio/mp3', controls=NA)
  })
  
  # check answer
  observeEvent(input$submit, {
    req(input$guess, quiz_data$species)
    if (tolower(input$guess)==tolower(quiz_data$species)){
      output$feedback<-renderText("correct!")
    } else {
      output$feedback<-renderText(paste('wrong! correct answer = :' , quiz_data$species))
    }
  })
}

shinyApp(ui = ui, server = server)

