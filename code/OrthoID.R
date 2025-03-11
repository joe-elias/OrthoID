library(shiny)
library(dplyr)

metadata<-read.csv('audio.csv', stringsAsFactors = FALSE)


random_row<-metadata[sample(row(metadata), 1), ]
random_file<-random_row$filename
correct_species<-random_row$species
common_name<-random_row$common

ui <- fluidPage(
  titlePanel('Ortho ID'), 
  
  sidebarLayout(
    sidebarPanel(
      actionButton('new_test', 'Orthopterate!'), 
      textInput('answer', "Answer:", value = "Gryllus rubens"),
      actionButton('submit', 'Submit Answer'), 
      textOutput('feedback')
    ), 
    mainPanel(
      uiOutput('audio_player')
    )
  )
)

server<-function(input, output, session){
  quiz_data<-reactiveValues(file=NULL, species=NULL, common=NULL)
  
  # select a random file 
  observeEvent(input$new_test, {
    selected<-metadata[sample(nrow(metadata), 1), ]
    quiz_data$file<-selected$filename
    quiz_data$species<-selected$species
    quiz_data$common<-selected$common
  })
  
  # display audio player
  output$audio_player<-renderUI({
    req(quiz_data$file)
    tags$audio(src=paste0('https://raw.githubusercontent.com/joe-elias/OrthoID/main/audio/', 
                          quiz_data$file), type='audio/mp3', controls=NA)
  })
  
  # check answer
  observeEvent(input$submit, {
    req(input$answer, quiz_data$species)
    if (tolower(input$answer)==tolower(quiz_data$species)){
      output$feedback<-renderText(paste("correct!", quiz_data$species, ';', quiz_data$common))
    } else {
      output$feedback<-renderText(paste('wrong! correct answer = :' , 
                                        quiz_data$species, ';', quiz_data$common))
    }
  })
}

shinyApp(ui = ui, server = server)

# end 
# next steps after we get the scientific and common to show - images of the species 
