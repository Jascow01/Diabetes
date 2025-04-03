library(shiny)
library(dplyr)

server <- function(input, output, session) {
  
  # Reactive: przetwarzanie przesłanego pliku
  raw_data <- reactive({
    req(input$upload_data)  # Ensure file is uploaded
    
    tryCatch({
      read.csv(input$upload_data$datapath)
    }, error = function(e){
      stop("Błąd podczas odczytu: ", e$message)
    })
    
  })
  
  
  # wyświetlanie danych z porzesłanego pliku
    output$data_preview <- renderTable(head(raw_data()))
  
  
}
