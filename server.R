library(shiny)
library(dplyr)
library(shinyWidgets)
library(DT)
library(RPostgres)
library(DBI)
library(pool)
library(dplyr)
library(dbplyr)
pool <- dbPool(
  RPostgres::Postgres(),
  dbname = "postgres",
  host = "postgres-1.cy76oqy82sx8.us-east-1.rds.amazonaws.com",
  port = 5432,
  user = "postgres",
  password = "Kbjsdkoa1kl"
)

pool %>% tbl("diabets") %>% head()


server <- function(input, output, session) {
  
  # Reactive: przetwarzanie przesłanego pliku
  raw_data <- reactive({
    req(input$upload_data)  # Ensure file is uploaded
    
    tryCatch({
      read.csv(input$upload_data$datapath)
      
      # 
        
    }, error = function(e){
      stop("Błąd podczas odczytu: ", e$message)
    })
    
  })
  
  
  # wyświetlanie danych z porzesłanego pliku
    output$data_preview <- renderDT({
      req(raw_data())
      datatable(raw_data(), options = list(pageLength = 10))  # Display first 10 rows
    })
  
    
  
}
