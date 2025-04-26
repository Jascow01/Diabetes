library(shiny)
library(dplyr)
library(shinyWidgets)
library(DT)
library(RPostgres)
library(DBI)
library(pool)
library(dplyr)
library(dbplyr)
library(tidymodels)
library(shinycssloaders)
library(rpart.plot)

library(vip)

options(shiny.maxRequestSize = 30 * 1024^2)

dec_tree <- readRDS("decision_tree.rds")

server <- function(input, output, session) {
  
  model_trained <- reactiveVal(FALSE)
  # Reactive value to store the DB pool
  rv <- reactiveValues(db_pool = NULL, tables = NULL, selected_table = NULL, data = NULL)
  
  ##############################################
  ########### Połączenie z bazą danych #########
  ##############################################
  
  observeEvent(input$connect_db, {
    req(input$db_user, input$db_password)
    
    tryCatch({
      pool <- dbPool(
        RPostgres::Postgres(),
        dbname = "postgres",
        host = "database-1.cy76oqy82sx8.us-east-1.rds.amazonaws.com",
        port = 5432,
        user = input$db_user,
        password = input$db_password
      )
      dbGetQuery(pool, "SELECT 1")  # test połączenia
      
      rv$db_pool <- pool
      rv$tables <- dbListTables(pool)
      rv$data <- NULL  # Wyczyść dane CSV, jeśli połączymy się z bazą
      rv$selected_table <- NULL
      
      showNotification("✅ Połączono z bazą danych.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("❌ Błąd połączenia:", e$message), type = "error")
      rv$db_pool <- NULL
      rv$tables <- NULL
      rv$selected_table <- NULL
    })
  })
  
  # Wyświetlanie statusu połączenia z bazą danych
  output$db_status <- renderUI({
    if (!is.null(rv$db_pool)) {
      span("🟢 Połączono z bazą danych", style = "color:green;")
    } else {
      span("🔴 Brak połączenia", style = "color:red;")
    }
  })
  
  # Wyświetlanie dostępnych tabel po połączeniu z bazą
  output$table_select_ui <- renderUI({
    req(rv$tables)
    
    tags$section(
      selectInput("selected_table", "Wybierz tabelę z bazy danych:", choices = rv$tables),
      tags$div(
        style= "display: flex; justify-content: center; align-items: center; padding-top: 10px; padding-bottom: 10px;",
        actionButton("load_table", "Załaduj tabelę")
      )
    )
    
  })
  
  
  # Ładowanie danych z wybranej tabeli
  observeEvent(input$load_table, {
    req(input$selected_table, rv$db_pool)
    
    tryCatch({
      df <- dbGetQuery(rv$db_pool, paste0(
        "SELECT * FROM ", dbQuoteIdentifier(rv$db_pool, input$selected_table)
      ))
      
      rv$data <- df                         # <-- Dane z bazy
      rv$selected_table <- input$selected_table
      showNotification(paste0("✅ Załadowano tabelę: ", input$selected_table), type = "message")
      
    }, error = function(e) {
      showNotification(paste("❌ Błąd wczytywania tabeli:", e$message), type = "error")
      rv$data <- NULL
      rv$selected_table <- NULL
    })
  })
  
  ####################################################
  ############## Ładowanie pliku CSV #################
  ####################################################
  
  observeEvent(input$csv_file, {
    req(input$csv_file)
    
    tryCatch({
      df <- read.csv(input$csv_file$datapath)
      
      rv$data <- df                         # <-- Dane z CSV
      rv$db_pool <- NULL                    # Blokujemy możliwość połączenia do bazy
      rv$tables <- NULL
      rv$selected_table <- NULL
      
      showNotification("✅ Plik CSV został załadowany.", type = "message")
      
    }, error = function(e) {
      showNotification("❌ Błąd wczytywania pliku CSV.", type = "error")
      rv$data <- NULL
    })
  })
  
    
    
    # Wyświetlanie podglądu danych z pliku CSV lub tabeli w bazie
    output$data_preview <- renderDT({
      tryCatch({
        req(rv$data)  # tylko jeśli dane zostały wczytane
        datatable(rv$data)
      }, error = function(e) {
        datatable(
          data.frame(Komunikat = "❌ Błąd wczytywania danych."),
          options = list(dom = 't'),
          rownames = FALSE
        )
      })
    })
    
    
    
    #observe({
    #  if (!is.null(rv$data)) {
    #    shinyjs::disable("connect_db_btn")  # Wyłączenie przycisku logowania do bazy
    #  } else {
    #    shinyjs::enable("connect_db_btn")   # Włączenie przycisku logowania, jeśli nie ma danych CSV
    #  }
    #})
    
    
   # onStop(function() {
   #   if (!is.null(rv$db_pool)) poolClose(rv$db_pool)
  #  })
    
    
  #############################################################
  ################### budowa modelu ###########################
  #############################################################
  
  
  # wybór zmiennej objaśnianej 

    output$var_select <- renderUI({
      df <- if (!is.null(rv$data)) {
        rv$data
      } else if (!is.null(rv$selected_table)) {
        rv$selected_table
      } else {
        return(NULL)
      }
      
      vars <- names(df)
      
      tagList(
        selectInput("target_var", "Wybierz zmienną objaśnianą:", choices = vars)
      )
    })
    
  
  
  
  # Zmienne do wykluczenia
 
    output$exclude_vars <- renderUI({ 
      req(input$target_var)
      
      df <- if (!is.null(rv$data)) {
        rv$data
      } else if (!is.null(rv$selected_table)) {
        rv$selected_table
      } else {
        return(NULL)
      }
      
      vars <- df %>%
        select(-input$target_var) %>%
        names()
      
      checkboxGroupInput("vars_to_exclude", "Wybierz zmienne do wykluczenia:", choices = vars)
    })
    
  
    
  
  # Trening modelu na podstawie wyboru użytkownika
    model <- eventReactive(input$train_button, {
      req(input$target_var, input$model_type)
      
      # Wybór źródła danych
      df <- if (!is.null(rv$data)) rv$data else rv$selected_table
      target <- input$target_var
      
      if (!is.factor(df[[target]])) {
        df[[target]] <- as.factor(df[[target]])
      }
      
      showPageSpinner("Budowanie modelu...", color = "#f3969a")
      
      split <- initial_split(df, strata = target)
      train_data <- training(split)
      test_data <- testing(split)
      
      rec <- recipe(as.formula(paste(target, "~ .")), data = train_data)
      
      hidePageSpinner()
      
      if (input$model_type == "Random Forest") {
        showPageSpinner("Tuning modelu...", color = "#f3969a")
        model_spec <- rand_forest(trees = tune()) %>%
          set_mode("classification") %>%
          set_engine("ranger", importance = "impurity")
        
        wf <- workflow() %>%
          add_recipe(rec) %>%
          add_model(model_spec)
        
      hidePageSpinner()
        folds <- bootstraps(train_data, strata = !!sym(target), times = 5)
        grid <- grid_regular(trees(range = c(1,5)), levels = 3)
        
        tuned <- tune_grid(
          wf, 
          resamples = folds,
          grid = grid,
          metrics = metric_set(roc_auc, ppv, npv)
        )
        
        best_auc <- select_best(tuned, metric = "roc_auc")
        final <- finalize_workflow(wf, best_auc)
        
        hidePageSpinner()
        
      } else if (input$model_type == "Decision Tree") {
        showPageSpinner("Tuning modelu...", color = "#f3969a")
        model_spec <- decision_tree(cost_complexity = tune(), tree_depth = tune()) %>%
          set_mode("classification") %>%
          set_engine("rpart")
        
        wf <- workflow() %>%
          add_recipe(rec) %>%
          add_model(model_spec)
        
        folds <- vfold_cv(train_data, v = 5, strata = !!sym(target))
        grid <- grid_regular(cost_complexity(), tree_depth(range = c(1,5)), levels = 3)
        
        tuned <- tune_grid(
          wf,
          resamples = folds,
          grid = grid,
          metrics = metric_set(roc_auc, accuracy)
        )
        
        best_auc <- select_best(tuned, metric = "roc_auc")
        final <- finalize_workflow(wf, best_auc)
        hidePageSpinner()
      }
      
      sentiment_final <- last_fit(final, split)
      
      showPageSpinner("Tworzenie wyników...", color = "#f3969a")
      
      output$metricsDT <- renderDT({
        sentiment_final %>%
          collect_metrics() %>%
          datatable()
      })
      
      output$confMatrix <- renderPlot({
        sentiment_final %>%
          collect_predictions() %>%
          conf_mat(!!sym(target), .pred_class) %>%
          autoplot(type = "heatmap")
      })
      
      output$varImportance <- renderPlot({
        if (input$model_type == "Random Forest") {
          final_model <- final %>%
            fit(train_data) %>%
            extract_fit_parsnip()
          vip::vip(final_model)
        } else if (input$model_type == "Decision Tree") {
          final_model <- final %>%
            fit(train_data) %>%
            extract_fit_parsnip()
          rpart.plot::rpart.plot(final_model$fit, type = 2)
        }
      })
      
      output$model_output_ui <- renderUI({
        layout_columns(
          card(card_header('Metryki'),
               card_body(DTOutput('metricsDT'))
          ),
          card(card_header('Macierz trafności'),
               card_body(plotOutput('confMatrix'))
          ),
          card(card_header(ifelse(input$model_type == "Random Forest",
                                  'Ważność zmiennych',
                                  'Drzewo decyzyjne')),
               card_body(plotOutput('varImportance'))
          ),
          col_widths = c(12, 6, 6)
        )
      })
      
      hidePageSpinner()
      model_trained(TRUE)
    })
    
      
      # Wyświetlanie wyników modelu
      output$model_output  <- renderPrint({
        req(model())
        model()
      })
    
      
      #####################################
      ############## PREDYKCJA ############
      #####################################
      
      # ReactiveVal na wynik predykcji lub gif
      prediction_result <- reactiveVal(NULL)
      
      # Obsługa kliknięcia przycisku PREDYKCJI
      observeEvent(input$predict_btn, {
        
        # Najpierw ustawiamy ikonę lub gifa jako tymczasowy wynik
        prediction_result(
          tags$img(src = "widetime2.gif", width = "200px", alt = "Czekaj na wynik...")
        )
        
        # Dane od użytkownika
        new_data <- data.frame(
          gender = factor(input$gender, levels = c("0", "1")),
          age = factor(input$age, levels = c("0-13", "14-18", "19-24", "25-34", "35-44", "45-59", "60+")),
          hypertension = factor(input$hypertension, levels = c(0, 1)),
          heart_disease = factor(input$heart_disease, levels = c(0, 1)),
          smoking_history = factor(input$smoking_history,
                                   levels = c("current","ever","former","never","No Info","not current")),
          HbA1c_level = input$hba1c,
          blood_glucose_level = input$glucose
        )
        
        # Predykcja
        prediction <- predict(dec_tree, new_data = new_data, type = "class")
        
        # Po otrzymaniu predykcji aktualizujemy wynik
        prediction_result(
          tags$h3(paste("✅ Model przewiduje:", as.character(prediction$.pred_class)))
        )
      })
      
      # Wyświetlenie prediction_result (gif lub wynik)
      output$prediction_result <- renderUI({
        prediction_result()
      })
      
  
  
}
