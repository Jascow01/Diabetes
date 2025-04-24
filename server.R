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

library(vip)


server <- function(input, output, session) {
  
  model_trained <- reactiveVal(FALSE)
  
  # Reactive value to store the DB pool
    rv <- reactiveValues(db_pool = NULL, tables = NULL, selected_table = NULL)
    
    observeEvent(input$connect_db, {
      req(input$db_user, input$db_password)
      
      tryCatch({
        pool <- dbPool(
          RPostgres::Postgres(),
          dbname = "diabetes",
          host = "localhost",
          port = 5432,
          user = input$db_user,
          password = input$db_password
        )
        
        # test połączenia
        dbGetQuery(pool, "SELECT 1")
        
        rv$db_pool <- pool
        rv$tables <- dbListTables(pool)
        
        showNotification("✅ Połączono z bazą danych", type = "message")
        
      }, error = function(e) {
        rv$db_pool <- NULL
        rv$tables <- NULL
        showNotification(paste("❌ Błąd połączenia:", e$message), type = "error")
      })
    })
    
    output$db_status <- renderUI({
      if (!is.null(rv$db_pool)) {
        span("🟢 Połączono z bazą danych", style = "color:green;")
      } else {
        span("🔴 Brak połączenia", style = "color:red;")
      }
    })
    
    output$table_select_ui <- renderUI({
      req(rv$tables)
      selectInput("selected_table", "Wybierz tabelę do załadowania:", choices = rv$tables)
    })
    
    observeEvent(input$load_table, {
      req(input$selected_table, rv$db_pool)
      
      tryCatch({
        df <- dbGetQuery(rv$db_pool, paste0("SELECT * FROM ", dbQuoteIdentifier(rv$db_pool, input$selected_table)))
        rv$selected_table <- df
      }, error = function(e) {
        showNotification(paste("❌ Nie udało się załadować tabeli:", e$message), type = "error")
        rv$selected_table <- NULL
      })
    })
    
    output$data_preview <- renderDT({
      req(rv$selected_table)
      datatable(rv$selected_table)
    })
    
    onStop(function() {
      if (!is.null(rv$db_pool)) poolClose(rv$db_pool)
    })
  
  

  
  
  #### Database Connection - TABLE PREVIEW ####
  
    output$data_preview <- renderDT({
      tryCatch({
        req(rv$db_pool, input$selected_table)  # Sprawdzenie: połączenie + wybrana tabela
        
        df <- dbGetQuery(rv$db_pool, paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(rv$db_pool, input$selected_table)))
        datatable(df)
        
      }, error = function(e) {
        datatable(
          data.frame(Komunikat = "❌ Błąd połączenia z bazą danych lub problem z wczytaniem tabeli."),
          options = list(dom = 't'),
          rownames = FALSE
        )
      })
    })
    
    
  
  #### budowa modelu ####
  
  
  # wybór zmiennej objaśnianej
  output$var_select <- renderUI({
    req(rv$selected_table)
    
    vars <- names(rv$selected_table)
    
    tagList(
      selectInput("target_var", "Wybierz zmienną objaśnianą:", choices = vars)
    )
  })
  
  
  # Zmienne do wykluczenia
  
  output$exclude_vars <- renderUI({ 
    req(rv$selected_table, input$target_var)
    
    vars <- rv$selected_table %>%
      select(!c(input$target_var)) %>%
      names()
    
    checkboxGroupInput("vars_to_exclude", "Wybierz zmienne do wykluczenia:", choices = vars)
  })
    
  
  # Trening modelu na podstawie wyboru użytkownika
  model <- eventReactive(input$train_button, {
    cat("Kliknięto train_button! Uruchamiam model...\n")
    #df <- data_from_db()
    cat("xd \n")
    req(rv$selected_table, input$target_var ,input$model_type)
    
    df <- rv$selected_table
    target <- input$target_var
    
    # Sprawdzenie, czy zmienna objaśniana jest typu factor
    
    if (!is.factor(df[[target]])) {
      df[[target]] <- as.factor(df[[target]])
    }
    
    
    showPageSpinner(caption = 'Budowanie modelu. Proszę czekać.',color = "#f3969a")
    split <- initial_split(df, strata = target)
    train_data <- training(split)
    test_data <- testing(split)
    
    # Recipe
    rec <- recipe(as.formula(paste(target, "~ .")), data = train_data)
    
    # Model na podstawie wyboru użytkownika
    if (input$model_type == "Random Forest") {
      model_spec <- rand_forest(trees = tune()) %>%
        set_mode("classification") %>%
        set_engine("ranger",importance = "impurity")
    } 
    
    # Workflow
    wf <- workflow() %>%
      add_recipe(rec) %>%
      add_model(model_spec)
    
    folds <- bootstraps(train_data,strata = !!sym(input$target_var),times = 10)
    
    grid <- grid_regular(trees(), levels =5)
    
    tuned <- tune_grid(
      wf,
      resamples = folds,
      grid = grid,
      metrics = metric_set(roc_auc, ppv, npv)
    )
    
    best_auc <- tuned %>% 
      select_best(metric = 'roc_auc')
    
    final <- finalize_workflow(wf,best_auc)
    
    sentiment_final <- last_fit(final,split)
    
    
    
    hidePageSpinner()
    showPageSpinner(caption = "Tworzenie wyników. Proszę czekać.",color = "#f3969a")
    
    
    output$metricsDT <- renderDT(
      sentiment_final %>% 
        collect_metrics() %>%
        
        # round(.estimate,digits = 2) %>% 
        datatable()
    )
    
    output$confMatrix <- renderPlot({
      cm <- sentiment_final %>%
        collect_predictions() %>%
        conf_mat(target, .pred_class)
      
      autoplot(cm,type = 'heatmap')
      
    })
    
    output$varImportance <- renderPlot({
      final %>% 
        fit(train_data) %>%
        extract_fit_parsnip() %>%
        vip::vi(lambda = best_auc$trees) %>%
        top_n(10, wt = abs(Importance)) %>%
        ungroup() %>%
        mutate(
          Importance = abs(Importance),
          Variable = str_remove(Variable, paste0("tfidf_",sym(text),"_")),
          Variable = forcats::fct_reorder(Variable, Importance)
        ) %>%
        ggplot(aes(x = Importance, y = Variable, fill = input$target_var)) +
        geom_col(show.legend = FALSE) +
        labs(y = NULL)
      
    })
    hidePageSpinner()
    model_trained(TRUE)
    output$model_output_ui <- renderUI({
      layout_columns(
        card(card_header('Metryki'),
             card_body(DTOutput('metricsDT'))
        ),
        card(card_header('Macierz trafności'),
             card_body(plotOutput('confMatrix'))
        ),
        card(card_header('Ważność słów do predykcji sentymentu'),
             card_body(plotOutput('varImportance'))
        ),col_widths = c(12,6,6)
        
      )
      
    })
    
    
        
      })
      
      # Wyświetlanie wyników modelu
      output$model_output  <- renderPrint({
        req(model())
        model()
      })
    
  
  
}
