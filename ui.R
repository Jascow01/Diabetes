#===============================================================================
# UI: Diabetes App (Shiny)
#===============================================================================

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(shinyWidgets)
library(htmltools)

#===============================================================================
# TAB: Eksploracja - Podgląd danych
#===============================================================================

tab_data_preview <- page_fillable(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery.nicescroll/3.7.6/jquery.nicescroll.min.js")
  ),
  tags$div(
    id = "data_preview_div",
    class = "preview_class",
    style = "
      width: 100%; 
      height: calc(100vh - 250px); 
      overflow-y: auto;
      overflow-x: hidden;
      display: block;
      padding: 1rem;
    ",
    uiOutput("data_preview")
  )
)



#===============================================================================
# TAB: Eksploracja - Analiza tekstu
#===============================================================================


#===============================================================================
# TAB: Eksploracja - Wizualizacja
#===============================================================================



#===============================================================================
# STRONA: Eksploracja
#===============================================================================

exploration <- page_fillable(
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Eksploracja Danych</span>"),
    nav_panel("Ramka danych", tab_data_preview)
    # nav_panel("Statystyki", tab_text_analysis)
    # nav_panel("Wizualizacja", tab_visualization)
  )
)

#===============================================================================
# STRONA: Modelowanie
#===============================================================================

# Tworzenie modelu
tab_model_create <- page_fillable(uiOutput("training_ui_wrapper"))

# Predykcja
tab_model_predict <- page_fillable(
  layout_sidebar(
    sidebar = sidebar(
      open = "open",
      width = 350,
      style = "background-color: #fafafa;
      padding: 20px;
      border-radius: 10px;",
      
      radioButtons("gender", "Płeć:", c("Mężczyzna" = 0, "Kobieta" = 1)),
      radioButtons("age", "Wiek:", c("0-13", "14-18", "19-24", "25-34", "35-44", "45-59", "60+")),
      radioButtons("hypertension", "Nadciśnienie:", c("Nie choruje" = 0, "Choruje" = 1)),
      radioButtons("heart_disease", "Choroba serca:", c("Nie" = 0, "Tak" = 1)),
      radioButtons("smoking_history", "Historia palenia:", c("Palę" = "current", "Czasami palę" = "ever", "Już nie palę" = "former", "Nigdy nie paliłem/łam" = "never", "Brak informacji" = "No Info")),
      numericInput("hba1c", "HbA1c:", value = 5.5),
      numericInput("glucose", "Poziom glukozy:", value = 100),
      input_task_button("predict_btn", "Przewiduj", class = "btn-centered")
    ),
    tags$div(
      id = "prediction_result_wrapper",
      class = "preview_class",
      style = "height: calc(100vh - 300px);",
      uiOutput("prediction_result")
    )
  )
)

page2 <- page_fillable(
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Model</span>"),
    nav_panel("Tworzenie", tab_model_create),
    nav_panel("Predykcja", tab_model_predict)
  )
)

#===============================================================================
# Sidebar główny
#===============================================================================


sidebar_main <- sidebar(
  open = "always",
  width = 300,
  style = "padding: 0;",
  #bg = "#FAEBE2",
  
  div(
    class = "p-2",
    accordion(
      id = "main_sidebar_accordion",
      #open = FALSE,
      accordion_panel(
        title = tagList(icon("sign-in-alt"), " Logowanie do bazy danych"),
        value = "logowanie",
        card_body(
          #style = "background-color: #e3f2fd;",
          textInput("db_user", "Użytkownik bazy danych:"),
          passwordInput("db_password", "Hasło do bazy danych:"),
          input_task_button("connect_db", "Połącz z bazą danych", class = "btn-centered"),
          uiOutput("db_status"),
          uiOutput("table_select_ui")
        )
      ),
      accordion_panel(
        title = tagList(icon("file-upload"), " Wybierz plik CSV"),
        value = "csv_upload",
        card_body(
          fileInput(
            inputId = "csv_file",
            label = NULL,
            accept = ".csv",
            width = "100%",
            buttonLabel = "Wybierz plik",
            placeholder = "Brak wybranego pliku"
          )
        )
      ),
      tags$hr(),
      accordion(
        open = FALSE,
        accordion_panel(
          title = tagList(icon("edit"), " Edycja danych"),
          value = "edycja",
          card_body(
            #style = "background-color: #fce444;",
            
            selectInput("selectedVar", tags$b("Wybierz zmienną do zmiany typu"), choices = NULL),
            selectInput("newVarType", "Wybierz nowy typ", choices = c("numeric", "character", "factor")),
            input_task_button("changeVarType", "Zmień typ zmiennej", class = "btn-centered"),
            hr(),
            selectInput("missingDataAction", tags$b("Wybierz akcję dla braków danych"), choices = c("Usuń rekordy")),
            input_task_button("handleMissingData", "Zastosuj akcję", class = "btn-centered")
          )
       )
      )
    )
  )
)



#===============================================================================
# Styl + Logika globalna
#===============================================================================

custom_styles <- tags$style(HTML("
  body {
    background-color: #white;
    font-family: 'Segoe UI', sans-serif;
  }
  
  
  .btn-centered {
    display: block;
    margin: 15px auto;
    background-color: #007bff;
    color: white;
    border-radius: 10px;
    padding: 10px 20px;
    font-weight: bold;
  }
  
  .btn-centered:hover {
    background-color: #0056b3;
  }
  
  .card {
    background-color: white;
    border-radius: 15px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.1);
    padding: 15px; 
  }
  
  .sidebar, .layout-sidebar > .sidebar { 
    background-color: #ffffff;
    border-right: 1px solid #e0e0e0; 
  }
  
   /* Styl paneli nawigacyjnych */
  .nav-tabs {
    display: flex;
    flex-wrap: wrap;
    justify-content: center;
  }

  .nav-tabs > li {
    flex: 1 1 auto;
    text-align: center;
    margin: 4px;
  }

  .nav-tabs > li > a {
    display: block;
    width: 100%;
    padding: 8px 12px;
    font-size: 1rem;
    white-space: nowrap;
  }

  /* Responsywność: mniejsze rozmiary czcionki i padding dla małych ekranów */
  @media (max-width: 768px) {
    .nav-tabs > li > a {
      font-size: 0.9rem;
      padding: 6px 8px;
    }
  }

  @media (max-width: 480px) {
    .nav-tabs > li {
      flex-basis: 100%;
    }
  }
  
  .navbar-nav > li > a {
    background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
    color: white !important;
    font-weight: 600;
    font-size: 16px;
    padding: 12px 20px;
    border-radius: 20px;
    box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
    transition: all 0.3s ease-in-out; border: none;
  }
  .navbar-nav > li > a:hover {
    background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);
    transform: translateY(-2px);
  }
  .navbar-nav > .active > a, .navbar-nav > li.active > a, .navbar-nav > li > a:focus {
    background: linear-gradient(135deg, #ff758c 0%, #ff7eb3 100%);
    font-weight: bold;
  }
   .nav-link {
      flex-grow: 1;
      text-align: center;
      font-weight: bold;
  }
    .nav-item {
      flex: 1;
      display: flex;
  }
    .navbar-nav {
      display: flex;
      width: 100%;
  }
  .section-title {
    text-align: center;
    font-size: 24px;
    font-weight: bold;
    color: #2c3e50;
    letter-spacing: 1px;
    text-shadow: 1px 1px #e0e0e0;
    margin-bottom: 20px;
  }
  
  /* Styl nagłówka accordion */
  .accordion-button {
    border-radius: 10px !important;
    background-color: #D3FAD6 !important;
    color: #202030;
    font-weight: bold;
    box-shadow: none;
    transition: background-color 0.3s ease;
  }

  /* Styl po rozwinięciu */
  .accordion-button:not(.collapsed) {
    background-color: #5F7470 !important;
    color: white;
    border-bottom-left-radius: 0 !important;
    border-bottom-right-radius: 0 !important;
  }

  /* Styl zawartości po rozwinięciu */
  .accordion-body {
    
    padding: 20px;
    border-bottom-left-radius: 10px;
    border-bottom-right-radius: 10px;
    box-shadow: 0 4px 8px rgba(0,0,0,0.1);
    transition: background-color 0.3s ease;
  }

  /* Dodatkowo usuń tło cienia z akordeonu (jeśli domyślne) */
  .accordion-item {
    border: none;
    margin-bottom: 10px;
  }
    
"))

custom_scripts <- tags$script(HTML("
  $(document).on('shiny:busy', function() {
    $('.main-content').fadeTo(200, 0.5);
  });
  $(document).on('shiny:idle', function() {
    $('.main-content').fadeTo(200, 1);
  });
  $('.navbar-nav a').on('click', function() {
    $('html, body').animate({ scrollTop: 0 }, 300);
  });
"))

#===============================================================================
# MAIN UI
#===============================================================================

page_navbar(
  title = HTML('<img src="logo-main.png" alt="Diabetes Logo" style="height:40px; width:260px;">'),
  underline = TRUE,
  sidebar = sidebar_main,
  padding = 0,
  header = tags$head(custom_styles, custom_scripts),
  
  nav_panel("Eksploracja Danych", exploration),
  nav_panel("Tworzenie Modelu", page2)
)
