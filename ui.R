
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(shinyWidgets)
library(htmltools)
#source("custom_theme.R")
#thematic::thematic_shiny()

#===============================================================================
# Page 1
#===============================================================================

# Tab 1

tab1 <- page_fillable(
  
  tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    # Include niceScroll library
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/jquery.nicescroll/3.7.6/jquery.nicescroll.min.js")
  ),

  div(
    id = 'data_preview_div',
    class = 'preview_class',
    style = "
    width: 100%; 
    height: calc(100vh - 250px);  /* dostosuj w zależności od layoutu */
    overflow: hidden;
    display: flex;
    align-items: center;
    justify-content: center;
  ",
    uiOutput("data_preview")
  )
)

# Tab 2

tab2 <- page_fillable(
  
  layout_sidebar(
    sidebar = sidebar(
      open = "open",
      style = "background-color: #fafafa; padding: 20px; border-radius: 10px;",
      
      uiOutput("ngramAnalysisUI"),
      hr(),
      selectInput("wordFreqVar", tags$b("Wybierz zmienną tekstową do analizy częstości słów"), choices = NULL),
      selectInput("sentimentVar", "Wybierz zmienną sentymentu", choices = NULL),
      uiOutput("sentimentValuesUI"),
      input_task_button("analyzeWords", "Analizuj słowa",class = "btn-centered"),
      hr(),
      selectInput("sentenceLengthVar", tags$b("Rozkład długości słów"), choices = NULL),
      checkboxInput("showDensity", "Funkcja gęstości", value = TRUE),
      sliderInput("barsNum",label = "Liczba słupków:",value = 30,min = 10, max = 50,step = 5),
      input_task_button("SLPbutton",'Długość zdań',class = "btn-centered")
      
    ),
    layout_columns(
      card(card_header('N-gramy'),
           DTOutput("ngramsTable")
      ),
      card(card_header('Częstość słów'),
           DTOutput('wordFreqTable')
      ),
      card(card_header('Długość zdań'),
           plotOutput('sentenceLengthPlot')),
      col_widths =c(6,6,12) 
      
    )
  ),
  
)



# Tab 3

tab3 <- fillPage(
  
  layout_sidebar(
    sidebar = sidebar(
      open = TRUE,
      style = "background-color: #fafafa; padding: 20px; border-radius: 10px;",
      
      selectInput('plotType', tags$b('Typ wykresu'), choices = c("Wordcloud")),
      conditionalPanel(
        condition = "input.plotType == 'Wordcloud'",
        selectInput('variable', 'Wybierz zmienną (dla wordcloud)', choices = NULL),
        selectInput("groupingVariable", "Wybierz zmienną grupującą", choices = NULL, selected = NULL),
        hr(),
        input_task_button("drawWordcloud", "Narysuj wordcloud",class = "btn-centered",width = '100%')
      ),
      
    ),
    layout_columns(
      card(card_body(
        plotOutput("plot")
      ),full_screen = TRUE)
    )
  )
)

# Exploration Page

exploration <- page_fillable(
  
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Eksploracja Danych</span>"),
    nav_panel("Ramka danych", tab1), # data browse and imputation
    #nav_panel("Statystyki", tab2), # data description, variable statistics
    #nav_panel("Wizualizacja", tab3) # distributions, boxplots, dependecies 
  )
  #,
  #theme = bs_theme(
  #  preset = "pulse"
  #)
)

#===============================================================================
# Page 2
#===============================================================================


tab_m1 <- page_fillable(
  uiOutput("training_ui_wrapper")  # całość sterowana przez serwer
)


tab_m2 <- page_fillable(
  
  layout_sidebar(
    sidebar = sidebar(
      open = "open",
      style = "background-color: #fafafa; padding: 20px; border-radius: 10px;",
      width = 350,
      
      radioButtons("gender", "Płeć:", c("Mężczyzna" = 0, "Kobieta" = 1)),
      radioButtons("age", "Wiek:", choices =  c("0-13", "14-18", "19-24", "25-34", "35-44", "45-59", "60+")),
      radioButtons("hypertension", "Nadciśnienie:", choices = c("Nie choruje" = 0, 
                                                                "Choruje" = 1)),
      radioButtons("heart_disease", "Choroba serca:", choices = c("Nie" = 0, 
                                                                  "Tak" = 1)),
      radioButtons("smoking_history", "Historia palenia:", 
                  choices = c("Palę" = "current",
                              "Czasami palę" = "ever",
                              "Już nie palę" = "former",
                              "Nigdy nie paliłem/łam" = "never",
                              #"Obecnie nie palę" = "not current",
                              "Brak informacji" = "No Info")),
      #numericInput("bmi", "BMI:", value = 25),
      numericInput("hba1c", "HbA1c:", value = 5.5),
      numericInput("glucose", "Poziom glukozy:", value = 100),
      input_task_button("predict_btn", "Przewiduj",class = "btn-centered")
    ),
        div(
          id = "prediction_result_wrapper",
          class = "preview_class",
          style = "
          height: calc(100vh - 300px);  /* dostosuj w zależności od layoutu */
          ",
          uiOutput("prediction_result")
        )
  )
)

page2 <- page_fillable(
  
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Model</span>"),
    nav_panel("Tworzenie", tab_m1),
    nav_panel("Predykcja", tab_m2)
  )
)



#===============================================================================
# Sidebar
#===============================================================================

sidebar_main <- sidebar(
  open = "open",
  style = "background-color: #fafafa; 
    padding: 20px; 
    border-radius: 10px;",
  width = 300,
  tags$head(
    # Include Font Awesome for icons
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
  ),
  
  # HTML5 Header
  tags$section(
    style = "padding: 15px; 
      background-color: #ffffff; 
      border-radius: 10px; 
      margin-bottom: 20px;",
    titlePanel(HTML("<span class='section-title'>Zaloguj do bazy</span>"))
  ),
  tags$section(
    textInput("db_user", "Użytkownik bazy danych:"),
    passwordInput("db_password", "Hasło do bazy danych:"),
    tags$div(
      input_task_button("connect_db", "Połącz z bazą danych",class = "btn-centered")
    ),
    uiOutput("db_status"),
    uiOutput("table_select_ui"),
  ),
  
  tags$div(
    style = "display: flex; 
      justify-content: center; 
      margin-top: 20px;",
    tags$label(
      class = "btn btn-success",
      "📁 Wybierz plik CSV",
      tags$input(
        id = "csv_file",
        type = "file",
        accept = ".csv",
        style = "display: none;"  # Ukryj input
      )
    )
  ),
  
  # HTML5 Section for summary and variable actions
  tags$section(
    style = "padding: 15px; 
      background-color: #ffffff; 
      border-radius: 10px; 
      margin-bottom: 20px;",
    titlePanel(HTML("<span class='section-title'>Podsumowanie</span>"))
  ),
  tags$section(
    hr(),
    DTOutput("varTypeTable"),
    selectInput("selectedVar", tags$b("Wybierz zmienną do zmiany typu"), choices = NULL),
    selectInput("newVarType", "Wybierz nowy typ", choices = c("numeric", "character", "factor")),
    
    # Centering button using a div and CSS flexbox
    tags$div(
      input_task_button("changeVarType", "Zmień typ zmiennej",class = "btn-centered")
    ),
    
    hr(),
    
    # Centering button using a div and CSS flexbox
  ),
  
  # HTML5 Section for handling missing data
  tags$section(
    hr(),
    selectInput("missingDataAction", tags$b("Wybierz akcję dla braków danych"), choices = c("Usuń rekordy")),
    
    # Centering button using a div and CSS flexbox
    tags$div(
      input_task_button("handleMissingData", "Zastosuj akcję",class = "btn-centered")
    )
  ),
  # HTML5 Footer
  tags$footer(
   
  )
)

#===============================================================================
# Combine
#===============================================================================

page_navbar(
  title = HTML("Diabetes 🍬"),
  underline = TRUE,
  sidebar = sidebar_main,
  padding = 0,
  
  # Custom CSS to style the tab titles with borders
  header = tags$head(
    # Existing CSS
    tags$style(HTML("
          body {
        background-color: #f5f7fa;
        font-family: 'Segoe UI', sans-serif;
      }
    
      .btn-centered {
        display: block;
        margin: 15px auto;
        text-align: center;
        background-color: #007bff;
        color: white;
        border-radius: 10px;
        padding: 10px 20px;
        font-weight: bold;
      }
    
      .btn-centered:hover {
        background-color: #0056b3;
        color: white;
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
    
      .main-content {
        padding: 20px;
      }
    
      .input-task-button {
        width: 100%;
        text-align: center;
      }
      .navbar-nav {
        width: 100%;
        text-align: center;
      }
      .navbar-nav > li {
        flex-grow: 1;
      }
      .navbar-nav > li > a {
        width: 100%;
        text-align: center;
        border: 2px solid #ddd;
        border-radius: 5px;
        padding: 10px 15px;
        margin: 5px;
        transition: all 0.3s ease;
      }
      .navbar-nav > li > a:hover {
        background-color: #f8f9fa;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
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
    ")),
    
    # New JavaScript for page transitions
    tags$script(HTML("
      $(document).on('shiny:busy', function() {
        $('.main-content').fadeTo(200, 0.5);
      });
      $(document).on('shiny:idle', function() {
        $('.main-content').fadeTo(200, 1);
      });
      
      // Smooth scroll to top when changing tabs
      $('.navbar-nav a').on('click', function() {
        $('html, body').animate({ scrollTop: 0 }, 300);
      });
    "))
  ),
  
  tabPanel(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Eksploracja Danych</span>"), 
    exploration
  ),
  tabPanel(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Tworzenie Modelu</span>"), 
    page2
  )
)
