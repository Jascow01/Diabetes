
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(shinyWidgets)
thematic::thematic_shiny()

#===============================================================================
# Page 1
#===============================================================================

# Tab 1

tab1 <- fillPage(
  
  tags$style(HTML("
      # data_preview_div{
      overflox-x: auto;
      
      }
    ")),
  
  
  # HTML5 Section with a DataTable output
  
  div(id = 'data_preview_div',DTOutput('data_preview'))
)

# Tab 2

tab2 <- fillPage(
  layout_sidebar(
    sidebar = sidebar(
      open = 'open',
      uiOutput("ngramAnalysisUI"),
      hr(),
      selectInput("wordFreqVar", tags$b("Wybierz zmienną tekstową do analizy częstości słów"), choices = NULL),
      selectInput("sentimentVar", "Wybierz zmienną sentymentu", choices = NULL),
      uiOutput("sentimentValuesUI"),
      actionButton("analyzeWords", "Analizuj słowa",class = "btn-centered"),
      hr(),
      selectInput("sentenceLengthVar", tags$b("Rozkład długości słów"), choices = NULL),
      checkboxInput("showDensity", "Funkcja gęstości", value = TRUE),
      sliderInput("barsNum",label = "Liczba słupków:",value = 30,min = 10, max = 50,step = 5),
      actionButton("SLPbutton",'Długość zdań',class = "btn-centered")
      
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
      selectInput('plotType', tags$b('Typ wykresu'), choices = c("Wordcloud")),
      conditionalPanel(
        condition = "input.plotType == 'Wordcloud'",
        selectInput('variable', 'Wybierz zmienną (dla wordcloud)', choices = NULL),
        selectInput("groupingVariable", "Wybierz zmienną grupującą", choices = NULL, selected = NULL),
        hr(),
        actionButton("drawWordcloud", "Narysuj wordcloud",class = "btn-centered",width = '100%')
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
  ),
  theme = bs_theme(
    preset = "pulse"
  )
)

#===============================================================================
# Page 2
#===============================================================================


tab_m1 <- fillPage(
  layout_sidebar(
    sidebar = sidebar(
      open = T,
      width = 350,
      uiOutput("var_select"),
      uiOutput("exclude_vars"),
      sliderInput("split", tags$b("Podział zbioru do uczenia"), min = 0.05, max = 0.95, step = 0.05, value = 0.8),
      selectInput("model_type", "Wybierz model:", choices = c("Random Forest","Decision Tree")),
      actionButton("train_button", "Trenuj Model"),
      verbatimTextOutput("model_output"),
    ),
    # Conditional panel that only shows after the model is trained
    uiOutput("model_output_ui")
  )
)

page2 <- page_fillable(
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Model</span>"),
    nav_panel("Tworzenie", tab_m1),
  ),
  theme = bs_theme(
    preset = "minty"
  )
)

#===============================================================================
# Sidebar
#===============================================================================

sidebar_main <- sidebar(
  width = 300,
  # HTML5 Header
  tags$header(
    style = "display: flex; justify-content: center; align-items: center;",
    titlePanel(HTML("<b>Zaloguj do bazy</b>"))
  ),
  
  tags$section(
    textInput("db_user", "Użytkownik bazy danych:"),
    passwordInput("db_password", "Hasło do bazy danych:"),
    tags$div(
      style = "display: flex; justify-content: center;",
      actionButton("connect_db", "Połącz z bazą danych")
    ),
    uiOutput("db_status"),
    uiOutput("table_select_ui"),
    hr()
  ),
  
  tags$section(
    style = "display: flex; justify-content: center;",
    hr(),
    fileInput("csv_file", "Lub załaduj plik CSV:", accept = ".csv")
    ),
  
   # HTML5 Section for file upload
#  tags$section(
#    fileInput("upload_data", "Wybierz plik CSV", accept = ".csv",buttonLabel = "Wybierz...",placeholder = 'Nie wybrano pliku'),
#    selectInput("sep", "Wybierz separator:",
#                choices = c("Przecinek" = ",", "Średnik" = ";", "Tabulacja" = "\t")),
#    checkboxInput("header", "Nagłówek", TRUE)
#  ),
  
  # HTML5 Section for summary and variable actions
  tags$section(
    style = "display: flex; justify-content: center; align-items: center;",
    titlePanel(HTML("<b>Podsumowanie</b>")),
  ),
  tags$section(
    hr(),
    DTOutput("varTypeTable"),
    selectInput("selectedVar", tags$b("Wybierz zmienną do zmiany typu"), choices = NULL),
    selectInput("newVarType", "Wybierz nowy typ", choices = c("numeric", "character", "factor")),
    
    # Centering button using a div and CSS flexbox
    tags$div(
      style = "display: flex; justify-content: center;",
      actionButton("changeVarType", "Zmień typ zmiennej")
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
      style = "display: flex; justify-content: center;",
      actionButton("handleMissingData", "Zastosuj akcję")
    )
  ),
  # HTML5 Footer
  tags$footer(
    #"Autor: Szymon Dufek"
  )
)

#===============================================================================
# Combine
#===============================================================================

navbarPage(
  title = HTML("Diabetes 🍬"),
  underline = TRUE,
  sidebar = sidebar_main,
  padding = 0,
  
  # Custom CSS to style the tab titles with borders
  header = tags$style(HTML("
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
    }
  ")),
  
  tabPanel(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Eksploracja Danych</span>"), 
    exploration
  ),
  tabPanel(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Tworzenie Modelu</span>"), 
    page2
  )
)
