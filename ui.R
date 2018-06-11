## DEPENDENCIES --------------------------------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(visNetwork)
library(highcharter)
library(DT)
library(shinyAce)
library(shinyalert)

## SIDEBAR -------------------------------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput(inputId = "upload_file", label = "Please upload ICN", accept = ".csv"),
    hr(),
    menuItem("Table", tabName = "table", icon = icon("table")),
    menuItem("Network", tabName = "viznetwork", icon = icon("connectdevelop")),
    menuItem("Hive", tabName = "hiveql", icon = icon("database")),
    useShinyalert(),
    # Action button to check if the app is connected to dsna
    actionButton("check_connection", 
                          "Check dsna connection",
                          icon = icon("wifi"),
                          style = "position: absolute; bottom: 10px")
  )
)


## BODY ----------------------------------------------------------------------------------------------------------------
body <- dashboardBody(
  fluidRow(
    tabItems(
      # First tab content
      tabItem(tabName = "table",
              fluidRow(
                box(DTOutput("table_1"), width = 12)
              )
      ),
      # Second tab content
      tabItem(tabName = "viznetwork",
              fluidRow(
                box(visNetworkOutput("network", height = "600px"), width = 12)
              )
      ),
      # Third tab content
      tabItem(tabName = "hiveql",
              fluidRow(
                tabBox(
                  width = 9,
                  tabPanel("Hive editor", 
                           # Ace editor for users to query dsna data
                           aceEditor(
                             "code",
                             mode = "sql",
                             theme = "tomorrow_night_eighties",
                             height = "200px",
                             fontSize = 14,
                             autoComplete = "live"
                           ),
                           # textAreaInput("sql", 
                           #               label = NULL,
                           #               width = "1000px",
                           #               height = "100px",
                           #               placeholder = "Type your SQL here..."),
                           actionButton("run_sql", 
                                        "Execute",
                                        icon = icon("cogs"),
                                        style = "color: #ffffff; background-color: #D81E05"),
                           downloadButton("downloadData", "Download"))
                ),
                tabBox(
                  width = 3,
                  tabPanel("Editor theme",
                           # Users can change Ace editor's theme
                           selectInput(
                             "editor_theme",
                             "Theme:",
                             choices = getAceThemes(),
                             selected = "tomorrow_night_eighties"))
                ),
                tabBox(
                  width = 12,
                  tabPanel("Result",
                           # Show current query result
                           DTOutput("sql_result")),
                  tabPanel("Recent queries",
                           # Show previous queries
                           DTOutput("recent_queries"))
                )
              )
      )
    ),
    
    
    ## ADDITIONAL CSS STYLE --------------------------------------------------------------------------------------------
    tags$head(
      tags$style(
        HTML(
          "/* logo */
          .skin-blue .main-header .logo {
          background-color: #D81E05;
          }
          
          /* logo when hovered */
          .skin-blue .main-header .logo:hover {
          background-color: #D81E05;
          }
          
          /* navbar (rest of the header) */
          .skin-blue .main-header .navbar {
          background-color: #D81E05;
          }
          
          /* toggle button when hovered  */
          .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #D81E05;
          }

          /* notification when connecting to dsna or retrieving data from dsna  */
          .shiny-notification {
          height: 61.8px;
          width: 200px;
          position: fixed;
          top: calc(50% - 50px);;
          left: 50%;
          }
          "
        )
      )
    )
  )
)


## DASHBOARD PAGE ------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "dsna bess network",
                  tags$li(class = "dropdown",
                          tags$a(tags$img(src = "SB_logo_White.png", width = 105)
                          )
                  )
  ),
  sidebar,
  body
)