## DEPENDENCIES --------------------------------------------------------------------------------------------------------
library(DBI)
library(pool)
library(tidyverse)
library(visNetwork)
library(formattable)
library(DT)
library(lubridate)
library(shinyAce)
library(shinyalert)

## CREATE CONNECTION TO dsna BY DSN ------------------------------------------------------------------------------------
pool <- dbPool(odbc::odbc(), dsn = "HP1-64bit", Port = 10000)

onStop(function() {
  poolClose(pool)
})

## SERVER --------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  ## UTILITTIES --------------------------------------------------------------------------------------------------------
  bess_data <- reactive({
    validate(
      need(input$upload_file != "", "Please upload ICN.")
    )
    
    # Take the UTR extract that users uploaded
    inFile <- input$upload_file
    
    read_csv(inFile$datapath)
  })
  
  ## ICN EXTRACTION FROM mds_bess_s1_mt103 -----------------------------------------------------------------------------
  bess <- reactive({
    withProgress(message = 'Connecting to dsna',
                 {
                   tbl(pool, "mds_bess_s1_mt103")
                 })
  })
  
  # Retrieve data once icn file is uploaded
  bess_sample <- reactive({
    withProgress(message = 'Retrieving data',
                 {
                   bess() %>% 
                     filter(icn %in% bess_data()$icn) %>% 
                     select(icn,
                            tag_32a_date,
                            tag_32a_currency,
                            tag_32a_amt,
                            sender_node_key,
                            receiver_node_key,
                            sender_name,
                            receiver_name,
                            sender_segment,
                            receiver_segment,
                            sender_cid,
                            receiver_cid) %>% 
                     collect()
                 })
  }) 
  
  # DataTables output with download function
  output$table_1 <- renderDT(datatable(bess_sample() %>% select(-sender_node_key, -receiver_node_key),
                                       extensions = c("Buttons", "Scroller", "FixedColumns"),
                                       options = list(dom = 'Bfrtip',
                                                      buttons = c("csv", "excel", "pdf"),
                                                      columnDefs = list(list(className = 'dt-center', targets = list(7, 8))),
                                                      deferRender = TRUE,
                                                      scroller = TRUE,
                                                      scrollY = 560,
                                                      scrollX = TRUE,
                                                      fixedColumns = list(leftColumns = 2))),
                             # Be able to download the whole table
                             server = FALSE)
                                        
  ## visNetwork --------------------------------------------------------------------------------------------------------
  # Edges for network
  edges_bess_sample <- reactive({
    bess_sample() %>%
      mutate(tag_32a_date = as.Date(tag_32a_date)) %>%
      rename(from = sender_node_key) %>%
      rename(to = receiver_node_key) %>%
      rename(weight = tag_32a_amt) %>%
      mutate(width = weight / 50000 + 1) %>%
      mutate(title = paste("ICN:", icn,
                           "<br/>",
                           "Transaction Amount:", accounting(weight),
                           "<br/>",
                           "Currency:", tag_32a_currency,
                           "<br/>",
                           "Value date:", tag_32a_date,
                           "<br/>",
                           "from:", from,
                           "<br/>",
                           "to:", to))
  }) 

  # Sender and receiver nodes
  sender_bess_sample <- reactive({
    bess_sample() %>%
      select(sender_node_key, sender_name, sender_segment) %>%
      rename(id = sender_node_key,
             label = sender_name,
             segment = sender_segment) %>%
      mutate(color = if_else(segment == "C", "red", "blue"))
  }) 

  receiver_bess_sample <- reactive({
    bess_sample() %>%
      select(receiver_node_key, receiver_name, receiver_segment) %>%
      rename(id = receiver_node_key,
             label = receiver_name,
             segment = receiver_segment) %>%
      mutate(color = if_else(segment == "C", "red", "blue"))
  }) 

  # Combine sender and receiver nodes
  node_bess_sample <- reactive({
    sender_bess_sample() %>%
      bind_rows(receiver_bess_sample()) %>%
      .[!duplicated(.$id), ]
  }) 

  # visNetwork output
  output$network <- renderVisNetwork({
    nodes <- node_bess_sample()
    edges <- edges_bess_sample()

    visNetwork(nodes = nodes,
               edges = edges,
               width = "100%") %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based",
                 stabilization = TRUE) %>%
      visEdges(arrows = "middle")
  })
  
  ## HIVE EDITOR -------------------------------------------------------------------------------------------------------
  # Store editor input
  sql_script <- eventReactive(input$run_sql, {
    input$code
  })
  
  # Initialize blank table for recent queries
  reactive_values <- reactiveValues()
  
  reactive_values$queries_table <- tibble(Time = character(0), Query = character(0))
  
  # Execute sql query and update recent queries table
  observeEvent(input$run_sql, {
    # Query result
    output$sql_result <- renderDT(
      withProgress(message = 'Retrieving data', {
        tryCatch({
          datatable(dbGetQuery(pool, sql_script()),
                    extensions = c("Buttons", "Scroller"),
                    options = list(dom = 'Bfrtip',
                                   buttons = "csv",
                                   deferRender = TRUE,
                                   scroller = TRUE,
                                   scrollY = 560,
                                   scrollX = TRUE))
        },
        error = function(e) {
          shinyalert("Oops!", conditionMessage(e), type = "error", closeOnClickOutside = TRUE)
        })
      }),
      server = FALSE
    )
    
    # Update recent queries table
    reactive_values$queries_table <- add_row(reactive_values$queries_table, 
                                             Time = format(Sys.time(), "%Y-%m-%d %r"), 
                                             Query = sql_script())
    
    output$recent_queries <- renderDT(
      datatable(reactive_values$queries_table, options = list(dom = 't')) %>% 
        formatStyle("Query", color = "red")
    )
  })
  
  ## DOWNLOAD SQL SCRIPT -----------------------------------------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      "Hive.sql"
    },
    content = function(file) {
      write_file(sql_script(), file)
    }
  )
  
  ## EDITOR THEME ------------------------------------------------------------------------------------------------------
  observe({
    updateAceEditor(session,
                    "code",
                    mode = "sql",
                    theme = input$editor_theme)
  })
  
  ## CHECK dsna CONNECTION ---------------------------------------------------------------------------------------------
  observeEvent(input$check_connection, {
    tryCatch({
      nrow(dbGetQuery(pool, "show tables")) >= 1
      shinyalert("You're connected!", 
                 "This app is connected to dsna.", 
                 type = "success", 
                 closeOnClickOutside = TRUE)
    },
    error = function(e) {
      shinyalert("Oops!", 
                 "This app is disconnected to dsna. Please refresh it", 
                 type = "error", 
                 closeOnClickOutside = TRUE)
    })
  })
}