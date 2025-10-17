
#### Define Columns ####

table_columns <- list(
  "Overview" = c("TOTINCM", "TOTSTAFF", "VISITS", "REGBOR", "TOTCIR", "TOTPHYS", "GPTERMS", "TOTPRO", "TOTATTEN"),
  "Revenue" = c("TOTINCM", "LOCGVT", "STGVT", "FEDGVT", "OTHINCM"),
  "Total Expenditures" = c("TOTOPEXP", "STAFFEXP", "TOTEXPCO", "OTHOPEXP"),
  "Staff Expenditures" = c("TOTOPEXP", "STAFFEXP", "SALARIES", "BENEFIT"),
  "Collection Expenditures" = c("TOTOPEXP", "TOTEXPCO", "PRMATEXP","ELMATEXP", "OTHMATEX"),
  "Circulation" = c("TOTCIR", "PHYSCIR", "KIDCIRCL", "ELMATCIR", "OTHPHCIR", "EBOOK_CIR", "EAUDIO_CIR", "EVIDEO_CIR", "ESERIAL_CIR"),
  "Collections" = c("TOTPHYS", "BKVOL", "AUDIO_PH", "VIDEO_PH", "OTHMATS"),
  "Number of Programs" = c("TOTPRO", "K0_5PRO", "K6_11PRO", "YAPRO", "ADULTPRO", "GENPRO"),
  "Program Attendance" = c("TOTATTEN", "K0_5ATTEN", "K6_11ATTEN", "YAATTEN", "ADULTATTEN", "GENATTEN"),
  "Visits, Borrowers, Reference, and ILL" = c("VISITS", "REFERENC", "REGBOR", "LOANTO", "LOANFM"),
  "Internet Access" = c("GPTERMS", "PITUSR", "WIFISESS", "HOTSPOT")
)

# Add base cols to the selected cols
all_cols <- reactive({
  table_columns[[input$table_selection]]
})


#### Reactive Data ####
df_table <- reactive({
  
  pls %>%
    filter(CURRENT_LIBNAME %in% input$library.table,
           FISCAL_YEAR %in% input$year.table,
           hide_lib == 0
           #STABR %in% input$state.table
           )
})


#### Render Table ####
output$table_all <- renderReactable({
  req(all_cols())  # make sure input is valid
  
  render_table(
    data = df_table(),
    cols = all_cols(),
    variable_key = variable_key,
    order_col = NULL,
    per_cap = input$per_cap.table,
    peer = FALSE,
    target_lib = NULL,
    peer_col = NULL,
    color_table = input$color_table
  )
})

output$csv_button <- renderUI({
  req(input$table_selection)
  
  cleaned_name <- input$table_selection %>%
    tolower() %>%
    gsub("[\\s]+", "_", .) %>%
    gsub(",", "", .)
  
  filename <- paste0(cleaned_name, "_table.csv")
  
  csvDownloadButton("table_all", filename = filename)
})


#### Reactive Peer Data ####

target_lib <- reactive({
  input$library.table_peer
})

df_table_peer <- reactive({
  
  df <- pls %>% filter(FISCAL_YEAR == input$year.table_peer,
                       hide_lib == 0)
  
  libs <- get_nclosest(df, 
                       n = 10,
                       libname = input$library.table_peer,
                       pg_col = input$peergroup,
                       percap = input$per_cap.table_peer)
  
  df %>%
    filter(CURRENT_LIBNAME %in% libs)

})

## Render Library Summary
output$header_peer <- renderUI({
  req(input$library.table_peer)
  req(input$peergroup)
  
  peer_pretty <- variable_key %>% filter(SHORTNAME == input$peergroup) %>% select(INDICATOR) %>% pull()
  
  HTML(paste0("<h4> Closest 10 libraries to ", input$library.table_peer, " based on ", peer_pretty, "</h4>"))
  
})


#### Get Selected Table Columns ####
peer_cols <- reactive({
  table_columns[[input$table_selection_peer]]
})

#### Render Peer Table ####
output$table_peer <- renderReactable({
  req(peer_cols())  # make sure input is valid
  
  render_table(
    data = df_table_peer(),
    cols = peer_cols(),
    variable_key = variable_key,
    order_col = input$peergroup,
    per_cap = input$per_cap.table_peer,
    peer = TRUE,
    target_lib = target_lib(),
    peer_col = input$peergroup,
    color_table = input$color_table_peer
  )
})

output$csv_button_peer <- renderUI({
  req(input$table_selection_peer)
  
  cleaned_name <- input$table_selection_peer %>%
    tolower() %>%
    gsub("[\\s]+", "_", .) %>%
    gsub(",", "", .)
  
  if(input$per_cap.table_peer){
    cleaned_name <- paste0(cleaned_name, "_per_capita")
  }
  
  prefix <- paste0(tolower(input$peergroup), "_peer_")
  
  filename <- paste0(prefix, cleaned_name, "_table.csv")
  
  csvDownloadButton("table_peer", filename = filename)
})
