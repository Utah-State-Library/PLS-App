
##### Update Pickers

observe({
  
  counties <- toupper(input$st_county)
  
  ae_name <- librarykey %>% 
    filter(COUNTY %in% input$st_county,
           FSCS_ID %in% current_FSCS) %>%
    summarise(ADMINISTRATIVE_ENTITY_NAME) %>% distinct() %>% pull() %>% sort()
  
  updatePickerInput(session, 
                    "st_ae",
                    "Select Libraries by System",
                    choices = ae_name,
                    selected = ae_name,
                    options = list(`live-search`=TRUE,
                                   `actions-box` = TRUE,
                                   `selected-text-format` = paste0("count > ", length(ae_name) -1), `count-selected-text` = "All Library Systems"))
})



##### Filter Data #####
mp_FSCS_filtered <- eventReactive(input$submitButton, {
  
  FSCS <- librarykey %>% 
    filter(COUNTY %in% input$st_county,
           ADMINISTRATIVE_ENTITY_NAME %in% input$st_ae
           ) %>%
    summarise(FSCS_ID) %>% 
    distinct() %>% pull()

}, 
ignoreNULL = FALSE)


###### Library Map ######
output$state_map <- renderLeaflet({
  
  ## Add certified vs emerging vs other
  
  input$submitButton
  
  FSCS <- isolate(mp_FSCS_filtered())
  
  leaflet_data <- librarykey %>% 
    filter(FSCS_ID %in% FSCS)
  
  leaflet_data %<>%
    mutate(LAT = as.numeric(LAT),
           LONG = as.numeric(LONG), 
           library_info = paste0( 
             "<table>
                        <div style='font-size: 18px;'><b>", ADMINISTRATIVE_ENTITY_NAME, "</div>
                        <div style='font-size: 12px;'>", LIBRARY_NAME, "</div>
                        <div style='font-size: 12px;'> Library Type: ", LIBRARY_TYPE, "</div>
                        <div style='font-size: 12px;'>", str_to_title(ADDRESS), ", ", str_to_title(CITY), ", ", ZIP,"</div>
        </table>"))
  
  if(nrow(leaflet_data > 0)){
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      addMarkers(data = leaflet_data,
                 lng = ~ LONG,
                 lat = ~ LAT,
                 label = ~ LIBRARY_NAME, #lapply(labelSchoolName, HTML),
                 popup = ~ lapply(library_info, HTML),
                 popupOptions = popupOptions(keepInView = TRUE),
      ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) %>%
      setMaxBounds(lng1 = -109, 
                   lat1 = 37, 
                   lng2 = -114, 
                   lat2 = 42 ) %>%
      onRender(
        "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")
  } else {
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      setMaxBounds(lng1 = -109, 
                   lat1 = 37, 
                   lng2 = -114, 
                   lat2 = 42 ) %>%
      onRender(
        "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")
  }
})



###### Per Cap Totals Table ######

output$percap_st <- renderReactable({
  
  cols <- c("VISITS", "REGBOR", "TOTSTAFF", "TOTSTAFF", "VLNT", "REFERENC", "TOTCIR", "TOTPHYS", "LOANFM", "TOTPRO", "TOTATTEN", "PITUSR", "WIFISESS")
  
  df <- pls %>% filter(FISCAL_YEAR == current_year) %>%
    filter(CURRENT_LIBNAME == "All Libraries"
           ) %>%
    select(POPU_LSA, cols) %>%
    pivot_longer(-c(POPU_LSA),
                 names_to = "METRIC", 
                 values_to = "VALUE") %>%
    mutate(VALUE = as.numeric(VALUE),
           POPU_LSA = as.numeric(POPU_LSA),
           PER_CAP = round(VALUE/POPU_LSA, 2)) %>%
    ungroup() %>%
    select(METRIC, VALUE, PER_CAP)
  
  df %<>% mutate(METRIC = case_when(METRIC == "REGBOR" ~ "Registered Borrowers",
                                    METRIC == "VISITS" ~ "Visits",
                                    METRIC == "TOTSTAFF" ~ "FTE",
                                    METRIC == "VLNT" ~ "Volunteers",
                                    METRIC == "REFERENC" ~ "Reference Transactions",
                                    METRIC == "TOTCIR" ~ "Total Circulation", 
                                    METRIC == "TOTPHYS" ~ "Physical Books",
                                    METRIC == "LOANFM" ~ "Inter Library Loans",
                                    METRIC == "TOTPRO" ~ "Number of Programs",
                                    METRIC == "TOTATTEN" ~ "Program Attendance",
                                    METRIC == "PITUSR" ~ "Public Computer Sessions",
                                    METRIC == "WIFISESS" ~ "Wifi Sessions"))
  
  df %>% 
    reactable(
      resizable = T,
      defaultExpanded = F,
      compact = T,
      striped = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(backgroundColor = "transparent",
                             headerStyle = list(#background = "#ecf0f1",
                               borderColor = "#555"
                             )),
      columns = list(
        METRIC = colDef(name = ""),
        VALUE = colDef(name = "Total", format = colFormat(separators = TRUE)),
        PER_CAP = colDef(name = "Per Capita")
      )
    )
  
  
  
})


##### Value Boxes #####


###### Visits ######
output$m_visitsCY <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "VISITS", pull = "CY")
})
output$m_visitsPY <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "VISITS", pull = "PY")
})
output$m_visitschange <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "VISITS", pull = "change")
})


###### Registered Borrowers ######
output$m_regborCY <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "REGBOR", pull = "CY")
})
output$m_regborPY <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "REGBOR", pull = "PY")
})
output$m_regborchange <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "REGBOR", pull = "change")
})


###### LSA ######
output$m_popu_lsaCY <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "POPU_LSA", pull = "CY")
})
output$m_popu_lsaPY <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "POPU_LSA", pull = "PY")
})
output$m_popu_lsachange <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "POPU_LSA", pull = "change")
})


###### FTE ######
output$m_fteCY <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "TOTSTAFF", pull = "CY")
})
output$m_ftePY <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "TOTSTAFF", pull = "PY")
})
output$m_ftechange <- renderUI({

  input$submitButton

  FSCS <- isolate(mp_FSCS_filtered())

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, "TOTSTAFF", pull = "change")
})



