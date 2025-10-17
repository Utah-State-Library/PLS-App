

##### Filter Data #####
selected_library <- reactive({
  input$libname
})


###### Library Info Box ######

output$libinfo_LSA <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  string_LSA <- df %>%
    filter(FISCAL_YEAR == current_year) %>%
    mutate(POPU_LSA = format(as.numeric(POPU_LSA), big.mark = ",")) %>%
    pull(POPU_LSA)
  
  tagList(p(strong("Population of Legal Service Area: "), string_LSA)
  )
})

output$libinfo_TOTOPEXP <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  string_TOTOPEXP <- df %>%
    filter(FISCAL_YEAR == current_year) %>%
    mutate(TOTOPEXP = paste0("$",format(as.numeric(TOTOPEXP), big.mark = ","))) %>%
    pull(TOTOPEXP)
  
  tagList(p(strong("Total Operating Expenses: "), string_TOTOPEXP)
  )
})

output$libinfo_TOTINCM <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  string_TOTINCM <- df %>%
    filter(FISCAL_YEAR == current_year) %>%
    mutate(TOTINCM = paste0("$",format(as.numeric(TOTINCM), big.mark = ","))) %>%
    pull(TOTINCM)
  
  tagList(
    p(strong("Total Revenue: ", string_TOTINCM))
  )
})

output$libinfo_ADDR <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  string_ADDRESS <- df %>%
    filter(FISCAL_YEAR == current_year) %>%
    mutate(ADDRESS_full = paste0(str_to_title(ADDRESS), ", ", str_to_title(CITY), ", ", ZIP)) %>%
    pull(ADDRESS_full)
  
  tagList(
    p(strong(string_ADDRESS))
  )
})




##### Value Boxes #####

###### Header ######
output$libheader <- renderUI({
  input$libname
})

###### Registered Borrowers ######
output$regborCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "REGBOR", pull = "CY")
})
output$regborPY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "REGBOR", pull = "PY")
})
output$regborchange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "REGBOR", pull = "change")
})

###### FTE ######
output$fteCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "TOTSTAFF", pull = "CY")
})
output$ftePY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "TOTSTAFF", pull = "PY")
})
output$ftechange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "TOTSTAFF", pull = "change")
})

###### Visits ######
output$visitsCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "VISITS", pull = "CY")
})
output$visitsPY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "VISITS", pull = "PY")
})
output$visitschange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "VISITS", pull = "change")
})

###### LSA ######
output$popu_lsaCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "POPU_LSA", pull = "CY")
})
output$popu_lsaPY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "POPU_LSA", pull = "PY")
})
output$popu_lsachange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_valuebox(df, "POPU_LSA", pull = "change")
})


###### HCs ######

## Add others - circulation (phys & elec)

output$hc_circ <- renderHighchart({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_value_over_time_1lib(df, 
                           values = c("PHYSCIR", "ELMATCIR", "OTHPHCIR"),  #TOTCIR not calculated right in 2023
                           ytt_format = "{point.y:.0,f}",
                           grouptext = "", 
                           ytext = "Circulation", 
                           ttext = "Circulation over Time",
                           year_range = (current_year-5):current_year)
})


output$hc_rev <- renderHighchart({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  get_value_over_time_1lib(df, 
                           values = c("LOCGVT", "STGVT", "FEDGVT", "OTHINCM"#, "TOTINCM"
                                      ), 
                           ytt_format = "${point.y:.0,f}",
                           grouptext = "Funding Source", 
                           ytext = "Revenue", 
                           ttext = "Revenue over Time",
                           year_range = (current_year-5):current_year)
})

###### Financial Table ######

output$financial_table <- renderReactable({
  
  df <- financials %>%
    filter(CURRENT_LIBNAME == selected_library(),
           FISCAL_YEAR == current_year) %>%
    select(GROUP, INDICATOR, VALUE)
  
  
  df %>%reactable(
    resizable = T,
    defaultExpanded = T,
    compact = T,
    striped = T,
    sortable = F,
    defaultColDef = colDef(
      align = "left",
    ),
    theme = reactableTheme(backgroundColor = "transparent",
                           headerStyle = list(borderColor = "#555"
                           )),
    groupBy = "GROUP",
    columns = list(
      GROUP = colDef(name = ""),
      INDICATOR = colDef(name = ""),
      VALUE = colDef(name = "Amount", 
                     format = colFormat(prefix = "$", separators = TRUE, digits = 0)#,
                     #aggregate = "max"
                     )
      
      
    )
  )
})

output$budgetpcnts <- renderReactable({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == selected_library())
  
  df %<>% 
    select(FISCAL_YEAR, STAFFEXP, TOTEXPCO, PRMATEXP, ELMATEXP, OTHMATEX, OTHOPEXP, TOTOPEXP) %>% 
    filter(FISCAL_YEAR %in% c(current_year, current_year-1)) %>%
    mutate(across(STAFFEXP:TOTOPEXP, ~ as.numeric(.))) %>%
    mutate(YEAR_INDIC = ifelse(FISCAL_YEAR == current_year, "CY", "PY"))
  
  totalexp <- df %>% 
    filter(FISCAL_YEAR == max(FISCAL_YEAR)) %>%
    summarise(tot = sum(STAFFEXP, PRMATEXP, ELMATEXP, OTHMATEX, OTHOPEXP)) %>%
    pull(tot)
  
  df %<>% 
    select(-FISCAL_YEAR) %>%
    pivot_longer(
      !c(YEAR_INDIC),
      names_to = "Expense",
      values_to = "Amount"
    ) %>%
    pivot_wider(
      names_from = "YEAR_INDIC",
      names_glue = "{.value}_{YEAR_INDIC}",
      values_from = "Amount"
    ) %>% 
    left_join(variable_key %>% select(INDICATOR, SHORTNAME), by = c("Expense" = "SHORTNAME")) %>%
    rowwise() %>%
    mutate(change = (Amount_CY-Amount_PY)/Amount_PY,
           pcnt_budget = Amount_CY/totalexp)
  
  
  df %<>%
    select(INDICATOR, Amount_PY, Amount_CY, change, pcnt_budget)
 
  df %>% reactable(resizable = T,
                   defaultExpanded = F,
                   compact = T,
                   striped = T,
                   defaultColDef = colDef(
                     align = "left",
                   ),
                   theme = reactableTheme(backgroundColor = "transparent",
                                          headerStyle = list(borderColor = "#555"
                                          )),
                   columns = list(
                     INDICATOR = colDef(name = ""),
                     Amount_PY = colDef(name = "Last Year", 
                                        format = colFormat(prefix = "$", separators = TRUE)),
                     Amount_CY = colDef(name = "Current Year", 
                                       format = colFormat(prefix = "$", separators = TRUE)),
                     change = colDef(name = "Change from Last Year",
                                     format = colFormat(percent = TRUE, digits = 2)),
                     pcnt_budget = colDef(name = "% of Total Expenses",
                                          format = colFormat(percent = TRUE, digits = 2))
                   )
  )
  
})


###### Per Cap Totals Table ######

output$percap_comp <- renderReactable({
  
  cols <- c("REGBOR", "VISITS", "TOTSTAFF", "REFERENC", "TOTCIR", "TOTPRO", "TOTATTEN", "PITUSR", "WIFISESS")
  
  df_l <- pls %>% 
    filter(FISCAL_YEAR == current_year) %>%
    select(CURRENT_LIBNAME, POPU_LSA, cols) %>%
    pivot_longer(-c(CURRENT_LIBNAME, POPU_LSA),
                 names_to = "METRIC", 
                 values_to = "VALUE_L") %>%
    group_by(CURRENT_LIBNAME) %>%
    mutate(VALUE_L = as.numeric(VALUE_L),
           POPU_LSA = as.numeric(POPU_LSA),
           PER_CAP_L = round(VALUE_L/POPU_LSA, 2)) %>%
    ungroup() %>%
    group_by(METRIC) %>%
    mutate(rank_value = rank(desc(PER_CAP_L), ties.method = "min")) %>%
    filter(CURRENT_LIBNAME == input$libname) %>%
    select(METRIC, VALUE_L, PER_CAP_L, rank_value)
  
  rank_n <- pls %>%
    filter(FISCAL_YEAR == current_year) %>%
    summarise(n_distinct(CURRENT_LIBNAME)) %>% pull()
  
  df_st <- pls %>%
    filter(FISCAL_YEAR == current_year) %>%
    select(POPU_LSA, cols) %>% 
    mutate(across(c(POPU_LSA, cols), ~ as.numeric(.))) %>%
    mutate(across(c(POPU_LSA, cols), ~ sum(., na.rm = T))) %>%
    distinct() %>%
    pivot_longer(-c(POPU_LSA),
                 names_to = "METRIC", 
                 values_to = "VALUE_ST") %>%
    rowwise() %>%
    mutate(VALUE_ST = as.numeric(VALUE_ST),
           POPU_LSA = as.numeric(POPU_LSA),
           PER_CAP_ST = round(VALUE_ST/POPU_LSA, 2)) %>%
    select(METRIC, PER_CAP_ST)
  
  df <- df_l %>% left_join(df_st, by = "METRIC")
  
  df %<>% mutate(METRIC = case_when(METRIC == "REGBOR" ~ "Registered Borrowers",
                                    METRIC == "VISITS" ~ "Visits",
                                    METRIC == "TOTSTAFF" ~ "FTE",
                                    METRIC == "REFERENC" ~ "Reference Transactions",
                                    METRIC == "TOTCIR" ~ "Total Circulation", 
                                    METRIC == "TOTPRO" ~ "Number of Programs",
                                    METRIC == "TOTATTEN" ~ "Program Attendance",
                                    METRIC == "PITUSR" ~ "Public Computer Sessions",
                                    METRIC == "WIFISESS" ~ "Wifi Sessions"))
  
  df %<>% select(METRIC, VALUE_L, PER_CAP_L, PER_CAP_ST, rank_value)
  
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
        VALUE_L = colDef(name = "Total", format = colFormat(separators = TRUE)),
        PER_CAP_L = colDef(name = "Per Capita"),
        PER_CAP_ST = colDef(name = "UT Per Capita"),
        rank_value = colDef(name = paste0("Per Capita Rank / ", rank_n))
      ),
      columnGroups = list(
        colGroup(name = input$libname, columns = c("VALUE_L", "PER_CAP_L"))
      )
    )
  
  
  
})


###### Library Map ######
output$library_map <- renderLeaflet({
  
  if(selected_library() == "All Libraries"){
    FSCS <- pls %>% 
      summarise(FSCSKEY) %>% distinct() %>% pull()
  } else {
    FSCS <- pls %>% 
      filter(CURRENT_LIBNAME == selected_library()) %>%
      summarise(FSCSKEY) %>% distinct() %>% pull()
  }

  
  df <- librarykey %>% 
    filter(FSCS_ID %in% FSCS) %>%
    mutate(LAT = as.numeric(LAT),
           LONG = as.numeric(LONG), 
           library_info = paste0( 
             "<table>
                        <div style='font-size: 18px;'><b>", ADMINISTRATIVE_ENTITY_NAME, "</div>
                        <div style='font-size: 12px;'><b>", LIBRARY_NAME, "</div>
                        <div style='font-size: 12px;'><b>", str_to_title(ADDRESS), ", ", str_to_title(CITY), ", ", ZIP,"</div>
        </table>"))
  
  if(nrow(df > 0)){
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      addMarkers(data = df,
                 lng = ~ LONG,
                 lat = ~ LAT,
                 label = ~ LIBRARY_NAME,
                 popup = ~ lapply(library_info, HTML),
                 popupOptions = popupOptions(keepInView = TRUE),
                 ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) %>%
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