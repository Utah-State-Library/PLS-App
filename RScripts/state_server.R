##### Update Pickers

observe({
  ae_name <- outlets %>%
    filter(CNTY %in% input$st_county, FSCSKEY %in% current_FSCS) %>%
    reframe(CURRENT_LIBNAME_AE) %>%
    distinct() %>%
    pull() %>%
    sort()

  updatePickerInput(
    session,
    "st_ae",
    "Select Libraries by System",
    choices = ae_name,
    selected = ae_name,
    options = list(
      `live-search` = TRUE,
      `actions-box` = TRUE,
      `selected-text-format` = paste0("count > ", length(ae_name) - 1),
      `count-selected-text` = "All Library Systems"
    )
  )
})

observe({
  updateCheckboxInput(
    session,
    "show_libs",
    "Show Library Locations?",
    if (!input$show_service) {
      TRUE
    }
  )
})

observe({
  updateCheckboxInput(
    session,
    "show_service",
    "Show Counties/Cities with Library Service?",
    if (!input$show_libs) {
      TRUE
    }
  )
})


##### Filter Data #####
map_libs_filtered <- eventReactive(
  input$submitButton,
  {
    outlets %>%
      filter(
        CNTY %in% input$st_county,
        CURRENT_LIBNAME_AE %in% input$st_ae
      )
  },
  ignoreNULL = FALSE
)


output$state_map <- renderLeaflet({
  input$submitButton

  map_df <- isolate(map_libs_filtered())

  render_map(
    map_libs_df = map_df,
    outlets = outlets,
    county_shp = county_shp,
    municipalities = municipalities,
    show_libs = input$show_libs,
    show_service = input$show_service
  )
})

###### Per Cap Totals Table ######

output$percap_st <- renderReactable({
  cols <- c(
    "VISITS",
    "REGBOR",
    "TOTSTAFF",
    "TOTSTAFF",
    "VLNT",
    "REFERENC",
    "TOTCIR",
    "TOTPHYS",
    "LOANFM",
    "TOTPRO",
    "TOTATTEN",
    "PITUSR",
    "WIFISESS"
  )

  df <- pls %>%
    filter(FISCAL_YEAR == current_year) %>%
    filter(CURRENT_LIBNAME == "All Libraries") %>%
    select(POPU_LSA, cols) %>%
    pivot_longer(-c(POPU_LSA), names_to = "METRIC", values_to = "VALUE") %>%
    mutate(
      VALUE = as.numeric(VALUE),
      POPU_LSA = as.numeric(POPU_LSA),
      PER_CAP = round(VALUE / POPU_LSA, 2)
    ) %>%
    ungroup() %>%
    select(METRIC, VALUE, PER_CAP)

  df %<>%
    mutate(
      METRIC = case_when(
        METRIC == "REGBOR" ~ "Registered Borrowers",
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
        METRIC == "WIFISESS" ~ "Wifi Sessions"
      )
    )

  df %>%
    reactable(
      resizable = T,
      defaultExpanded = F,
      compact = T,
      striped = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        backgroundColor = "transparent",
        headerStyle = list(
          #background = "#ecf0f1",
          borderColor = "#555"
        )
      ),
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

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "VISITS", pull = "CY")
})
output$m_visitsPY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "VISITS", pull = "PY")
})
output$m_visitschange <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "VISITS", pull = "change")
})


###### Registered Borrowers ######
output$m_regborCY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "REGBOR", pull = "CY")
})
output$m_regborPY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "REGBOR", pull = "PY")
})
output$m_regborchange <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "REGBOR", pull = "change")
})


###### LSA ######
output$m_popu_lsaCY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "POPU_LSA", pull = "CY")
})
output$m_popu_lsaPY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "POPU_LSA", pull = "PY")
})
output$m_popu_lsachange <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "POPU_LSA", pull = "change")
})


###### FTE ######
output$m_fteCY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTSTAFF", pull = "CY")
})
output$m_ftePY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTSTAFF", pull = "PY")
})
output$m_ftechange <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTSTAFF", pull = "change")
})
