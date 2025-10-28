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


## Update map control checkboxes so that there is always one library layer showing
# else it defaults to the endless world and no one wants that
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


# #### Define Columns ####

# table_columns_state <- list(
#   "Overview" = c(
#     "TOTINCM",
#     "TOTSTAFF",
#     "VLNT",
#     "VLNT_HRS",
#     "VISITS",
#     "REGBOR",
#     "TOTCIR",
#     "TOTPHYS",
#     "GPTERMS",
#     "TOTPRO",
#     "TOTATTEN"
#   ),
#   "Revenue" = c("TOTINCM", "LOCGVT", "STGVT", "FEDGVT", "OTHINCM"),
#   "Total Expenditures" = c("TOTOPEXP", "STAFFEXP", "TOTEXPCO", "OTHOPEXP"),
#   "Staff Expenditures" = c("TOTOPEXP", "STAFFEXP", "SALARIES", "BENEFIT"),
#   "Collection Expenditures" = c(
#     "TOTOPEXP",
#     "TOTEXPCO",
#     "PRMATEXP",
#     "ELMATEXP",
#     "OTHMATEX"
#   ),
#   "Circulation" = c(
#     "TOTCIR",
#     "PHYSCIR",
#     "KIDCIRCL",
#     "ELMATCIR",
#     "HOTSPOT_CIRC",
#     "OTHPHCIR",
#     "EBOOK_CIR",
#     "EAUDIO_CIR",
#     "EVIDEO_CIR",
#     "ESERIAL_CIR"
#   ),
#   "Collections" = c("TOTPHYS", "BKVOL", "AUDIO_PH", "VIDEO_PH", "OTHMATS"),
#   "Number of Programs" = c(
#     "TOTPRO",
#     "K0_5PRO",
#     "K6_11PRO",
#     "YAPRO",
#     "ADULTPRO",
#     "GENPRO"
#   ),
#   "Program Attendance" = c(
#     "TOTATTEN",
#     "K0_5ATTEN",
#     "K6_11ATTEN",
#     "YAATTEN",
#     "ADULTATTEN",
#     "GENATTEN"
#   ),
#   "Visits, Borrowers, Reference, and ILL" = c(
#     "VISITS",
#     "REFERENC",
#     "REGBOR",
#     "LOANTO",
#     "LOANFM"
#   ),
#   "Internet Access" = c(
#     "GPTERMS",
#     "PITUSR",
#     "WIFISESS",
#     "HOTSPOT",
#     "HOTSPOT_CIRC"
#   )
# )

# all_cols_state <- reactive({
#   req(input$table_selection_state)

#   table_columns_state[[input$table_selection_state]]
# })

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


#### Render State Map ####
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


#### Render Percent Change Tables ####
output$table_state <- renderReactable({
  req(state_all_libs)

  render_pct_change_table(
    df = state_all_libs,
    variable_key = variable_key,
    year = current_year,
    cols = all_cols_state(),
    percap = input$percap.state
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

###### Circulation ######
output$m_totcirCY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTCIR", pull = "CY")
})
output$m_totcirPY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTCIR", pull = "PY")
})
output$m_totcirchange <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTCIR", pull = "change")
})

###### Revenue ######
output$m_totincmCY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTINCM", pull = "CY")
})
output$m_totincmPY <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTINCM", pull = "PY")
})
output$m_totincmchange <- renderUI({
  input$submitButton

  FSCS <- isolate(map_libs_filtered()$FSCSKEY)

  df <- pls %>%
    filter(FSCSKEY %in% FSCS)

  get_valuebox(df, year = current_year, "TOTINCM", pull = "change")
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
