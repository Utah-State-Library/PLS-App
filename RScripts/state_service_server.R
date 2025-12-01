##### Update Pickers

# observe({
#   ae_name <- outlets %>%
#     filter(CNTY %in% input$st_county, FSCSKEY %in% current_FSCS) %>%
#     reframe(CURRENT_LIBNAME_AE) %>%
#     distinct() %>%
#     pull() %>%
#     sort()

#   updatePickerInput(
#     session,
#     "st_ae",
#     "Select Libraries by System",
#     choices = ae_name,
#     selected = ae_name,
#     options = list(
#       `live-search` = TRUE,
#       `actions-box` = TRUE,
#       `selected-text-format` = paste0("count > ", length(ae_name) - 1),
#       `count-selected-text` = "All Library Systems"
#     )
#   )
# })

## Update map control checkboxes so that there is always one library layer showing
# else it defaults to the endless world and no one wants that
observeEvent(input$submitButton, {
  updateCheckboxInput(
    session,
    "show_libs",
    "Show Library Locations?",
    if (is.null(input$service_areas)) {
      TRUE
    }
  )
})

observeEvent(input$submitButton, {
  updatePickerInput(
    session,
    "service_areas",
    label = NULL,
    if (!input$show_libs & is.null(input$service_areas)) {
      selected = c(
        "County Library Service",
        "City Library Service",
        "Agreed Service Through a City Library",
        "Bookmobile Library Service",
        "No County Library Service",
        "No City Library Service"
      )
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
        CNTY %in% input$st_county #,
        #CURRENT_LIBNAME_AE %in% input$st_ae
      )
  },
  ignoreNULL = FALSE
)

county_map_df <- eventReactive(
  input$submitButton,
  {
    county_map %>% filter(NAME %in% input$st_county)
  },
  ignoreNULL = FALSE
)

municipalities_map_df <- eventReactive(
  input$submitButton,
  {
    municipalities_map %>% filter(CNTY %in% input$st_county)
  },
  ignoreNULL = FALSE
)

service_areas_picker <- eventReactive(
  input$submitButton,
  {
    input$service_areas
  },
  ignoreNULL = FALSE
)


#### Render State Map ####
output$state_map <- renderLeaflet({
  input$submitButton

  map_df <- isolate(map_libs_filtered())
  county_df_p <- isolate(county_map_df())
  municipalities_df_p <- isolate(municipalities_map_df())
  service_areas_p <- isolate(service_areas_picker())

  render_map(
    map_libs_df = map_df,
    outlets = outlets,
    county_map = county_df_p,
    municipalities_map = municipalities_df_p,
    show_libs = input$show_libs,
    service_areas = service_areas_p
  )
})

#### Render Service Table ####
output$municipality_table <- renderReactable({
  input$submitButton

  municipality_p <- isolate(municipalities_map_df())

  municipality_p %<>%
    st_drop_geometry()

  yes_county <- data.frame()
  yes_city <- data.frame()
  yes_bookmobile <- data.frame()
  yes_agreed <- data.frame()
  no_city <- data.frame()
  no_county <- data.frame()

  if ("County Library Service" %in% input$service_areas) {
    yes_county <- municipality_p %>% filter(county_service != "None")
  }
  if ("City Library Service" %in% input$service_areas) {
    yes_city <- municipality_p %>% filter(city_service != "None")
  }
  if ("Bookmobile Library Service" %in% input$service_areas) {
    yes_bookmobile <- municipality_p %>% filter(bookmobile_service != "None")
  }
  if ("Agreed Service Through a City Library" %in% input$service_areas) {
    yes_agreed <- municipality_p %>%
      filter(agreed_service_city != "None" | agreed_service_county != "None")
  }
  if (
    "No City Library Service" %in%
      input$service_areas &
      !"City Library Service" %in% input$service_areas
  ) {
    no_city <- municipality_p %>% filter(city_service == "None")
  }
  if (
    "No County Library Service" %in%
      input$service_areas &
      !"County Library Service" %in% input$service_areas
  ) {
    no_county <- municipality_p %>% filter(county_service == "None")
  }

  all <- rbind(
    yes_city,
    yes_county,
    yes_bookmobile,
    yes_agreed,
    no_city,
    no_county
  ) %>%
    distinct()

  all %<>%
    select(
      county = CNTY,
      county_fips = COUNTYNBR,
      municipality = NAME,
      city_fips = CITY_FIPS,
      population = POPULATION,
      county_service,
      city_service,
      bookmobile_service,
      agreed_service_city,
      agreed_service_county
    ) %>%
    arrange(county, municipality)

  # Render reactable
  all %>%
    reactable(
      resizable = TRUE,
      searchable = TRUE,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = TRUE,
      compact = TRUE,
      theme = reactableTheme(
        headerStyle = list(
          background = "#ecf0f1",
          borderColor = "#555"
        )
      ),
      defaultColDef = colDef(align = "left"),
      columns = list(
        county = colDef(name = "County"),
        county_fips = colDef(show = FALSE),
        municipality = colDef(
          name = "Municipality",
          style = list(fontweight = "bold")
        ),
        city_fips = colDef(show = FALSE),
        population = colDef(name = paste0(current_year, " Population")),
        city_service = colDef(name = "City Library Service"),
        county_service = colDef(name = "County Library Service"),
        bookmobile_service = colDef(name = "Bookmobile Service"),
        agreed_service_city = colDef(name = "Agreed Service to a City"),
        agreed_service_county = colDef(name = "Agreed Service to a County")
      )
    )
})


#### Render Percent Change Tables ####
# output$table_state <- renderReactable({
#   req(state_all_libs)

#   render_pct_change_table(
#     df = state_all_libs,
#     variable_key = variable_key,
#     year = current_year,
#     cols = all_cols_state(),
#     percap = input$percap.state
#   )
# })

##### Value Boxes #####

## City Access
output$pop_access_city <- renderUI({
  input$submitButton

  county_df_p <- isolate(county_map_df())

  x <- county_df_p %>%
    reframe(
      pop_access_city = sum(pop_access_city, na.rm = T),
      pop_access_city = format(pop_access_city, big.mark = ",")
    ) %>%
    pull(pop_access_city)

  HTML(paste0("<b>City Libraries: </b>", x))
})

output$pop_access_city_undup <- renderUI({
  input$submitButton

  county_df_p <- isolate(county_map_df())

  x <- county_df_p %>%
    reframe(
      pop_access_city_undup = sum(pop_access_city_undup, na.rm = T),
      pop_access_city_undup = format(pop_access_city_undup, big.mark = ",")
    ) %>%
    pull(pop_access_city_undup)

  HTML(paste0("<b>City Libraries: </b>", x))
})

## County Access
output$pop_access_county <- renderUI({
  input$submitButton

  county_df_p <- isolate(county_map_df())

  x <- county_df_p %>%
    reframe(
      pop_access_county = sum(pop_access_county, na.rm = T),
      pop_access_county = format(pop_access_county, big.mark = ",")
    ) %>%
    pull(pop_access_county)

  HTML(paste0("<b>County Libraries: </b>", x))
})

output$pop_access_county_undup <- renderUI({
  input$submitButton

  county_df_p <- isolate(county_map_df())

  x <- county_df_p %>%
    reframe(
      pop_access_county_undup = sum(pop_access_county_undup, na.rm = T),
      pop_access_county_undup = format(pop_access_county_undup, big.mark = ",")
    ) %>%
    pull(pop_access_county_undup)

  HTML(paste0("<b>County Libraries: </b>", x))
})

## Bookmobile Access
output$pop_access_bookmobile <- renderUI({
  input$submitButton

  county_df_p <- isolate(county_map_df())

  x <- county_df_p %>%
    reframe(
      pop_access_bookmobile = sum(pop_access_bookmobile, na.rm = T),
      pop_access_bookmobile = format(pop_access_bookmobile, big.mark = ",")
    ) %>%
    pull(pop_access_bookmobile)

  HTML(paste0("<b>Bookmobiles: </b>", x))
})

output$pop_access_bookmobile_undup <- renderUI({
  input$submitButton

  county_df_p <- isolate(county_map_df())

  x <- county_df_p %>%
    reframe(
      pop_access_bookmobile_undup = sum(pop_access_bookmobile_undup, na.rm = T),
      pop_access_bookmobile_undup = format(
        pop_access_bookmobile_undup,
        big.mark = ","
      )
    ) %>%
    pull(pop_access_bookmobile_undup)

  HTML(paste0("<b>Bookmobiles: </b>", x))
})

## Agreed Service
output$pop_access_agreed <- renderUI({
  input$submitButton

  county_df_p <- isolate(county_map_df())

  x <- county_df_p %>%
    reframe(
      pop_access_agreed = sum(pop_access_agreed_city, na.rm = T) +
        #only one case for county, so we'll just sum the _undup column here
        sum(pop_access_agreed_county_undup, na.rm = T),
      pop_access_agreed = format(pop_access_agreed, big.mark = ",")
    ) %>%
    pull(pop_access_agreed)

  HTML(paste0("<b>", "Service Agreements: </b>", x))
})

output$pop_access_agreed_undup <- renderUI({
  input$submitButton

  county_df_p <- isolate(county_map_df())

  x <- county_df_p %>%
    reframe(
      pop_access_agreed_undup = sum(pop_access_agreed_city_undup, na.rm = T) +
        sum(pop_access_agreed_county_undup, na.rm = T),
      pop_access_agreed_undup = format(pop_access_agreed_undup, big.mark = ",")
    ) %>%
    pull(pop_access_agreed_undup)

  HTML(paste0("<b>", "Service Agreements: ", "</b>", x))
})


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
