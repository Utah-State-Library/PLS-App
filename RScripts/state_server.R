##### Update Pickers

observe({
  counties <- toupper(input$st_county)

  ae_name <- outlets %>%
    filter(CNTY %in% input$st_county, FSCSKEY %in% current_FSCS) %>%
    summarise(CURRENT_LIBNAME_AE) %>%
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
    show_service = input$show_service,
    show_no_service = input$show_no_service
  )
})


# ###### Library Map ######
# output$state_map <- renderLeaflet({
#   ## Add certified vs emerging vs other

#   input$submitButton

#   map_df <- isolate(map_libs_filtered())

#   leaflet_data <- outlets %>%
#     filter(FISCAL_YEAR == current_year) ######### TODO

#   county_libs <- leaflet_data %>%
#     filter(str_detect(CURRENT_LIBNAME_AE, "County"))

#   city_libs <- leaflet_data %>%
#     filter(!str_detect(CURRENT_LIBNAME_AE, "County"))

#   counties_w_service <- county_shp %>%
#     filter(NAME %in% county_libs$CNTY) ## need to filter specifically to county libraries

#   municipalities_w_service <- municipalities %>%
#     filter(
#       NAME %in% city_libs$CITY
#     )

#   leaflet_data %<>%
#     mutate(
#       LAT = as.numeric(LAT),
#       LONG = as.numeric(LONG),
#       library_info = paste0(
#         "<table>
#                         <div style='font-size: 18px;'><b>",
#         CURRENT_LIBNAME_AE,
#         "</div>
#                         <div style='font-size: 12px;'>",
#         CURRENT_LIBNAME_OUTLET,
#         "</div>
#                         <div style='font-size: 12px;'>",
#         str_to_title(ADDRESS),
#         ", ",
#         str_to_title(CITY),
#         ", ",
#         ZIP,
#         "</div>
#         </table>"
#       )
#     )

#   if (nrow(leaflet_data > 0)) {
#     leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
#       addTiles() %>%
#       addMarkers(
#         data = leaflet_data,
#         lng = ~LONG,
#         lat = ~LAT,
#         label = ~CURRENT_LIBNAME_OUTLET,
#         popup = ~ lapply(library_info, HTML),
#         popupOptions = popupOptions(keepInView = TRUE),
#       ) %>%
#       addProviderTiles(
#         "CartoDB.Positron",
#         group = "CartoDB.Positron"
#       ) %>%
#       setMaxBounds(lng1 = -109, lat1 = 37, lng2 = -114, lat2 = 42) %>%
#       onRender(
#         "function(el, x) {
#           L.control.zoom({position:'bottomright'}).addTo(this);
#         }"
#       ) %>%
#       addPolygons(
#         data = counties_w_service,
#         label = ~NAME,
#         weight = 1,
#         opacity = 1,
#         color = "#4EC3E0",
#         fillOpacity = 0.7,
#         highlightOptions = highlightOptions(
#           weight = 3,
#           color = "#002F6C",
#           fillOpacity = 0.7
#         )
#       ) %>%
#       addPolygons(
#         data = municipalities_w_service,
#         label = ~NAME,
#         weight = 1,
#         opacity = 1,
#         color = "#002F6C",
#         fillOpacity = 0.7,
#         highlightOptions = highlightOptions(
#           weight = 3,
#           color = "#4EC3E0",
#           fillOpacity = 0.7
#         )
#       )
#   } else {
#     leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
#       addTiles() %>%
#       addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
#       setMaxBounds(lng1 = -109, lat1 = 37, lng2 = -114, lat2 = 42) %>%
#       onRender(
#         "function(el, x) {
#           L.control.zoom({position:'bottomright'}).addTo(this);
#         }"
#       )
#   }
# })

# ###### Library Map - No Library Service ######
# output$state_map_nolib <- renderLeaflet({
#   ## Add certified vs emerging vs other

#   FSCS <- current_FSCS

#   leaflet_data <- outlets %>%
#     filter(FSCSKEY %in% FSCS)

#   county_libs <- leaflet_data %>%
#     filter(str_detect(CURRENT_LIBNAME_AE, "County"))

#   city_libs <- leaflet_data %>%
#     filter(!str_detect(CURRENT_LIBNAME_AE, "County"))

#   bookmobile_counties <- c(
#     "Utah",
#     "Iron",
#     "Garfield", # Multicounty
#     "Kane", # Multicounty
#     "Sevier", # Tricounty
#     "Piute", # Tricounty
#     "Wayne", # Tricounty
#     "Beaver" # shared by the 3 cities
#   )
#   other_service_counties <- c(
#     "Beaver" # shared by the 3 cities
#   )

#   municipalities_w_agreed_service <- c(
#     "Nibley", # Hyrum City
#     "Wellsville", # Hyrum City
#     "East Carbon", # Helper
#     "Chester", # Ephraim
#     "Aurora", # Salina
#     "Redmont" # Salina
#   )

#   agreed_service_municipalities <- municipalities %>%
#     filter(NAME %in% municipalities_w_agreed_service)

#   bookmobile_counties2 <- county_shp %>%
#     filter(NAME %in% bookmobile_counties)

#   counties_wo_service <- county_shp %>%
#     filter(
#       !NAME %in% county_libs$CNTY,
#       !NAME %in% other_service_counties,
#       !NAME %in% bookmobile_counties
#     )
#   counties_w_service <- county_shp %>%
#     filter(NAME %in% county_libs$CNTY)

#   municipalities_w_service <- municipalities %>%
#     filter(
#       NAME %in% city_libs$CITY
#     )
#   municipalities_wo_service <- municipalities %>%
#     filter(
#       !NAME %in% city_libs$CITY,
#       !NAME %in% municipalities_w_agreed_service,
#       !COUNTYNBR %in% counties_w_service$COUNTYNBR,
#     )

#   leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
#     addTiles() %>%
#     addProviderTiles(
#       "CartoDB.Positron",
#       group = "CartoDB.Positron"
#     ) %>%
#     setMaxBounds(lng1 = -109, lat1 = 37, lng2 = -114, lat2 = 42) %>%
#     onRender(
#       "function(el, x) {
#           L.control.zoom({position:'bottomright'}).addTo(this);
#         }"
#     ) %>%
#     addPolygons(
#       data = counties_wo_service,
#       label = ~NAME,
#       weight = 1,
#       opacity = 1,
#       color = "#f17f33ff",
#       fillOpacity = 0.7,
#       highlightOptions = highlightOptions(
#         weight = 3,
#         color = "#f2322bff",
#         fillOpacity = 0.7
#       )
#     ) %>%
#     addPolygons(
#       data = municipalities_wo_service,
#       label = ~NAME,
#       weight = 1,
#       opacity = 1,
#       color = "#f2322bff",
#       fillOpacity = 0.7,
#       highlightOptions = highlightOptions(
#         weight = 3,
#         color = "#f17f33ff",
#         fillOpacity = 0.7
#       )
#     )
# })

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
