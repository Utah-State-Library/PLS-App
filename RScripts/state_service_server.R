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

service_areas_reactive <- eventReactive(
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
  service_areas_p <- isolate(service_areas_reactive())

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
  service_areas_p <- isolate(service_areas_reactive())

  municipality_p %<>%
    st_drop_geometry()

  yes_county <- data.frame()
  yes_city <- data.frame()
  yes_bookmobile <- data.frame()
  yes_agreed <- data.frame()
  no_county <- data.frame()
  no_service <- data.frame()
  noncertified <- data.frame()

  if ("County Library Service" %in% service_areas_p) {
    yes_county <- municipality_p %>%
      filter(`Library_1 Type` == "County" | `Library_2 Type` == "County")
  }
  if ("City Library Service" %in% service_areas_p) {
    yes_city <- municipality_p %>%
      filter(`Library_1 Type` == "City" | `Library_2 Type` == "City")
  }
  if ("Bookmobile Library Service" %in% service_areas_p) {
    yes_bookmobile <- municipality_p %>%
      filter(
        `Library_1 Type` == "Bookmobile" | `Library_2 Type` == "Bookmobile"
      )
  }
  if ("Agreed Service Through a City Library" %in% service_areas_p) {
    yes_agreed <- municipality_p %>%
      filter(
        str_detect(`Library_1 Type`, "Agreed") |
          str_detect(`Library_2 Type`, "Agreed")
      )
  }
  if (
    "No Library Service" %in%
      service_areas_p
  ) {
    no_service <- municipality_p %>%
      filter(`Library_1 Type` == "No Library Service")
  }
  if (
    "No County Library Service" %in%
      service_areas_p &
      !"County Library Service" %in% service_areas_p
  ) {
    no_county <- municipality_p %>%
      filter(`Library_1 Type` != "County", `Library_2 Type` != "County")
  }
  if (
    "Non-Certified Library Service" %in%
      service_areas_p
  ) {
    noncertified <- municipality_p %>%
      filter(
        `Library_1 Type` == "Non-Certified" |
          `Library_2 Type` == "Non-Certified"
      )
  }

  all <- rbind(
    yes_city,
    yes_county,
    yes_bookmobile,
    yes_agreed,
    no_service,
    no_county,
    noncertified
  ) %>%
    distinct()

  all %<>%
    select(
      county = CNTY,
      county_fips = COUNTY,
      municipality = NAME,
      city_fips = CITY_FIPS,
      population = POPESTIMATE,
      Library_1,
      `Library_1 Type`,
      Library_2,
      `Library_2 Type`
    ) %>%
    mutate(population = format(population, big.mark = ",")) %>%
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
        Library_1 = colDef(name = "Library 1"),
        `Library_1 Type` = colDef(name = "Library 1 Type"),
        Library_2 = colDef(name = "Library 2"),
        `Library_2 Type` = colDef(name = "Library 2 Type")
      )
    )
})


output$city_map <- renderLeaflet({
  input$submitButton

  municipalities_df_p <- isolate(municipalities_map_df())

  ## City Views
  city_libs_map <- municipalities_df_p %>%
    filter(`Library_1 Type` == "City")
  city_libs_county_service_map <- municipalities_df_p %>%
    filter(`Library_1 Type` == "County")
  agreed_service_city_map <- municipalities_df_p %>%
    filter(`Library_1 Type` == "Agreed Service")
  bookmobile_service_city_map <- municipalities_df_p %>%
    filter(`Library_1 Type` == "Bookmobile")
  noncertified_service_city_map <- municipalities_df_p %>%
    filter(`Library_1 Type` %in% c("Non-Certified", "Non-Certified County"))
  no_service_map <- municipalities_df_p %>%
    filter(`Library_1 Type` == "No Library Service")

  ## Set base map
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    addMapPane("county_pane", zIndex = 420) %>%
    addMapPane("city_pane", zIndex = 430) %>% # Cities will be on top
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    #setMaxBounds(lng1 = -109, lat1 = 37, lng2 = -114, lat2 = 42) %>%
    onRender(
      "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }"
    )

  if (nrow(city_libs_map) > 0) {
    map <- map %>%
      addPolygons(
        data = city_libs_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#002F6C",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#000000",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }
  if (nrow(city_libs_county_service_map) > 0) {
    map <- map %>%
      addPolygons(
        data = city_libs_county_service_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#4EC3E0",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#000000",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }
  if (nrow(agreed_service_city_map) > 0) {
    map <- map %>%
      addPolygons(
        data = agreed_service_city_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#f632f3ff",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#000000",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }
  if (nrow(bookmobile_service_city_map) > 0) {
    map <- map %>%
      addPolygons(
        data = bookmobile_service_city_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#08f476ff",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#000000",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }
  if (nrow(noncertified_service_city_map) > 0) {
    map <- map %>%
      addPolygons(
        data = noncertified_service_city_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#ffbd31",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#000000",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }
  if (nrow(no_service_map) > 0) {
    map <- map %>%
      addPolygons(
        data = no_service_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#808080",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#000000",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }

  map
})
