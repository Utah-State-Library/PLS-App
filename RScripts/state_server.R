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

# #### Define Columns ####

table_columns_state <- list(
  "Overview" = c(
    "TOTINCM",
    "TOTSTAFF",
    "VLNT",
    "VLNT_HRS",
    "VISITS",
    "REGBOR",
    "TOTCIR",
    "TOTPHYS",
    "GPTERMS",
    "TOTPRO",
    "TOTATTEN"
  ),
  "Revenue" = c("TOTINCM", "LOCGVT", "STGVT", "FEDGVT", "OTHINCM"),
  "Total Expenditures" = c("TOTOPEXP", "STAFFEXP", "TOTEXPCO", "OTHOPEXP"),
  "Staff Expenditures" = c("TOTOPEXP", "STAFFEXP", "SALARIES", "BENEFIT"),
  "Collection Expenditures" = c(
    "TOTOPEXP",
    "TOTEXPCO",
    "PRMATEXP",
    "ELMATEXP",
    "OTHMATEX"
  ),
  "Circulation" = c(
    "TOTCIR",
    "PHYSCIR",
    "KIDCIRCL",
    "ELMATCIR",
    "HOTSPOT_CIRC",
    "OTHPHCIR",
    "EBOOK_CIR",
    "EAUDIO_CIR",
    "EVIDEO_CIR",
    "ESERIAL_CIR"
  ),
  "Collections" = c("TOTPHYS", "BKVOL", "AUDIO_PH", "VIDEO_PH", "OTHMATS"),
  "Number of Programs" = c(
    "TOTPRO",
    "K0_5PRO",
    "K6_11PRO",
    "YAPRO",
    "ADULTPRO",
    "GENPRO"
  ),
  "Program Attendance" = c(
    "TOTATTEN",
    "K0_5ATTEN",
    "K6_11ATTEN",
    "YAATTEN",
    "ADULTATTEN",
    "GENATTEN"
  ),
  "Visits, Borrowers, Reference, and ILL" = c(
    "VISITS",
    "REFERENC",
    "REGBOR",
    "LOANTO",
    "LOANFM"
  ),
  "Internet Access" = c(
    "GPTERMS",
    "PITUSR",
    "WIFISESS",
    "HOTSPOT",
    "HOTSPOT_CIRC"
  )
)

all_cols_state <- reactive({
  req(input$table_selection_state)

  table_columns_state[[input$table_selection_state]]
})

state_df <- reactive({
  state_all_libs %>% filter(FISCAL_YEAR == input$st_year)
})


### Render Percent Change Tables ####
output$table_state <- renderReactable({
  req(state_all_libs)

  render_pct_change_table(
    df = state_all_libs,
    variable_key = variable_key,
    year = input$st_year,
    cols = all_cols_state(),
    percap = input$percap.state
  )
})

#### Render Comparison HC ####

output$hc_comparison_state <- renderHighchart({
  req(input$st_year)

  render_statewide_hc(
    df = pls,
    variable_key = variable_key,
    col = input$hc_col.state,
    per_cap = input$st_hc_percap,
    year = input$st_year,
    restrict_years = TRUE
  )
})

##### Value Boxes #####

## Render Library Header
output$library_header.state <- renderUI({
  req(input$st_year)

  HTML(paste0(
    "<h1>",
    input$year.single,
    " Certified Utah Libraries at a Glance",
    "</h1>"
  ))
})

###### Visits ######
output$m_visitsTitle <- renderUI({
  title <- paste0("Visits")
})
output$m_visitsCY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "VISITS", pull = "CY")
})
output$m_visitsPY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "VISITS", pull = "PY")
})
output$m_visitschange <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "VISITS", pull = "change")
})


###### Registered Borrowers ######
output$m_regborTitle <- renderUI({
  title <- paste0("Registered Borrowers")
})
output$m_regborCY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "REGBOR", pull = "CY")
})
output$m_regborPY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "REGBOR", pull = "PY")
})
output$m_regborchange <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "REGBOR", pull = "change")
})


###### LSA ######
output$m_popu_lsaTitle <- renderUI({
  title <- paste0("Population of Legal Service Area")
})
output$m_popu_lsaCY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "POPU_LSA", pull = "CY")
})
output$m_popu_lsaPY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "POPU_LSA", pull = "PY")
})
output$m_popu_lsachange <- renderUI({
  get_valuebox(
    state_all_libs,
    year = input$st_year,
    "POPU_LSA",
    pull = "change"
  )
})

###### Circulation ######
output$m_totcirTitle <- renderUI({
  title <- paste0("Total Circulation")
})
output$m_totcirCY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "TOTCIR", pull = "CY")
})
output$m_totcirPY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "TOTCIR", pull = "PY")
})
output$m_totcirchange <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "TOTCIR", pull = "change")
})

###### Revenue ######
output$m_totincmTitle <- renderUI({
  title <- paste0("Revenue")
})
output$m_totincmCY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "TOTINCM", pull = "CY")
})
output$m_totincmPY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "TOTINCM", pull = "PY")
})
output$m_totincmchange <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "TOTINCM", pull = "change")
})

###### FTE ######
output$m_fteTitle <- renderUI({
  title <- paste0("FTE")
})
output$m_fteCY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "TOTSTAFF", pull = "CY")
})
output$m_ftePY <- renderUI({
  get_valuebox(state_all_libs, year = input$st_year, "TOTSTAFF", pull = "PY")
})
output$m_ftechange <- renderUI({
  get_valuebox(
    state_all_libs,
    year = input$st_year,
    "TOTSTAFF",
    pull = "change"
  )
})
