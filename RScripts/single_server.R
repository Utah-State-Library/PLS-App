#### Define Columns ####

table_columns_single <- list(
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
all_cols_single <- reactive({
  
  req(input$table_selection_single)
  
  table_columns_single[[input$table_selection_single]]
})



#### Reactive ####

single_lib_df <- reactive({
  
  req(input$library.single)
  
  pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
})

selected_year <- reactive({
  
  req(input$year.single)
  
  input$year.single
})

output$csv_button.single <- renderUI({
  req(input$table_selection_single)
  
  cleaned_libname <- input$library.single %>%
    tolower() %>%
    gsub("[\\s]+", "_", .) %>%
    gsub(",", "", .)
  
  cleaned_name <- input$table_selection_single %>%
    tolower() %>%
    gsub("[\\s]+", "_", .) %>%
    gsub(",", "", .)
  
  filename <- paste0(cleaned_name, "_",cleaned_libname,"_table.csv")
  
  csvDownloadButton("table_single", filename = filename)
})


## Render Library Header
output$library_header.single <- renderUI({
  req(input$library.single)
  req(input$year.single)
  
  HTML(paste0("<h1>", input$year.single, " Library at a Glance", " - ", input$library.single, "</h1>"))
  
})


# ## Render Conditional Inputs
# output$conditional_inputs.single <- renderUI({
#   req(input$active_tab_single)
# 
#   if (input$active_tab_single == "Percent Change Tables") {
#     tagList(
#       pickerInput("table_selection_single",
#                   "Select a Table",
#                   choices = list(
#                     "Overview",
#                     "Revenue and Expenditures" = c("Revenue", "Total Expenditures", "Staff Expenditures", "Collection Expenditures"),
#                     "Resources and Services" = c("Circulation", "Collections", "Visits, Borrowers, Reference, and ILL", "Internet Access"),
#                     "Programs" = c("Number of Programs", "Program Attendance")
#                   ),
#                   selected = "Overview",
#                   multiple = FALSE),
#       checkboxInput("percap.single",
#                     "Show Values Per Capita?",
#                     value = FALSE),
#       hr()
#     )
#   } else if (input$active_tab_single == "Comparison Chart") {
#     tagList(
#       pickerInput("hc_col.single",
#                   "Select a Value to Graph",
#                   choices = c("Visits" = "VISITS",
#                               "Revenue" = "TOTINCM",
#                               "FTE" = "TOTSTAFF",
#                               "Card Holders" = "REGBOR",
#                               "Number of Programs" = "TOTPRO",
#                               "Program Attendance" = "TOTATTEN",
#                               "Total Collection" = "TOTPHYS",
#                               "Total Circulation" = "TOTCIR")),
#       checkboxInput("under_50k.single",
#                     "Include only libraries serving < 50,000?",
#                     value = FALSE),
#       hr()
#     )
#   }
# })


#### Render Percent Change Tables ####

output$table_single <- renderReactable({
  
  req(selected_year())
  req(single_lib_df())
  
  render_pct_change_table(df = single_lib_df(),
                          variable_key = variable_key,
                          year = selected_year(),
                          cols = all_cols_single(),
                          percap = input$percap.single)
})

#### Render Staff Workload Table ####

output$staffworkload_expl <- renderUI({
  
  n_staff <- single_lib_df() %>% filter(FISCAL_YEAR == input$year.single) %>% select(TOT_LIB_STAFF) %>% pull()
  n_FTE <- single_lib_df() %>% filter(FISCAL_YEAR == input$year.single) %>% select(TOTSTAFF) %>% pull()
  
  HTML(paste0("FTE represents a 40 hour work week and does not necessarily align with the number of employees at the library. Some staff may be full time or part time. In ", input$year.single, ", ", input$library.single, " had 
", n_staff," employees who split ", n_FTE, " FTE. <br>
NOTE: This table is a workload proxy - it does not show actual numbers for staff work.<br>"))
})

output$staffworkload_table <- renderReactable({
  
  req(input$library.single)
  req(input$year.single)
  
  df <- single_lib_df() %>% filter(FISCAL_YEAR == input$year.single)
  
  staff_handle <- df %>%
    select(
      TOTSTAFF,
      POPU_LSA,
      VISITS,
      REGBOR,
      TOTCIR,
      REFERENC,
      TOTPRO,
      TOTATTEN
    ) %>%
    pivot_longer(
      !TOTSTAFF,
      names_to = "Metric",
      values_to = "Values"
    ) %>%
    mutate(
      TOTSTAFF = as.numeric(TOTSTAFF),
      Values = as.numeric(Values),
      handle = round(Values / TOTSTAFF, 2),
      Metric = case_when(
        Metric == "POPU_LSA" ~ "Population per 1 FTE",
        Metric == "VISITS" ~ "Visits per 1 FTE",
        Metric == "TOTPRO" ~ "Programs per 1 FTE",
        Metric == "TOTCIR" ~ "Circulation per 1 FTE",
        Metric == "REFERENC" ~ "Reference Transactions per 1 FTE",
        Metric == "REGBOR" ~ "Card Holders per 1 FTE",
        Metric == "TOTATTEN" ~ "Program Attendance per 1 FTE"
      )
    )
  
  staff_handle %>%
    select(Metric, handle) %>%
    reactable(
      resizable = T,
      defaultExpanded = F,
      compact = T,
      striped = T,
      theme = reactableTheme(
        backgroundColor = "transparent",
        headerStyle = list(borderColor = "#555")
      ),
      columns = list(
        Metric = colDef(name = ""),
        handle = colDef(
          name = "",
          #format = colFormat(separators = TRUE, digits = 2),
          cell = function(value, index) {
            if (is.na(value)) {
              "No Data"
            } else if (is.infinite(value)) {
              ""
            } else {
              format(value, big.mark = ",")
            }
          }
        )
      )
    )
})

#### Render Staff HC ####

output$hc_staff_single <- renderHighchart({
  
  req(input$library.single)
  req(input$year.single)
  
  render_comparison_hc(df = pls, 
                       target_lib = input$library.single, 
                       variable_key = variable_key, 
                       col = "TOTSTAFF", 
                       under_50k = FALSE,#input$under_50k.single, # filter to only libraries <= 50,000
                       year = input$year.single, # temporary?
                       restrict_years = TRUE)
})




#### Render Comparison HC ####

output$hc_comparison_single <- renderHighchart({
  
  req(input$library.single)
  req(input$hc_col.single)
  req(input$year.single)
  
  render_comparison_hc(df = pls, 
                       target_lib = input$library.single, 
                       variable_key = variable_key, 
                       col = input$hc_col.single, 
                       under_50k = input$under_50k.single, # filter to only libraries <= 50,000
                       year = input$year.single, # temporary?
                       restrict_years = TRUE)
})





#### Value Boxes ####

###### Revenue ######
output$totincmTitle <- renderUI({
  title <- paste0("Revenue")
})
output$totincmCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTINCM", pull = "CY")
})
output$totincmPY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTINCM", pull = "PY")
})
output$totincmchange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTINCM", pull = "change")
})

###### Item Circulation ######
output$totcirTitle <- renderUI({
  title <- paste0("Total Circulation")
})
output$totcirCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTCIR", pull = "CY")
})
output$totcirPY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTCIR", pull = "PY")
})
output$totcirchange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTCIR", pull = "change")
})

###### Registered Borrowers ######
output$regborTitle <- renderUI({
  title <- paste0("Registered Borrowers")
})
output$regborCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "REGBOR", pull = "CY")
})
output$regborPY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "REGBOR", pull = "PY")
})
output$regborchange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "REGBOR", pull = "change")
})

###### FTE ######
output$fteTitle <- renderUI({
  title <- paste0("FTE")
})
output$fteCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTSTAFF", pull = "CY")
})
output$ftePY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTSTAFF", pull = "PY")
})
output$ftechange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "TOTSTAFF", pull = "change")
})

###### Visits ######
output$visitsTitle <- renderUI({
  title <- paste0("Visits")
})
output$visitsCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "VISITS", pull = "CY")
})
output$visitsPY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "VISITS", pull = "PY")
})
output$visitschange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "VISITS", pull = "change")
})

###### LSA ######
output$popu_lsaTitle <- renderUI({
  title <- paste0("Population of Legal Service Area")
})
output$popu_lsaCY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "POPU_LSA", pull = "CY")
})
output$popu_lsaPY <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "POPU_LSA", pull = "PY")
})
output$popu_lsachange <- renderUI({
  
  df <- pls %>% 
    filter(CURRENT_LIBNAME == input$library.single)
  
  get_valuebox(df, year = input$year.single, "POPU_LSA", pull = "change")
})
