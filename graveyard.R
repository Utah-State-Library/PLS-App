
### Functions
# get_nclosest <- function(df, n, libname, pg_col, keep_cols){
#   # use pls as df
#   
#   df %<>% 
#     select(CURRENT_LIBNAME, FISCAL_YEAR, keep_cols) %>%
#     mutate(pg_col = as.numeric(.data[[pg_col]]))
#   
#   target_value <- df %>% 
#     filter(CURRENT_LIBNAME == libname, 
#            FISCAL_YEAR == current_year,
#     ) %>% 
#     pull(pg_col)
#   
#   df_diff <- df %>% 
#     filter(FISCAL_YEAR == current_year) %>%
#     rowwise() %>%
#     mutate(difference = abs(pg_col - target_value)) %>%
#     arrange(difference)
#   
#   # Select the top 'n' rows + 1 for the target library
#   closest_values <- head(df_diff, n + 1)
#   
#   closest_libs <- pls %>% 
#     filter(FISCAL_YEAR %in% c(current_year, current_year - 1), 
#            CURRENT_LIBNAME %in% closest_values$CURRENT_LIBNAME) %>% 
#     select(CURRENT_LIBNAME, FISCAL_YEAR,keep_cols) %>%
#     mutate(pg_col = as.numeric(.data[[pg_col]]))
#   
#   closest_libs  %<>%
#     mutate(YEAR_INDIC = ifelse(FISCAL_YEAR == current_year, "CY", "PY")) %>%
#     mutate(across(c(3:pg_col), ~ as.numeric(.))) %>%
#     select(-FISCAL_YEAR) %>%
#     pivot_wider(names_from = "YEAR_INDIC",
#                 names_glue = "{.value}_{YEAR_INDIC}",
#                 values_from = c("pg_col", keep_cols)
#                 ) %>%
#     arrange(desc(pg_col_CY))
#   
#   closest_libs
# }


### PG Server
# df_pg <- reactive({
#   
#   table_cols <- c("POPU_LSA", "TOTINCM", "TOTSTAFF", "TOTOPEXP", "VISITS", "REGBOR")
#   names(table_cols) <- c("Population of Legal Service Area", "Revenue", "FTE", "Expenditures", "Visits", "Registered Borrowers")
#   
#   get_nclosest(pls, 
#                n = 10,
#                libname = input$libname,
#                pg_col = input$peergroup,
#                keep_cols = table_cols) # defined above
# })

### PG Server - Reactable data prep
# df_pg() %>% 
#   arrange(pg_col_CY) %>%
#   summarise(Library = CURRENT_LIBNAME,
#             
#             ## Previous Year
#             #`Population of Legal Service Area PY` = format(POPU_LSA_PY, big.mark = ","),
#             #`Revenue PY` = TOTINCM_PY,
#             #`FTE PY` = round(TOTSTAFF_PY, 2),
#             #`Expenditures PY` = TOTOPEXP_PY,
#             #`Visits PY` = format(VISITS_PY, big.mark = ","),
#             #`Registered Borrowers PY` = format(REGBOR_PY, big.mark = ","),
#             #`Registered Borrowers Per Capita` = format(TOTINCM_CY/POPU_LSA_CY, big.mark = ",")#,
#             #`Change from PY` = paste0(round(((plot_col_CY - plot_col_PY)/plot_col_PY)*100, 2), "%")
#             
#             ## Current Year
#             `Population of Legal Service Area` = format(POPU_LSA_CY, big.mark = ","),
#             `Revenue` = TOTINCM_CY,
#             `Revenue Per Capita` = round(TOTINCM_CY/POPU_LSA_CY, 2),
#             `FTE` = round(TOTSTAFF_CY, 2),
#             `FTE Per Capita` = round(TOTSTAFF_CY/POPU_LSA_CY, 6),
#             `Expenditures` = TOTOPEXP_CY,
#             `Expenditures Per Capita` = round(TOTOPEXP_CY/POPU_LSA_CY, 2),
#             `Visits` = format(VISITS_CY, big.mark = ","),
#             `Visits Per Capita` = format(round(VISITS_CY/POPU_LSA_CY, 2), big.mark = ","),
#             `Registered Borrowers` = format(REGBOR_CY, big.mark = ",")
#             ) %>%



### PG Server - Dumbbells
# target_name <- reactive({
#   input$libname
# })

# 
# output$dumbbell_hc <- renderHighchart({
#   
#   highlight_lib <- target_name() 
#   df <- df_pg()
#   
#   # df  %<>%
#   # mutate(YEAR_INDIC = ifelse(FISCAL_YEAR == current_year, "CY", "PY")) %>%
#   # select(CURRENT_LIBNAME, YEAR_INDIC, pg_col, plot_col) %>%
#   # pivot_wider(names_from = "YEAR_INDIC",
#   #             names_glue = "{.value}_{YEAR_INDIC}",
#   #             values_from = c("pg_col","plot_col")) %>%
#   # mutate(across(c(plot_col_PY, plot_col_CY), ~ as.numeric(.))) %>%
#   # arrange(desc(plot_col_CY))
#   
#   formatter_code <- sprintf(
#     "function () {
#      if (this.value === '%s') {
#        return '<b>' + this.value + '</b>';
#      } else {
#        return this.value;
#      }
#    }", highlight_lib
#   )
#  
#   hchart(df,
#          type = "dumbbell",
#          hcaes(x = CURRENT_LIBNAME,
#                low = plot_col_PY,
#                high = plot_col_CY)) %>%
#     hc_tooltip(pointFormat = paste0("<b>{point.CURRENT_LIBNAME}</b> <br> <b>Metric: </b>",input$plotmetric,"<br> <b>Last Year:</b> {point.plot_col_PY}<br><b>This Year:</b> {point.plot_col_CY}"),
#                headerFormat = "") %>%
#     hc_xAxis(title = "",
#       labels = list(
#         formatter = JS(formatter_code)
#       )
#     ) %>%
#     hc_yAxis(title = list(text = input$plotmetric)) %>%
#     hc_chart(
#       inverted = TRUE
#     )
# })

### Single Library Server - Modals
# 
# ##### Initial Modal #####
# observeEvent(session, {
#   shinyalert(
#     html = TRUE,  # Crucial to render Shiny UI elements like selectInput
#     text = tagList(
#       pickerInput(
#         "libname",
#         label = "",
#         choices = libnames,
#         selected = NULL,
#         multiple = TRUE,
#         options = pickerOptions(maxOptions = 1)
#       )
#     ),
#     title = "Select a Library",
#     confirmButtonText = "Go",
#     closeOnEsc = FALSE, # Prevent closing without selection
#     closeOnClickOutside = FALSE # Prevent closing without selection
#   )
# }, once = TRUE) # Ensures this observer only runs once at startup
# 
# ##### Library Select Modal #####
# observeEvent(input$btn, {
#   shinyalert(
#     html = TRUE,  # Crucial to render Shiny UI elements like selectInput
#     text = tagList(
#       pickerInput(
#         "libname",
#         label = "",
#         choices = libnames,
#         selected = NULL,
#         multiple = TRUE,
#         options = pickerOptions(maxOptions = 1)
#       )
#     ),
#     title = "Select a Library",
#     confirmButtonText = "Go",
#     closeOnEsc = FALSE, # Prevent closing without selection
#     closeOnClickOutside = FALSE # Prevent closing without selection
#   )
# })


### State Page

# pickerInput(
#   "st_city",
#   "Select Libraries by City",
#   choices = cities,
#   selected = cities,
#   multiple = TRUE,
#   options = list(`live-search`=TRUE,
#                  `actions-box` = TRUE,
#                  `selected-text-format` = paste0("count > ", length(cities) -1),
#                  `count-selected-text` = "All Cities"
#   )
# ),

# observe({
#   
#   counties <- toupper(input$st_county)
#   
#   cities <- librarykey %>% 
#     filter(COUNTY %in% input$st_county) %>%
#     summarise(CITY) %>% distinct() %>% pull() %>% sort()
#   
#   updatePickerInput(session, 
#                     "st_city",
#                     "Select Libraries by City",
#                     choices = cities,
#                     selected = cities,
#                     options = list(`live-search`=TRUE,
#                                    `actions-box` = TRUE,
#                                    `selected-text-format` = paste0("count > ", length(cities) -1), `count-selected-text` = "All Cities"))
# })