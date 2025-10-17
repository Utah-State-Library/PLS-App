nav_menu("Data Tables",
         
         nav_panel(title = "All Libraries",
                   
                   #### Sidebar ####
                   
                   layout_sidebar(
                     sidebar = sidebar(
                       width = "25%",
                       
                       accordion(
                         id = "sidebar-accordion", open = "Data Selection",
                         accordion_panel(
                           title = "Data Selection",
                           
                           pickerInput("table_selection",
                                       "Select a Table",
                                       choices = list(
                                         "Overview",
                                         "Revenue and Expenditures" = c("Revenue", "Total Expenditures", "Staff Expenditures", "Collection Expenditures"), 
                                         "Resources and Services" = c("Circulation", "Collections", "Visits, Borrowers, Reference, and ILL", "Internet Access"),
                                         "Programs" = c("Number of Programs", "Program Attendance")
                                       ),
                                       selected = "Overview",
                                       multiple = FALSE),
                           pickerInput("library.table",
                                       "Library Name",
                                       choices = libnames,
                                       selected = libnames,
                                       multiple = TRUE,
                                       options = list(`live-search`=TRUE,
                                                      `actions-box` = TRUE,
                                                      `selected-text-format` = paste0("count > ", length(libnames) -1),
                                                      `count-selected-text` = "All Libraries"
                                       )
                           ),
                           pickerInput("year.table",
                                       "Fiscal Year",
                                       choices = years,
                                       selected = max(years),
                                       multiple = TRUE,
                                       options = list(`live-search`=TRUE,
                                                      `actions-box` = TRUE,
                                                      `selected-text-format` = paste0("count > ", length(years) -1),
                                                      `count-selected-text` = "All Years"
                                       )
                           )
                           
                         ),
                         accordion_panel(
                           title = "Comparison Options",
                           checkboxInput("per_cap.table",
                                         "Show Values Per Capita?",
                                         value = FALSE),
                           checkboxInput("color_table",
                                         "Color Table by High/Low Values?",
                                         value = FALSE)
                         )
                         
                       ),
                       ##### CSV Download Button #####
                       uiOutput("csv_button")
                       
                       
                     ),
                     
                     #### Main Body ####
                     
                     reactableOutput("table_all")
                     
                   )
         ),
         
         nav_panel(title = "Peer Libraries",
                   
                   #### Sidebar ####
                   
                   layout_sidebar(
                     sidebar = sidebar(
                       width = "25%",
                       
                       accordion(
                         id = "sidebar-accordion", open = "Data Selection",
                         accordion_panel(
                           title = "Data Selection",
                           pickerInput("table_selection_peer",
                                       "Select a Table",
                                       choices = list(
                                         "Overview",
                                         "Revenue and Expenditures" = c("Revenue", "Total Expenditures", "Staff Expenditures", "Collection Expenditures"), 
                                         "Resources and Services" = c("Circulation", "Collections", "Visits, Borrowers, Reference, and ILL", "Internet Access"),
                                         "Programs" = c("Number of Programs", "Program Attendance")
                                       ),
                                       selected = "Overview",
                                       multiple = FALSE),
                           pickerInput("library.table_peer",
                                       "Library Name",
                                       choices = libnames,
                                       selected = libnames[1],
                                       multiple = F
                           ),
                           pickerInput("year.table_peer",
                                       "Fiscal Year",
                                       choices = years,
                                       selected = max(years),
                                       multiple = F
                           ),
                           pickerInput(
                             "peergroup",
                             label = "Choose Closest 10 Libraries Based On: ",
                             choices = c("Population of Legal Service Area" = "POPU_LSA", 
                                         "Total Operating Revenue" = "TOTINCM", 
                                         "Total FTE of Paid Staff" = "TOTSTAFF"),
                             selected = "Population of Legal Service Area",
                             multiple = FALSE
                           )
                         ),
                         
                         accordion_panel(
                           title = "Comparison Options",
                           checkboxInput("per_cap.table_peer",
                                         "Show Values Per Capita?",
                                         value = FALSE),
                           checkboxInput("color_table_peer",
                                         "Color Table by High/Low Values?",
                                         value = FALSE)
                         )
                       ),
                       # ##### CSV Download Button #####
                       
                       uiOutput("csv_button_peer")
                     ),
                     
                     #### Main Body ####
                     uiOutput("header_peer"),
                     reactableOutput("table_peer")
                     
                   )
         )
)
