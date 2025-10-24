nav_menu(
  title = div(
    class = "d-flex align-items-center gap-2",
    bsicons::bs_icon("table", class = "icon-color fs-4"),
    tags$h5(class = "fw-bold mb-0", "Data Tables")
  ),
  nav_panel(
    title = "All Libraries",
    class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
    style = "width: 95vw; height: 92vh; padding: 0; margin: 1;",
    #### Sidebar ####

    layout_sidebar(
      sidebar = sidebar(
        width = "25%",

        accordion(
          id = "sidebar-accordion",
          open = "Data Selection",
          accordion_panel(
            title = "Data Selection",
            icon = bsicons::bs_icon("filter-square-fill", class = "icon-color"),
            #class = "my-bg-secondary",
            div(
              class = "mb-2",
              tags$h5(class = "mb-1 mt-0", "Select a Table"),
              pickerInput(
                "table_selection",
                label = NULL,
                choices = list(
                  "Overview",
                  "Revenue and Expenditures" = c(
                    "Revenue",
                    "Total Expenditures",
                    "Staff Expenditures",
                    "Collection Expenditures"
                  ),
                  "Resources and Services" = c(
                    "Circulation",
                    "Collections",
                    "Visits, Borrowers, Reference, and ILL",
                    "Internet Access"
                  ),
                  "Programs" = c("Number of Programs", "Program Attendance")
                ),
                selected = "Overview",
                multiple = FALSE
              )
            ),
            div(
              class = "mb-2",
              tags$h5(class = "mb-1 mt-0", "Select Libraries"),
              pickerInput(
                "library.table",
                label = NULL,
                choices = libnames,
                selected = libnames,
                multiple = TRUE,
                options = list(
                  `live-search` = TRUE,
                  `actions-box` = TRUE,
                  `selected-text-format` = paste0(
                    "count > ",
                    length(libnames) - 1
                  ),
                  `count-selected-text` = "All Libraries"
                )
              )
            ),
            div(
              class = "mb-2",
              tags$h5(class = "mb-1 mt-0", "Fiscal year"),
              pickerInput(
                "year.table",
                label = NULL,
                choices = years,
                selected = max(years),
                multiple = TRUE,
                options = list(
                  `live-search` = TRUE,
                  `actions-box` = TRUE,
                  `selected-text-format` = paste0(
                    "count > ",
                    length(years) - 1
                  ),
                  `count-selected-text` = "All Years"
                )
              )
            )
          ),
          accordion_panel(
            title = "Comparison Options",
            icon = bsicons::bs_icon("toggles", class = "icon-color"),
            #class = "my-bg-secondary",
            checkboxInput(
              "per_cap.table",
              "Show Values Per Capita?",
              value = FALSE
            ),
            checkboxInput(
              "color_table",
              "Color Table by High/Low Values?",
              value = FALSE
            )
          )
        ),
        ##### CSV Download Button #####
        uiOutput("csv_button")
      ),
      #### Main Body ####
      card(
        reactableOutput("table_all")
      )
    )
  ),

  nav_panel(
    title = "Peer Libraries",
    class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
    style = "width: 95vw; height: 92vh; padding: 0; margin: 1;",
    #### Sidebar ####

    layout_sidebar(
      sidebar = sidebar(
        width = "25%",

        accordion(
          id = "sidebar-accordion",
          open = "Data Selection",
          accordion_panel(
            title = "Data Selection",
            icon = bsicons::bs_icon("filter-square-fill", class = "icon-color"),
            #class = "my-bg-secondary",
            div(
              class = "mb-2",
              tags$h5(class = "mb-1 mt-0", "Select a Table"),
              pickerInput(
                "table_selection_peer",
                label = NULL,
                choices = list(
                  "Overview",
                  "Revenue and Expenditures" = c(
                    "Revenue",
                    "Total Expenditures",
                    "Staff Expenditures",
                    "Collection Expenditures"
                  ),
                  "Resources and Services" = c(
                    "Circulation",
                    "Collections",
                    "Visits, Borrowers, Reference, and ILL",
                    "Internet Access"
                  ),
                  "Programs" = c("Number of Programs", "Program Attendance")
                ),
                selected = "Overview",
                multiple = FALSE
              )
            ),
            div(
              class = "mb-2",
              tags$h5(class = "mb-1 mt-0", "Select Libraries"),
              pickerInput(
                "library.table_peer",
                label = NULL,
                choices = libnames,
                selected = libnames[1],
                multiple = F
              )
            ),
            div(
              class = "mb-2",
              tags$h5(class = "mb-1 mt-0", "Fiscal Year"),
              pickerInput(
                "year.table_peer",
                label = NULL,
                choices = years,
                selected = max(years),
                multiple = F
              ),
              div(
                class = "mb-2",
                tags$h5(
                  class = "mb-1 mt-0",
                  "Choose Closest 10 Libraries Based On:"
                ),
                pickerInput(
                  "peergroup",
                  label = NULL,
                  choices = c(
                    "Population of Legal Service Area" = "POPU_LSA",
                    "Total Operating Revenue" = "TOTINCM",
                    "Total FTE of Paid Staff" = "TOTSTAFF"
                  ),
                  selected = "Population of Legal Service Area",
                  multiple = FALSE
                )
              )
            )
          ),

          accordion_panel(
            title = "Comparison Options",
            icon = bsicons::bs_icon("toggles", class = "icon-color"),
            #class = "my-bg-secondary",
            checkboxInput(
              "per_cap.table_peer",
              "Show Values Per Capita?",
              value = FALSE
            ),
            checkboxInput(
              "color_table_peer",
              "Color Table by High/Low Values?",
              value = FALSE
            )
          )
        ),
        # ##### CSV Download Button #####

        uiOutput("csv_button_peer")
      ),
      #### Main Body ####
      card(
        uiOutput("header_peer"),
        reactableOutput("table_peer")
      )
    )
  )
)
