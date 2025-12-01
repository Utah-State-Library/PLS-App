nav_panel(
  title = tags$h5(class = "fw-bold", "Library Service Map"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  #style = "width: 95vw; height: 92vh; padding: 0; margin: 1;",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_sidebar(
    fill = TRUE,
    sidebar = sidebar(
      title = "Filters",
      width = "25%",

      div(
        class = "mb-2",
        tags$h5(class = "mb-1 mt-0", "Select Libraries by County"),
        pickerInput(
          "st_county",
          label = NULL,
          choices = counties,
          selected = counties,
          multiple = TRUE,
          options = list(
            `live-search` = TRUE,
            `actions-box` = TRUE,
            `selected-text-format` = paste0(
              "count > ",
              length(counties) - 1
            ),
            `count-selected-text` = "All Counties"
          )
        )
      ),
      # div(
      #   class = "mb-2",
      #   tags$h5(class = "mb-1 mt-0", "Select Libraries by System"),
      #   pickerInput(
      #     "st_ae",
      #     label = NULL,
      #     choices = ae_name,
      #     selected = ae_name,
      #     multiple = TRUE,
      #     options = list(
      #       `live-search` = TRUE,
      #       `actions-box` = TRUE,
      #       `selected-text-format` = paste0(
      #         "count > ",
      #         length(ae_name) - 1
      #       ),
      #       `count-selected-text` = "All Library Systems"
      #     )
      #   )
      # ),
      div(
        class = "mb-2",
        tags$h5(class = "mb-1 mt-0", "Map Controls"),
        checkboxInput(
          "show_libs",
          "Show Library Locations?",
          FALSE
        ),
        pickerInput(
          "service_areas",
          label = NULL,
          choices = c(
            "County Library Service",
            "City Library Service",
            "Agreed Service Through a City Library",
            "Bookmobile Library Service",
            "No County Library Service",
            "No City Library Service"
          ),
          selected = c(
            "County Library Service",
            "City Library Service",
            "Agreed Service Through a City Library",
            "Bookmobile Library Service",
            "No County Library Service",
            "No City Library Service"
          ),
          multiple = T,
          options = list(
            `live-search` = TRUE,
            `actions-box` = TRUE,
            `selected-text-format` = paste0(
              "count > ",
              length(6) - 1
            ),
            `count-selected-text` = "All Service Areas"
          )
        )
      ),
      actionButton(
        "submitButton",
        "Submit",
        width = "100%"
      )
    ),
    layout_columns(
      fill = TRUE,
      col_widths = c(12, 12),
      class = "p-0 m-0",
      card(
        title = NULL,
        min_height = "85vh",
        max_height = "85vh",
        nav_panel(
          title = NULL,
          layout_columns(
            col_widths = c(8, 4),
            #fill = TRUE,
            class = "p-0 m-0",
            leafletOutput("state_map", height = '92vh') |>
              withSpinner() |>
              as_fill_carrier(),
            layout_columns(
              fill = TRUE,
              class = "p-0 m-0",
              col_widths = c(12, 12, 12, 12),

              value_box(
                title = NULL,
                value = NULL,
                h5(HTML(
                  "<b>Population With Access to Library Services Through...</b>"
                )),
                hr(),
                htmlOutput("pop_access_city"),
                htmlOutput("pop_access_county"),
                htmlOutput("pop_access_bookmobile"),
                htmlOutput("pop_access_agreed"),
                showcase = NULL,
                theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              ),
              value_box(
                title = NULL,
                value = NULL,
                h5(
                  HTML(
                    "<b>Population With Access to Library Services ONLY Through...</b>"
                  )
                ),
                hr(),
                htmlOutput("pop_access_city_undup"),
                htmlOutput("pop_access_county_undup"),
                htmlOutput("pop_access_bookmobile_undup"),
                htmlOutput("pop_access_agreed_undup"),
                showcase = NULL,
                theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              ),

              # value_box(
              #   title = "City Library Service Only",
              #   value = htmlOutput("pop_access_city_undup"),
              #   htmlOutput("pop_access_city"),
              #   showcase = bs_icon("geo-alt-fill"),
              #   theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              #   # title = "Annual Visits",
              #   # value = htmlOutput("m_visitsCY"),
              #   # htmlOutput("m_visitsPY"),
              #   # htmlOutput("m_visitschange"),
              #   # showcase = bs_icon("people-fill"),
              #   # theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              # ),
              # value_box(
              #   title = "County Library Service Only",
              #   value = htmlOutput("pop_access_county_undup"),
              #   htmlOutput("pop_access_county"),
              #   showcase = bs_icon("pin-map-fill"),
              #   theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              #   # title = "Population of Legal Service Area",
              #   # value = htmlOutput("m_popu_lsaCY"),
              #   # htmlOutput("m_popu_lsaPY"),
              #   # htmlOutput("m_popu_lsachange"),
              #   # showcase = bs_icon("houses"),
              #   # theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              # ),
              # value_box(
              #   title = "Bookmobile Service Only",
              #   value = htmlOutput("pop_access_bookmobile_undup"),
              #   htmlOutput("pop_access_bookmobile"),
              #   showcase = bs_icon("truck-front-fill"),
              #   theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              #   # title = "Registered Borrowers",
              #   # value = htmlOutput("m_regborCY"),
              #   # htmlOutput("m_regborPY"),
              #   # htmlOutput("m_regborchange"),
              #   # showcase = bs_icon("person-vcard"),
              #   # theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              # ),
              # value_box(
              #   title = "Agreed Service Only",
              #   value = htmlOutput("pop_access_agreed_undup"),
              #   htmlOutput("pop_access_agreed"),
              #   showcase = bs_icon("check-square-fill"),
              #   theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              #   # title = "FTE",
              #   # value = htmlOutput("m_fteCY"),
              #   # htmlOutput("m_ftePY"),
              #   # htmlOutput("m_ftechange"),
              #   # showcase = bs_icon("file-person"),
              #   # theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              # )
            )
          )
        )
      ),
      card(
        title = NULL,
        min_height = "85vh",
        max_height = "85vh",
        reactableOutput("municipality_table")
      ) #,
      # nav_panel(
      #   "Overview",
      #   layout_sidebar(
      #     sidebar = sidebar(
      #       width = "25%",
      #       pickerInput(
      #         "table_selection_state",
      #         "Select a Table",
      #         choices = list(
      #           "Overview",
      #           "Revenue and Expenditures" = c(
      #             "Revenue",
      #             "Total Expenditures",
      #             "Staff Expenditures",
      #             "Collection Expenditures"
      #           ),
      #           "Resources and Services" = c(
      #             "Circulation",
      #             "Collections",
      #             "Visits, Borrowers, Reference, and ILL",
      #             "Internet Access"
      #           ),
      #           "Programs" = c("Number of Programs", "Program Attendance")
      #         ),
      #         selected = "Overview",
      #         multiple = FALSE
      #       ),
      #       checkboxInput(
      #         "percap.state",
      #         "Show Values Per Capita?",
      #         value = FALSE
      #       ) #,
      #       ##### CSV Download Button #####
      #       #uiOutput("csv_button.single") #fix reactable output when downloading
      #     ),
      #     reactableOutput("table_state")
      #   )
      # )
    )
  )
)
