nav_panel(
  title = tags$h5(class = "fw-bold", "Library Service Map"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
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
      tags$h5(class = "mb-1 mt-0", "Map Controls"),
      checkboxInput(
        "show_libs",
        "Show Library Locations?",
        FALSE
      ),

      # virtualSelectInput(
      #   "service_areas",
      #   label = NULL,
      #   choices = list(
      #     "County View" = c(
      #       "County Library Service",
      #       "Bookmobile Library Service",
      #       "Non-Certified Library Service",
      #       "No County Library Service"
      #     ),
      #     "City View" = c(
      #       "City Library Service",
      #       "No Library Service"
      #     )
      #   )
      # ),

      checkboxGroupInput(
        "service_areas",
        label = NULL,
        choices = c(
          "County Library Service",
          "No County Library Service",
          "Bookmobile Library Service",
          "City Library Service",
          "Agreed Service Through a City Library",
          "Non-Certified Library Service",
          "No Library Service"
        ),
        selected = c(
          "County Library Service",
          "No County Library Service",
          "Bookmobile Library Service",
          "City Library Service",
          "Agreed Service Through a City Library",
          "Non-Certified Library Service",
          "No Library Service"
        )
      ),
      actionButton(
        "submitButton",
        "Submit",
        width = "100%"
      )
    ),
    navset_card_tab(
      nav_panel(
        "Original",
        card(
          title = NULL,
          min_height = "85vh",
          max_height = "85vh",
          nav_panel(
            title = NULL,
            layout_columns(
              col_widths = c(8, 4),
              class = "p-0 m-0",
              leafletOutput("state_map", height = '92vh') |>
                withSpinner() |>
                as_fill_carrier(),
              # card(
              #   card_header("About Library Service Areas"),
              #   class = "my-bg-white",
              #   p(
              #     HTML(
              #       paste0(
              #         "A library's service area is the number of people in the geographical area for which a public library has been established to offer services and from which (or on behalf of which) the library derives revenue, plus any areas served under contract for which the library is the primary service provider. This figure is determined by the Utah State Library Division based on the most recent U.S. Census Bureau population estimates available. <br><br>",

              #       )
              #     )
              #   )
              # ),
              card(
                card_header("Quick Definitions"),
                class = "my-bg-white",
                p(
                  HTML(
                    paste0(
                      "<b>City Libraries</b> provide library service to residents of the city <br><br>",
                      "<b>County Libraries</b> provide library service to residents of the county <br><br>",
                      "<b>Bookmobiles</b> provide library service county-wide, and to some additional locations, either physically with a bookmobile or with online access to library resources <br><br>",
                      "<b>Agreed Service</b> is a formal agreement for a library to extend service outside of its given jurisdiction <br><br>",
                      "<b>Non-Certified / Emerging Libraries</b> provide library service but are not certified through the State Library Division <br><br>",
                      "<b>No Library Service</b> indicates that there is no known library providing service to the area"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      ##### How to handle unincorporated areas?
      # nav_panel(
      #   "City View",
      #   card(
      #     title = NULL,
      #     min_height = "85vh",
      #     max_height = "85vh",
      #     leafletOutput("city_map")
      #   )
      # ),
      nav_panel(
        "Table View",
        card(
          title = NULL,
          min_height = "85vh",
          max_height = "85vh",
          reactableOutput("municipality_table")
        )
      ),
      nav_panel(
        "Definitions",
        card(
          class = "my-bg-white",
          p(
            HTML(
              paste0(
                "<b>Service Areas</b> refer to the number of people in the geographical area for which a public library has been established to offer services and from which (or on behalf of which) the library derives revenue, plus any areas served under contract for which the library is the primary service provider. This figure is determined by the Utah State Library Division based on the most recent U.S. Census Bureau population estimates available. <br><br>",
                "<b>City Libraries</b> provide library service to residents of the city <br><br>",
                "<b>County Libraries</b> provide library service to residents of the county <br><br>",
                "<b>Bookmobiles</b> provide library service county-wide, and to some additional locations, either physically with a bookmobile or with online access to library resources <br><br>",
                "<b>Agreed Service</b> is a formal agreement for a library to extend service outside of its given jurisdiction <br><br>",
                "<b>Non-Certified / Emerging Libraries</b> provide library service but are not certified through the State Library Division <br><br>",
                "<b>No Library Service</b> indicates that there is no known library providing service to the area"
              )
            )
          )
        )
      )
    )
  )
)
