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
      tags$h5(class = "mb-1 mt-0", "Map Controls"),
      checkboxInput(
        "show_libs",
        "Show Library Locations?",
        FALSE
      ),
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
                showcase = NULL,
                theme = value_box_theme(bg = "#ffffff", fg = "#002f6C")
              )
            )
          )
        )
      ),
      card(
        title = NULL,
        min_height = "85vh",
        max_height = "85vh",
        reactableOutput("municipality_table")
      )
    )
  )
)
