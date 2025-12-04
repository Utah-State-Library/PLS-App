nav_panel(
  title = tags$h5(class = "fw-bold", "All Libraries"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; min-height: 92vh; padding: 0; margin: 1;",

  page_sidebar(
    fill = TRUE,
    sidebar = sidebar(
      title = "Filters",
      width = "25%",
      open = FALSE,
      div(
        class = "mb-2",
        tags$h5(class = "mb-1 mt-0", "Select a Year"),
        pickerInput(
          "st_year",
          label = NULL,
          choices = years,
          selected = max(years),
          multiple = FALSE
        )
      ),
    ),

    #### Main Body ####

    navset_card_tab(
      id = "active_tab_ut",
      # title = popover(
      #   span("Filters", bs_icon("gear")),
      #   div(
      #     class = "mb-2",
      #     tags$h5(class = "mb-1 mt-0", "Select a Year"),
      #     pickerInput(
      #       "st_year",
      #       label = NULL,
      #       choices = years,
      #       selected = max(years),
      #       multiple = FALSE
      #     )
      #   )
      # ),
      nav_panel(
        "Overview",
        uiOutput("library_header.state"),
        #hr(),
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title = htmlOutput("m_visitsTitle"),
            value = htmlOutput("m_visitsCY"),
            htmlOutput("m_visitsPY"),
            htmlOutput("m_visitschange"),
            showcase = bsicons::bs_icon("people-fill"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("m_regborTitle"),
            value = htmlOutput("m_regborCY"),
            htmlOutput("m_regborPY"),
            htmlOutput("m_regborchange"),
            showcase = bsicons::bs_icon("person-vcard"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("m_popu_lsaTitle"),
            value = htmlOutput("m_popu_lsaCY"),
            htmlOutput("m_popu_lsaPY"),
            htmlOutput("m_popu_lsachange"),
            showcase = bsicons::bs_icon("houses"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("m_totcirTitle"),
            value = htmlOutput("m_totcirCY"),
            htmlOutput("m_totcirPY"),
            htmlOutput("m_totcirchange"),
            showcase = bsicons::bs_icon("bookmark-check"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("m_totincmTitle"),
            value = htmlOutput("m_totincmCY"),
            htmlOutput("m_totincmPY"),
            htmlOutput("m_totincmchange"),
            showcase = bsicons::bs_icon("currency-dollar"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("m_fteTitle"),
            value = htmlOutput("m_fteCY"),
            htmlOutput("m_ftePY"),
            htmlOutput("m_ftechange"),
            showcase = bsicons::bs_icon("file-person"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          )
        )
      ),
      nav_panel(
        "Percent Change Tables",
        layout_sidebar(
          sidebar = sidebar(
            width = "25%",
            pickerInput(
              "table_selection_state",
              "Select a Table",
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
            ),
            checkboxInput(
              "percap.state",
              "Show Values Per Capita?",
              value = FALSE
            ) #,
            ##### CSV Download Button #####
            #uiOutput("csv_button.single") #fix reactable output when downloading
          ),
          reactableOutput("table_state"),
          card(
            class = "my-bg-white",
            fill = FALSE,
            p(
              HTML(
                paste0(
                  "<b>This table shows how key library variables changed from the previous year.</b><br><br>",
                  "A large percent change may reflect a meaningful shift, but may also be indicative of additional context (e.g., physical circulation may appear lower, but perhaps patrons are checking out more e-books). Keep this in mind as you interpret this table.<br><br>",
                  "Comparing across rows can show which parts of library service grew, which declined, and where the most notable changes occurred. Along with context, this table can support identifying trends and highlighting shifts in usage patterns, funding, or service."
                )
              )
            )
          )
        )
      ),
      nav_panel(
        "Comparison Chart",

        layout_sidebar(
          width = c(12, 12),
          sidebar = sidebar(
            width = "25%",
            pickerInput(
              "hc_col.state",
              "Select a Value to Graph",
              choices = c(
                "Visits" = "VISITS",
                "Revenue" = "TOTINCM",
                "FTE" = "TOTSTAFF",
                "Card Holders" = "REGBOR",
                "Number of Programs" = "TOTPRO",
                "Program Attendance" = "TOTATTEN",
                "Total Collection" = "TOTPHYS",
                "Total Circulation" = "TOTCIR"
              )
            ),
            checkboxInput(
              "st_hc_percap",
              "Show Values Per Capita?",
              value = TRUE
            )
          ),
          card(class = "my-bg-white", highchartOutput("hc_comparison_state")),
          card(
            class = "my-bg-white",
            p(
              HTML(
                paste0(
                  "<b>This chart shows three different ways of understanding per capita values across libraries in the state.</b><br><br>",

                  "<b>Statewide Per Capita</b> treats the entire state as a single system, dividing the total value for all libraries by the total state population. This measure reflects the overall resource level available per resident and is strongly influenced by large libraries. <br>",

                  "<b>Average Per Capita Across Libraries</b> calculates each library’s per capita value and then averages those figures, giving equal weight to every library regardless of size. This reflects the typical per capita value if all libraries were considered equally.<br>",

                  "<b>Median Per Capita Across Libraries</b> identifies the middle per capita value, offering a picture of what the “typical” library looks like while reducing the influence of outliers.<br><br>",

                  "Comparing these lines can reveal important patterns, such as whether large or small libraries tend to have higher per capita values, and whether the distribution of resources is skewed by a handful of high- or low-performing libraries."
                )
              )
            )
          )
        )
      )
    )
  )
)
