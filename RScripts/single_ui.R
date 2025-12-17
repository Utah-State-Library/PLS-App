nav_panel(
  title = tags$h5(class = "fw-bold", "Single Library"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; min-height: 92vh; padding: 0; margin: 1;",

  layout_sidebar(
    fill = TRUE,
    sidebar = sidebar(
      title = "Filters",
      width = "25%",

      div(
        class = "mb-2",
        tags$h5(class = "mb-1 mt-0", "Select a Library"),
        pickerInput(
          "library.single",
          label = NULL,
          choices = libnames,
          selected = libnames[1],
          multiple = FALSE
        )
      ),
      div(
        class = "mb-2",
        tags$h5(class = "mb-1 mt-0", "Select a Year"),
        pickerInput(
          "year.single",
          label = NULL,
          choices = years,
          selected = max(years),
          multiple = FALSE
        )
      ),
    ),

    #### Main Body ####

    navset_card_tab(
      id = "active_tab_single",
      nav_panel(
        "Overview",
        uiOutput("library_header.single"),
        #hr(),
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title = htmlOutput("visitsTitle"),
            value = htmlOutput("visitsCY"),
            htmlOutput("visitsPY"),
            htmlOutput("visitschange"),
            showcase = bsicons::bs_icon("people-fill"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("regborTitle"),
            value = htmlOutput("regborCY"),
            htmlOutput("regborPY"),
            htmlOutput("regborchange"),
            showcase = bsicons::bs_icon("person-vcard"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("popu_lsaTitle"),
            value = htmlOutput("popu_lsaCY"),
            htmlOutput("popu_lsaPY"),
            htmlOutput("popu_lsachange"),
            showcase = bsicons::bs_icon("houses"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("totcirTitle"),
            value = htmlOutput("totcirCY"),
            htmlOutput("totcirPY"),
            htmlOutput("totcirchange"),
            showcase = bsicons::bs_icon("bookmark-check"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("totincmTitle"),
            value = htmlOutput("totincmCY"),
            htmlOutput("totincmPY"),
            htmlOutput("totincmchange"),
            showcase = bsicons::bs_icon("currency-dollar"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          ),
          value_box(
            title = htmlOutput("fteTitle"),
            value = htmlOutput("fteCY"),
            htmlOutput("ftePY"),
            htmlOutput("ftechange"),
            showcase = bsicons::bs_icon("file-person"),
            theme = value_box_theme(bg = "#ffffff", fg = "#002f6C"),
            class = "p-0 nopad"
          )
        )
      ),
      nav_panel(
        "Change from Last Year",
        layout_sidebar(
          sidebar = sidebar(
            width = "25%",
            pickerInput(
              "table_selection_single",
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
              "percap.single",
              "Show Values Per Capita?",
              value = FALSE
            ) #,
            ##### CSV Download Button #####
            #uiOutput("csv_button.single") #fix reactable output when downloading
          ),
          reactableOutput("table_single"),
          card(
            class = "my-bg-white",
            p(
              HTML(
                paste0(
                  "<b>This table shows how key library variables changed from the previous year.</b><br><br>",
                  "A large percent change may reflect a meaningful shift, but may also be indicative of additional context (e.g., construction disrupting service, removal of expired accounts, or the loss of a storytime employee). Keep this in mind as you interpret this table.<br><br>",
                  "Comparing across rows can show which parts of library service grew, which declined, and where the most notable changes occurred. Along with context, this table can support identifying trends and highlighting shifts in usage patterns, funding, or service."
                )
              )
            )
          )
        )
      ),
      nav_panel(
        "Staff Workload",
        layout_columns(
          col_widths = c(5, 7),

          layout_columns(
            col_widths = c(12, 12),
            card(
              class = "my-bg-white",
              fill = FALSE,
              card_header("Workload Per 1 FTE"),
              reactableOutput("staffworkload_table"),
              p(
                "NOTE: This table is a workload proxy - it does not show actual numbers for staff work"
              )
            ),
            card(
              class = "my-bg-white",
              fill = FALSE,
              p(
                HTML(
                  paste0(
                    "<b>About FTE</b><br><br>",
                    "FTE represents a 40 hour work week and does not necessarily align with the number of employees at the library. Some staff may be full time or part time.<br>"
                  )
                )
              ),
              uiOutput("staffworkload_expl")
            )
          ),
          card(class = "my-bg-white", highchartOutput("hc_staff_single"))
        )
      ),
      nav_panel(
        "Comparison Chart",

        layout_sidebar(
          sidebar = sidebar(
            width = "25%",
            pickerInput(
              "hc_col.single",
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
              "under_50k.single",
              "Include only libraries serving < 50,000?",
              value = FALSE
            )
          ),
          card(class = "my-bg-white", highchartOutput("hc_comparison_single"))
        )
      ),
      nav_panel(
        title = "Peer Libraries",
        #### Sidebar ####

        layout_sidebar(
          sidebar = sidebar(
            width = "25%",
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
              ),
              checkboxInput(
                "per_cap.table_peer",
                "Show Values Per Capita?",
                value = FALSE
              ),
              checkboxInput(
                "color_table_peer",
                "Color Table by High/Low Values?",
                value = TRUE
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
  )
)
