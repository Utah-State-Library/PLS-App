
nav_panel(title = "Home",
          
          layout_columns(
            col_widths = c(9,3),
            fillable = TRUE,
            
            layout_columns(
              fillable = TRUE,
              col_widths = c(12,12),
              selectInput(
                    "libname",
                    "",
                    choices = libnames,
                    selected = libnames[1]
                  ),
              layout_columns(
                fillable = TRUE,
                col_widths = c(4,8), 
                layout_columns(
                  fillable = TRUE,
                  col_widths = c(12,12),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    leafletOutput("library_map")
                  ),
                  card(
                    height = 200
                  )
                ),

                navset_card_tab(
                  height = 700,
                  full_screen = TRUE,
                  # nav_panel(
                  #   "Map",
                  #   leafletOutput("library_map")
                  # ),
                  nav_panel(
                    "Totals",
                    reactableOutput("percap_comp")
                  ),
                  # nav_panel(
                  #   layout_sidebar(
                  #     sidebar = sidebar(
                  # 
                  #     ),
                  #     highchartOutput("hc_rev")
                  #     highchartOutput("hc_circ")
                  #   )
                  # )
                  nav_panel(
                    "Revenue Graph",
                    highchartOutput("hc_rev")
                  ),
                  nav_panel(
                    "Circulation Graph",
                    highchartOutput("hc_circ")
                  ),
                  nav_panel(
                    "Financial Table",
                    #reactableOutput("budgetpcnts")
                    reactableOutput("financial_table")
                  ),
                  nav_panel(
                    shiny::icon("circle-info"),
                    markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
                  )
                )
                
              )
            ),
            
            layout_columns(
              fill = FALSE,
              col_widths = c(12, 12, 12, 12),
              value_box(
                title = "Annual Visits",
                value = htmlOutput("visitsCY"),
                htmlOutput("visitsPY"),
                htmlOutput("visitschange"),
                showcase = bs_icon("people-fill")
              ),
              value_box(
                title = "Registered Borrowers",
                value = htmlOutput("regborCY"),
                htmlOutput("regborPY"),
                htmlOutput("regborchange"),
                showcase = bs_icon("person-vcard")
              ),
              value_box(
                title = "Population of Legal Service Area",
                value = htmlOutput("popu_lsaCY"),
                htmlOutput("popu_lsaPY"),
                htmlOutput("popu_lsachange"),
                showcase = bs_icon("houses")
              ),
              value_box(
                title = "FTE",
                value = htmlOutput("fteCY"),
                htmlOutput("ftePY"),
                htmlOutput("ftechange"),
                showcase = bs_icon("file-person")
              )
            )
          )
)