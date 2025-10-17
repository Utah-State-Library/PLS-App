nav_panel(title = "State Overview",
          
          layout_columns(
            col_widths = c(9,3),
            fillable = TRUE,
            
            layout_columns(
              fillable = TRUE,
              col_widths = c(12,12),
              # selectInput(
              #   "libname",
              #   "",
              #   choices = libnames,
              #   selected = libnames[1]
              # ),
              # card(
              #   max_height = 60,
              #   h4("State of Utah - Public Libraries")
              # ),
              # layout_columns(
              #   height = 700,
              #   fillable = TRUE,
              #   col_widths = c(8,4), 
              #   layout_columns(
              #     fillable = TRUE,
              #     col_widths = c(12),
              #     card(
              #       height = 500,
              #       full_screen = TRUE,
              #       leafletOutput("state_map")
              #     )
              #   ),
              #   
              #   card(
              #     reactableOutput("percap_st")
              #   )
              
              navset_card_tab(
                title = "Utah Public Libraries",
                #height = 700,
                full_screen = TRUE,
                
                nav_panel(
                  "Map",
                  layout_sidebar(
                    sidebar = sidebar(
                      width = "30%",
                      pickerInput(
                        "st_county",
                        "Select Libraries by County",
                        choices = counties,
                        selected = counties,
                        multiple = TRUE,
                        options = list(`live-search`=TRUE,
                                       `actions-box` = TRUE,
                                       `selected-text-format` = paste0("count > ", length(counties) -1),
                                       `count-selected-text` = "All Counties"
                        )
                      ),
                      pickerInput("st_ae",
                                  "Select Libraries by System",
                                  choices = ae_name,
                                  selected = ae_name,
                                  multiple = TRUE,
                                  options = list(`live-search`=TRUE,
                                                 `actions-box` = TRUE,
                                                 `selected-text-format` = paste0("count > ", length(ae_name) -1), `count-selected-text` = "All Library Systems")),
                      actionButton("submitButton", 
                                   "Submit", 
                                   #style="background-color: #3d4766", 
                                   width = "100%"
                      )
                      # pickerInput(
                      #   "st_popgroup",
                      #   "Select Libraries by Population Group",
                      #   choices = population_groups,
                      #   selected = population_groups,
                      #   options = list(`live-search`=TRUE,
                      #                  `actions-box` = TRUE,
                      #                  `selected-text-format` = paste0("count > ", length(population_groups) -1),
                      #                  `count-selected-text` = "All Population Groups"
                      #   )
                      # )
                    ),
                    leafletOutput("state_map")
                  )
                  
                ),
                nav_panel(
                  "Totals",
                  reactableOutput("percap_st")
                )
              )  
            ),
            
            layout_columns(
              fill = FALSE,
              col_widths = c(12, 12, 12, 12),
              value_box(
                title = "Annual Visits",
                value = htmlOutput("m_visitsCY"),
                htmlOutput("m_visitsPY"),
                htmlOutput("m_visitschange"),
                showcase = bs_icon("people-fill")
              ),
              value_box(
                title = "Population of Legal Service Area",
                value = htmlOutput("m_popu_lsaCY"),
                htmlOutput("m_popu_lsaPY"),
                htmlOutput("m_popu_lsachange"),
                showcase = bs_icon("houses")
              ),
              value_box(
                title = "Registered Borrowers",
                value = htmlOutput("m_regborCY"),
                htmlOutput("m_regborPY"),
                htmlOutput("m_regborchange"),
                showcase = bs_icon("person-vcard")
              ),
              value_box(
                title = "FTE",
                value = htmlOutput("m_fteCY"),
                htmlOutput("m_ftePY"),
                htmlOutput("m_ftechange"),
                showcase = bs_icon("file-person")
              )
            )
          )
)