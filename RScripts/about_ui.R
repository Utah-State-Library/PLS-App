nav_panel(
  title = tags$h5(class = "fw-bold", "About"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  #style = "width: 95vw; height: 92vh; padding: 0; margin: 1;",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_columns(
    col_widths = c(12),
    card(
      h4("About this Dashboard"),
      hr(),
      h5(HTML("<u>Data Sources</u>")),
      p(HTML(paste0(
        "<b>Public Library Survey: </b>",
        "The Public Library Survey is conducted annually by each state to gather information on libraries, their service, and their usage across the United States. Any data concerning libraries (e.g., visits, circulation, FTE, etc.) comes from this data. Every year each state sends this data to the Institute for Museum and Library Services (IMLS) which, in conjunction with the American Institute for Research (AIR), validates the data. Unless unavailable, the data used in this dashboard is the IMLS and AIR validated data which can be found here*."
      ))),
      p(HTML(paste0(
        "<b>Census Bureau: </b>",
        "All population data comes from the Census Bureau's annual estimate of the population, and can be found here*."
      ))),

      h5(HTML("<u>Library Service Areas</u>")),
      p(HTML(paste0(
        "Libary service areas are the locations, and more specifically, the <i>populations</i> that are served by a given library. There are a few different types of library service:<br>",
        "<b>City Libraries</b>: Libraries that are funded by city taxes that serve residents of that city<br>",
        "<b>County Libraries</b>: Libraries that are funded by county taxes that serve residents of that county<br>",
        "<b>Bookmobiles</b>: Bookmobile libraries that serve an entire county or multiple counties<br>",
        "<b>Service Agreements</b>: Formal agreements between a library and another city(ies) or county to exend library service to that population<br>"
      )))
    )
  )
)
