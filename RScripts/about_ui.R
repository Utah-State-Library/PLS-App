nav_panel(
  title = tags$h5(class = "fw-bold", "About"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_columns(
    col_widths = c(12),
    card(
      h4("About this Dashboard"),
      hr(),
      h5(HTML("<u>Data Sources</u>")),
      p(HTML(paste0(
        "<b>Public Library Survey: </b>",
        "The Public Library Survey is conducted annually by each state to gather information on libraries, their service, and their usage across the United States. Any data concerning libraries (e.g., visits, circulation, FTE, etc.) comes from this data. Every year each state sends this data to the Institute for Museum and Library Services (IMLS) which, in conjunction with the American Institute for Research (AIR), validates the data. Unless unavailable, the data used in this dashboard is the IMLS and AIR validated data which can be found <a href='https://www.imls.gov/research-evaluation/surveys/public-libraries-survey-pls' target='_blank'>here</a>."
      ))),
      p(HTML(paste0(
        "<b>Census Bureau: </b>",
        "All population data comes from the Census Bureau's annual estimate of the population, and can be found <a href='https://www.census.gov/data/datasets/time-series/demo/popest/2020s-total-cities-and-towns.html' target='_blank'>here</a>."
      ))),
      p(HTML(paste0(
        "<b>Utah Geospatial Resource Center: </b>",
        "Geographic boundary data for municipalities and counties can be found <a href='https://opendata.gis.utah.gov/' target='_blank'>here</a>"
      ))),

      h5(HTML("<u>Library Service Areas</u>")),
      p(HTML(paste0(
        "<b>Service Areas</b> refer to the number of people in the geographical area for which a public library has been established to offer services and from which (or on behalf of which) the library derives revenue, plus any areas served under contract for which the library is the primary service provider. This figure is determined by the Utah State Library Division based on the most recent U.S. Census Bureau population estimates available. <br><br>",
        "<b>City Libraries</b> provide library service to residents of the city <br><br>",
        "<b>County Libraries</b> provide library service to residents of the county <br><br>",
        "<b>Bookmobiles</b> provide library service county-wide, and to some additional locations, either physically with a bookmobile or with online access to library resources <br><br>",
        "<b>Agreed Service</b> is a formal agreement for a library to extend service outside of its given jurisdiction <br><br>",
        "<b>Non-Certified / Emerging Libraries</b> provide library service but are not certified through the State Library Division <br><br>",
        "<b>No Library Service</b> indicates that there is no known library providing service to the area"
      )))
    )
  )
)
