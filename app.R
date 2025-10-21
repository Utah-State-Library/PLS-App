# Load necessary packages
library(highcharter)
library(tidyverse)
library(magrittr)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(leaflet)
library(shinycssloaders)
library(reactable)
library(reactablefmtr)
library(bslib)
library(bsicons)
library(shinyalert)
library(sjmisc)
library(htmlwidgets)
library(shinya11y)
library(sf)

#### Color Palette ####
head_color <- "#002F6C"
sub1_color <- "#0086BF"
sub2_color <- "#4EC3E0"

#### Set Options ####
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


#### Load Data ####

# Created in `./***/Combine PLS Data.R`
pls <- readRDS("data/pls_national.rds") %>%
  filter(STABR == "UT", !str_detect(CURRENT_LIBNAME, "Bookmobile")) %>%
  mutate(CNTY = str_to_title(CNTY), CITY = str_to_title(CITY))
variable_key <- read.csv("data/pls_variable_key.csv")
librarykey <- readRDS("data/librarykey.rds")
outlets <- readRDS("data/pls_outlet_national.rds") %>%
  filter(STABR == "UT") %>%
  mutate(
    CITY = case_when(
      CITY == "South Salt Lake City" ~ "South Salt Lake",
      CITY == "Mt. Pleasant" ~ "Mount Pleasant",
      .default = CITY
    ),
  )

county_shp <- st_read("data/counties/counties.shp") %>%
  mutate(NAME = str_to_title(NAME))
county_shp <- st_transform(county_shp, '+proj=longlat +datum=WGS84')
municipalities <- st_read("data/municipalities/municipalities.shp") %>%
  mutate(
    NAME = case_when(
      NAME == "Magna City" ~ "Magna",
      NAME == "South Salt Lake City" ~ "South Salt Lake",
      .default = NAME
    )
  )
municipalities <- st_transform(municipalities, '+proj=longlat +datum=WGS84')

#### Input Lists ####
source("RScripts/lists.R", local = TRUE)

#### Functions ####
source("RScripts/functions.R", local = TRUE)


#### UI ####

ui <- page_navbar(
  title = "",
  #theme = bs_theme(preset = "sandstone"),
  navbar_options = navbar_options(
    bg = NULL,
    underline = TRUE
  ),

  #### USL Logo in Header ####
  shiny::includeCSS("www/styles.css"),
  use_tota11y(), # for accessibility checking - remove/comment out in final product

  tags$head(
    tags$script(
      HTML(
        '
          $(document).ready(function() {
            $(".navbar-brand").replaceWith(
              $("<a target=\'_blank\' rel=\'noopener noreferrer\' class = \'navbar-brand\' href = \'https://library.utah.gov/\'></a>")
            );
            var containerHeight = $(".navbar .container-fluid").height() + "px";
            $(".navbar-brand")
              .append(
                "<img id = \'www\' src=\'usllogo.jpeg\' role=\'presentation\'" +
                " height = " + containerHeight + ">"  
              );
            });'
      )
    )
  ),

  source("RScripts/state_ui.R", local = TRUE)$value,
  source("RScripts/single_ui.R", local = TRUE)$value,
  source("RScripts/tables_menu_ui.R", local = TRUE)$value,

  ## To put links and whatnot at the right of the navbar at some point
  # nav_spacer(),
  # nav_panel(
  #   shiny::icon("circle-info"),
  #   markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
  # )
)


#### Server ####
server <- function(input, output, session) {
  source("RScripts/state_server.R", local = TRUE)$value
  source("RScripts/single_server.R", local = TRUE)$value
  source("RScripts/tables_server.R", local = TRUE)$value
}


#### Run App ####
shinyApp(ui = ui, server = server)
