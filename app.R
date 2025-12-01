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
library(thematic)

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

current_year <- max(as.numeric(pls$FISCAL_YEAR))

census <- read.csv("data/census.csv") %>%
  select(COUNTY, PLACE, NAME, POPULATION = contains(as.character(current_year)))

variable_key <- read.csv("data/pls_variable_key.csv")

outlets <- readRDS("data/pls_outlet_national.rds") %>%
  filter(STABR == "UT", hide_lib == 0, FISCAL_YEAR == current_year) %>%
  mutate(
    CITY = case_when(
      CITY == "South Salt Lake City" ~ "South Salt Lake",
      CITY == "Mt. Pleasant" ~ "Mount Pleasant",
      .default = CITY
    ),
  )

## NOTE Do not delete any files in the shape file folders! Even though they're not read in directly, they are still used when reading in the data

county_shp <- sf::st_read("data/counties/Counties.shp") %>%
  select(NAME, COUNTYNBR, COUNTY_FIPS = FIPS, geometry) %>%
  mutate(NAME = str_to_title(NAME)) %>%
  left_join(
    census %>%
      filter(PLACE == 0) %>%
      select(COUNTY, POPULATION) %>%
      mutate(POPULATION = format(POPULATION, big.mark = ",")),
    by = c("COUNTY_FIPS" = "COUNTY")
  ) %>%
  left_join(
    census %>%
      filter(PLACE == 99990) %>%
      select(COUNTY, POPULATION_CNTY_BALANCE = POPULATION) %>%
      mutate(
        POPULATION_CNTY_BALANCE = format(
          POPULATION_CNTY_BALANCE,
          big.mark = ","
        )
      ),
    by = c("COUNTY_FIPS" = "COUNTY")
  ) %>%
  mutate(across(c(POPULATION, POPULATION_CNTY_BALANCE), ~ gsub(",", "", .))) %>%
  mutate(across(c(POPULATION, POPULATION_CNTY_BALANCE), ~ as.numeric(.))) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

municipalities <- sf::st_read("data/municipalities/Municipalities.shp") %>%
  select(NAME, COUNTYNBR, CITY_FIPS = FIPS, geometry) %>%
  mutate(
    NAME = case_when(
      NAME == "Magna City" ~ "Magna",
      NAME == "South Salt Lake City" ~ "South Salt Lake",
      .default = NAME
    ),
    CITY_FIPS = as.numeric(CITY_FIPS)
  ) %>%
  left_join(
    census %>%
      filter(COUNTY == 0) %>%
      distinct() %>%
      select(PLACE, POPULATION) %>%
      mutate(POPULATION = format(POPULATION, big.mark = ",")),
    by = c("CITY_FIPS" = "PLACE")
  ) %>%
  mutate(
    POPULATION = gsub(",", "", POPULATION),
    POPULATION = as.numeric(POPULATION)
  ) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')
# Because some cities cross county lines, there may be 'duplicates', but it's okay practically because we don't care about the specific county-line city populations; we're keeping COUNTYNBR in for now because we will use it in some data prep for the map

#### Input Lists ####
source("RScripts/lists.R", local = TRUE)
source("RScripts/data_prep.R", local = TRUE)
source("RScripts/theme.R", local = TRUE)

#### Functions ####
source("RScripts/functions.R", local = TRUE)


#### UI ####

ui <- fluidPage(
  class = "container-fluid align-self-center mx-1 px-0",
  style = "width: 95vw; height: 95vh; padding: 0; margin: 1;",
  theme = usl_theme,

  # div(
  #   class = "container-fluid text-center mx-1 px-0",
  #   div(
  #     class = "row justify-content-center",
  #     div(
  #       class = "col-12 col-md-8 col-lg-8 align-self-center py-0 my-1 px-0 mx-1 bg-body-tertiary rounded-3",
  #       tags$h1(class = "display-5 fw-bold", "Utah Libraries Data Dashboard"),
  #       p(
  #         class = "fs-5",
  #         "Welcome to the Utah Libraries Data Dashboard! This tool helps library directors, city and county council members, and Utahns understand library service statewide."
  #       )
  #     )
  #   )
  # ),
  #end Container

  page_navbar(
    title = "",
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

    #source("RScripts/state_service_ui.R", local = TRUE)$value,
    source("RScripts/state_ui.R", local = TRUE)$value,
    source("RScripts/single_ui.R", local = TRUE)$value,
    source("RScripts/tables_menu_ui.R", local = TRUE)$value,
    source("RScripts/state_service_ui.R", local = TRUE)$value,
    nav_spacer(),
    source("RScripts/about_ui.R", local = TRUE)$value
  )
)


#### Server ####
server <- function(input, output, session) {
  #source("RScripts/state_service_ui.R", local = TRUE)$value
  source("RScripts/state_server.R", local = TRUE)$value
  source("RScripts/single_server.R", local = TRUE)$value
  source("RScripts/tables_server.R", local = TRUE)$value
  source("RScripts/state_service_server.R", local = TRUE)$value
}


#### Run App ####
shinyApp(ui = ui, server = server)

## notes for tomorrow - check outlet file against pls file
## add logic for system dropdown if show libs not selected, etc.
