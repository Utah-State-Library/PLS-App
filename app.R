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

# census data - pop demogs / growth
# school data - title I, literacy
# public transit?? - when established, distance?
# donation flow? - library ROI


#### Color Palette ####
head_color <- "#002F6C"
sub1_color <- "#0086BF"
sub2_color <- "#4EC3E0"

# font: Neue Kabel; secondary font: Mencken; Raleway is primary font for websites
# size: h1 36px, h2 24px, h3 18px, body copy 16pt, captions/citations in Raleway medium italic 12px (h3 and lower 1.25" line spacing)


#### Set Options ####
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

 
#### Load Data ####

# Created in `./***/Combine PLS Data.R`
pls <- readRDS("data/pls_national.rds") %>% filter(STABR == "UT", 
                                                   !str_detect(CURRENT_LIBNAME, "Bookmobile")) %>% 
                                                     mutate(CNTY = str_to_title(CNTY))
variable_key <- read.csv("data/pls_variable_key.csv")
librarykey <- readRDS("data/librarykey.rds")
financials <- readRDS("data/financials.rds")


#### Input Lists ####

current_year <- max(as.numeric(pls$FISCAL_YEAR))

years <- pls %>%
  summarise(unique(FISCAL_YEAR)) %>% pull() %>% sort(decreasing = TRUE)

libnames <- pls %>% filter(CURRENT_LIBNAME != "All Libraries",
                           FISCAL_YEAR == current_year,  ## TODO, handle this in servers with updatePicker logic
                           hide_lib == 0) %>% 
  select(CURRENT_LIBNAME) %>% distinct() %>% 
  arrange(CURRENT_LIBNAME) %>% pull()

current_FSCS <- pls %>% filter(FISCAL_YEAR == current_year, hide_lib == 0) %>%
  summarise(FSCSKEY) %>% unique() %>% pull()

counties <- librarykey %>%
  summarise(unique(COUNTY)) %>% 
  pull() %>% sort()

ae_name <- librarykey %>% 
  filter(FSCS_ID %in% current_FSCS) %>%
  summarise(unique(ADMINISTRATIVE_ENTITY_NAME)) %>% 
  pull() %>% sort()

per1000_cols <- c("TOTSTAFF", "GPTERMS", "HOTSPOT", "K0_5PRO", "K6_11PRO", "YAPRO", "ADULTPRO", "GENPRO", "TOTPRO")



source("RScripts/functions.R", local = T)


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
  use_tota11y(),
  #useShinyjs(),
  
  tags$head(
    tags$script(
      HTML('
          $(document).ready(function() {
            $(".navbar-brand").replaceWith(
              $("<a class = \'navbar-brand\' href = \'#\'></a>")
            );
            var containerHeight = $(".navbar .container-fluid").height() + "px";
            $(".navbar-brand")
              .append(
                "<img id = \'www\' src=\'usllogo.jpeg\'" +
                " height = " + containerHeight + ">"  
              );
            });'
      ) 
    )
  ),
  
  #source("RScripts/state_ui.R", local = TRUE)$value,
  #source("RScripts/single_library_ui.R", local = TRUE)$value,
  source("RScripts/single_ui.R", local = TRUE)$value,
  source("RScripts/tables_menu_ui.R", local = TRUE)$value,
  
  
  ## To put links and whatnot at the right of the navbar
  # nav_spacer(),
  # nav_panel(
  #   shiny::icon("circle-info"),
  #   markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
  # )
  
)


#### Server ####
server <- function(input, output, session){
  
  #source("RScripts/state_server.R", local = TRUE)$value
  #source("RScripts/single_library_server.R", local = TRUE)$value
  source("RScripts/single_server.R", local = TRUE)$value
  source("RScripts/tables_server.R", local = TRUE)$value
  
}


#### Run App ####
shinyApp(ui = ui, server = server)