#### Any data prep that needs to happen in-project goes here

#### Service Area Map Prep ####
# We are doing this in-project because this is where the shapefile data and up to date outlet data are stored

# Read in these files from app.R:
#1) outlets
#2) municipalities
#3) county_shp

#### Mapping Related Counties and Cities - update if needed
bookmobile_counties <- data.frame(
  CNTY = c(
    "Utah",
    "Iron",
    "Garfield", # Multicounty
    "Kane", # Multicounty
    "Sevier", # Tricounty
    "Piute", # Tricounty
    "Wayne" # Tricounty)
  )
) %>%
  mutate(
    service_provider = case_when(
      CNTY == "Utah" ~ "Utah County Bookmobile",
      CNTY == "Iron" ~ "Iron County Bookmobile",
      CNTY %in% c("Garfield", "Kane") ~ "MultiCounty Bookmobile",
      CNTY %in% c("Sevier", "Piute", "Wayne") ~ "Tri-County Bookmobile"
    ),
    service_label = paste0(
      "<table>
                      <div style='font-size: 18px;'><b>",
      CNTY,
      " County",
      "</div>
                      <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      service_provider,
      "</div>
      </table>"
    )
  )

other_service_counties <- data.frame(
  CNTY = "Beaver" # shared by the 3 cities
) %>%
  mutate(
    service_provider = "Beaver, Milford, and Minersville Libraries",
    service_label = paste0(
      "<table>
                  <div style='font-size: 18px;'><b>",
      CNTY,
      " County",
      "</div>
                  <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      service_provider,
      "</div>
  </table>"
    )
  )

municipalities_w_agreed_service <- data.frame(
  CITY = c(
    "Nibley", # Hyrum City
    "Wellsville", # Hyrum City
    "East Carbon", # Helper
    "Chester", # Ephraim
    "Aurora", # Salina
    "Redmont" # Salina
  )
) %>%
  mutate(
    service_provider = case_when(
      CITY %in% c("Nibley", "Wellsville") ~ "Hyrum Library",
      CITY == "East Carbon" ~ "Helper City Library",
      CITY == "Chester" ~ "Ephraim City Library",
      CITY %in% c("Aurora", "Redmont") ~ "Salina Public Library"
    ),
    service_label = paste0(
      "<table>
                      <div style='font-size: 18px;'><b>",
      CITY,
      "</div>
                      <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      service_provider,
      "</div>
      </table>"
    )
  )

county_libs <- outlets %>%
  filter(SERVICE_AREA == "county") %>%
  mutate(
    service_label = paste0(
      "<table>
                      <div style='font-size: 18px;'><b>",
      CNTY,
      " County",
      "</div>
                      <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      CURRENT_LIBNAME_AE,
      "</div>
      </table>"
    )
  ) %>%
  select(
    CNTY,
    service_label
  ) %>%
  distinct()

city_libs <- outlets %>%
  filter(SERVICE_AREA == "city") %>%
  mutate(
    service_label = paste0(
      "<table>
                      <div style='font-size: 18px;'><b>",
      CITY,
      "</div>
                      <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      CURRENT_LIBNAME_AE,
      "</div>
      </table>"
    )
  ) %>%
  select(
    CITY,
    service_label
  ) %>%
  distinct()


##### Make Map dfs #####
agreed_service_municipalities_map <- municipalities %>%
  filter(NAME %in% municipalities_w_agreed_service$CITY) %>%
  left_join(municipalities_w_agreed_service, by = c("NAME" = "CITY"))

other_service_counties_map <- county_shp %>%
  filter(NAME %in% other_service_counties$CNTY) %>%
  left_join(other_service_counties, by = c("NAME" = "CNTY"))

bookmobile_counties_map <- county_shp %>%
  filter(NAME %in% bookmobile_counties$CNTY) %>%
  left_join(bookmobile_counties, by = c("NAME" = "CNTY"))

counties_wo_service_map <- county_shp %>%
  filter(
    !NAME %in% county_libs$CNTY &
      !NAME %in% other_service_counties$CNTY &
      !NAME %in% bookmobile_counties$CNTY
  ) %>%
  mutate(
    service_label = paste0(
      "<table>
                    <div style='font-size: 18px;'><b>",
      NAME,
      " County",
      "</div>
                    <div style='font-size: 12px;'>",
      "No County Library Service",
      "</div>
    </table>"
    )
  )

counties_w_service_map <- county_shp %>%
  filter(NAME %in% county_libs$CNTY) %>%
  left_join(county_libs, by = c("NAME" = "CNTY"))

municipalities_w_service_map <- municipalities %>%
  filter(NAME %in% city_libs$CITY) %>%
  left_join(city_libs, by = c("NAME" = "CITY"))

municipalities_wo_service_map <- municipalities %>%
  filter(
    !NAME %in% city_libs$CITY &
      !NAME %in% municipalities_w_agreed_service$CITY &
      !COUNTYNBR %in% bookmobile_counties_map$COUNTYNBR &
      !COUNTYNBR %in% counties_w_service_map$COUNTYNBR
  ) %>%
  mutate(
    service_label = paste0(
      "<table>
                    <div style='font-size: 18px;'><b>",
      NAME,
      "</div>
                    <div style='font-size: 12px;'>",
      "No City Library Service",
      "</div>
    </table>"
    )
  )

map_all <- outlets %>%
  mutate(
    LAT = as.numeric(LAT),
    LONG = as.numeric(LONG),
    library_info = paste0(
      "<table>
                        <div style='font-size: 18px;'><b>",
      CURRENT_LIBNAME_AE,
      "</div>
                        <div style='font-size: 12px;'>",
      CURRENT_LIBNAME_OUTLET,
      "</div>
                        <div style='font-size: 12px;'>",
      str_to_title(ADDRESS),
      ", ",
      str_to_title(CITY),
      ", ",
      ZIP,
      "</div>
        </table>"
    )
  )
