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
    )
  )

other_service_counties <- data.frame(
  CNTY = "Beaver" # shared by the 3 cities
) %>%
  mutate(
    service_provider = "Beaver, Milford, and Minersville Libraries"
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
    )
  )

county_libs <- outlets %>%
  filter(SERVICE_AREA == "county") %>%
  select(
    CURRENT_LIBNAME_AE,
    CNTY
  ) %>%
  distinct()

city_libs <- outlets %>%
  filter(SERVICE_AREA == "city") %>%
  select(
    CURRENT_LIBNAME_AE,
    CITY
  ) %>%
  distinct()


##### Make Map dfs #####
agreed_service_municipalities_map <- municipalities %>%
  filter(NAME %in% municipalities_w_agreed_service$CITY) %>%
  left_join(municipalities_w_agreed_service, by = c("NAME" = "CITY")) %>%
  mutate(
    service_label = paste0(
      "<table>
                      <div style='font-size: 18px;'><b>",
      NAME,
      "</div>
                      <div style='font-size: 12px;'>",
      "Population: ",
      POPULATION,
      "</div>
                      <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      service_provider,
      "</div>
      </table>"
    )
  )

other_service_counties_map <- county_shp %>%
  filter(NAME %in% other_service_counties$CNTY) %>%
  left_join(other_service_counties, by = c("NAME" = "CNTY")) %>%
  mutate(
    service_label = paste0(
      "<table>
                <div style='font-size: 18px;'><b>",
      NAME,
      " County",
      "</div>
        <div style='font-size: 12px;'>",
      "Population: ",
      POPULATION,
      "</div>
        <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      service_provider,
      "</div>
</table>"
    )
  )

bookmobile_counties_map <- county_shp %>%
  filter(NAME %in% bookmobile_counties$CNTY) %>%
  left_join(bookmobile_counties, by = c("NAME" = "CNTY")) %>%
  mutate(,
    service_label = paste0(
      "<table>
                      <div style='font-size: 18px;'><b>",
      NAME,
      " County",
      "</div>
        <div style='font-size: 12px;'>",
      "Population: ",
      POPULATION,
      "</div>
                      <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      service_provider,
      "</div>
      </table>"
    )
  )

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
      "Population: ",
      POPULATION,
      "</div>
                    <div style='font-size: 12px;'>",
      "No County Library Service",
      "</div>
    </table>"
    )
  )

counties_w_service_map <- county_shp %>%
  filter(NAME %in% county_libs$CNTY) %>%
  left_join(county_libs, by = c("NAME" = "CNTY")) %>%
  mutate(
    service_label = paste0(
      "<table>
                        <div style='font-size: 18px;'><b>",
      NAME,
      " County",
      "</div>
        <div style='font-size: 12px;'>",
      "Population: ",
      POPULATION,
      "</div>
                        <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      CURRENT_LIBNAME_AE,
      "</div>
        </table>"
    )
  )

municipalities_w_service_map <- municipalities %>%
  filter(NAME %in% city_libs$CITY) %>%
  left_join(city_libs, by = c("NAME" = "CITY")) %>%
  mutate(
    service_label = paste0(
      "<table>
                        <div style='font-size: 18px;'><b>",
      NAME,
      "</div>
        <div style='font-size: 12px;'>",
      "Population: ",
      POPULATION,
      "</div>
                        <div style='font-size: 12px;'>",
      "Library Service Provider: ",
      CURRENT_LIBNAME_AE,
      "</div>
        </table>"
    )
  )

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
      "Population: ",
      POPULATION,
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
    library_info = case_when(
      CURRENT_LIBNAME_OUTLET != CURRENT_LIBNAME_AE ~
        paste0(
          "<table>
                        <div style='font-size: 18px;'><b>",
          CURRENT_LIBNAME_OUTLET,
          "</div>
                        <div style='font-size: 12px;'>",
          CURRENT_LIBNAME_AE,
          "</div>
                        <div style='font-size: 12px;'>",
          str_to_title(ADDRESS),
          ", ",
          str_to_title(CITY),
          ", ",
          ZIP,
          "</div>
        </table>"
        ),
      CURRENT_LIBNAME_OUTLET == CURRENT_LIBNAME_AE ~
        paste0(
          "<table>
                        <div style='font-size: 18px;'><b>",
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
  )


#### Statewide Per Cap Table ####
state_all_libs <- pls %>%
  filter(hide_lib == 0) %>%
  group_by(FISCAL_YEAR) %>%
  reframe(
    ## Overview
    POPU_LSA = sum(POPU_LSA, na.rm = T),
    N_SYSTEM = n_distinct(FSCSKEY),
    TOTSTAFF = sum(TOTSTAFF, na.rm = T),
    VLNT = sum(VLNT, na.rm = T),
    VLNT_HRS = sum(VLNT_HRS, na.rm = T),

    ## Revenue
    LOCGVT = sum(LOCGVT, na.rm = T),
    STGVT = sum(STGVT, na.rm = T),
    FEDGVT = sum(FEDGVT, na.rm = T),
    OTHINCM = sum(OTHINCM, na.rm = T),
    TOTINCM = sum(TOTINCM, na.rm = T),

    ## Expenses
    TOTOPEXP = sum(TOTOPEXP, na.rm = T),
    OTHOPEXP = sum(OTHOPEXP, na.rm = T),

    ## Staff Expenditures
    SALARIES = sum(SALARIES, na.rm = T),
    BENEFIT = sum(BENEFIT, na.rm = T),
    STAFFEXP = sum(STAFFEXP, na.rm = T),

    ## Collections Expenditures
    TOTEXPCO = sum(TOTEXPCO, na.rm = T),
    PRMATEXP = sum(PRMATEXP, na.rm = T),
    ELMATEXP = sum(ELMATEXP, na.rm = T),
    OTHMATEX = sum(OTHMATEX, na.rm = T),

    ## Circulation
    TOTCIR = sum(TOTCIR, na.rm = T),
    PHYSCIR = sum(PHYSCIR, na.rm = T),
    KIDCIRCL = sum(KIDCIRCL, na.rm = T),
    ELMATCIR = sum(ELMATCIR, na.rm = T),
    OTHPHCIR = sum(OTHPHCIR, na.rm = T),
    EBOOK_CIR = sum(EBOOK_CIR, na.rm = T),
    EAUDIO_CIR = sum(EAUDIO_CIR, na.rm = T),
    EVIDEO_CIR = sum(EVIDEO_CIR, na.rm = T),
    ESERIAL_CIR = sum(ESERIAL_CIR, na.rm = T),

    ## Collections
    TOTPHYS = sum(TOTPHYS, na.rm = T),
    BKVOL = sum(BKVOL, na.rm = T),
    AUDIO_PH = sum(AUDIO_PH, na.rm = T),
    VIDEO_PH = sum(VIDEO_PH, na.rm = T),
    OTHMATS = sum(OTHMATS, na.rm = T),

    ## Programs
    TOTPRO = sum(TOTPRO, na.rm = T),
    K0_5PRO = sum(K0_5PRO, na.rm = T),
    K6_11PRO = sum(K6_11PRO, na.rm = T),
    YAPRO = sum(YAPRO, na.rm = T),
    ADULTPRO = sum(ADULTPRO, na.rm = T),
    GENPRO = sum(GENPRO, na.rm = T),

    ## Attendance
    TOTATTEN = sum(TOTATTEN, na.rm = T),
    K0_5ATTEN = sum(K0_5ATTEN, na.rm = T),
    K6_11ATTEN = sum(K6_11ATTEN, na.rm = T),
    YAATTEN = sum(YAATTEN, na.rm = T),
    ADULTATTEN = sum(ADULTATTEN, na.rm = T),
    GENATTEN = sum(GENATTEN, na.rm = T),

    ## Visits, reference, and ILL
    REFERENC = sum(REFERENC, na.rm = T),
    VISITS = sum(VISITS, na.rm = T),
    REGBOR = sum(REGBOR, na.rm = T),
    LOANTO = sum(LOANTO, na.rm = T),
    LOANFM = sum(LOANFM, na.rm = T),

    ## Internet Access
    GPTERMS = sum(GPTERMS, na.rm = T),
    PITUSR = sum(PITUSR, na.rm = T),
    WIFISESS = sum(WIFISESS, na.rm = T),
    HOTSPOT = sum(HOTSPOT, na.rm = T),
    HOTSPOT_CIRC = sum(HOTSPOT_CIRC, na.rm = T)
  )
