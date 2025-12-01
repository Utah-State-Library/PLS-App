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
    bookmobile_service = case_when(
      CNTY == "Utah" ~ "Utah County Bookmobile",
      CNTY == "Iron" ~ "Iron County Bookmobile",
      CNTY %in% c("Garfield", "Kane") ~ "MultiCounty Bookmobile",
      CNTY %in% c("Sevier", "Piute", "Wayne") ~ "Tri-County Bookmobile"
    )
  )

agreed_service_counties <- data.frame(
  CNTY = "Beaver" # shared by the 3 cities
) %>%
  mutate(
    agreed_service_county = "Beaver, Milford, and Minersville Libraries"
  )

agreed_service_city <- data.frame(
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
    agreed_service_city = case_when(
      CITY %in% c("Nibley", "Wellsville") ~ "Hyrum Library",
      CITY == "East Carbon" ~ "Helper City Library",
      CITY == "Chester" ~ "Ephraim City Library",
      CITY %in% c("Aurora", "Redmont") ~ "Salina Public Library"
    )
  )

county_libs <- outlets %>%
  filter(SERVICE_AREA == "county") %>%
  select(
    county_service = CURRENT_LIBNAME_AE,
    CNTY
  ) %>%
  distinct()

city_libs <- outlets %>%
  filter(SERVICE_AREA == "city") %>%
  select(
    city_service = CURRENT_LIBNAME_AE,
    CITY
  ) %>%
  distinct()

##### Make Map dfs #####

## Make a crosswalk df to get county names into the municipality df
county_xwalk <- county_shp %>%
  select(CNTY = NAME, COUNTYNBR) %>%
  st_drop_geometry()

## Create the county map df
county_map <- county_shp %>%
  left_join(county_libs, by = c("NAME" = "CNTY")) %>%
  left_join(bookmobile_counties, by = c("NAME" = "CNTY")) %>%
  left_join(agreed_service_counties, by = c("NAME" = "CNTY")) %>%
  mutate(across(
    c(county_service, bookmobile_service, agreed_service_county),
    ~ ifelse(is.na(.), "None", .)
  ))

## Create the city map df
municipalities_map <- municipalities %>%
  left_join(county_xwalk, by = "COUNTYNBR") %>%

  # 4 cities dip into another county and have negligible pop differences; drop here
  mutate(
    drop = case_when(
      NAME == "Bluffdale" & CNTY == "Utah" ~ 1,
      NAME == "Draper" & CNTY == "Utah" ~ 1,
      NAME == "Park City" & CNTY == "Wasatch" ~ 1,
      NAME == "Santaquin" & CNTY == "Juab" ~ 1,
      .default = 0
    )
  ) %>%
  filter(drop == 0) %>%
  select(-drop) %>%
  left_join(county_libs, by = "CNTY") %>%
  left_join(bookmobile_counties, by = "CNTY") %>%
  left_join(agreed_service_counties, by = "CNTY") %>%
  left_join(agreed_service_city, by = c("NAME" = "CITY")) %>%
  left_join(city_libs, by = c("NAME" = "CITY")) %>%
  mutate(across(
    c(
      county_service,
      bookmobile_service,
      agreed_service_county,
      agreed_service_city,
      city_service
    ),
    ~ ifelse(is.na(.), "None", .)
  )) %>%
  mutate(
    POPULATION = gsub(",", "", POPULATION),
    POPULATION = as.numeric(POPULATION),

    ## Population with access to..
    pop_access_city = ifelse(city_service != "None", POPULATION, 0),
    pop_access_county = ifelse(county_service != "None", POPULATION, 0),
    pop_access_bookmobile = ifelse(bookmobile_service != "None", POPULATION, 0),
    pop_access_agreed = ifelse(
      agreed_service_city != "None" | agreed_service_county != "None",
      POPULATION,
      0
    ),

    # ## unduplicated service populations
    # # City as primary service provider
    # pop_city_serv = ifelse(city_service != "None", POPULATION, 0),
    # # Agreed city service as main service provider
    # pop_agreed_serv_city = ifelse(agreed_service_city != "None", POPULATION, 0),
    # # If not served by a city lib or agreed city lib, bookmobile as main service provider
    # pop_bookmobile_serv = ifelse(
    #   bookmobile_service != "None",
    #   POPULATION - pop_city_serv - pop_agreed_serv_city,
    #   0
    # ),
    # # If not served by a city lib or agreed city lib, county as main service provider
    # # no counties have a county lib & bookmobile at the same time
    # pop_county_serv = ifelse(
    #   county_service != "None",
    #   POPULATION - pop_city_serv - pop_agreed_serv_city,
    #   0
    # ),
    city_pop_served = ifelse(
      county_service != "None" |
        bookmobile_service != "None" |
        agreed_service_county != "None" |
        agreed_service_city != "None" |
        city_service != "None",
      POPULATION,
      0
    ),
    agreed_service_label = ifelse(
      agreed_service_city == "None",
      agreed_service_county,
      agreed_service_city
    ),
    # Header Label
    label_header = paste0(
      "<div style='font-size:18px; font-weight:bold;'>",
      NAME,
      "</div>",
      "<hr>",
      "<div style='font-size:14px; font-weight:bold;'>",
      current_year,
      " Library Service Areas</div>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>City Population:</td>",
      "<td style='text-align:right;'>",
      POPULATION,
      "</td></tr>",
      "</table>"
    ),

    # Population
    label_pops = paste0(
      "<hr>",
      "<div style='font-size:14px; font-weight:bold;'>Population With Library Service Through...</div>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>City Libraries:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_city, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>County Libraries:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_county, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Bookmobiles:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_bookmobile, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Service Agreements:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_agreed, big.mark = ","),
      "</td></tr>",
      "</table>"
    ),

    # Providers
    label_provider = paste0(
      "<hr>",
      "<div style='font-size:14px; font-weight:bold;'>Service Providers</div>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>City Libraries:</td>",
      "<td style='text-align:right;'>",
      city_service,
      "</td></tr>",
      "<tr><td style='text-align:left;'>County Libraries:</td>",
      "<td style='text-align:right;'>",
      county_service,
      "</td></tr>",
      "<tr><td style='text-align:left;'>Bookmobiles:</td>",
      "<td style='text-align:right;'>",
      bookmobile_service,
      "</td></tr>",
      "<tr><td style='text-align:left;'>Service Agreements:</td>",
      "<td style='text-align:right;'>",
      agreed_service_label,
      "</td></tr>",
      "</table>"
    ),

    # Combined Label & Popup
    service_label = paste0(label_header, "<hr><i>Click for more details</i>"),
    service_popup = paste0(label_header, label_pops, label_provider)
  )

municipalities_county_pop <- municipalities_map %>%
  st_drop_geometry() %>%
  left_join(
    county_map %>%
      select(COUNTYNBR, COUNTY_POP = POPULATION, POPULATION_CNTY_BALANCE),
    by = "COUNTYNBR"
  ) %>%
  ungroup() %>%
  group_by(CNTY) %>%
  reframe(
    n_cities = n_distinct(NAME),
    n_cities_w_city_service = n_distinct(NAME[city_service != "None"]),
    n_cities_w_county_service_only = n_distinct(NAME[
      city_service == "None" &
        county_service != "None" &
        bookmobile_service == "None" &
        agreed_service_city == "None" &
        agreed_service_county == "None"
    ]),
    county_population = COUNTY_POP,
    county_balance_pop = POPULATION_CNTY_BALANCE,

    ## Population with access to..
    pop_access_city = sum(POPULATION[city_service != "None"], na.rm = T),
    # Agreed service as main service provider
    pop_access_agreed_city = sum(
      POPULATION[agreed_service_city != "None"],
      na.rm = T
    ),
    pop_access_county = sum(POPULATION[county_service != "None"], na.rm = T),
    pop_access_bookmobile = sum(
      POPULATION[bookmobile_service != "None"],
      na.rm = T
    ),

    # prep for unduplicated rowwise calculations; -1's will be replaced with populations
    pop_access_county_undup = ifelse(county_service != "None", -1, 0),
    pop_access_bookmobile_undup = ifelse(bookmobile_service != "None", -1, 0),
    pop_access_agreed_county_undup = ifelse(
      agreed_service_county != "None",
      -1,
      0
    )
  ) %>%
  distinct() %>%
  rowwise() %>%
  mutate(
    ## Add county balance to county-wide service areas
    pop_access_county = ifelse(
      pop_access_county > 0,
      pop_access_county + county_balance_pop,
      pop_access_county
    ),
    pop_access_bookmobile = ifelse(
      pop_access_bookmobile > 0,
      pop_access_bookmobile + county_balance_pop,
      pop_access_bookmobile
    ),

    # City as main service provider; if service through county or bookmobile, 0
    pop_access_city_undup = ifelse(
      pop_access_county == 0 &
        pop_access_bookmobile == 0,
      pop_access_city, #if no county-wide, city service only
      0
    ),
    pop_access_agreed_city_undup = ifelse(
      pop_access_county == 0 &
        pop_access_bookmobile == 0,
      pop_access_agreed_city, #if no county-wide, agreed city service only
      0
    ),
    # County as main service provider
    pop_access_county_undup = ifelse(
      pop_access_county_undup == -1,
      county_population -
        pop_access_city -
        pop_access_bookmobile -
        pop_access_agreed_county_undup -
        pop_access_agreed_city,
      pop_access_county_undup
    ),
    # Bookmobile as main service provider
    pop_access_bookmobile_undup = ifelse(
      pop_access_bookmobile_undup == -1,
      county_population -
        pop_access_city -
        pop_access_county -
        pop_access_agreed_county_undup -
        pop_access_agreed_city,
      pop_access_bookmobile_undup
    ),
    pop_access_agreed_county_undup = ifelse(
      pop_access_agreed_county_undup == -1,
      county_balance_pop,
      pop_access_agreed_county_undup
    ),

    # Total People served in the county
    county_pop_served = ifelse(
      # if there is some kind of county wide service, serv pop is county pop
      pop_access_county != 0 |
        pop_access_bookmobile != 0 |
        pop_access_agreed_county_undup != 0,
      county_population,
      # if not, city serv pop plus agreed city serv pop
      pop_access_city + pop_access_agreed_city
    ),
    # population without service
    pop_no_serv = county_population - county_pop_served
  )

## Create the library locations df
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

rm(
  agreed_service_city,
  agreed_service_counties,
  bookmobile_counties,
  county_libs,
  city_libs
)

county_map %<>%
  left_join(municipalities_county_pop, by = c("NAME" = "CNTY")) %>%
  mutate(
    POPULATION = as.numeric(gsub(",", "", POPULATION)),

    agreed_service_county_for_label = case_when(
      pop_access_agreed_city != 0 ~ "Individual City Agreements",
      .default = agreed_service_county
    ),

    city_service = case_when(
      n_cities_w_city_service == 1 ~ paste0(
        n_cities_w_city_service,
        " City Library"
      ),
      n_cities_w_city_service > 1 ~ paste0(
        n_cities_w_city_service,
        " City Libraries"
      ),
      .default = "None"
    ),

    # Header Label
    label_header = paste0(
      "<div style='font-size:18px; font-weight:bold;'>",
      NAME,
      " County</div>",
      "<hr>",
      "<div style='font-size:14px; font-weight:bold;'>",
      current_year,
      " Library Service Areas</div>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>County Population:</td>",
      "<td style='text-align:right;'>",
      format(POPULATION, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>% of Population with Library Service:</td>",
      "<td style='text-align:right;'>",
      round((county_pop_served / POPULATION) * 100, 2),
      "%</td></tr>",
      "<tr><td style='text-align:left;'>Population With Library Service:</td>",
      "<td style='text-align:right;'>",
      format(county_pop_served, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Population Remainder:</td>",
      "<td style='text-align:right;'>",
      format(pop_no_serv, big.mark = ","),
      "</td></tr>",
      "</table>"
    ),

    # Population
    label_pops = paste0(
      "<hr>",
      "<div style='font-size:14px; font-weight:bold;'>Population With Library Service Through...</div>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>City Libraries:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_city, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>County Libraries:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_county, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Bookmobiles:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_bookmobile, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Service Agreements:</td>",
      "<td style='text-align:right;'>",
      format(
        pop_access_agreed_city + pop_access_agreed_county_undup,
        big.mark = ","
      ),
      "</td></tr>",
      "</table>"
    ),

    label_undup_pops = paste0(
      "<hr>",
      "<div style='font-size:14px; font-weight:bold;'>Population With Library Service ONLY Through...</div>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>City Libraries:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_city_undup, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>County Libraries:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_county_undup, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Bookmobiles:</td>",
      "<td style='text-align:right;'>",
      format(pop_access_bookmobile_undup, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Service Agreements:</td>",
      "<td style='text-align:right;'>",
      format(
        pop_access_agreed_city_undup + pop_access_agreed_county_undup,
        big.mark = ","
      ),
      "</td></tr>",
      "</table>"
    ),

    # Providers
    label_provider = paste0(
      "<hr>",
      "<div style='font-size:14px; font-weight:bold;'>Service Providers</div>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>City Libraries:</td>",
      "<td style='text-align:right;'>",
      city_service,
      "</td></tr>",
      "<tr><td style='text-align:left;'>County Libraries:</td>",
      "<td style='text-align:right;'>",
      county_service,
      "</td></tr>",
      "<tr><td style='text-align:left;'>Bookmobiles:</td>",
      "<td style='text-align:right;'>",
      bookmobile_service,
      "</td></tr>",
      "<tr><td style='text-align:left;'>Service Agreements:</td>",
      "<td style='text-align:right;'>",
      agreed_service_county_for_label,
      "</td></tr>",
      "</table>"
    ),

    # =======================
    # Combined Label & Popup
    # =======================
    service_label = paste0(label_header, "<hr><i>Click for more details</i>"),
    service_popup = paste0(
      label_header,
      label_pops,
      label_undup_pops,
      label_provider
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
