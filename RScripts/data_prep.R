#### Any data prep that needs to happen in-project goes here

#### Service Area Map Prep ####
# We are doing this in-project because this is where the shapefile data and up to date outlet data are stored

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


#####################################

#### READ IN UPDATED VERSION AND SAVE
# census_by_city <- googlesheets4::read_sheet(
#   "https://docs.google.com/spreadsheets/d/1Ch9ZM6AxTyy9bG8w9qo_0gOG6wNvlk2jIZ60K4eT5K0/edit?usp=sharing",
#   sheet = "Census Data - By County"
# ) %>%
#   filter(PLACE != 0)

# saveRDS(census_by_city, "data/census_by_city.RDS")

census_by_city <- readRDS("data/census_by_city.RDS")

## Make a crosswalk df to get county names into the municipality df
county_xwalk <- county_shp %>%
  select(CNTY = NAME, COUNTYNBR, COUNTY_FIPS) %>%
  mutate(CNTY = str_to_title(CNTY), COUNTYNBR = as.numeric(COUNTYNBR)) %>%
  st_drop_geometry()

census_by_city %<>%
  left_join(county_xwalk, by = c("COUNTY" = "COUNTY_FIPS"))


municipalities_map <-
  left_join(
    municipalities %>%
      mutate(COUNTYNBR = as.numeric(COUNTYNBR)),
    census_by_city %>%
      select(-NAME),
    by = c("CITY_FIPS" = "PLACE", "COUNTYNBR")
  )
# Bluffdale, Draper, Park City, Santaquin span counties

municipalities_map %<>%
  mutate(
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
      format(POPESTIMATE, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Library:</td>",
      "<td style='text-align:right;'>",
      Library_1,
      "</td></tr>",
      "</table>"
    ),

    # Population
    label_libs = paste0(
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>Library Type:</td>",
      "<td style='text-align:right;'>",
      `Library_1 Type`,
      "</td></tr>",
      "</table>"
    ),
    # Population
    label_libs_2plus = paste0(
      "<hr>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>Additional Library Service:</td>",
      "<td style='text-align:right;'>",
      Library_2,
      "</td></tr>",
      "<tr><td style='text-align:left;'>Library Type:</td>",
      "<td style='text-align:right;'>",
      `Library_2 Type`,
      "</td></tr>",
      "</table>"
    ),

    # Combined Label & Popup
    service_label = paste0(label_header, "<hr><i>Click for more details</i>"),
    service_popup = ifelse(
      is.na(Library_2), # if no secondary library
      paste0(
        label_header,
        label_libs
      ), # else yes secondary library, add the extra bit to the popup
      paste0(label_header, label_libs, label_libs_2plus)
    )
  )


#####

#### READ IN UPDATED VERSION AND SAVE
# census_by_county <- googlesheets4::read_sheet(
#   "https://docs.google.com/spreadsheets/d/1Ch9ZM6AxTyy9bG8w9qo_0gOG6wNvlk2jIZ60K4eT5K0/edit?usp=sharing",
#   sheet = "Census Data - By County"
# ) %>%
#   filter(PLACE != 0) %>%
#   group_by(COUNTY) %>%
#   reframe(
#     total_pop = sum(POPESTIMATE),
#     total_service_pop = sum(POPESTIMATE[
#       `Library_1 Type` != "No Library Service"
#     ]),
#     total_cert_service_pop = sum(POPESTIMATE[
#       !`Library_1 Type` %in% c("No Library Service", "Non-Certified")
#     ]),
#     city_pop = sum(POPESTIMATE[`Library_1 Type` == "City"]),
#     county_pop = sum(POPESTIMATE[`Library_1 Type` == "County"]),
#     bookmobile_pop = sum(POPESTIMATE[`Library_1 Type` == "Bookmobile"]),
#     agreed_service_pop = sum(POPESTIMATE[`Library_1 Type` == "Agreed Service"]),
#     noncertified_city_pop = sum(POPESTIMATE[
#       `Library_1 Type` == "Non-Certified"
#     ]),
#     noncertified_county_pop = sum(POPESTIMATE[
#       `Library_1 Type` == "Non-Certified County"
#     ]),
#     no_service_pop = sum(POPESTIMATE[`Library_1 Type` == "No Library Service"])
#   ) %>%
#   ungroup()

# saveRDS(census_by_county, "data/census_by_county.RDS")

census_by_county <- readRDS("data/census_by_county.RDS")

county_map <- county_shp %>%
  left_join(census_by_county, by = c("COUNTY_FIPS" = "COUNTY")) %>%
  mutate(NAME = str_to_title(NAME))

county_map %<>%
  mutate(
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
      format(total_pop, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>% of Population with Library Service:</td>",
      "<td style='text-align:right;'>",
      round((total_service_pop / total_pop) * 100, 2),
      "%</td></tr>",
      "<tr><td style='text-align:left;'>Population With Library Service:</td>",
      "<td style='text-align:right;'>",
      format(total_service_pop, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Population Remainder:</td>",
      "<td style='text-align:right;'>",
      format(no_service_pop, big.mark = ","),
      "</td></tr>",
      "</table>"
    ),

    # Population
    label_pops = paste0(
      "<hr>",
      "<div style='font-size:14px; font-weight:bold;'>Service Area Population Breakdown</div>",
      "<table style='width:100%; font-size:13px;'>",
      "<tr><td style='text-align:left;'>City Libraries:</td>",
      "<td style='text-align:right;'>",
      format(city_pop, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>County Libraries:</td>",
      "<td style='text-align:right;'>",
      format(county_pop, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Bookmobiles:</td>",
      "<td style='text-align:right;'>",
      format(bookmobile_pop, big.mark = ","),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Service Agreements:</td>",
      "<td style='text-align:right;'>",
      format(
        agreed_service_pop,
        big.mark = ","
      ),
      "</td></tr>",
      "<tr><td style='text-align:left;'>Non-Certified Libraries:</td>",
      "<td style='text-align:right;'>",
      format(
        noncertified_city_pop + noncertified_county_pop,
        big.mark = ","
      ),
      "</td></tr>",
      "</table>"
    ),

    # Combined Label & Popup
    service_label = paste0(label_header, "<hr><i>Click for more details</i>"),
    service_popup = paste0(
      label_header,
      label_pops
    )
  )


###############################

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
