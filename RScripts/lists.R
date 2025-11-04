#### Input Lists ####

years <- pls %>%
  reframe(unique(FISCAL_YEAR)) %>%
  pull() %>%
  sort(decreasing = TRUE)

libnames <- pls %>%
  filter(
    CURRENT_LIBNAME != "All Libraries",
    FISCAL_YEAR == current_year, ## TODO, handle this in servers with updatePicker logic
    hide_lib == 0
  ) %>%
  select(CURRENT_LIBNAME) %>%
  distinct() %>%
  arrange(CURRENT_LIBNAME) %>%
  pull()

current_FSCS <- pls %>%
  filter(FISCAL_YEAR == current_year, hide_lib == 0) %>%
  reframe(FSCSKEY) %>%
  unique() %>%
  pull()

counties <- county_shp %>%
  reframe(unique(NAME)) %>%
  pull() %>%
  sort()

ae_name <- outlets %>%
  filter(FSCSKEY %in% current_FSCS) %>%
  reframe(unique(CURRENT_LIBNAME_AE)) %>%
  pull() %>%
  sort()


#### Special Handling Columns

## Per Capita cases where the column needs to be calculated per 1000 people
per1000_cols <- c(
  "TOTSTAFF",
  "GPTERMS",
  "HOTSPOT",
  "K0_5PRO",
  "K6_11PRO",
  "YAPRO",
  "ADULTPRO",
  "GENPRO",
  "TOTPRO"
)

## Currency Columns
currency_cols <- c(
  "TOTOPEXP",
  "STAFFEXP",
  "TOTEXPCO",
  "OTHOPEXP",
  "SALARIES",
  "BENEFIT",
  "PRMATEXP",
  "ELMATEXP",
  "OTHMATEX",
  "TOTINCM",
  "LOCGVT",
  "STGVT",
  "FEDGVT",
  "OTHINCM"
)
