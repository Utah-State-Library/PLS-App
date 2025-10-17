##### Process Data #####

current_year <- 2024

#### Define Functions ####

## Initial readin to filter and cut columns
initial_readin <- function(df){
  
  # # Filter out flags & misc others 
  df %<>%
    select(!starts_with("C_") & !starts_with("F_") & !starts_with("FILL") & !starts_with("C19") &
             !ends_with("_D") & !ends_with("_DELETE") & !ends_with("_M") & !ends_with("_OUT") & !ends_with("_AE") & !ends_with("_CO") & !ends_with("_SL"))
  
  if("CITY_A" %in% names(df)){
    df %<>% rename("CITY" = "CITY_A")
  }
  if("TOT_PHYS" %in% names(df)){
    df %<>% rename("TOTPHYS" = "TOT_PHYS") # total physical items
  }
  if("PHMATCIR" %in% names(df)){
    df %<>% rename("PHYSCIR" = "PHMATCIR") # physical material circ
  }
  
  keep_cols <- variable_key %>% filter(APPCOL == 1)
  df <- df[, names(df) %in% c(keep_cols$SHORTNAME, "STABR")]
 
  return(df)
}

## Function to rename/add columns if necessary for rbind
add_cols <- function(df, cols) {
  
  add <- cols[!cols %in% names(df)]
  if(length(add) !=0 ) df[add] <- NA
  return(df)
}


#### Read in Data ####
variable_key <- read.csv("data/pls_variable_key.csv")
## File Path to IMLS processed datasets
path <- "I:/CCSL-Library/Public Libraries/Data/Data/Statistical Annual Reports/IMLS Data/IMLS PLS by year/"

## Read in data - IMLS processed is priority, then unprocessed PLS data if IMLS is unavailable (e.g., 2023 and on)
df03 <- read.csv(paste0(path, "PLS_FY2003 PUD_CSV/pupld03a.csv")) %>% mutate(FISCAL_YEAR = 2003) %>% initial_readin()
df04 <- read.csv(paste0(path, "PLS_FY2004 PUD_CSV/pupld04a.csv")) %>% mutate(FISCAL_YEAR = 2004) %>% initial_readin()
df05 <- read.csv(paste0(path, "PLS_FY2005 PUD_CSV/pupld05a.csv")) %>% mutate(FISCAL_YEAR = 2005) %>% initial_readin()
df06 <- read.csv(paste0(path, "PLS_FY2006 PUD_CSV/pupld06a.csv")) %>% mutate(FISCAL_YEAR = 2006) %>% initial_readin()
df07 <- read.csv(paste0(path, "PLS_FY2007 PUD_CSV/pupld07.csv")) %>% mutate(FISCAL_YEAR = 2007) %>% initial_readin()
df08 <- read.csv(paste0(path, "PLS_FY2008 PUD_CSV/pupld08a.csv")) %>% mutate(FISCAL_YEAR = 2008) %>% initial_readin()
df09 <- read.csv(paste0(path, "PLS_FY2009 PUD_CSV/pupld09a.csv")) %>% mutate(FISCAL_YEAR = 2009) %>% initial_readin()
df10 <- read.csv(paste0(path, "PLS_FY2010 PUD_CSV/pupld10a.csv")) %>% mutate(FISCAL_YEAR = 2010) %>% initial_readin()
df11 <- read.csv(paste0(path, "PLS_FY2011 PUD_CSV/pupld11b.csv")) %>% mutate(FISCAL_YEAR = 2011) %>% initial_readin()
df12 <- read.csv(paste0(path, "PLS_FY2012 PUD_CSV/Pupld12a.csv")) %>% mutate(FISCAL_YEAR = 2012) %>% initial_readin()
df13 <- read.csv(paste0(path, "PLS_FY2013 PUD_CSV/Pupld13a.csv")) %>% mutate(FISCAL_YEAR = 2013) %>% initial_readin()
df14 <- read.csv(paste0(path, "PLS_FY2014 PUD_CSV/PLS_FY2014_AE_pupld14a.csv")) %>% mutate(FISCAL_YEAR = 2014) %>% initial_readin()
df15 <- read.csv(paste0(path, "PLS_FY2015 PUD_CSV/PLS_FY2015_AE_pupld15a.csv")) %>% mutate(FISCAL_YEAR = 2015) %>% initial_readin()
df16 <- read.csv(paste0(path, "PLS_FY2016 PUD_CSV/PLS_FY2016_AE_pupld16a_updated.csv")) %>% mutate(FISCAL_YEAR = 2016) %>% initial_readin()
df17 <- read.csv(paste0(path, "PLS_FY2017 PUD_CSV/PLS_FY17_AE_pud17i.csv")) %>% mutate(FISCAL_YEAR = 2017) %>% initial_readin()
df18 <- read.csv(paste0(path, "PLS_FY2018 PUD_CSV/pls_fy18_ae_pud18i.csv")) %>% mutate(FISCAL_YEAR = 2018) %>% initial_readin()
df19 <- read.csv(paste0(path, "PLS_FY2019 PUD_CSV/PLS_FY19_AE_pud19i.csv")) %>% mutate(FISCAL_YEAR = 2019) %>% initial_readin()
df20 <- read.csv(paste0(path, "PLS_FY2020 PUD_CSV/PLS_FY20_AE_pud20i.csv")) %>% mutate(FISCAL_YEAR = 2020) %>% initial_readin()
df21 <- read.csv(paste0(path, "PLS_FY2021 PUD_CSV/PLS_FY21_AE_pud21i.csv")) %>% mutate(FISCAL_YEAR = 2021) %>% initial_readin()
df22 <- read.csv(paste0(path, "PLS_FY2022 PUD_CSV/PLS_FY22_AE_pud22i.csv")) %>% mutate(FISCAL_YEAR = 2022) %>% initial_readin()
df23 <- read.csv(paste0(path, "PLS_FY2023 PUD_CSV/PLS_FY23_AE_pud23i.csv")) %>% mutate(FISCAL_YEAR = 2023) %>% initial_readin()

## Data from LIBPAS because IMLS version is unavailable
df24 <- openxlsx::read.xlsx("I:/CCSL-Library/Public Libraries/Data/Data/Statistical Annual Reports/LibPAS/PLS Yearly Export/PLS_24.xlsx") %>% rename("FISCAL_YEAR" = "Period_ID") %>% initial_readin()


# Get all the column names and identify ones to remove (add to initial_readin() function above)
cols <- c(colnames(df03), colnames(df04), colnames(df05), colnames(df06),
          colnames(df07), colnames(df08), colnames(df09), colnames(df10), colnames(df11), colnames(df12),
          colnames(df13), colnames(df14), colnames(df15), colnames(df16), colnames(df17), colnames(df18),
          colnames(df19), colnames(df20), colnames(df21), colnames(df22), colnames(df23), colnames(df24)
          ) %>% unique()

cols %>% sort()

setdiff(colnames(df24), colnames(df22))
setdiff(colnames(df22), colnames(df24))

#### Standardize Columns and Combine Data ####

## Add in column names from above for rbind below
df03 %<>% add_cols(cols) %>% select(all_of(cols))
df04 %<>% add_cols(cols) %>% select(all_of(cols))
df05 %<>% add_cols(cols) %>% select(all_of(cols))
df06 %<>% add_cols(cols) %>% select(all_of(cols))
df07 %<>% add_cols(cols) %>% select(all_of(cols))
df08 %<>% add_cols(cols) %>% select(all_of(cols))
df09 %<>% add_cols(cols) %>% select(all_of(cols))
df10 %<>% add_cols(cols) %>% select(all_of(cols))
df11 %<>% add_cols(cols) %>% select(all_of(cols))
df12 %<>% add_cols(cols) %>% select(all_of(cols))
df13 %<>% add_cols(cols) %>% select(all_of(cols))
df14 %<>% add_cols(cols) %>% select(all_of(cols))
df15 %<>% add_cols(cols) %>% select(all_of(cols))
df16 %<>% add_cols(cols) %>% select(all_of(cols))
df17 %<>% add_cols(cols) %>% select(all_of(cols))
df18 %<>% add_cols(cols) %>% select(all_of(cols))
df19 %<>% add_cols(cols) %>% select(all_of(cols))
df20 %<>% add_cols(cols) %>% select(all_of(cols))
df21 %<>% add_cols(cols) %>% select(all_of(cols))
df22 %<>% add_cols(cols) %>% select(all_of(cols))
df23 %<>% add_cols(cols) %>% select(all_of(cols))
df24 %<>% add_cols(cols) %>% select(all_of(cols)) %>% mutate(STABR = "UT")


## Create National Dataset ##
df_nat <- rbind(df03, df04, df05, df06, df07, df08,
                df09, df10, df11, df12, df13,
                df14, df15, df16, df17, df18,
                df19, df20, df21, df22, df23
                ) # No national yet for 2024

df_nat[df_nat == "-9"] <- NA

df_nat %<>%
  group_by(STABR, FISCAL_YEAR) %>%
  summarise(sum_POPU_LSA = sum(POPU_LSA, na.rm = T),
            total_libs = n_distinct(FSCSKEY, na.rm = T),
            sum_CENTLIB = sum(CENTLIB, na.rm = T),
            sum_BRANLIB = sum(BRANLIB, na.rm = T),
            sum_TOTSTAFF = sum(TOTSTAFF, na.rm = T),
            sum_MASTER = sum(MASTER, na.rm = T),
            sum_LIBRARIA = sum(LIBRARIA, na.rm = T),
            sum_OTHPAID = sum(OTHPAID, na.rm = T),
            sum_LOCGVT = sum(LOCGVT, na.rm = T),
            sum_STGVT = sum(STGVT, na.rm = T),
            sum_FEDGVT = sum(FEDGVT, na.rm = T),
            sum_SALARIES = sum(SALARIES, na.rm = T),
            sum_STAFFEXP = sum(STAFFEXP, na.rm = T),
            sum_PRMATEXP = sum(PRMATEXP, na.rm = T),
            sum_ELMATEXP = sum(ELMATEXP, na.rm = T),
            sum_OTHMATEX = sum(OTHMATEX, na.rm = T),
            sum_TOTEXPCO = sum(TOTEXPCO, na.rm = T),
            sum_OTHOPEXP = sum(OTHOPEXP, na.rm = T),
            sum_TOTOPEXP = sum(TOTOPEXP, na.rm = T),
            sum_BKVOL = sum(BKVOL, na.rm = T),
            sum_VISITS = sum(VISITS, na.rm = T),
            sum_TOTCIR = sum(TOTCIR, na.rm = T),
            sum_PHYSCIR = sum(PHYSCIR, na.rm = T),
            sum_OTHPHCIR = sum(OTHPHCIR, na.rm = T),
            sum_ELMATCIR = sum(ELMATCIR, na.rm = T),
            sum_KIDCIRCL = sum(KIDCIRCL, na.rm = T),
            sum_GPTERMS = sum(GPTERMS, na.rm = T),
            sum_TOTPRO = sum(TOTPRO, na.rm = T),
            sum_TOTATTEN = sum(TOTATTEN, na.rm = T),
            sum_KIDPRO = sum(KIDPRO, na.rm = T),
            sum_WIFISESS = sum(WIFISESS, na.rm = T))

### Filter dfs down to just Utah for the rbind

df03 %<>% filter(STABR == "UT") 
df04 %<>% filter(STABR == "UT")
df05 %<>% filter(STABR == "UT")
df06 %<>% filter(STABR == "UT")
df07 %<>% filter(STABR == "UT")
df08 %<>% filter(STABR == "UT") 
df09 %<>% filter(STABR == "UT") 
df10 %<>% filter(STABR == "UT")
df11 %<>% filter(STABR == "UT")
df12 %<>% filter(STABR == "UT")
df13 %<>% filter(STABR == "UT")
df14 %<>% filter(STABR == "UT") 
df15 %<>% filter(STABR == "UT") 
df16 %<>% filter(STABR == "UT")
df17 %<>% filter(STABR == "UT")
df18 %<>% filter(STABR == "UT")
df19 %<>% filter(STABR == "UT")
df20 %<>% filter(STABR == "UT") 
df21 %<>% filter(STABR == "UT") 
df22 %<>% filter(STABR == "UT")
df23 %<>% filter(STABR == "UT")
df24 %<>% filter(STABR == "UT")

## Bind all datasets
df_all <- rbind(df03, df04, df05, df06, df07, df08,
                df09, df10, df11, df12, df13,
                df14, df15, df16, df17, df18,
                df19, df20, df21, df22, df23, df24)



#### Extra Tidying ####

# Remove some empties that are artifacts from LIBPAS export (for non-IMLS files)
df_all %<>% filter(!is.na(FSCSKEY), FSCSKEY != "59830", FSCSKEY != "-1", !is.na(LIBNAME))

df_all[df_all == "-1"] <- NA
df_all[df_all == "-3"] <- NA

# Remove any columns that are fully empty
df_all <- df_all[,colSums(is.na(df_all))<nrow(df_all)]

# Adjust names to have a current name column based on the most recent year we have data for a given library
current_names <- df_all %>% select(FISCAL_YEAR, FSCSKEY, LIBID, LIBNAME) %>% group_by(FSCSKEY, LIBID) %>% filter(FISCAL_YEAR == max(FISCAL_YEAR)) %>% select(-FISCAL_YEAR) %>% rename("CURRENT_LIBNAME" = "LIBNAME")

df_all %<>% left_join(current_names, by = c("FSCSKEY", "LIBID"))

df_all %<>% 
  select(FISCAL_YEAR, FSCSKEY, LIBID, CURRENT_LIBNAME, LIBNAME, everything(), -STABR) %>%
  mutate(CURRENT_LIBNAME = str_to_title(CURRENT_LIBNAME))


df_all %<>%
  mutate(across(!c("FSCSKEY", "LIBID", "CURRENT_LIBNAME", "LIBNAME", "ADDRESS", "CITY", "ZIP", "CNTY", "VISITRPT", "REFERRPT", "PITUSRRPT", "WIFISRPT", "ODFINE", "PROUD_MOMENT", "BIGGEST_CHALLENGE", "AUTORENEW"), ~ as.numeric(.)))

rm(df03, df04, df05, df06, df07, df08,
   df09, df10, df11, df12, df13,
   df14, df15, df16, df17, df18,
   df19, df20, df21, df22, df23, df24, current_names)


#### Add in State Numbers from LIBPAS data ####

## Read in PLS data and add state-specific columns that are not in IMLS data
# df <- read.csv("I:/CCSL-Library/Public Libraries/Data/Data/Statistical Annual Reports/LibPAS/LibPAS Combined Data and Info Files/PLS_ALL_YEARS.csv")
# 
# df %<>% 
#   select("FISCAL_YEAR" = "Period_ID", "FSCSKEY", "LIBID", "LOCEXP", "STEXP", "FEDEXP", "OTHEXP", "TOTEXP") %>% 
#   filter(!is.na(LIBID))
# 
# ## Join
# df_all %<>% 
#   select(-c("LOCEXP", "STEXP", "FEDEXP", "OTHEXP", "TOTEXP")) %>% 
#   left_join(df, by = c("FISCAL_YEAR", "LIBID", "FSCSKEY"))
# 
# df_all %<>% 
#   filter(!str_detect(CURRENT_LIBNAME, "Bookmobile"), !is.na(POPU_LSA)) # Remove Bookmobiles and Garden City (closed)
# 
# df_st <- df_all %>%
#   mutate(across(c("FSCSKEY", "LIBID", "CURRENT_LIBNAME", "LIBNAME", "ADDRESS", "CITY", "ZIP", "CNTY", "VISITRPT", "REFERRPT", "PITUSRRPT", "WIFISRPT", "ODFINE", "PROUD_MOMENT", "BIGGEST_CHALLENGE", "AUTORENEW"), ~ NA)) %>%
#   group_by(FISCAL_YEAR) %>%
#   mutate(across(!c("FSCSKEY", "LIBID", "CURRENT_LIBNAME", "LIBNAME", "ADDRESS", "CITY", "ZIP", "CNTY", "VISITRPT", "REFERRPT", "PITUSRRPT", "WIFISRPT", "ODFINE", "PROUD_MOMENT", "BIGGEST_CHALLENGE", "AUTORENEW"), ~ sum(., na.rm = T))) %>%
#   mutate(CURRENT_LIBNAME = "All Libraries") %>%
#   distinct()
# 
# df_all %<>% rbind(df_st)
# 
# rm(df)


#setdiff(names(df_all), variable_key$SHORTNAME)


##### Manual Adjustments #####

# In 2023 LibPAS didn't sum up circ counts correctly
df_all %<>%
  group_by(CURRENT_LIBNAME, FISCAL_YEAR) %>%
  mutate(TOTCIR_libpas = TOTCIR,
         TOTCIR = ELMATCIR + PHYSCIR) %>%
  ungroup()


##### Make DFs #####

###### Programs ######
program_cols <- variable_key %>% filter(GROUP %in% c("Children (Ages 0-5)", "Ya/Teens (Ages 12-18)", "Adults (Ages 19+)", "Other/Family/All Ages", "Self-Directed Activities")) %>% pull(SHORTNAME)

programs <- df_all %>% 
  filter(FISCAL_YEAR >= 2023) %>%
  select(FISCAL_YEAR, FSCSKEY, LIBID, CURRENT_LIBNAME, all_of(program_cols)) %>% 
  pivot_longer(
    !c(FISCAL_YEAR, FSCSKEY, LIBID, CURRENT_LIBNAME),
    names_to = c("LOCATION", "GROUP", "MEASURE"),
    names_pattern = "(.*)_(.*)_(.*)",
    values_to = "VALUE"
  ) %>% 
  mutate(VALUE = ifelse(VALUE %in% c("-1", "-2", "-3"), NA, VALUE)) %>%
  filter(!is.na(VALUE)) %>%
  mutate(GROUP = case_when(GROUP == "BABY" ~ "Kids 0-5",
                           GROUP == "CHILD" ~ "Kids 6-11",
                           GROUP == "YA" ~ "Teens 12-18",
                           GROUP == "ADULT" ~ "Adults 19+",
                           GROUP == "GENERAL" ~ "General Interest"),
         LOCATION = case_when(LOCATION == "INPERSON" ~ "In Person",
                              LOCATION == "OFFSITE" ~ "Offsite",
                              LOCATION == "LIVE" ~ "Live Virtual",
                              LOCATION == "RECORDED" ~ "Recorded",
                              LOCATION == "SELFDIR" ~ "Self Directed"),
         MEASURE = case_when(MEASURE == "PRO" ~ "Number of Programs",
                             MEASURE == "ATTEND" ~ "Attendance"))

###### Financial ######

financial_cols <- variable_key %>% filter(GROUP %in% c("Operating Revenue", "Operating Expenditures", "Staff Expenditures", "Collection Expenditures", "Other Operating Expenditures", "Total Operating Expenditures", "Capital Revenue", "Capital Expenditures")) %>% pull(SHORTNAME)

financials <- df_all %>% 
  select(FISCAL_YEAR, FSCSKEY, CURRENT_LIBNAME, all_of(financial_cols)) %>%
  mutate(across(financial_cols, ~ as.numeric(.))) %>%
  pivot_longer(!c(FISCAL_YEAR, FSCSKEY, CURRENT_LIBNAME),
               names_to = "METRIC",
               values_to = "VALUE") %>%
  mutate(VALUE = ifelse(VALUE < 0, NA, VALUE))

financial_indics <- variable_key %>% 
  filter(GROUP %in% c("Operating Revenue", "Staff Expenditures", "Collection Expenditures", "Other Operating Expenditures", "Total Operating Expenditures", "Capital Revenue", "Capital Expenditures"
                      #, "Operating Expenditures"
  )) %>%
  select(-APPCOL)

financials %<>% 
  left_join(financial_indics, by = c("METRIC" = "SHORTNAME")) %>% 
  filter(!is.na(INDICATOR))

## total expense - staff expense


# ##### Outlets #####
# 
# # Individual Library Lat/Long
librarykey <- openxlsx::read.xlsx("I:/CCSL-Library/Public Libraries/Data/Data/Library Directories/LibraryKey.xlsx", sheet = "Libraries") %>% filter(!is.na(FSCS_ID), LIBRARY_TYPE != "Bookmobile") %>% select(FSCS_ID, ADMINISTRATIVE_ENTITY_NAME, LIB_CODE, FSCS_ID_SEQ, LIBRARY_NAME, LIBRARY_TYPE, ADDRESS, CITY, ZIP, COUNTY, LAT, LONG) %>% distinct()


## Save
saveRDS(df_all, "./data/pls_all.rds")
saveRDS(programs, "./data/programs.rds")
saveRDS(librarykey, "./data/librarykey.rds")
saveRDS(financials, "./data/financials.rds")




# 
# ## Test
# x <- df_all %>% filter(str_detect(LIBNAME, "GRAND"))
# 
# 
# #### Make DFs ####
# 
# CY_df <- df_all %>% filter(FISCAL_YEAR == max(FISCAL_YEAR)) %>% select(FSCSKEY, LIBID, CURRENT_LIBNAME, ADDRESS, CITY, ZIP, CNTY, POPU_LSA)


