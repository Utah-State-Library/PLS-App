### Checks ###

## Check: Do the Outlet and PLS files contain the same AEs?
pls_aes <- pls %>% select(FISCAL_YEAR, CURRENT_LIBNAME) %>% distinct()
outlet_aes <- outlets %>%
  select(FISCAL_YEAR, CURRENT_LIBNAME = CURRENT_LIBNAME_AE) %>%
  distinct()

setdiff(
  pls_aes$CURRENT_LIBNAME[pls_aes$FISCAL_YEAR == 2024],
  outlet_aes$CURRENT_LIBNAME
)
setdiff(
  outlet_aes$CURRENT_LIBNAME,
  pls_aes$CURRENT_LIBNAME[pls_aes$FISCAL_YEAR == 2024]
)

rm(outlet_aes, pls_aes)


## Check: No duplicate AEs by year
pls %>%
  group_by(FISCAL_YEAR, CURRENT_LIBNAME) %>%
  summarise(n = n()) %>%
  filter(n > 1)
outlets %>%
  group_by(FISCAL_YEAR, CURRENT_LIBNAME_OUTLET) %>%
  summarise(n = n()) %>%
  filter(n > 1)


## Check NAs: which columns have > 0% NA

# PLS
pls_nas <- data.frame(col = character(), na_pct = numeric())
pls_24 <- pls[pls$FISCAL_YEAR == 2024, ]
for (col_p in colnames(pls_24)) {
  na_pct_p <- pls_24 %>%
    select(col = col_p) %>%
    summarise(n_na = sum(is.na(col)), pct = (sum(is.na(col)) / n()) * 100) %>%
    pull(pct)

  df_p <- data.frame(col = col_p, na_pct = na_pct_p)
  pls_nas %<>% rbind(df_p)
}
pls_nas %>% filter(na_pct > 0)

# Outlets
outlet_nas <- data.frame(col = character(), na_pct = numeric())
outlet_24 <- outlets[outlets$FISCAL_YEAR == 2024, ]
for (col_p in colnames(outlet_24)) {
  na_pct_p <- outlet_24 %>%
    select(col = col_p) %>%
    summarise(n_na = sum(is.na(col)), pct = (sum(is.na(col)) / n()) * 100) %>%
    pull(pct)

  df_p <- data.frame(col = col_p, na_pct = na_pct_p)
  outlet_nas %<>% rbind(df_p)
}
outlet_nas %>% filter(na_pct > 0)


rm(outlet_nas, pls_nas, outlet_24, pls_24, col_p)
