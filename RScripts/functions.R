

get_valuebox <- function(df, year, col, pull){
  
    
  # ensure numeric
  year <- as.numeric(year)
    
    valueCY <- df %>% 
      filter(FISCAL_YEAR == year) %>% 
      reframe(out = sum(!!sym(col))) %>%
      pull(out)
    
    valuePY <- df %>% 
      filter(FISCAL_YEAR == year - 1) %>% 
      reframe(out = sum(!!sym(col))) %>%
      pull(out)
    
    # Define Pull values - Current Year (CY), Previous Year (PY), and Change
    
    CY <- if (!is.na(valueCY)) format(valueCY, big.mark = ",") else "No Data"
    PY <- if (!is.na(valuePY)) paste0(as.character(year - 1), ": ", format(valuePY, big.mark = ",")) else ""
    
    valueChange <- round(((valueCY-valuePY)/valuePY)*100, 2)
    
    
    if(year - 1 == 2002){
      change <- ""
      PY <- ""
    } else if(is.infinite(valueChange) | is.na(valueChange)){
      change <- ""
    } else if (valueChange > 0){
      change <- paste0("Increase: ▲", valueChange, "%")
    } else if(valueChange < 0){
      change <- paste0("Decrease: ▼", valueChange, "%")
    } else {
      change <- paste0("No Change from ", as.character(year - 1))
    }

    # Pull whichever value we need
    get(pull)
}

  

 
get_value_over_time_1lib <- function(df, 
                                     values = c(), 
                                     ytt_format = "{point.y:.0,f}",
                                     grouptext = "", 
                                     xtext = "Fiscal Year", 
                                     ytext = "", 
                                     ttext = "",
                                     year_range = (current_year-5):current_year){
  
  shiny::validate(
    need((nrow(df) != 0), "")
  )
  
  df %<>% filter(FISCAL_YEAR %in% year_range)
  
  #df <- pls %>% filter(str_detect(CURRENT_LIBNAME, "Grand"))
  
  df %<>% 
    select(CURRENT_LIBNAME, FISCAL_YEAR, values) %>%
    pivot_longer(!c(CURRENT_LIBNAME, FISCAL_YEAR),
                 names_to = "COLS",
                 values_to = "VALUE") %>%
    left_join(variable_key, by = c("COLS" = "SHORTNAME")) %>%
    mutate(VALUE = as.numeric(VALUE),
           FISCAL_YEAR = as.numeric(FISCAL_YEAR))
  
  hc <- highchart() %>%
    hc_add_series(df, 
                  type = "line",
                  hcaes(x = FISCAL_YEAR, 
                        y = VALUE,
                        group = INDICATOR)
    )
  
  hc %<>%
    hc_yAxis(title = list(text = ytext)) %>%
    hc_xAxis(title = list(text = xtext),
             allowDecimals = FALSE) %>%
    hc_tooltip(pointFormat = paste0("<b>{point.INDICATOR}: ",ytt_format,"</b><br>{point.x}"),
               headerFormat = "") %>%
    hc_title(text = paste0(ttext)) %>%
    hc_subtitle(text = unique(df$CURRENT_LIBNAME)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE)))
  
  hc
} 


# per_capita_fmt <- function(popu_lsa = POPU_LSA, 
#                            col, 
#                            label = FALSE)
#   {
#   
#   if (!label){
#     if(col %in% c("TOTSTAFF", "GPTERMS", "HOTSPOT")){
#       (col / popu_lsa) * 1000
#     } else {
#       (col / popu_lsa)
#     }
#   } else if (label) {
#     if(col %in% c("TOTSTAFF", "GPTERMS", "HOTSPOT")){
#       "Per 1000"
#     } else {
#       "Per Capita"
#     }
#   }
# }



get_nclosest <- function(df, n, libname, pg_col, percap = FALSE){
  # use pls as df
  per1000_cols <- per1000_cols
  
  df_mod <- df %>%
    select(CURRENT_LIBNAME, POPU_LSA, value = !!sym(pg_col)) %>%
    mutate(
      value = as.numeric(value)  # convert actual values, not column name string
      )
  
  if(percap & pg_col != "POPU_LSA"){
    df_mod <- df_mod %>%
      mutate(value = case_when(
        percap & pg_col %in% per1000_cols ~ value / (POPU_LSA / 1000),  # per 1000 people
        percap & !(pg_col %in% per1000_cols) ~ value / POPU_LSA,        # per capita
        TRUE ~ value                                                    # raw value
      ))
  }
  
  target_value <- df_mod %>% 
    filter(CURRENT_LIBNAME == libname) %>% 
    pull(value)
  
  df_diff <- df_mod %>% 
    rowwise() %>%
    mutate(difference = abs(value - target_value)) %>%
    arrange(difference)
  
  # Select the top 'n' rows + 1 for the target library
  closest_values <- head(df_diff, n + 1)
  
  closest_values %>% pull(CURRENT_LIBNAME)
}


csvDownloadButton <- function(id, filename = "data.csv", label = "Download Current Table as CSV") {
  tags$button(
    class = "btn btn-default",
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}



format_currency_cells <- function(value) {
  if (!is.na(value) && value == -9) {
    "Masked"
  } else if (!is.na(value) && value == -3) {
    "Missing"
  } else if (!is.na(value) && value == -1) {
    "Missing"
  } else {
    paste0("$", formatC(value, format = "f", big.mark = ",", digits = 0))
  }
}

format_currency_2_decimals <- function(value) {
  if (!is.na(value) && value == -9) {
    "Masked"
  } else if (!is.na(value) && value == -3) {
    "Missing"
  } else if (!is.na(value) && value == -1) {
    "Missing"
  } else {
    paste0("$", formatC(value, format = "f", big.mark = ",", digits = 2))
  }
}


render_table <- function(data, cols, variable_key,
                         order_col = NULL, # which column to order table by
                         per_cap = FALSE, # are we showing per capita?
                         peer = FALSE, # is this a peer library table?
                         target_lib = NULL, # for peer library table, which is the target lib (for bolding/styling)
                         peer_col = NULL, # add peer group column to left sticky columns for peer tables
                         color_table = FALSE # remove later once we figure out how best to style
                         ) { 

  # ensure logical
  per_cap <- as.logical(per_cap)
  color_table <- as.logical(color_table)
  
  key <- variable_key %>% dplyr::filter(SHORTNAME %in% c(cols, if (!is.null(peer_col)) peer_col))
  keylist <- split(key$INDICATOR, key$SHORTNAME)
  
  # Pre-define currency columns
  currency_cols <- c("TOTOPEXP", "STAFFEXP", "TOTEXPCO", "OTHOPEXP", "SALARIES", "BENEFIT",
                     "PRMATEXP","ELMATEXP", "OTHMATEX",
                     "TOTINCM", "LOCGVT", "STGVT", "FEDGVT", "OTHINCM")
  per1000_cols <- per1000_cols
  
  # Apply per capita transformations
  if (per_cap) {
    all_cols <- c(cols, peer_col)
    per_cap_cols <- setdiff(all_cols, "POPU_LSA")
    
    data <- data %>%
      rowwise() %>%
      mutate(across(
        all_of(per_cap_cols), 
        ~ ifelse(
          cur_column() %in% per1000_cols,
          round(. / (POPU_LSA / 1000), 2),
          round(. / POPU_LSA, 2)
        )
      ))
  }
  
  # Define static columns (the sticky ones)
  static_columns <- list(
    CURRENT_LIBNAME = colDef(name = "Library",
                             maxWidth = 125,
                             sticky = "left",
                             style = if (!peer) list(backgroundColor = "#f7f7f7") else 
                               # if we're doing a peer library table, make the target library bold
                               function(value) {
                                 if (value == target_lib) {
                                   fontweight = "bold"
                                 } else {
                                   fontweight = 300
                                 }
                                 list(fontWeight = fontweight,
                                      backgroundColor = "#f7f7f7")
                               }),
    FISCAL_YEAR = colDef(name = "Year",
                         maxWidth = 75,
                         sticky = "left",
                         style = list(backgroundColor = "#f7f7f7")),
    POPU_LSA = colDef(name = "Population of Legal Service Area",
                      maxWidth = 125,
                      format = colFormat(separators = TRUE),
                      sticky = "left",
                      style = list(backgroundColor = "#f7f7f7"))
  )
  
  # Define peer column if applicable
  if (peer) {
    if(peer_col != "POPU_LSA"){
      peer_column <- lapply(peer_col, function(col) {
        is_currency <- col %in% currency_cols
        
        display_name <- keylist[[col]] %||% col
        
        if (per_cap) {
          display_name <- paste0(display_name, if (col %in% per1000_cols) " (Per 1,000)" else " (Per Capita)"
          )
        }
        
        colDef(
          name = display_name, 
          format = if (!is_currency) colFormat(separators = TRUE) else NULL,
          cell = if (is_currency) {
            format_currency_cells
          } else {
            NULL
          },
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        )
      }) %>% setNames(peer_col)
    } else if (peer_col == "POPU_LSA"){
      peer_column <- NULL
    } 
    
  } else if (!peer) {
    peer_column <- NULL
  }
  
  # Define dynamic columns
  dynamic_columns <- lapply(cols, function(col) {
    is_currency <- col %in% currency_cols
    
    display_name <- keylist[[col]] %||% col
    
    if (per_cap) {
      display_name <- paste0(display_name, if (col %in% per1000_cols) " (Per 1,000)" else " (Per Capita)"
                             )
    }
    
    colDef(
      name = display_name,
      format = if (!is_currency) {
        if (per_cap) colFormat(separators = TRUE, digits = 2)
        else colFormat(separators = TRUE)
      } else {
        NULL
      },
      cell = if (is_currency && !per_cap) {
        format_currency_cells
      } else if (is_currency && per_cap) {
        format_currency_2_decimals
      } else {
        NULL
      }
    )
  }) %>% setNames(cols)
  
  # Combine all columns
  all_columns <- c(
    static_columns,
    if (peer) peer_column,
    dynamic_columns
  )
  
  arrange_call <- c("FISCAL_YEAR")  # always sort by year descending
  
  if (!is.null(order_col)) {
    arrange_call <- c("FISCAL_YEAR", order_col)
  }
  
  # Create reactable
  data %>%
    select(CURRENT_LIBNAME, FISCAL_YEAR, POPU_LSA, if(peer) peer_col, all_of(cols)) %>%
    arrange(across(all_of(arrange_call), ~ desc(.))) %>% 
    reactable(
      resizable = TRUE,
      pagination = !peer, # base pagination on if it's a peer table or not (peer tables are not paginated)
      highlight = TRUE,
      defaultExpanded = FALSE,
      compact = TRUE,
      defaultColDef = colDef(align = "left",
                              style = if (color_table) color_scales(data, colors = c("#002F6C", "#0086BF", "#4EC3E0")) else NULL
                             ),
      theme = reactableTheme(
        headerStyle = list(
          background = "#ecf0f1",
          borderColor = "#555"
        )
      ),
      columns = all_columns
    )
}



#### Single Library Pct Change Table ####

render_pct_change_table <- function(df, variable_key, year, cols,
                                    percap = FALSE) 
  {
  
  year <- as.numeric(year)
  percap <- as.logical(percap)
  
  # Pre-define currency columns
  currency_cols <- c("TOTOPEXP", "STAFFEXP", "TOTEXPCO", "OTHOPEXP", "SALARIES", "BENEFIT",
                     "PRMATEXP","ELMATEXP", "OTHMATEX",
                     "TOTINCM", "LOCGVT", "STGVT", "FEDGVT", "OTHINCM")
  per1000_cols <- per1000_cols
  
  key <- variable_key %>% filter(SHORTNAME %in% c(cols, "POPU_LSA")) %>% select(INDICATOR, SHORTNAME)
  
  # Data for selected library
  lib_data <- df %>% 
    mutate(FISCAL_YEAR = as.numeric(FISCAL_YEAR)) %>%
    filter(FISCAL_YEAR %in% c(year - 1, year)) %>%
    select(FISCAL_YEAR, all_of(cols), POPU_LSA) %>%
    mutate(FISCAL_YEAR = ifelse(FISCAL_YEAR == year, "CY", "PY")) %>%
    pivot_longer(-c(FISCAL_YEAR, POPU_LSA), 
                 names_to = "SHORTNAME", 
                 values_to = "value") %>%
    mutate(value_temp = ifelse(value %in% c(-1, -3, -9), NA, value),
           percapita = case_when(SHORTNAME %in% c(per1000_cols) ~ 
                                   round(value_temp / (POPU_LSA / 1000), 2),
                                 .default = round(value_temp / POPU_LSA, 2)),
           table_value = !!sym(if (percap) "percapita" else "value")) %>%
    select(-c(POPU_LSA, value_temp, percapita, value)) %>%
    pivot_wider(names_from = "FISCAL_YEAR", 
                values_from = c("table_value")) %>%
    left_join(key, by = "SHORTNAME")
  
  if (!"PY" %in% colnames(lib_data)){
    py <- data.frame("PY" = c(NA))
    lib_data <- cbind(lib_data, py)
  }
  
  table_data <- lib_data %>%
    mutate(CY_calc = case_when(CY %in% c(-1, -3, -9) ~ NA,
                               .default = CY),
           PY_calc = case_when(PY %in% c(-1, -3, -9) ~ NA,
                               .default = PY),
           diff_pct = round(((CY_calc - PY_calc) / PY_calc), 4) # the reactable handles the *100, so keep this as is
           ) %>% 
    select(SHORTNAME, INDICATOR, PY, CY, diff_pct)
  
  # Render reactable
  table_data %>%
    reactable(
      resizable = TRUE,
      pagination = FALSE, 
      highlight = TRUE,
      defaultExpanded = TRUE,
      compact = TRUE,
      theme = reactableTheme(
        headerStyle = list(
          background = "#ecf0f1",
          borderColor = "#555"
        )
      ),
      defaultColDef = colDef(align = "left"),
      columns = list(
        SHORTNAME = colDef(show = FALSE),
        INDICATOR = colDef(name = "",
                           style = list(fontweight = "bold")),
        PY = colDef(name = paste0(as.character(year - 1), if (percap) " Per Capita"),
                    cell = function(value, index) {
                      if (is.na(value)) {
                        "No Data"
                      } else if (table_data$PY[index] %in% c(-1, -3, -9)){
                        "Missing" 
                      } else if (table_data$SHORTNAME[index] %in% currency_cols) {
                        # Format as US Dollars
                        paste0("$", format(value, big.mark = ",", decimal.mark = "."))
                      } else if (table_data$SHORTNAME[index] %in% c(per1000_cols) && percap) {
                        paste0(value, " Per 1000")
                      } else {
                        # Keep the original value without currency formatting
                        format(value, big.mark = ",")
                      }}),
        CY = colDef(name = paste0(as.character(year), if (percap) " Per Capita"),
                    cell = function(value, index) {
                      if (is.na(value)) {
                        "No Data"
                      } else if (table_data$CY[index] %in% c(-1, -3, -9)){
                        "Missing" 
                      } else if (table_data$SHORTNAME[index] %in% currency_cols) {
                        # Format as US Dollars
                        paste0("$", format(value, big.mark = ",", decimal.mark = "."))
                      } else if (table_data$SHORTNAME[index] %in% c(per1000_cols) && percap) {
                        paste0(value, " Per 1000")
                      } else {
                        # Keep the original value without currency formatting
                        format(value, big.mark = ",")
                      }}),
        diff_pct = colDef(
          name = "Change from Last Year",
          headerStyle = list(textAlign = "left"),
          align = "right",
          cell = function(value) {
            if (is.na(value)) return()
            if (is.infinite(value)) return()
            out <- if (value > 0) paste(scales::percent(value, accuracy = 0.1), '▲') 
            else if (value < 0) paste(scales::percent(value, accuracy = 0.1),'▼') 
            else 'No Change'
            htmltools::HTML(out)
          }
        )
      )
  ) #%>%
    #add_source(if (percap) "* Per 1000 People" else "")
}



render_comparison_hc <- function(df, target_lib, variable_key, 
                                 col, 
                                 under_50k = FALSE, # filter to only libraries <= 50,000
                                 year = current_year, # temporary?
                                 restrict_years = TRUE) # temporary?
  {
  
  ## Setup
  
  # Pre-define currency columns
  currency_cols <- c("TOTOPEXP", "STAFFEXP", "TOTEXPCO", "OTHOPEXP", "SALARIES", "BENEFIT",
                     "PRMATEXP","ELMATEXP", "OTHMATEX",
                     "TOTINCM", "LOCGVT", "STGVT", "FEDGVT", "OTHINCM")
  
  # Temporary? This just sets restrict_years to only keep the past 5 years of data
  year_range <- (as.numeric(year) - 4):as.numeric(year)
  
  # Determine if per 1000 calculation is needed
  per_1000 <- col %in% c("TOTSTAFF", "GPTERMS", "HOTSPOT")
  per_cap_label <- if (per_1000) "Per 1000" else "Per Capita"
  
  # Get pretty name for the column
  col_name_pretty <- variable_key$INDICATOR[variable_key$SHORTNAME == col]
  
  # Set tooltip formatting
  if (col %in% currency_cols){
    ytt_format <- "${point.y}"
    actual_tt_format <- "${point.actual:,.0f}"
  } else {
    ytt_format <- "{point.y}"
    actual_tt_format <- "{point.actual:,.0f}"
  }
  
  # Restrict Years as default for now
  if (restrict_years) {
    df <- df %>% filter(FISCAL_YEAR %in% year_range)
  }
  
  ## Manipulate the data
  
  # Clean and filter data
  df <- df %>%
    mutate(across(c(POPU_LSA, !!sym(col)), as.numeric)) %>%
    mutate(across(everything(), ~ replace(., . %in% c(-1, -3, -9), NA)))
  
  # Compute per capita or per 1000
  df <- df %>%
    mutate(value = if (per_1000) !!sym(col) / (POPU_LSA / 1000) else !!sym(col) / POPU_LSA,
           value_actual = !!sym(col))
  
  # Optionally filter to under 50k population
  if (under_50k) {
    ut_df <- df %>% filter(POPU_LSA <= 50000)
    subtitle <- "Compared to All Libraries Serving < 50,000 People"
  } else {
    ut_df <- df
    subtitle <- "Compared to All Libraries"
  }
  
  
  # Create Utah average (including target)
  avg_df <- ut_df %>%
    #filter(CURRENT_LIBNAME != target_lib) %>%
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = if (under_50k) "Utah Average (< 50,000)" else "Utah Average",
      population = sum(POPU_LSA, na.rm = T),
      plot_col = round(mean(value, na.rm = TRUE), 2),
      actual = round(sum(value_actual, na.rm = T), 2)
      )
  
  # Create Utah median (including target)
  median_df <- ut_df %>%
    #filter(CURRENT_LIBNAME != target_lib) %>%
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = if (under_50k) "Utah Median (< 50,000)" else "Utah Median",
      population = sum(POPU_LSA, na.rm = T),
      plot_col = round(median(value, na.rm = TRUE), 2),
      actual = round(sum(value_actual, na.rm = T), 2)
    )
  
  # Create target library data
  target_df <- df %>%
    filter(CURRENT_LIBNAME == target_lib) %>%
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = target_lib,
      population = sum(POPU_LSA, na.rm = T),
      plot_col = round(value, 2),
      actual = round(sum(value_actual, na.rm = T), 2)
    )
  
  # Create highchart
  highchart() %>%
    hc_add_series(target_df,
                  type = "line", color = "#4EC3E0",
                  hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)) %>%
    hc_add_series(avg_df,
                  type = "line", color = "#000000",
                  hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)) %>%
    hc_add_series(median_df,
                  type = "line", color = "#FC8B22",
                  hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)) %>%
    hc_tooltip(pointFormat = paste0("<b>{series.name}</b><br>",
                                    "<b>", col_name_pretty," ", per_cap_label, ": ", ytt_format,"</b><br>", 
                                    col_name_pretty, ": ", actual_tt_format, "<br>",
                                    "Population of Legal Service Area: {point.population:,.0f}<br>", 
                                    "{point.x}"),
               headerFormat = "") %>%
    hc_xAxis(allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = paste(col_name_pretty, per_cap_label))) %>%
    hc_title(text = paste(col_name_pretty, per_cap_label)) %>%
    hc_subtitle(text = subtitle) %>%
    hc_caption(text = paste0("The comparison lines (in black and orange) represent all libraries including ", target_lib)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE))) %>%
    hc_legend(verticalAlign = "top") %>%
    hc_exporting(
      enabled = TRUE,
      filename = paste0(col_name_pretty, "_graph")
    )
}


