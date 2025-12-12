##### Functions #####

# We'll use these functions throughout the shiny app to cut down on repetition and to make adjustments more easily

### List of Functions

## get_valuebox()
# - this function pulls either the current year (CY), previous year (PY) or percent change for a given value. The function handles the years, so no need to pipe in a df that is year - 1 when pulling the PY value
# - Used in state_server.R and single_server.R

## get_value_over_time_1lib()
# - this plots a given value for a given library over time using highcharter
# - used???

## get_n_closest()
# - this takes a given target library and a given value for which to find the closest N libraries based on that value; can handle per capita
# - used in the peer data tables page

## csvDownloadButton()
# - download handler for reactable objects
# - used on the data tables page mostly
# - update single and state for downloading reactables there

## format_currency_cells()
# - helper function for the render_table() function - formats reactable cells for currency

## format_currency_2_decimals()
# - ???

## render_table()
# - renders a reactable for given libraries for given years from select tables with predefined columns
# - used in the data tables pages

## render_pct_change_table()
# - renders a reactable to show the pct change between a given year and its previous year
# - used in state_server.R and single_server.R

## render_comparison_hc()
# - renders an hc for a given library for a given value as compared to the mean and median of all libraries
# - used in single_server.R

## render_statewide_hc()
# - renders an hc with total values, total per cap, library avg per cap, and library median per cap
# - used in state_server.R

## render_map()
# - renders a leaflet map showing given libraries and given areas of service
# - UPDATE DATA INPUT
# - used in state_service_server.R

#####

get_valuebox <- function(df, year, col, pull) {
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
  PY <- if (!is.na(valuePY)) {
    paste0(as.character(year - 1), ": ", format(valuePY, big.mark = ","))
  } else {
    ""
  }

  valueChange <- round(((valueCY - valuePY) / valuePY) * 100, 2)

  # Handle NAs, Infs, etc
  # 2003 is earliest year, so handle 2003 - 1 here:
  if (year - 1 == 2002) {
    change <- ""
    PY <- ""
  } else if (is.infinite(valueChange) | is.na(valueChange)) {
    change <- ""
  } else if (valueChange > 0) {
    change <- paste0("Increase: ▲", valueChange, "%")
  } else if (valueChange < 0) {
    change <- paste0("Decrease: ▼", valueChange, "%")
  } else {
    change <- paste0("No Change from ", as.character(year - 1))
  }

  # Pull whichever value we need
  get(pull)
}


get_value_over_time_1lib <- function(
  df,
  values = c(),
  ytt_format = "{point.y:.0,f}",
  grouptext = "",
  xtext = "Fiscal Year",
  ytext = "",
  ttext = "",
  year_range = (current_year - 5):current_year
) {
  shiny::validate(
    need((nrow(df) != 0), "")
  )

  df %<>% filter(FISCAL_YEAR %in% year_range)

  df %<>%
    select(CURRENT_LIBNAME, FISCAL_YEAR, values) %>%
    pivot_longer(
      !c(CURRENT_LIBNAME, FISCAL_YEAR),
      names_to = "COLS",
      values_to = "VALUE"
    ) %>%
    left_join(variable_key, by = c("COLS" = "SHORTNAME")) %>%
    mutate(VALUE = as.numeric(VALUE), FISCAL_YEAR = as.numeric(FISCAL_YEAR))

  hc <- highchart() %>%
    hc_add_series(
      df,
      type = "line",
      hcaes(x = FISCAL_YEAR, y = VALUE, group = INDICATOR)
    )

  hc %<>%
    hc_yAxis(title = list(text = ytext)) %>%
    hc_xAxis(title = list(text = xtext), allowDecimals = FALSE) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>{point.INDICATOR}: ",
        ytt_format,
        "</b><br>{point.x}"
      ),
      headerFormat = ""
    ) %>%
    hc_title(text = paste0(ttext)) %>%
    hc_subtitle(text = unique(df$CURRENT_LIBNAME)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE, radius = 7)))

  hc
}

get_nclosest <- function(df, n, libname, pg_col, percap = FALSE) {
  # use pls as df
  per1000_cols <- per1000_cols

  df_mod <- df %>%
    select(CURRENT_LIBNAME, POPU_LSA, value = !!sym(pg_col)) %>%
    mutate(
      value = as.numeric(value) # convert actual values, not column name string
    )

  if (percap & pg_col != "POPU_LSA") {
    df_mod <- df_mod %>%
      mutate(
        value = case_when(
          percap & pg_col %in% per1000_cols ~ value / (POPU_LSA / 1000), # per 1000 people
          percap & !(pg_col %in% per1000_cols) ~ value / POPU_LSA, # per capita
          TRUE ~ value # raw value
        )
      )
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


csvDownloadButton <- function(
  id,
  filename = "data.csv",
  label = "Download Current Table as CSV"
) {
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


render_table <- function(
  data,
  cols,
  variable_key,
  order_col = NULL, # which column to order table by
  per_cap = FALSE, # are we showing per capita?
  peer = FALSE, # is this a peer library table?
  target_lib = NULL, # for peer library table, which is the target lib (for bolding/styling)
  peer_col = NULL, # add peer group column to left sticky columns for peer tables
  color_table = FALSE # remove later once we figure out how best to style?
) {
  # ensure logical
  per_cap <- as.logical(per_cap)
  color_table <- as.logical(color_table)

  key <- variable_key %>%
    dplyr::filter(SHORTNAME %in% c(cols, if (!is.null(peer_col)) peer_col))
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  # Pre-define special handling columns - defined in /lists.R
  currency_cols <- currency_cols
  per1000_cols <- per1000_cols

  # Apply per capita transformations
  if (per_cap) {
    all_cols <- c(cols, peer_col)
    per_cap_cols <- setdiff(all_cols, "POPU_LSA") # dont do per capita for the population

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
    CURRENT_LIBNAME = colDef(
      name = "Library",
      minWidth = 200,
      sticky = "left",
      style = if (!peer) {
        list(backgroundColor = "#f7f7f7")
      } else {
        # if we're doing a peer library table, make the target library bold
        function(value) {
          if (value == target_lib) {
            fontweight = "bold"
          } else {
            fontweight = 300
          }
          list(fontWeight = fontweight, backgroundColor = "#f7f7f7")
        }
      }
    ),
    FISCAL_YEAR = colDef(
      name = "Year",
      maxWidth = 75,
      sticky = "left",
      style = list(backgroundColor = "#f7f7f7")
    ),
    POPU_LSA = colDef(
      name = "Population of Legal Service Area",
      maxWidth = 125,
      format = colFormat(separators = TRUE),
      sticky = "left",
      style = list(backgroundColor = "#f7f7f7")
    )
  )

  # Define peer column if applicable
  if (peer) {
    if (peer_col != "POPU_LSA") {
      peer_column <- lapply(peer_col, function(col) {
        is_currency <- col %in% currency_cols

        display_name <- keylist[[col]] %||% col

        if (per_cap) {
          display_name <- paste0(
            display_name,
            if (col %in% per1000_cols) " (Per 1,000)" else " (Per Capita)"
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
      }) %>%
        setNames(peer_col)
    } else if (peer_col == "POPU_LSA") {
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
      display_name <- paste0(
        display_name,
        if (col %in% per1000_cols) " (Per 1,000)" else " (Per Capita)"
      )
    }

    colDef(
      name = display_name,
      format = if (!is_currency) {
        if (per_cap) {
          colFormat(separators = TRUE, digits = 2)
        } else {
          colFormat(separators = TRUE)
        }
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
  }) %>%
    setNames(cols)

  # Combine all columns
  all_columns <- c(
    static_columns,
    if (peer) peer_column,
    dynamic_columns
  )

  arrange_call <- c("FISCAL_YEAR") # always sort by year descending

  if (!is.null(order_col)) {
    arrange_call <- c("FISCAL_YEAR", order_col)
  }

  # Create reactable
  data %>%
    select(
      CURRENT_LIBNAME,
      FISCAL_YEAR,
      POPU_LSA,
      if (peer) peer_col,
      all_of(cols)
    ) %>%
    arrange(across(all_of(arrange_call), ~ desc(.))) %>%
    reactable(
      resizable = TRUE,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = FALSE,
      compact = TRUE,
      theme = reactableTheme(
        headerStyle = list(
          background = "#ecf0f1",
          borderColor = "#555"
        )
      ),
      defaultColDef = colDef(
        align = "left",
        style = if (color_table) {
          color_scales(data, colors = c("#4EC3E0", "#0086BF", "#002F6C"))
        } else {
          NULL
        }
      ),
      columns = all_columns
    )
}


#### Single Library Pct Change Table ####

render_pct_change_table <- function(
  df,
  variable_key,
  year,
  cols,
  percap = FALSE
) {
  year <- as.numeric(year)
  percap <- as.logical(percap)

  # Pre-define special handling columns - defined in /lists.R
  currency_cols <- currency_cols
  per1000_cols <- per1000_cols

  key <- variable_key %>%
    filter(SHORTNAME %in% c(cols, "POPU_LSA")) %>%
    select(INDICATOR, SHORTNAME)

  # Data for selected library
  lib_data <- df %>%
    mutate(FISCAL_YEAR = as.numeric(FISCAL_YEAR)) %>%
    filter(FISCAL_YEAR %in% c(year - 1, year)) %>%
    select(FISCAL_YEAR, all_of(cols), POPU_LSA) %>%
    mutate(FISCAL_YEAR = ifelse(FISCAL_YEAR == year, "CY", "PY")) %>%
    pivot_longer(
      -c(FISCAL_YEAR, POPU_LSA),
      names_to = "SHORTNAME",
      values_to = "value"
    ) %>%
    mutate(
      value_temp = ifelse(value %in% c(-1, -3, -9), NA, value),
      percapita = case_when(
        SHORTNAME %in% c(per1000_cols) ~
          round(value_temp / (POPU_LSA / 1000), 2),
        .default = round(value_temp / POPU_LSA, 2)
      ),
      table_value = !!sym(if (percap) "percapita" else "value")
    ) %>%
    select(-c(POPU_LSA, value_temp, percapita, value)) %>%
    pivot_wider(names_from = "FISCAL_YEAR", values_from = c("table_value")) %>%
    left_join(key, by = "SHORTNAME")

  if (!"PY" %in% colnames(lib_data)) {
    py <- data.frame("PY" = c(NA))
    lib_data <- cbind(lib_data, py)
  }

  table_data <- lib_data %>%
    mutate(
      CY_calc = case_when(CY %in% c(-1, -3, -9) ~ NA, .default = CY),
      PY_calc = case_when(PY %in% c(-1, -3, -9) ~ NA, .default = PY),
      diff_pct = round(((CY_calc - PY_calc) / PY_calc), 4), # the reactable handles the *100, so keep this calculation as is
      diff = round(CY_calc - PY_calc, 2)
    ) %>%
    select(SHORTNAME, INDICATOR, PY, CY, diff_pct, diff)

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
        INDICATOR = colDef(
          name = "",
          style = list(fontweight = "bold"),
          minWidth = 225
        ),
        PY = colDef(
          name = paste0(as.character(year - 1), if (percap) " Per Capita"),
          cell = function(value, index) {
            if (is.na(value)) {
              "No Data"
            } else if (table_data$PY[index] %in% c(-1, -3, -9)) {
              "Missing"
            } else if (table_data$SHORTNAME[index] %in% currency_cols) {
              # Format as US Dollars
              paste0("$", format(value, big.mark = ",", decimal.mark = "."))
            } else if (
              table_data$SHORTNAME[index] %in% c(per1000_cols) && percap
            ) {
              paste0(value, " Per 1000")
            } else {
              # Keep the original value without currency formatting
              format(value, big.mark = ",")
            }
          }
        ),
        CY = colDef(
          name = paste0(as.character(year), if (percap) " Per Capita"),
          cell = function(value, index) {
            if (is.na(value)) {
              "No Data"
            } else if (table_data$CY[index] %in% c(-1, -3, -9)) {
              "Missing"
            } else if (table_data$SHORTNAME[index] %in% currency_cols) {
              # Format as US Dollars
              paste0("$", format(value, big.mark = ",", decimal.mark = "."))
            } else if (
              table_data$SHORTNAME[index] %in% c(per1000_cols) && percap
            ) {
              paste0(value, " Per 1000")
            } else {
              # Keep the original value without currency formatting
              format(value, big.mark = ",")
            }
          }
        ),
        diff_pct = colDef(
          name = "Change from Last Year",
          headerStyle = list(textAlign = "left"),
          align = "right",
          cell = function(value) {
            if (is.na(value)) {
              return()
            }
            if (is.infinite(value)) {
              return()
            }
            out <- if (value > 0) {
              paste(scales::percent(value, accuracy = 0.01), '▲')
            } else if (value < 0) {
              paste(scales::percent(value, accuracy = 0.01), '▼')
            } else {
              'No Change'
            }
            htmltools::HTML(out)
          }
        ),
        diff = colDef(
          name = paste0(if (percap) "Per Capita Difference" else "Difference"),
          headerStyle = list(textAlign = "right"),
          align = "right",
          cell = function(value, index) {
            if (is.na(value)) {
              "No Data"
            } else if (table_data$CY[index] %in% c(-1, -3, -9)) {
              "Missing"
            } else if (table_data$SHORTNAME[index] %in% currency_cols) {
              # Format as US Dollars
              paste0("$", format(value, big.mark = ",", decimal.mark = "."))
            } else if (
              table_data$SHORTNAME[index] %in% c(per1000_cols) && percap
            ) {
              paste0(value, " Per 1000")
            } else {
              # Keep the original value without currency formatting
              format(value, big.mark = ",")
            }
          }
        )
      )
    )
}


render_comparison_hc <- function(
  df,
  target_lib,
  variable_key,
  col,
  under_50k = FALSE, # filter to only libraries <= 50,000
  year = current_year, # temporary?
  restrict_years = TRUE # temporary?
) {
  # Pre-define special handling columns - defined in /lists.R
  currency_cols <- currency_cols
  per1000_cols <- per1000_cols

  # Temporary? This just sets restrict_years to only keep the past 5 years of data
  year_range <- (as.numeric(year) - 4):as.numeric(year)

  # Determine if per 1000 calculation is needed
  per_1000 <- col %in% per1000_cols
  per_cap_label <- if (per_1000) "Per 1000" else "Per Capita"

  # Get pretty name for the column
  col_name_pretty <- variable_key$INDICATOR[variable_key$SHORTNAME == col]

  # Set tooltip formatting
  if (col %in% currency_cols) {
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
    mutate(
      value = if (per_1000) {
        !!sym(col) / (POPU_LSA / 1000)
      } else {
        !!sym(col) / POPU_LSA
      },
      value_actual = !!sym(col)
    )

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
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = if (under_50k) {
        "Average of Libraries (< 50,000)"
      } else {
        "Average of Libraries"
      },
      population = sum(POPU_LSA, na.rm = T),
      plot_col = round(mean(value, na.rm = TRUE), 2),
      actual = round(sum(value_actual, na.rm = T), 2)
    )

  # Create Utah median (including target)
  median_df <- ut_df %>%
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = if (under_50k) {
        "Median of Libraries (< 50,000)"
      } else {
        "Median of Libraries"
      },
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
    hc_add_series(
      target_df,
      type = "line",
      color = "#4EC3E0",
      hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)
    ) %>%
    hc_add_series(
      avg_df,
      type = "line",
      color = "#000000",
      hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)
    ) %>%
    hc_add_series(
      median_df,
      type = "line",
      color = "#FC8B22",
      hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)
    ) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>{series.name}</b><br>",
        "<b>",
        col_name_pretty,
        " ",
        per_cap_label,
        ": ",
        ytt_format,
        "</b><br>",
        col_name_pretty,
        ": ",
        actual_tt_format,
        "<br>",
        "Population of Legal Service Area: {point.population:,.0f}<br>",
        "{point.x}"
      ),
      headerFormat = ""
    ) %>%
    hc_xAxis(allowDecimals = FALSE) %>%
    hc_yAxis(title = list(text = paste(col_name_pretty, per_cap_label))) %>%
    hc_title(text = paste(col_name_pretty, per_cap_label)) %>%
    hc_subtitle(text = subtitle) %>%
    hc_caption(
      text = paste0(
        "The comparison lines (in black and orange) represent all libraries including ",
        target_lib
      )
    ) %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, radius = 7)),
      line = list(animation = FALSE)
    ) %>%
    hc_legend(verticalAlign = "top") %>%
    hc_exporting(
      enabled = TRUE,
      filename = paste0(col_name_pretty, "_graph")
    )
}


render_statewide_hc <- function(
  df,
  variable_key,
  col,
  per_cap = FALSE,
  year = current_year, # temporary?
  restrict_years = TRUE # temporary?
) {
  # Pre-define special handling columns - defined in /lists.R
  currency_cols <- currency_cols
  per1000_cols <- per1000_cols

  # Temporary? This just sets restrict_years to only keep the past 5 years of data
  year_range <- (as.numeric(year) - 4):as.numeric(year)

  # Determine if per 1000 calculation is needed
  per_1000 <- col %in% per1000_cols
  per_cap_label <- if (per_1000) "Per 1000" else "Per Capita"

  # Get pretty name for the column
  col_name_pretty <- variable_key$INDICATOR[variable_key$SHORTNAME == col]

  # Set tooltip formatting
  if (col %in% currency_cols) {
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
    mutate(
      value = if (per_1000) {
        !!sym(col) / (POPU_LSA / 1000)
      } else {
        !!sym(col) / POPU_LSA
      },
      value_actual = !!sym(col)
    )

  # Create Statewide Per Capita
  utah_total_df <- df %>%
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = "Statewide Per Capita",
      tt_exp = "Total value across all libraries divided by the total population served by libraries.",
      tt_subexp = "Shows the overall per-resident level of resources statewide.",
      population = sum(POPU_LSA, na.rm = T),
      actual = round(sum(value_actual, na.rm = T), 2)
    ) %>%
    mutate(
      plot_col = if (per_1000) {
        round(actual / (population / 1000), 2)
      } else {
        round(actual / population, 2)
      }
    )

  # Create Library Per Capita Average
  avg_df <- df %>%
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = "Average Per Capita Across Libraries",
      tt_exp = "The average of all libraries' individual per capita values.",
      tt_subexp = "Treats each library equally and reflects the typical per capita level across libraries.",
      population = sum(POPU_LSA, na.rm = T),
      plot_col = round(mean(value, na.rm = TRUE), 2),
      actual = round(sum(value_actual, na.rm = T), 2)
    )

  # Create Library Per Capita Median
  median_df <- df %>%
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = "Median Per Capita Across Libraries",
      tt_exp = "The middle per capita value among all libraries.",
      tt_subexp = "Indicates what the 'middle' library looks like and reduces the impact of outliers.",
      population = sum(POPU_LSA, na.rm = T),
      plot_col = round(median(value, na.rm = TRUE), 2),
      actual = round(sum(value_actual, na.rm = T), 2)
    )

  # Create Statewide Total
  total_df <- df %>%
    group_by(FISCAL_YEAR) %>%
    reframe(
      Library = "Statewide Total",
      population = sum(POPU_LSA, na.rm = T),
      plot_col = round((value), 2),
      actual = round(sum(value_actual, na.rm = T), 2)
    )

  # Create highchart
  if (!per_cap) {
    hc <- highchart() %>%
      hc_add_series(
        total_df,
        type = "line",
        color = "#4EC3E0",
        hcaes(x = FISCAL_YEAR, y = actual, group = Library)
      ) %>%
      hc_tooltip(
        pointFormat = paste0(
          "<b>{series.name}</b><br>",
          "<b>",
          col_name_pretty,
          ": ",
          ytt_format,
          "</b><br>",
          "Population of Legal Service Area: {point.population:,.0f}<br>",
          "{point.x}"
        ),
        headerFormat = ""
      ) %>%
      hc_xAxis(allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = paste(col_name_pretty))) %>%
      hc_title(text = paste(col_name_pretty))
  } else if (per_cap) {
    hc <- highchart() %>%
      hc_add_series(
        utah_total_df,
        type = "line",
        color = "#16b1feff",
        hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)
      ) %>%
      hc_add_series(
        avg_df,
        type = "line",
        color = "#000000",
        hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)
      ) %>%
      hc_add_series(
        median_df,
        type = "line",
        color = "#FC8B22",
        hcaes(x = FISCAL_YEAR, y = plot_col, group = Library)
      ) %>%
      hc_tooltip(
        pointFormat = paste0(
          "<b>{series.name}</b><br>",
          "<b>",
          col_name_pretty,
          " ",
          per_cap_label,
          ": ",
          ytt_format,
          "</b><br>",
          "{point.tt_exp}",
          "<br>",
          "<em>{point.tt_subexp}</em>"
          # col_name_pretty,
          # ": ",
          # actual_tt_format,
          # "<br>",
          # "Population of Legal Service Area: {point.population:,.0f}<br>",
          # "{point.x}"
        ),
        headerFormat = ""
      ) %>%
      hc_xAxis(allowDecimals = FALSE) %>%
      hc_yAxis(title = list(text = paste(col_name_pretty, per_cap_label))) %>%
      hc_title(text = paste(col_name_pretty, per_cap_label))
  }

  hc %>%
    hc_plotOptions(
      series = list(marker = list(enabled = TRUE, radius = 7)),
      line = list(animation = FALSE)
    ) %>%
    hc_legend(verticalAlign = "top") %>%
    hc_exporting(
      enabled = TRUE,
      filename = paste0(col_name_pretty, "_statewide_graph")
    )
}


#### Map! ####

render_map <- function(
  map_libs_df,
  outlets = outlets,
  county_map = county_map,
  municipalities_map = municipalities_map,
  show_libs = T,
  #show_service = T,
  service_areas = NULL
) {
  # Pre-done map dfs so they don't have to run each time the map is updated
  # Made in /data_prep.R
  map_all <- map_all

  service_areas_legend <- data.frame(
    colors = c(
      "#4EC3E0",
      "#d3d3d3",
      "#08f476ff",
      "#002F60",
      "#f632f3ff",
      "#ffbd31",
      "#808080"
    ),
    labels = c(
      "County Library Service",
      "No County Library Service",
      "Bookmobile Library Service",
      "City Library Service",
      "Agreed Service Through a City Library",
      "Non-Certified Library Service",
      "No Library Service"
    )
  ) %>%
    filter(labels %in% service_areas)

  map_all <- map_all %>%
    filter(CURRENT_LIBNAME_AE %in% map_libs_df$CURRENT_LIBNAME_AE)

  county_libs_map <- county_map %>% filter(county_pop > 0)
  city_libs_map <- municipalities_map %>% filter(`Library_1 Type` == "City")
  agreed_service_city_map <- municipalities_map %>%
    filter(`Library_1 Type` == "Agreed Service")
  agreed_service_county_map <- county_map %>%
    filter(NAME == "Beaver")
  bookmobile_service_map <- county_map %>%
    filter(bookmobile_pop > 0 & !NAME %in% c("Juab", "Tooele")) #only one town in each gets bookmobile service
  noncertified_service_county_map <- county_map %>%
    filter(noncertified_county_pop > 0)
  noncertified_service_city_map <- municipalities_map %>%
    filter(`Library_1 Type` == "Non-Certified")
  no_county_service_map <- county_map %>%
    filter(
      county_pop == 0 &
        noncertified_county_pop == 0 &
        bookmobile_pop == 0 |
        NAME %in% c("Juab", "Tooele")
    )
  no_service_map <- municipalities_map %>%
    filter(`Library_1 Type` == "No Library Service")

  ## Set base map
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    addMapPane("county_pane", zIndex = 420) %>%
    addMapPane("city_pane", zIndex = 430) %>% # Cities will be on top
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    #setMaxBounds(lng1 = -109, lat1 = 37, lng2 = -114, lat2 = 42) %>%
    onRender(
      "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }"
    )

  ## Show Library Locations
  if (show_libs) {
    map <- map %>%
      addMarkers(
        data = map_all,
        lng = ~LONG,
        lat = ~LAT,
        label = ~ lapply(library_info, HTML),
        #popup = ~ lapply(library_info, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
      )
  }

  ## Show County Library Service Areas
  if ("County Library Service" %in% service_areas & nrow(county_libs_map) > 0) {
    map <- map %>%
      addPolygons(
        data = county_libs_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#4EC3E0",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#4EC3E0",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "county_pane")
      )
  }

  ## Show City Library Service Areas
  if ("City Library Service" %in% service_areas & nrow(city_libs_map) > 0) {
    map <- map %>%
      addPolygons(
        data = city_libs_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#002F6C",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#002F6C",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }

  ## Show Agreed Service
  if (
    "Agreed Service Through a City Library" %in%
      service_areas &
      nrow(agreed_service_city_map) > 0
  ) {
    map <- map %>%
      addPolygons(
        data = agreed_service_city_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#f632f3ff",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#f632f3ff",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }
  if (
    "Agreed Service Through a City Library" %in%
      service_areas &
      nrow(agreed_service_county_map) > 0
  ) {
    map <- map %>%
      addPolygons(
        data = agreed_service_county_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#f632f3ff",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#f632f3ff",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "county_pane")
      )
  }

  ## Show Bookmobile Service
  if (
    "Bookmobile Library Service" %in%
      service_areas &
      nrow(bookmobile_service_map) > 0
  ) {
    map <- map %>%
      addPolygons(
        data = bookmobile_service_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#08f476ff",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#08f476ff",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "county_pane")
      )
  }

  ## Show Counties without Library Service
  if (
    "No County Library Service" %in%
      service_areas &
      nrow(no_county_service_map) > 0
  ) {
    map <- map %>%
      addPolygons(
        data = no_county_service_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#d3d3d3",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#d3d3d3",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "county_pane")
      )
  }

  ## Show Cities without Library Service
  if ("No Library Service" %in% service_areas & nrow(no_service_map) > 0) {
    map <- map %>%
      addPolygons(
        data = no_service_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#808080",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#808080",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }

  if (
    "Non-Certified Library Service" %in%
      service_areas &
      nrow(noncertified_service_city_map) > 0
  ) {
    map <- map %>%
      addPolygons(
        data = noncertified_service_city_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#ffbd31",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#ffbd31",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "city_pane")
      )
  }

  if (
    "Non-Certified Library Service" %in%
      service_areas &
      nrow(noncertified_service_county_map) > 0
  ) {
    map <- map %>%
      addPolygons(
        data = noncertified_service_county_map,
        label = ~ lapply(service_label, HTML),
        popup = ~ lapply(service_popup, HTML),
        popupOptions = popupOptions(keepInView = TRUE),
        weight = 1,
        opacity = 1,
        color = "#ffbd31",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#ffbd31",
          fillOpacity = 0.7
        ),
        options = pathOptions(pane = "county_pane")
      )
  }

  ## Show Legend if Service Areas are Selected
  if (!is.null(service_areas)) {
    map <- map %>%
      addLegend(
        position = c("bottomright"),
        colors = service_areas_legend$colors,
        opacity = 0.7,
        labels = service_areas_legend$labels
      )
  }

  map
}

## testing
render_map(
  map_libs_df = outlets,
  outlets = outlets,
  county_map = county_map,
  municipalities_map = municipalities_map,
  show_libs = F,
  #show_service = T
  service_areas = c(
    "County Library Service",
    "City Library Service",
    "No County Library Service",
    "Bookmobile Library Service",
    "Agreed Service Through a City Library",
    "Non-Certified Library Service"
  )
)
