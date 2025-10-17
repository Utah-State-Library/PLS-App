#### Helper Functions ####

# Get Value over time for single library

get_value_over_time_1lib <- function(df, 
                                     values = c(), 
                                     ytt_format = "{point.y:.0,f}",
                                     grouptext = "", 
                                     xtext = "Fiscal Year", 
                                     ytext = "", 
                                     ttext = ""){
  
  df %<>% 
    select(LIBNAME, Year, values) %>%
    pivot_longer(!c(LIBNAME, Year),
                 names_to = "Group",
                 values_to = "Value") %>%
    left_join(variable_key, by = c("Group" = "ShortName"))
  
  hc <- highchart() %>%
    hc_add_series(df, 
                  type = "line",
                  hcaes(x = Year, 
                        y = Value,
                        group = Indicator)
    )
  
  hc %>%
    hc_yAxis(title = list(text = ytext)) %>%
    hc_xAxis(title = list(text = xtext),
             allowDecimals = FALSE) %>%
    hc_tooltip(pointFormat = paste0("<b>{point.Indicator}: ",ytt_format,"</b><br>{point.x}"),
               headerFormat = "") %>%
    hc_title(text = paste0(ttext)) %>%
    hc_subtitle(text = unique(df$LIBNAME)) %>%
    hc_plotOptions(series = list(marker = list(enabled = TRUE)))
}




csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}


format_cells <- function(value) {
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


render_table <- function(data, cols, variable_key) {
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)
  
  # Define standard columns (left sticky ones)
  static_columns <- list(
    CURRENT_LIBNAME = colDef(name = "Library",
                             maxWidth = 125,
                             sticky = "left",
                             style = list(backgroundColor = "#f7f7f7")),
    FISCAL_YEAR = colDef(name = "Year",
                         maxWidth = 75,
                         sticky = "left",
                         style = list(backgroundColor = "#f7f7f7")),
    POPU_LSA = colDef(name = "Population of Legal Service Area",
                      maxWidth = 125,
                      format = colFormat(separators = TRUE),
                      sticky = "left",
                      style = list(borderRight = "1px solid #aaa",
                                   backgroundColor = "#f7f7f7"),
                      headerStyle = list(borderRight = "1px solid #aaa"))
  )
  
  # Define dynamic columns based on `cols` and `keylist`
  dynamic_columns <- lapply(cols, function(col) {
    colDef(name = keylist[[col]] %||% col,
           format = colFormat(separators = TRUE))
  }) %>% setNames(cols)
  
  # Combine both
  all_columns <- c(static_columns, dynamic_columns)
  
  # Create the reactable
  data %>%
    select(CURRENT_LIBNAME, FISCAL_YEAR, POPU_LSA, all_of(cols)) %>%
    arrange(desc(FISCAL_YEAR), CURRENT_LIBNAME) %>%
    reactable(
      resizable = TRUE,
      pagination = TRUE,
      highlight = TRUE,
      defaultExpanded = FALSE,
      compact = TRUE,
      defaultColDef = colDef(align = "left"),
      theme = reactableTheme(
        headerStyle = list(
          background = "#ecf0f1",
          borderColor = "#555"
        )
      ),
      columns = all_columns
    )
}
