# Google COVID-19 Community Mobility Reports
google_site <- "https://www.google.com/covid19/mobility/"

# Get country and url
get_country_data <- function(url) {
  
  # Read HTML
  html <- read_html(url)
  
  # Get country names
  countries <- html %>%
    html_nodes(".country-description") %>%
    html_text() %>%
    str_trim()
  
  # Fix naming issue
  countries[125] <- "United States"
  
  # Turn list into dataframe
  countries <- countries %>% enframe(name = NULL, value = "country")
  
  # URL links to download PDF for all countries and US regions
  pdf_links <- html %>%
    html_nodes(".download-link") %>%
    html_attr("href")
  
  # Subset PDF urls for all countries
  countries_pdf_urls <- pdf_links[-c(126:176)] %>% enframe(name = NULL, value = "url")
  
  # Bind country names and their respective PDF urls
  countries_tbl <- countries %>% 
    bind_cols(countries_pdf_urls)
  
  return(countries_tbl)
  
}

# Get country report: World
get_country_report <- function(url) {
  
  # Extract text
  all_text <- extract_text(url) %>% read_lines() 
  
  # Date string to remove 
  remove_date <- all_text[2] %>%
    word(c(-3:-1)) %>%   
    paste(sep = " ", collapse= " ") 
  
  # Get country by removing date from string 
  report_country <- all_text[2] %>% 
    str_remove(pattern = remove_date) %>%
    str_trim() %>%
    enframe(name=NULL, value="Country")
  
  # Date of report as tibble
  report_date <- remove_date %>%
    enframe(name = NULL) %>%
    rename(Date = value) 
  
  # Select rows with relevant data
  relevant_data <- all_text[c(13,14,24,25,36,37,47,48,57,58,66,67)] %>% enframe(name=NULL)
  
  # Subset categories into a column
  relevant_data_category <- relevant_data[-c(2,4,6,8,10,12), ]
  
  # Subset numeric values into a column
  relevant_data_values <- relevant_data[c(2,4,6,8,10,12), ]
  
  # Bind columns
  final_tbl <- bind_cols(relevant_data_category, relevant_data_values) %>%
    rename(area_type = value, pct_change = value1) %>%
    mutate(pct_change = parse_number(pct_change) / 100)
  
  final_tbl_wide <- final_tbl %>%
    pivot_wider(names_from = area_type, values_from = pct_change) %>%
    bind_cols(report_date, report_country) %>%
    select(Date, Country, everything())
  
  return(final_tbl_wide)
}

# Get country report: Guinea-Bissau
get_country_report_gb <- function(url) {
  
  # Extract text
  all_text <- extract_text(url) %>% read_lines() 
  
  # Date string to remove 
  remove_date <- all_text[2] %>%
    word(c(-3:-1)) %>%   
    paste(sep = " ", collapse= " ") 
  
  # Get country by removing date from string 
  report_country <- all_text[2] %>% 
    str_remove(pattern = remove_date) %>%
    str_trim() %>%
    enframe(name=NULL, value="Country")
  
  # Date of report as tibble
  report_date <- remove_date %>%
    enframe(name = NULL) %>%
    rename(Date = value) 
  
  # Select rows with relevant data: Guinea-Bissau Only
  relevant_data <- all_text[c(13,14,25,26,37,38,52,53,63,64,73,74)] %>% enframe(name=NULL)
  
  # Subset categories into a column
  relevant_data_category <- relevant_data[-c(2,4,6,8,10,12), ]
  
  # Subset numeric values into a column
  relevant_data_values <- relevant_data[c(2,4,6,8,10,12), ]
  
  # Bind columns
  final_tbl <- bind_cols(relevant_data_category, relevant_data_values) %>%
    rename(area_type = value, pct_change = value1) %>%
    mutate(pct_change = parse_number(pct_change) / 100)
  
  final_tbl_wide <- final_tbl %>%
    pivot_wider(names_from = area_type, values_from = pct_change) %>%
    bind_cols(report_date, report_country) %>%
    select(Date, Country, everything())
  
  return(final_tbl_wide)
}

# Get country report: Liechtenstein
get_country_report_lts <- function(url) {
  
  # Extract text
  all_text <- extract_text(url) %>% read_lines() 
  
  # Date string to remove 
  remove_date <- all_text[2] %>%
    word(c(-3:-1)) %>%   
    paste(sep = " ", collapse= " ") 
  
  # Get country by removing date from string 
  report_country <- all_text[2] %>% 
    str_remove(pattern = remove_date) %>%
    str_trim() %>%
    enframe(name=NULL, value="Country")
  
  # Date of report as tibble
  report_date <- remove_date %>%
    enframe(name = NULL) %>%
    rename(Date = value) 
  
  # Select rows with relevant data: Guinea-Bissau Only
  relevant_data <- all_text[c(13,14,25,26,38,39,53,54,63,64,73,74)] %>% enframe(name=NULL)
  
  # Subset categories into a column
  relevant_data_category <- relevant_data[-c(2,4,6,8,10,12), ]
  
  # Subset numeric values into a column
  relevant_data_values <- relevant_data[c(2,4,6,8,10,12), ]
  
  # Bind columns
  final_tbl <- bind_cols(relevant_data_category, relevant_data_values) %>%
    rename(area_type = value, pct_change = value1) %>%
    mutate(pct_change = parse_number(pct_change) / 100)
  
  final_tbl_wide <- final_tbl %>%
    pivot_wider(names_from = area_type, values_from = pct_change) %>%
    bind_cols(report_date, report_country) %>%
    select(Date, Country, everything())
  
  return(final_tbl_wide)
}

# Get country report: Réunion
get_country_report_rn <- function(url) {
  
  # Extract text
  all_text <- extract_text(url) %>% read_lines() 
  
  # Date string to remove 
  remove_date <- all_text[2] %>%
    word(c(-3:-1)) %>%   
    paste(sep = " ", collapse= " ") 
  
  # Get country by removing date from string 
  report_country <- all_text[2] %>% 
    str_remove(pattern = remove_date) %>%
    str_trim() %>%
    enframe(name=NULL, value="Country")
  
  # Date of report as tibble
  report_date <- remove_date %>%
    enframe(name = NULL) %>%
    rename(Date = value) 
  
  # Select rows with relevant data: Guinea-Bissau Only
  relevant_data <- all_text[c(13,14,24,25,36,37,47,48,58,59,68,69)] %>% enframe(name=NULL)
  
  # Subset categories into a column
  relevant_data_category <- relevant_data[-c(2,4,6,8,10,12), ]
  
  # Subset numeric values into a column
  relevant_data_values <- relevant_data[c(2,4,6,8,10,12), ]
  
  # Bind columns
  final_tbl <- bind_cols(relevant_data_category, relevant_data_values) %>%
    rename(area_type = value, pct_change = value1) %>%
    mutate(pct_change = parse_number(pct_change) / 100)
  
  final_tbl_wide <- final_tbl %>%
    pivot_wider(names_from = area_type, values_from = pct_change) %>%
    bind_cols(report_date, report_country) %>%
    select(Date, Country, everything())
  
  return(final_tbl_wide)
}

# IRREGULAR DATA
# Map modified get_country_report to URLs
get_country_report_2 <- function(url, filter, fxn) {
  t <- get_country_data(url = google_site) %>%
    filter(country %in% c(filter)) %>%
    mutate(nest = map(.x = url, .f = fxn)) %>%
    unnest(cols = nest) %>%
    select(-country, -url)
  
  colnames(t) <- colnames(t) %>% str_trim()
  return(t)
}

# FINAL DATA: RUN TO SCRAPE INFO ON ALL COUNTRIES
# Map get_country_report function to URLs
get_final_data <- function(url) {
  
  # Custom 'not in' filter
  `%notin%` <- Negate(`%in%`)
  
  # Countries except ones with issues
  world <- get_country_data(url = url) %>%
    filter(country %notin% c('Antigua and Barbuda','Guinea-Bissau', 
                             'Liechtenstein', 'Papua New Guinea', 'Réunion')) %>%
    mutate(nest = map(.x = url, .f = get_country_report)) %>%
    unnest(cols = nest) %>%
    select(-country, -url)
  
  # Irregular data 1: Antigua
  antigua <- get_country_report_2(url = google_site, 
                                  filter = "Antigua and Barbuda", 
                                  fxn = get_country_report)
  
  # Irregular data 2: Guinea-Bissau (using modified get_country_report)
  guinea <- get_country_report_2(url = google_site, 
                                 filter = "Guinea-Bissau", 
                                 fxn = get_country_report_gb)
  
  # Irregular data 3: Liechtenstein (using modified get_country_report)
  liechtenstein <- get_country_report_2(url = google_site, 
                                        filter = "Liechtenstein", 
                                        fxn = get_country_report_lts)
  
  # Irregular data 4: Papua New Guinea
  papua <- get_country_report_2(url = google_site, 
                                filter = "Papua New Guinea", 
                                fxn = get_country_report)
  
  # Irregular data 5: Réunion (using modified get_country_report)
  reunion <- get_country_report_2(url = google_site, 
                                  filter = "Réunion", 
                                  fxn = get_country_report_rn)
  
  # Combine all tables
  final_tbl <- bind_rows(world, antigua, guinea, liechtenstein, papua, reunion) %>%
    arrange(Country) %>%
    mutate(Date = lubridate::mdy(Date))
  
  return(final_tbl)
}

# Get data on all countries
all_countries <- get_final_data(url = google_site)

# Write to csv with today's date in the file name
currentDate <- Sys.Date()
csvFileName <- paste("/cloud/project/all_countries_",currentDate,".csv",sep="")
write_csv(all_countries, path = csvFileName)