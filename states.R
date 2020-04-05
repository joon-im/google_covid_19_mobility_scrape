# Google COVID-19 Community Mobility Reports
google_site <- "https://www.google.com/covid19/mobility/"

# Get state and url
get_state_data <- function(url) {
  
  # Read HTML
  html <- read_html(url)

  # Get US state names
  states <- html %>%
    html_nodes(".region-name") %>%
    html_text() %>%
    str_trim() %>%
    enframe(name = NULL, value = "state")
  
  # URL links to download PDF for all countries and US regions
  pdf_links <- html %>%
    html_nodes(".download-link") %>%
    html_attr("href")
  
  # Subset PDF urls for US states
  states_pdf_urls <- pdf_links[126:176] %>% enframe(name = NULL, value = "url")
  
  # Bind state names and their respective PDF urls
  states_tbl <- states %>% bind_cols(states_pdf_urls) 
  
  return(states_tbl)
  
}

# Get report: States
get_state_report <- function(url) {
  
  # Extract text
  all_text <- extract_text(url) %>% read_lines() 
  
  # Date string to remove 
  remove_date <- all_text[2] %>%
    word(c(-3:-1)) %>%   
    paste(sep = " ", collapse= " ") 
  
  # Get state by removing date from string 
  report_state <- all_text[2] %>% 
    str_remove(pattern = remove_date) %>%
    str_trim() %>%
    enframe(name=NULL, value="State")
  
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
    bind_cols(report_date, report_state) %>%
    select(Date, State, everything())
  
  return(final_tbl_wide)
}

# FINAL DATA: RUN TO SCRAPE INFO ON ALL STATES
# Map get_country_report function to URLs
get_final_states <- function(url) {
  
  # Countries except ones with issues
  states <- get_state_data(url = url) %>%
    mutate(nest = map(.x = url, .f = get_state_report)) %>%
    unnest(cols = nest) %>%
    select(-state, -url) %>%
    mutate(Date = lubridate::mdy(Date))
  
  return(states)
}

# Get data on all states
all_states <- get_final_states(url = google_site)

# Write to csv with today's date in the file name
currentDate <- Sys.Date()
csvFileName <- paste("/cloud/project/all_states_",currentDate,".csv",sep="")
write_csv(all_states, path = csvFileName)