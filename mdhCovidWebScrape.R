require(rvest) ## web scrape library
require(tidyverse)
require(lubridate)
## Remove existing variables ####
rm(list=ls(all=TRUE))

url = "https://www.health.state.mn.us/diseases/coronavirus/situation.html"

mdhData = read_html(url) %>% 
  ## Use CSS tools to figure out needed nodes for data 
  html_nodes("li, p") %>% 
  html_text() %>% 
  as.tibble() %>% 
  ## get necessary data
  filter(str_detect(value, "Updated|Total approximate number of completed tests|Total positive:|Patients no longer needing isolation:|Deaths:|Hospitalized as of today: |Hospitalized in ICU as of today:")) %>% 
  ## format date string
  mutate(value = ifelse(str_detect(value, "Updated"), paste("Date:", format(mdy(str_match(value, "\r\n\tUpdated (.*?).\r\n\tUpdated")),"%m/%d/%y")), value),
         ## format hospitalized as of today and remove extra info in the end
         value = ifelse(str_detect(value, "Hospitalized as of today:"), word(value, sep = "\r\n  "), value)) %>% 
  ## filter out info we don't need
  filter(str_detect(value, "\r\n|from", negate = T)) %>% 
  ## separate variable into two columns by ":"
  separate(value, c("Name", "Value"), sep = ":") %>%
  ## remove space in the beginning, then remove "0" before month or remove comma for numbers
  mutate(Value = ifelse(str_detect(Value,"/"), str_remove(str_trim(Value, "left"),"^0"), str_remove(str_trim(Value, "left"), ","))) %>% 
  ## convert to wide format 
  pivot_wider(names_from = Name,
              values_from = Value) %>%
  ## rename colnames to match csv file "MNCovidData.csv"
  rename(Total.cases = `Total positive`, 
         Total.tested = `Total approximate number of completed tests`,
         Currently.hospitalized = `Hospitalized as of today`,
         ICU = `Hospitalized in ICU as of today`,
         Total.deaths = Deaths,          
         Total.recovered = `Patients no longer needing isolation`) %>% 
  ## convert variables to integer except for Date
  mutate_at(vars(-Date), as.integer)

## Add current data from MDH website to existing csv file  
read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  full_join(mdhData) %>% 
  write.csv("MNCovidData.csv", row.names = F)
  
## Read in response data from MN response dashboard
responseUrl = "https://mn.gov/covid19/data/response.jsp"
responseData = read_html(responseUrl) %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  filter(str_detect(value, "StateofMNResponseDashboardCSV")) %>%
  mutate(value = paste0("https://mn.gov", value)) %>% 
  pull(value) %>% 
  read.csv(na.strings = c("NA","")) %>% 
  select_if(~!all(is.na(.))) %>% 
  mutate(Date = format(mdy(Data.Date..MM.DD.YYYY.), "%m/%d/%y"),Value = as.integer(as.character(Value_NUMBER))) %>% 
  select(-starts_with("Geographic"), -URLLINK, -Value_Text, -Data.Date..MM.DD.YYYY., -Value_NUMBER) %>%
  filter(COVID.Team %in% c("Hospital Surge Capacity")) %>% 
  mutate(Metric = ifelse(str_detect(Metric, "Ventilator"), "Ventilator", "ICU beds"),
         Detail1 = factor(Detail1, levels = c("Surge - 72 hour", "Surge - 24 hour", "On back order", "Surge", "Current")))



