## data source: https://mn.gov/covid19/data/response
## data source: https://www.health.state.mn.us/diseases/coronavirus/situation.html
## The R script scrape MDH website current COVID-19 data and merge it with existing csv file "MNCovidData.csv"
## This script also scrape url from MN reposne dashboard and import the csv data as a data frame "responseData" and pass it into "COVID.R" for plots 
## Credit: coolbaby0208

require(rvest) ## web scrape library
require(tidyverse)
require(lubridate) ## deal with date 

## Remove existing variables ####
rm(list=ls(all=TRUE))

## Scrape MDH website COVID-19 data  
url = "https://www.health.state.mn.us/diseases/coronavirus/situation.html"

mdhData = read_html(url) %>% 
  ## use CSS tools to figure out needed nodes for data 
  html_nodes("li, p") %>% 
  html_text() %>% 
  ## convert list to a tibble
  as_tibble() %>% 
  ## get necessary data for COVID.R
  filter(str_detect(value, "Updated|Total approximate number of completed tests|Total positive:|Patients no longer needing isolation:|Deaths:|Hospitalized as of today: |Hospitalized in ICU as of today:")) %>% 
  ## format date string using regular expression
  mutate(value = ifelse(str_detect(value, "Updated"), 
                        paste("Date:", value %>% str_match("\r\n\tUpdated (.*?).\r\n\tUpdated") %>% mdy() %>% format("%m/%d/%y")), value),
       ## format hospitalized as of today and remove extra info in the end
       value = ifelse(str_detect(value, "Hospitalized as of today:"), word(value, sep = "\r\n  "), value)) %>% 
  ## filter out info we don't need
  filter(str_detect(value, "\r\n|from", negate = T)) %>% 
  ## separate variable into two columns by ":"
  separate(value, c("Name", "Value"), sep = ":") %>%
  ## remove space in the beginning, then remove "0" before month or remove comma for numbers
  mutate(Value = ifelse(str_detect(Value,"/"), Value %>% str_trim("left") %>% str_remove("^0"), Value %>% str_trim("left") %>% str_remove(","))) %>% 
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
  
## Read in MN response data for hospital capacity 
responseUrl = "https://mn.gov/covid19/data/response.jsp"
responseData = read_html(responseUrl) %>% 
  ## use CSS tools to find nodes
  html_nodes("a") %>% 
  ## find attributes we need 
  html_attr("href") %>% 
  as_tibble() %>% 
  ## extract part of csv data url
  filter(str_detect(value, "StateofMNResponseDashboardCSV")) %>%
  ## complete the csv data url
  mutate(value = paste0("https://mn.gov", value)) %>% 
  pull(value) %>% 
  ## read in data
  read.csv(na.strings = c("NA","")) %>% 
  ## remove columns with All NAs
  select_if(~!all(is.na(.))) %>%
  ## format Date and Values 
  mutate(Date = Data.Date..MM.DD.YYYY. %>% mdy() %>% format("%m/%d/%y"), 
         Value = Value_NUMBER %>% as.character() %>% as.integer(),
         DateUpdate = Date.and.time.of.update %>% mdy_hm() %>% format("%m/%d/%y")) %>% 
  ## remove unnecessary columns
  select(-starts_with("Geographic"), -URLLINK, -Value_Text, -Data.Date..MM.DD.YYYY., -Value_NUMBER) %>%
  filter(COVID.Team %in% c("Hospital Surge Capacity")) %>% 
  ## rename levels and refactor Detail1
  mutate(Metric = ifelse(str_detect(Metric, "Ventilator"), "Ventilator", "ICU beds"),
         Detail1 = factor(Detail1, levels = c("Surge - 72 hour", "Surge - 24 hour", "On back order", "Surge", "Current")))
