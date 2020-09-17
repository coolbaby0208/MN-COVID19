## data source: https://mn.gov/covid19/data/response
## data source: https://www.health.state.mn.us/diseases/coronavirus/situation.html
## The R script scrape MDH website current COVID-19 data and merge it with existing csv file "MNCovidData.csv"
## This script also scrape url from MN reposne dashboard and import the csv data as a data frame "responseData" and pass it into "COVID.R" for plots 
## Credit: coolbaby0208

require(rvest) ## web scrape library
require(tidyverse)
require(lubridate) ## deal with date 
require(purrr)
require(RCurl)

## Remove existing variables ####
rm(list=ls(all=TRUE))

## Scrape MDH website COVID-19 data  
url = "https://www.health.state.mn.us/diseases/coronavirus/situation.html"

## Updated on 2020-08-18 to include total number of people tested
## instead of total tests since some people got tested multiple times

mdhData = url %>% 
  read_html() %>% 
  ## use CSS tools to figure out needed nodes for data 
  html_nodes("li, p") %>% 
  html_text() %>% 
  ## convert list to a tibble
  as_tibble() %>% 
  ## Change on 2020-08-24 due to MDH website change
  filter(str_detect(value, "Updated|Total approximate number of completed tests:|Total approximate number of people tested:|Total positive cases:|Patients no longer needing isolation:|Deaths:|Hospitalized"),
         str_detect(value, "Updated every", negate = T)) %>% 
  mutate(value = ifelse(str_detect(value, "Updated"), 
                        paste("Date:", value %>% str_match("Updated (.*?).\r\nUpdated") %>% mdy() %>% format("%m/%d/%y")), value),
         ## format hospitalized as of today and remove extra info in the end
         value = ifelse(str_detect(value, "Hospitalized as of today|Deaths:|Total positive cases:"), word(value, sep = "\r\n"), value),
         ## Change on 2020-08-24 due to MDH website change
         value = str_remove(value, "Refer to \"More about hospitalizations\" for notes.")) %>% 
  separate(value, c("Name", "Value"), sep = ":") %>%
  ## remove space in the beginning, then remove "0" before month or remove comma for numbers
  mutate(Value = ifelse(str_detect(Value,"/"), Value %>% str_trim("left") %>% str_remove("^0"), Value %>% str_trim("left") %>% str_replace_all("[[:punct:]]", ""))) %>% 
  pivot_wider(names_from = Name,
              values_from = Value) %>%
  ## rename colnames to match csv file "MNCovidData.csv"
  rename(Total.cases = `Total positive cases`, 
         Total.tested = `Total approximate number of completed tests`,
         Currently.hospitalized = `Hospitalized as of today`,
         ICU = `Hospitalized in ICU as of today`,
         Total.deaths = Deaths,          
         Total.recovered = `Patients no longer needing isolation`,
         Total.tested.people = `Total approximate number of people tested`) %>% 
  ## convert variables to integer except for Date
  mutate_at(vars(-Date), as.integer) %>% 
  select(-starts_with("Total approximate")) %>%
  ## edit 2020-05-18 due to MDH website change
  # mutate(Total.recovered = Total.recovered - Total.deaths,
  #        Date = Date %>% mdy()) 
  mutate(Date = Date %>% mdy()) %>% 
  # Fix stupid typo on MDH website
  mutate(Total.cases = ifelse(Date == "2020-06-18", 31675, Total.cases))

## Output data to csv file
read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  mutate(Date = Date %>% ymd()) %>% 
  full_join(mdhData) %>% 
  distinct(Date, .keep_all = T) %>% 
  write.csv("MNCovidData.csv", row.names = F) 

## Added on 2020-07-14 to take care of the following two variables changed by MDH within the past 7 days
## 1. Total testing
## 2. Positive cases

## For extracting data for positive cases by specimen date
mdhDataTable = url %>% 
  read_html() %>% 
  ## use CSS tools to figure out needed nodes for data 
  html_nodes("table") %>% 
  lmap(html_table, fill = TRUE)

## Get testing number from report date
testReportDate = mdhDataTable[[5]] %>% 
  rename(DateReport = `Date reported to MDH`,
         MDHTestsByReportDate = `Completed tests reported from the MDH Public Health Lab (daily)`,
         ExternalTestsByReportDate  = `Completed tests reported from external laboratories (daily)`,
         TotalTestsByReportDate = `Total approximate number of completed tests (cumulative)`) %>% 
  mutate(DateReport = as.Date(DateReport , "%m/%d")) %>% 
  mutate_at(vars(-DateReport), ~str_remove_all(., ",") %>% as.double())

## Get positve cases for specimen collection date
dataSpecimenDate = mdhDataTable[[6]] %>% 
  rename(DateReport = `Specimen collection date`,
         New.casesBySpecimenDate = `Positive cases`,
         Total.casesBySpecimenDate = `Total positive cases (cumulative)`) %>% 
  mutate(DateReport = as.Date(DateReport, "%m/%d"),
         Total.casesBySpecimenDate =  str_remove_all(Total.casesBySpecimenDate, ",") %>% as.double()) %>% 
  ## combine with testing data
  full_join(testReportDate, by = "DateReport") %>% 
  mutate(Date = DateReport + 1) %>%
  ## drop Data with missing date
  drop_na(DateReport)

## Combine data from daily update with data reported by specimen & reported dates
## edit on 2020-07-15
data = read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  ## remove data with missing date
  drop_na(Date) %>% 
  mutate_at(vars(starts_with("Date")), ymd) %>%
  ## since the values in duplicated variables will change
  ## remove duplicated variables before full_join
  select(-DateReport:-TotalTestsByReportDate) %>% 
  full_join(dataSpecimenDate) %>% 
  arrange(Date) %>% 
  write.csv("MNCovidData.csv", row.names = F) 

## Read in MN response data for hospital capacity
## Web Address changed https://mn.gov/covid19/data/response-prep on 2020-04-17
## 
## Function to extract csv file url
## Avoid error reading in reponse url 
# getResponseDataUrl = function(url){
#   out = read_html(url) %>% 
#     html_nodes("a") %>%
#     ## find attributes we need
#     html_attr("href") %>%
#     as_tibble() %>%
#     ## extract part of csv data url
#     filter(str_detect(value, "StateofMNResponseDashboardCSV")) %>%
#     ## complete the csv data url
#     mutate(value = paste0("https://mn.gov", value)) %>%
#     pull(value)
#   return(out)
# }

## default url for response prep
 responseUrl = "https://mn.gov/covid19/data/response-prep/response-capacity.jsp" 
## alternative direct link to csv file
## "https://mn.gov/covid19/assets/StateofMNResponseDashboardCSV_tcm1148-427143.csv"
## extract url for csv file
## possibly is from "purrr"
## This does not work because of website change on 2020-05-14
# responseData = possibly(getResponseDataUrl, 
#                            otherwise ="https://mn.gov/covid19/assets/StateofMNResponseDashboardCSV_tcm1148-427143.csv")(responseUrl) %>% 

## Revised on 2020-06-17  
## Get response prep data
## Use tryCatch if response csv file is not available (access denied)
## If error, the function will look for a local csv file: "StateofMNResponseDashboardCSV_tcm1148-427143.csv"
 responseDataExtract = function(fileLoc){
   out = tryCatch(read.csv(fileLoc, na.strings = c("NA","")) %>% 
                    ## remove columns with All NAs
                    select_if(~!all(is.na(.))) %>%
                    filter(!is.na(Detail3)),
                  error = function(e) read.csv("StateofMNResponseDashboardCSV_tcm1148-427143.csv", na.strings = c("NA","")) %>% 
                    ## remove columns with All NAs
                    select_if(~!all(is.na(.))) %>%
                    filter(!is.na(Detail3)))
   out = out %>% 
     ## remove punctuation
     mutate(Value = Value_NUMBER %>% as.character() %>% str_replace_all("[[:punct:]]", "") %>% as.integer()) %>% 
     # mutate(Date = Data.Date..MM.DD.YYYY. %>% as.character() %>% as.numeric() %>% as_date(origin = "1899-12-30") %>% ymd() %>% format("%m/%d/%y"),
     #        DateUpdate = Date.and.time.of.update %>% as.character() %>% as.numeric() %>% as_date(origin = "1899-12-30") %>% ymd() %>% format("%m/%d/%y")) %>%
     ## edit on 2020-06-16 due to change back to original dat format
     ## try to accomodate both formats
     mutate(Date = ifelse(Data.Date..MM.DD.YYYY. %>% ymd() %>% is.na(), 
                           Data.Date..MM.DD.YYYY. %>% as.character() %>% as.numeric() %>% as_date(origin = "1899-12-30") %>% ymd() %>% format("%m/%d/%y"), 
                           Data.Date..MM.DD.YYYY. %>% ymd() %>% format("%m/%d/%y")),
            DateUpdate = ifelse(Date.and.time.of.update %>% ymd() %>% is.na(), 
                                Date.and.time.of.update %>% as.character() %>% as.numeric() %>% as_date(origin = "1899-12-30") %>% ymd() %>% format("%m/%d/%y"), 
                                Date.and.time.of.update %>% ymd() %>% format("%m/%d/%y"))) %>% 
     # mutate(Date = Data.Date..MM.DD.YYYY. %>% mdy() %>% format("%m/%d/%y"), 
     #        DateUpdate = Date.and.time.of.update %>% mdy() %>% format("%m/%d/%y")) %>% 
     ## remove unnecessary columns
     select(-starts_with("Geographic"), -starts_with("URL"), -Value_Text, -Data.Date..MM.DD.YYYY., -Value_NUMBER) %>%
     filter(COVID.Team %in% c("Hospital Surge Capacity")) %>% 
     ## rename levels and refactor Detail1
     mutate(Metric = ifelse(str_detect(Metric, "Ventilator"), "Ventilator", "ICU beds"),
            Detail1 = factor(Detail1, levels = c("Surge - 72 hour", "Surge - 24 hour", "On back order", "Surge", "Current")))
   return(out)
 }
 responseData = responseDataExtract("https://mn.gov/covid19/assets/StateofMNResponseDashboardCSV_tcm1148-427143.csv")
 
 