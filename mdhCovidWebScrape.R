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
## Updated on 2020-10-14 due to website change
DateUpdated = url %>% 
  read_html() %>% 
  ## use CSS tools to figure out needed nodes for data 
  html_nodes("li, p") %>% 
  html_text() %>% 
  ## convert list to a tibble
  as_tibble() %>% 
  ## Change on 2020-08-24 due to MDH website change
  filter(str_detect(value, "Updated")) %>% 
  ## revised on 11-24-2020 due to website change
  mutate(value = ifelse(str_detect(value, "Updated"), 
                        paste("Date:", value %>% str_match("Updated (.*?).\r\nUpdated") %>% mdy %>% format("%m/%d/%y")), value)) %>% 
  separate(value, c("Variable", "Value"), sep = ":") %>%
  mutate(Date = Value %>% str_trim("left")) %>%
  filter(Date!="NA") %>% 
  select(Date)

## For extracting all data
mdhDataTable = url %>% 
  read_html() %>% 
  ## use CSS tools to figure out needed nodes for data 
  html_nodes("table") %>% 
  lmap(html_table, fill = TRUE)

## Add vaccine data on 2020-12-29
## Remove vaccine data on 2021-01-12
# vacUrl = "https://www.health.state.mn.us/diseases/coronavirus/vaccine/stats/admin.html"
# vacData = vacUrl %>% 
#   read_html() %>% 
#   ## use CSS tools to figure out needed nodes for data 
#   html_nodes("table") %>% 
#   lmap(html_table, fill = TRUE)

## Vaccine csv data on 2021-01-14
vacUrl = "Doses Administered_tcm1148-462846.csv"
vacData = read.csv(vacUrl, header = T) %>%
  filter(dimension == "Total vaccine doses administered") %>% 
  rename(X1 = dimension, X2 = Count) %>% 
  select(X1,X2)

## Add vaccince data by people
## added on 2021-03-18
vacPplUrl = "People Vaccinated, By Age_tcm1148-467653.csv"
vacPplData = read.csv(vacPplUrl, header = T) %>%
  rename(Age = Age.group,
         OneDose = People.with.at.least.one.vaccine.dose,
         Completed = People.with.completed.vaccine.series,
         DateReport = reportedDate) %>%
  pivot_longer(cols = c(OneDose,Completed), names_to = "Variable", values_to = "People") %>% 
  mutate(DateReport = mdy(DateReport),
         Age = fct_recode(Age, "Missing" = "Unknown/missing")) 
## Used for plotting by age group
vacPplDataSum = vacPplData %>% 
  group_by(DateReport,webDate, Variable) %>% 
  summarise(People = sum(People)) %>% 
  mutate(Age = "Total") %>% 
  full_join(vacPplData) %>% 
  mutate(Variable = fct_relevel(Variable, "Completed", after = 1))
## For merging into the main "MNCovidData.csv" data
vacPpl = vacPplDataSum %>% 
  filter(Age == "Total") %>% 
  ungroup() %>% 
  select(Variable,People) %>% 
  rename(X1 = Variable, X2 = People) %>% 
  mutate(X1 = recode_factor(X1, "Completed"= "Total.people.vaccine.completed",
                            "OneDose"="Total.people.vaccine.onedose"))

## Added on 2020-10-14
## edited on 2021-03-22
## change it back on 2021-03-23
# totalCase = mdhDataTable[[1]]
# newCase = mdhDataTable[[2]]
# newDeath = mdhDataTable[[4]]
# totalTests = mdhDataTable[[7]]
# totalPeopleTested = mdhDataTable[[9]]
# totalRecovered = mdhDataTable[[13]]
# totalDeath = mdhDataTable[[14]]
# totalHospitalized = mdhDataTable[[17]]

mdhData = rbind(mdhDataTable[[1]], mdhDataTable[[2]], mdhDataTable[[7]], mdhDataTable[[9]], mdhDataTable[[13]], mdhDataTable[[14]], vacData, vacPpl) %>% 
# Add on 2021-05-10
#mdhData = rbind(mdhDataTable[[1]], mdhDataTable[[2]], mdhDataTable[[5]], mdhDataTable[[7]], mdhDataTable[[11]], mdhDataTable[[12]], vacData, vacPpl) %>%   
  rename(Variable = X1, Value = X2) %>%
  mutate(Value = Value %>% str_remove_all("[[:punct:]]") %>% as.numeric()) %>% 
  pivot_wider(names_from = Variable, values_from = Value) %>% 
  bind_cols(DateUpdated) %>%
  rename(Total.cases = `Total positive cases (cumulative)`,
         Total.tested = `Total approximate completed tests (cumulative)`,
         Total.tested.people = `Total approximate number of people tested (cumulative)`, 
         Total.recovered = `Patients no longer needing isolation (cumulative)`,
         Total.deaths = `Total deaths (cumulative)`,
         Total.vaccine = `Total vaccine doses administered`) %>% 
  select(Date,starts_with("Total.")) %>% 
  mutate(Date = Date %>% mdy)

## Output data to csv file
read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  mutate(Date = Date %>% ymd) %>% 
  full_join(mdhData) %>% 
  distinct(Date, .keep_all = T) %>% 
  write.csv("MNCovidData.csv", row.names = F) 

## Get testing number from report date
## Revised on 2020-10-14
## Fix date issue on 2021-01-02
testReportDate = mdhDataTable[[8]] %>% 
# Add on 2021-05-10
#testReportDate = mdhDataTable[[6]] %>%   
  rename(DateReport = `Date reported to MDH`,
         MDHTestsByReportDate = `Completed PCR tests reported from the MDH Public Health Lab`,
         ExternalPcr = `Completed PCR tests reported from external laboratories`,
         TotalPcr = `Total approximate number of completed PCR tests (cumulative)`,
         ExternalAntigen = `Completed antigen tests reported from external laboratories`,
         TotalAntigen = `Total approximate number of completed antigen tests (cumulative)`,
         TotalTestsByReportDate = `Total approximate number of completed tests (cumulative)`) %>% 
  mutate_at(vars(-DateReport), .%>% as.character() %>% str_remove_all("[[:punct:]]") %>% as.numeric) %>% 
  mutate(ExternalTestsByReportDate = ExternalPcr+ExternalAntigen,
         DateReport = as.Date(DateReport , "%m/%d/%y"))

## Get positve cases for specimen collection date
## Updated on 2020-10-14
## Edit on 2021-03-05 DateReport is now with "year" added
dataSpecimenDate = mdhDataTable[[11]] %>% 
# Add on 2021-05-10
#dataSpecimenDate = mdhDataTable[[9]] %>% 
  rename(DateReport = `Specimen collection date`,
         ProbableCase = starts_with("Probable"),
         ConfirmedCase = starts_with("Confirmed cases"),
         Total.casesBySpecimenDate = `Total positive cases (cumulative)`) %>% 
  mutate_at(vars(-DateReport), .%>% as.character() %>% str_remove_all("[[:punct:]]") %>% as.numeric) %>% 
  mutate(New.casesBySpecimenDate = Total.casesBySpecimenDate - lag(Total.casesBySpecimenDate),
         DateReport = as.Date(DateReport , "%m/%d/%y")) %>% 
  select(DateReport, ends_with("SpecimenDate"))%>% 
  ## combine with testing data
  full_join(testReportDate, by = "DateReport") %>% 
  mutate(Date = DateReport + 1) %>%
  ## drop Data with missing date
  drop_na(DateReport)

## Get hospital admitted data: completely changed on Sep24
## Edit on 2021-03-06 DateReport is now with "year" added
hospitalData = mdhDataTable[[18]] 
# Add on 2021-05-10
#hospitalData = mdhDataTable[[16]] 
names(hospitalData)<-str_replace_all(names(hospitalData), c(" " = "" , "," = "" ))
hospitalAdmitData = hospitalData %>% 
  rename(IcuAdmit = `CasesadmittedtoanICU`,
         HospitalAdmit = `Casesadmittedtoahospital`,
         Total.ICU = `TotalICUhospitalizations(cumulative)`,
         Total.Hospital = `Totalhospitalizations(cumulative)`) %>% 
  mutate_at(vars(-Date), ~str_remove_all(., ",") %>% as.double()) %>% 
  mutate(DateReport = as.Date(Date , "%m/%d/%y")) %>% 
  select(DateReport, IcuAdmit:Total.ICU) %>% 
  drop_na(DateReport) %>% 
  full_join(dataSpecimenDate, by = "DateReport") %>% 
  arrange(DateReport)

## Get hospitalization data
## Add on 2020-10-09
## Revise on 2020-11-30
hospitalizationExtract = function(fileLoc){
  out = tryCatch(read.csv(fileLoc, na.strings = c("NA","")) %>% 
                   ## remove columns with All NAs
                   select_if(~!all(is.na(.))) %>% 
                   mutate(Value = Value_NUMBER), 
                 error = function(e) read.csv("TELETRACKING_ICU_NonICU_Beds_in_Use_CSV_tcm1148-455097.csv", na.strings = c("NA","")) %>% 
                   ## remove columns with All NAs
                   select_if(~!all(is.na(.))))
  return(out)
}

hospitalizationData = hospitalizationExtract("TELETRACKING_ICU_NonICU_Beds_in_Use_CSV_tcm1148-455097.csv") %>% 
  mutate(DateReport = Data.Date..MM.DD.YYYY. %>% ymd(),
         ## Add on 2020-10-26 to fix excel date entry value
         ## Revise on 2020-10-31 to fix excel date entry value
         DateReport = if_else(is.na(DateReport), as.character(Data.Date..MM.DD.YYYY.) %>% 
                                as.numeric %>% 
                                as.Date(origin = "1899-12-30"), DateReport)) %>% 
  arrange(DateReport) %>% 
  filter(GeographicLevel == "State", Detail3 == "COVID+") %>% 
  select(DateReport, Detail1, Value_NUMBER) %>%
  pivot_wider(names_from = Detail1, values_from = Value_NUMBER) %>% 
  rename(Currently.hospitalized = `Non-ICU`) %>% 
  ## After 2020-08-01 the hospitalizations for non-ICU and ICU are reported separately
  mutate(Currently.hospitalized = Currently.hospitalized+ICU)

## Combine data from daily update with data reported by specimen & reported dates
## edit on 2020-07-15
## edit on 2020-10-09 to add back hospitalization data
data = read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  ## remove data with missing date
  drop_na(Date) %>% 
  mutate_at(vars(starts_with("Date")), ymd) %>%
  ## since the values in duplicated variables will change
  ## remove duplicated variables before full_join
  select(-DateReport:-ExternalTestsByReportDate) %>% 
  full_join(hospitalAdmitData) 

data %>% write.csv("MNCovidData.csv", row.names = F)

## Check if the ICU data is upto date
## Check if the date is not Sunday and Saturday
if (!last(data$Date) %>% wday %in% c(1,7) && (!hospitalizationData %>% filter(DateReport == last(DateReport)) %>% pull(ICU) %>% is.na)) {
  data %>%
    mutate(ICU = ifelse(Date>"2020-08-01", NA, ICU),
           Currently.hospitalized = ifelse(Date>"2020-08-01", NA, Currently.hospitalized)) %>%
    select(-ends_with(".y"), -ends_with(".x")) %>%
    full_join(hospitalizationData, by = "DateReport", copy = T) %>%
    mutate(ICU = coalesce(ICU.x, ICU.y),
           Currently.hospitalized = coalesce(Currently.hospitalized.x, Currently.hospitalized.y)) %>%
    select(-ends_with(".x"),-ends_with(".y")) %>%
    #filter(!is.na(DateReport)) %>%
    arrange(Date) %>%
    write.csv("MNCovidData.csv", row.names = F)
}

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

# https://mn.gov/covid19/assets/HospitalCapacity_HistoricCSV_tcm1148-449110.csv 
# https://mn.gov/covid19/assets/StateofMNResponseDashboardCSV_tcm1148-427143.csv 

## Revised on 2020-06-17  
## Get response prep data
## Use tryCatch if response csv file is not available (access denied)
## If error, the function will look for a local csv file: "StateofMNResponseDashboardCSV_tcm1148-427143.csv"
responseDataExtract = function(fileLoc){
  out = tryCatch(read.csv(fileLoc, na.strings = c("NA","")) %>% 
                   ## remove columns with All NAs
                   select_if(~!all(is.na(.))) %>% 
                   mutate(Value = Value_NUMBER), 
                 error = function(e) read.csv("StateofMNResponseDashboardCSV_tcm1148-427143.csv", na.strings = c("NA","")) %>% 
                   ## remove columns with All NAs
                   select_if(~!all(is.na(.))))
  out = out %>% 
    ## remove punctuation
    mutate(Value = Value_NUMBER %>% as.character() %>% str_replace_all("[[:punct:]]", "") %>% as.integer()) %>% 
    # mutate(Date = Data.Date..MM.DD.YYYY. %>% as.character() %>% as.numeric() %>% as_date(origin = "1899-12-30") %>% ymd() %>% format("%m/%d/%y"),
    #        DateUpdate = Date.and.time.of.update %>% as.character() %>% as.numeric() %>% as_date(origin = "1899-12-30") %>% ymd() %>% format("%m/%d/%y")) %>%
    ## edit on 2020-06-16 due to change back to original dat format
    ## try to accomodate both formats
    mutate(Date = ifelse(Data.Date..MM.DD.YYYY. %>% mdy() %>% is.na(), 
                         Data.Date..MM.DD.YYYY. %>% as.character() %>% as.numeric() %>% as_date(origin = "1899-12-30") %>% ymd() %>% format("%m/%d/%y"), 
                         Data.Date..MM.DD.YYYY. %>% mdy() %>% format("%m/%d/%y")),
           DateUpdate = ifelse(Date.and.time.of.update %>% mdy() %>% is.na(), 
                               Date.and.time.of.update %>% as.character() %>% as.numeric() %>% as_date(origin = "1899-12-30") %>% ymd() %>% format("%m/%d/%y"), 
                               Date.and.time.of.update %>% mdy() %>% format("%m/%d/%y"))) %>% 
    filter(COVID.Team %in% c("Hospital Surge Capacity"), GeographicLevel %in% c("State"), Value>0) %>% 
    mutate(Detail1 = ifelse(str_detect(Detail1,"Beds"), paste(Detail1,"surge"), as.character(Detail1)),
           Detail2 = str_to_sentence(Detail2),
           Detail3 = ifelse(is.na(Detail3), Detail2, as.character(Detail3)) %>% factor(levels = c("In warehouse","Surge","Capacity","In use", "Available"))) %>%
    group_by(Detail1,Detail2) %>%
    mutate(count = n()) %>%
    arrange(Date)
  return(out)
}
responseData = responseDataExtract("https://mn.gov/covid19/assets/StateofMNResponseDashboardCSV_tcm1148-427143.csv") 