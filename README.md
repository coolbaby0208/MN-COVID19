# MN-COVID-19
This r markdown file was setup for my personal use to look at COVID-19 data in Minnesota (not a lot of comments: added later if I have time). 

1. Please download all files to the same directory and run "COVID.Rproj"

2. Open "COVID_markdown.Rmd" and knit the pdf. This file will call "mdhCovidWebScrape.R" to grab response data and merge current COVID-19 data (from MDH website) into "MNCovidData.csv", and "COVID.R", which will preprocess "MNCovidData.csv" and generate plots.

3. Outputs a pdf file named "COVID_markdown.pdf".

Note: 
  - The csv data (current version: 2020-09-01) may not be updated regularly, but you can find the most recent data in the following link. The daily positive case number reported here is using today's total positve cases minus the the total positive cases from the previous day in the MDH Daily Update. So the positive case number may be slightly different from the positive case count by specimen colletced date.   
  
  - COVID Data source: https://www.health.state.mn.us/diseases/coronavirus/situation.html
  
  - Response data source: "https://mn.gov/covid19/data/response.jsp"
  
  - Library needed for this R markdown: tidyverse, ggrepel, gridExtra, knitr, kableExtra, rvest, lubridate  
  
  - If you are interested in COVID-19 data by county, take a look in [CountyDataFromNYTimes](../master/CountyDataFromNYTimes). 

Minor update: 2021-01-05
====
Add html version: https://htmlpreview.github.io/?https://github.com/coolbaby0208/MN-COVID19/blob/master/COVID_markdown_html.html

Minor change: 2020-12-29
====
Add vaccination data (source: https://mn.gov/covid19/vaccine/data/index.jsp)


Major change: 2020-10-14
====
Major change due to MDH reports data in new format


Major change: 2020-10-09
====
Major change due to MDH reports "Current hospitalization" with a differetn dataset
Link: https://mn.gov/covid19/assets/HospitalCapacity_HistoricCSV_tcm1148-449110.csv

Add current hospitalization and ICU numbers to the 2nd plot of the right column. 
Also made changes to the 3rd plot of the right column to reflect change in data structure from MDH.

Table is revised to show current hospitalization and current ICU numbers.

Major change: 2020-09-24
====
Major change due to MDH changes how they report "Hospitalization"

Top right plot now shows the following 3 curves and two bars:
Curves:
- Total hospitalization (cumulative)
- Total ICU (cumulative)
- Total deaths (cumulative)

Bars:
- New hospitalization
- New ICU

Table is revised to show daily new hospitalization and daily new ICU numbers.

Another change in the 2nd plot: 2020-08-18
====
Add positive percentage by number of people tested daily in addition to number of daily tests. This data was reported starting on 2020-07-30 to account for multiple testings of the same individual.

Major change in the 1st and 2nd plots: 2020-07-15
====
Data for New cases, Daily tests, Daily positive rate and Case fatality rate are using either specimen collection date (e.g., New cases) or date reported to MDH (e.g., Daily tests from testing data table) if the data is more than a week old. 

For data within a week, the variables mentioned above will be calculated using MDH Daily Update. 
 - New cases = Today's Total positive cases (cumulative) - Yesterday's Total positive cases (cumulative) 
 - Daily tests = Today's Total approximate number of completed tests - Yesterday's Total approximate number of completed tests

For data older than a week, the variables mentioned above will be calculated as the following. 
 - New cases from *Positive cases by date specimen collected data table*
 - Daily tests from *Testing data table*: Total approximate number of completed tests - Total approximate number of completed tests from the previous day

Differences between the 1st and 2nd plots (left column). 

Because Positive cases by date specimen collected data table starts on 3/5 and Testing data table starts on 3/28, you only see new cases data from 3/5 to 3/28 and you won't see data before 3/28 in the 2nd plot. 

Example output
====
![Example output](https://github.com/coolbaby0208/MN-COVID19/blob/master/COVID_markdown.png)
