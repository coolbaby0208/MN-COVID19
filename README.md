# MN-COVID-19
This r markdown file was setup for my personal use to look at COVID-19 data in Minnesota (not a lot of comments: added later if I have time). 

1. Please download all files to the same directory and run "COVID.Rproj"

2. Open "COVID_markdown.Rmd" and knit the pdf. This file will call "mdhCovidWebScrape.R" to grab response data and merge current COVID-19 data (from MDH website) into "MNCovidData.csv", and "COVID.R", which will preprocess "MNCovidData.csv" and generate plots.

3. Outputs a pdf file named "COVID_markdown.pdf".

Note: 
  - The csv data (current version: 2020-07-24) may not be updated regularly, but you can find the most recent data in the following link. The daily positive case number reported here is using today's total positve cases minus the the total positive cases from the previous day in the MDH Daily Update. So the positive case number may be slightly different from the positive case count by specimen colletced date.   
  
  - COVID Data source: https://www.health.state.mn.us/diseases/coronavirus/situation.html
  
  - Response data source: "https://mn.gov/covid19/data/response.jsp"
  
  - Library needed for this R markdown: tidyverse, ggrepel, gridExtra, knitr, kableExtra, rvest, lubridate  
  
  - If you are interested in COVID-19 data by county, take a look in [CountyDataFromNYTimes](../master/CountyDataFromNYTimes). 
  
Major change in the 1st and 2nd plots: 2020-07-15

Data for New cases, Daily tests, Daily positive rate and Case fatality rate are using either specimen collection date (e.g., New cases) or date reported to MDH (e.g., Daily tests from testing data table) if the data is more than a week old. 

For data within a week, the variables mentioned above will be calculated using MDH Daily Update. 
For example, 
New cases = Today's Total positive cases (cumulative) - Yesterday's Total positive cases (cumulative) 
Daily tests = Today's Total approximate number of completed tests - Yesterday's Total approximate number of completed tests

Differences between the 1st and 2nd plots. 

Because Positive cases by date specimen collected data table starts on 3/5 and Testing data table starts on 3/28, you only see new cases data from 3/5 to 3/28 and you won't see data before 3/28 in the 2nd plot. 


**Example output**
![Example output](https://github.com/coolbaby0208/MN-COVID19/blob/master/COVID_markdown.png)
