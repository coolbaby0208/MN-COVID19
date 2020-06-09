# MN-COVID-19
This r markdown file was setup for my personal use to look at COVID-19 data in Minnesota (not a lot of comments: added later if I have time). 

1. Please download all files to the same directory and run "COVID.Rproj"

2. Open "COVID_markdown.Rmd" and knit the pdf. This file will call "mdhCovidWebScrape.R" to grab response data and merge current COVID-19 data (from MDH website) into "MNCovidData.csv", and "COVID.R", which will preprocess "MNCovidData.csv" and generate plots.

3. Outputs a pdf file named "COVID_markdown.pdf".

Note: 
  - The csv data (current version: 2020-06-09) may not be updated regularly, but you can find the most recent data in the following link. 
  
  - COVID Data source: https://www.health.state.mn.us/diseases/coronavirus/situation.html
  
  - Response data source: "https://mn.gov/covid19/data/response.jsp"
  
  - Library needed for this R markdown: tidyverse, ggrepel, gridExtra, knitr, kableExtra, rvest, lubridate  
  
  - If you are interested in COVID-19 data by county, take a look in [CountyDataFromNYTimes](../master/CountyDataFromNYTimes). 

**Example output**
![Example output](https://github.com/coolbaby0208/MN-COVID19/blob/master/COVID_markdown.png)
