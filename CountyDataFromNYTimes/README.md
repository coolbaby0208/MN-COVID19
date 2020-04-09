# MN-COVID-19
This r markdown file was setup for my personal use to look at COVID-19 data in Minnesota (not a lot of comments: added later if I have time). 

1. Please download all files to the same directory and run "COVID.Rproj"

2. Open "COVID_markdown.Rmd" and knit the pdf. This file will call "COVID.R", which will preprocess "MNCovidData.csv" and generate plots and save a png file as "MN-COVID-19_2020-0x-0x.png".

3. Outputs a pdf file named "COVID_markdown.pdf".

Note: 
  - The csv data (current version: 2020-04-08) may not be updated regularly, but you can find the most recent data in the following link. 
  
  - Data source: https://www.health.state.mn.us/diseases/coronavirus/situation.html
  
  - Library needed for this R markdown: tidyverse, gridExtra, knitr, kableExtra  

**Example output**
![Example output](https://github.com/coolbaby0208/MN-COVID19/blob/master/CountyDataFromNYTimes/county.png)
