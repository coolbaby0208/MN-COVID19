# MN-COVID-19 by county
This r script was setup for my personal use to look at COVID-19 data in Minnesota (not a lot of comments: added later if I have time). 

1. Please download both files "COVID-NYTImeData.R" & "MNCountyPopulation.csv" to the same directory

2. Run "COVID-NYTImeData.R", which will grab county data from NYtimes github repo and generate plots and save png files as "MNcounty_COVID-19_Lollipop.png" and "MNcounty_COVID-19_Map.png".


Note: 
  
  - COVID-19 data source: https://github.com/nytimes/covid-19-data
  
  - Population data source: https://www.minnesota-demographics.com/counties_by_population
  
  - Library needed for this R markdown: tidyverse, ggpubr, usmap 

**Example output**
![Example output](https://github.com/coolbaby0208/MN-COVID19/blob/master/CountyDataFromNYTimes/MNcounty_COVID-19_Lollipop.png)
![Example output](https://github.com/coolbaby0208/MN-COVID19/blob/master/CountyDataFromNYTimes/MNcounty_COVID-19_Map.png)
