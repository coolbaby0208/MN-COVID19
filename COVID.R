## data source: https://www.twincities.com/coronavirus-mn/cases-map/
## data source: https://www.health.state.mn.us/diseases/coronavirus/situation.html
## The R script imports COVID data from "MNCovidData.csv" and generate 4 plots
## p1: Daily new cases and deaths 
## p2: Daily positive case percentage
## p3: Current hospitalized, current ICU, total death, current hospitalized percentage of active cases and current ICU percentage of hospitalized cases
## p4: Hospital surge capacity for ICU and ventilators

## Credit: coolbaby0208

## Load library
library(tidyverse)
library(ggrepel)
library(lubridate)

## Data preprocessing
## tidy up code on 2020-04-17
## edit on 2020-07-14 due to adding data by report/specimen collection date
## edit on 2020-08-18 to include number of people tested
## edit on 2020-09-24 due to change in reporting ICU and hospitalized number 
## add exception date to fix errors due to holidays 
exceptionDate = c("2020-11-27")

dataWide = read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  drop_na(Date) %>% 
  ## add on 2020-09-24
  ## add it back on 2020-10-09
  #select(-Currently.hospitalized, -ICU) %>% 
  # format date string and compute values for plotting 
  mutate(Date = Date %>% ymd(),
         Daily.tests = ifelse(Date != exceptionDate, Total.tested - lag(Total.tested),Total.tested - lag(Total.tested,2)),
         ## add on 2020-08-18
         Daily.tests.people = ifelse(Date != exceptionDate, Total.tested.people - lag(Total.tested.people),Total.tested.people - lag(Total.tested.people,2)),
         New.cases = ifelse(Date != exceptionDate, Total.cases - lag(Total.cases),Total.cases - lag(Total.cases,2)), 
         Currently.sick = Total.cases - Total.recovered - Total.deaths,
         New.deaths = ifelse(Date != exceptionDate, Total.deaths - lag(Total.deaths),Total.deaths - lag(Total.deaths,2)),
         New.ICU = ifelse(Date != exceptionDate,Total.ICU - lag(Total.ICU), Total.ICU - lag(Total.ICU,2)),
         New.hospitalized = ifelse(Date != exceptionDate,Total.Hospital - lag(Total.Hospital),Total.Hospital - lag(Total.Hospital,2)),
         New.sick = ifelse(Date != exceptionDate, Currently.sick - lag(Currently.sick),Currently.sick - lag(Currently.sick,2)),
         PositivePercent = New.cases/Daily.tests, 
         ICUPercent = Total.ICU/Total.Hospital, 
         HospitalizedPercent = Total.Hospital/Total.cases,
         DeathPercent = Total.deaths/Total.cases,
         # add on 2020-07-14
         DailyTestByReportDate = TotalTestsByReportDate - lag(TotalTestsByReportDate)) %>% 
  #select(Date, Day, Daily.tests, starts_with("New"), starts_with("Current"), ICU, starts_with("Total"), ends_with("Percent"))
  #drop_na(Total.tested) %>% 
  ## added on 2020-07-15 
  ## create new variables combining data by specimen date and routine daily updates for plotting only 
  ## since data for most recent 5 days are reported in the table
  ## edit on 2020-08-18 to include number of people tested
  mutate(Daily.testsPlot = ifelse(last(.$Date) - Date > 6,  DailyTestByReportDate, Daily.tests),
         New.casesPlot = ifelse(last(.$Date) - Date > 6,  New.casesBySpecimenDate, New.cases),
         Total.casesPlot = ifelse(last(.$Date) - Date > 6,  Total.casesBySpecimenDate, Total.cases),
         PositivePercentPlot = New.casesPlot/Daily.testsPlot,
         PositivePercentPeoplePlot = New.casesPlot/Daily.tests.people,
         DeathPercentPlot = Total.deaths/Total.casesPlot) 

## Convert to long format for p1 and p3 plots
## edit on 2020-08-18 to include number of people tested
dataLongDailyTests = dataWide %>% 
  # edit on 2020-07-14 due to adding data by report/specimen collection date
  # edit on 2020-07-15: add "Daily.testsPlot"
  pivot_longer(c(-Date, -DateReport, -DailyTestByReportDate, -Daily.testsPlot, -Daily.tests.people), names_to = "Variable", values_to = "Value")


## Long data for New cases and deaths with n day moving average
# set number of days for moving average 
# add moving average value weighted by number of daily tests on 2020-06-22
moveAvg = 7
dataLongAvg = dataLongDailyTests %>%
  group_by(Variable) %>%
  mutate(movAvgValue = RcppRoll::roll_mean(Value, moveAvg, fill = "left", align = "right", na.rm = TRUE))
        # 2020-10-10: comment out since it's not used in plots anymore 
         #movAvgValue2 = TTR::VWMA(Value, moveAvg, volume = Daily.testsPlot))

#### Plots ####
## set up vline dates and vline labels for important events
vlineDf = tibble(Date = as.Date(c("2020-03-17","2020-03-18","2020-03-28","2020-05-04","2020-05-18", "2020-05-26","2020-05-27", "2020-06-01", "2020-06-10", "2020-07-04", "2020-07-25", "2020-08-16", "2020-09-07", "2020-09-30", "2020-10-15", "2020-10-18", "2020-10-31", "2020-11-13", "2020-11-20")),
                Label = c("Bar\nclose","School\nclose","Stay\nHome", "Curbside\npickup", "Stay\nSafe", "Unrest","25%\nworship", "Outdoor\ndining", "Indoor\ndining", "Jul4th", "Mask\nmandate", "Sturgis\nrally", "Labor\nday", "Duluth\nrally", "MEA\nweek", "Cold\nstreak","Halloween", "Limited\nsocial", "Dial\nback"))

p1LabelLoc = c(0, 800, 5000, 1000, 5000, 4000, 3000, 2000, 1000, 850, 5000, 1400, 800, 800, 1500, 2500, 3000, 3500, 5000)+3000
p2LabelLoc = c(9, 13, 30, 26, 30, 24, 21, 18, 12, 13, 30, 20, 22, 18, 20, 23, 26, 30, 33)
p3LabelLoc = c(900, 1300, 3100, 1500, 3100, 2900, 2200, 2600, 1600, 1000, 3100, 1400, 1550, 1700, 1900, 2600, 2800, 3100, 3500)
p5LabelLoc = c(38000, 5000, 38000, 30000, 25000, 20000, 12000, 1500, 38000, 15000, 5000, 10000, 18000, 24000, 28000, 32000, 38000)+8000  
  
# for p1 sec_axis
secAxisConstant = 20
p1Constant = 5
## plot daily new cases and deaths
## edit on 2020-07-15 using the new variables for plotting new cases
p1 = ggplot(dataLongDailyTests %>% mutate(Value = ifelse(Variable == "New.deaths", Value*secAxisConstant*p1Constant, Value)) %>% filter(Variable %in% c("New.deaths","New.casesPlot")))+
  aes(Date, Value, fill = Variable, label = Value)+
  geom_col(position = "dodge", alpha = .5, width = .85)+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4)+
  geom_vline(xintercept = dataLongDailyTests %>%
               distinct(Date) %>% 
               filter(Date %in% vlineDf$Date) %>% 
               pull(Date), lty = 2, alpha = .4)+
  annotate("label", x = vlineDf$Date, y = p1LabelLoc, label = vlineDf$Label, lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  # n day moving average
  geom_path(data = dataLongAvg %>% mutate(movAvgValue = ifelse(Variable == "New.deaths", movAvgValue*secAxisConstant*p1Constant, movAvgValue)) %>% filter(Variable %in% c("New.deaths","New.casesPlot")) , aes(Date, movAvgValue, color = Variable, group = Variable), size = 1.6, alpha = .6)+
  #geom_path(data = dataLongAvg %>% mutate(movAvgValue2 = ifelse(Variable == "New.deaths", movAvgValue2*secAxisConstant, movAvgValue2)) %>% filter(Variable %in% c("New.deaths","New.casesPlot")) , aes(Date, movAvgValue2, group = Variable, lty = "DailyTestsWeighted"), size = .5, alpha = .8, color = "blue")+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("New.casesPlot")) %>% filter(Date == last(Date)), aes(x= Date, y = Value), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 3.5, vjust = -10, size = 4, color = RColorBrewer::brewer.pal(3, "Dark2")[1], fontface = "bold")+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("New.deaths")) %>% filter(Date == last(Date)), aes(x= Date, y = Value*secAxisConstant*p1Constant), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 3.5, vjust = -.5, size = 4, ylim = c(0, Inf),color = RColorBrewer::brewer.pal(3, "Dark2")[2],fontface = "bold")+
  annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .2)+
  labs(x = "", y = "New cases", title = "Daily new cases & deaths", fill = "")+
  guides(fill = guide_legend(order = 1))+
  scale_fill_brewer(name = "", palette = "Set2", labels = c("New case", "New death"))+
  scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), values = RColorBrewer::brewer.pal(3, "Dark2"), labels = c("New case", "New death"))+
  scale_linetype_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average weighted by daily tests"), values = c(1,1), labels = "")+
  scale_x_date(date_breaks = "14 days", date_labels = "%b %d")+
  scale_y_continuous(labels = scales::label_number_si(), sec.axis = sec_axis(~ ./(secAxisConstant*p1Constant), 		
                                         name = "New deaths"))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),legend.margin=margin(),legend.box="vertical",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12),
        axis.title.y.left = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[1]), axis.title.y.right = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[2]),
        axis.text.y.left = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[1]),axis.text.y.right = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[2]))
plot(p1)

p2 = ggplot()+
  geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("PositivePercentPlot","DeathPercentPlot")) %>% 
             mutate(Value = Value*100, 
                    Variable = factor(Variable, levels = c("PositivePercentPlot","DeathPercentPlot"))), 
           aes(x = Date, y = Value, fill = Variable), color = NA, position = "dodge", width = .85, alpha = .5)+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4)+
  geom_vline(xintercept = dataLongDailyTests %>%
               #drop_na(Daily.testsPlot) %>%
               distinct(Date) %>%
               filter(Date %in% vlineDf$Date) %>%
               pull(Date), lty = 2, alpha = .4)+
  # add dialback positivity threshold
  geom_hline(yintercept = 15, color = "red", lty = 2)+
  annotate("label", x = vlineDf$Date, y = p2LabelLoc, label = vlineDf$Label, lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  # n day moving average
  geom_path(data = dataLongAvg %>% filter(Variable %in% c("PositivePercentPlot","DeathPercentPlot","Daily.tests")) %>% ungroup %>% mutate(movAvgValue = ifelse(Variable == "Daily.tests", movAvgValue*0.0005, movAvgValue*100), Variable = factor(Variable, levels = c("PositivePercentPlot","DeathPercentPlot","Daily.tests"))), aes(Date, movAvgValue, color = Variable, group = Variable, size = Variable), alpha = .7)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("PositivePercentPlot","DeathPercentPlot","Daily.tests")) %>% filter(Date == last(Date)) %>%
                    mutate(Value = ifelse(Variable == "Daily.tests", Value, Value*100),
                           Variable = factor(Variable, levels = c("PositivePercentPlot","DeathPercentPlot","Daily.tests"))), aes(x= Date, y = Value, label = round(Value,2), color = Variable), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 5, vjust = c(0,-5,-1), size = 3.5, fontface = "bold",show.legend = FALSE)+
  scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), values = (RColorBrewer::brewer.pal(3, "Dark2")[c(1,2,3)]), label = c("Positive rate","Fatality rate","Daily tests"))+
  #geom_text(aes(x = "2020-03-15" %>% as.Date, y = 15), label = "Positivity\nthreshold", vjust = -.5, color = "red", size = 3, lineheight = .8, fontface = "bold", check_overlap = TRUE)+
  annotate("label", x = "2020-03-15" %>% as.Date, y = 17, label = "Positivity\nthreshold", lineheight = .8, size = 3, label.padding = unit(0.2, "lines"), label.size = .02, color = "red", fontface = "bold")+
  annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .2)+
  labs(x = "", y = "Percentage (%)", title = "Daily positive rate & case fatality rate", fill = "Positive rate")+
  guides(color = guide_legend(), size = guide_legend(), fill = guide_legend())+
  scale_x_date(date_breaks = "14 days", date_labels = "%b %d")+
  scale_fill_manual(name = "", label = c("Positive rate","Fatality rate \n(total death/total case)"), values = (RColorBrewer::brewer.pal(3, "Set2")[c(1,2)]))+
  #scale_linetype_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average weighted by daily tests"), values = c(1,1), labels = "")+
  scale_size_manual(values =c(1.8,1.8,1), name = moveAvg %>% as.character() %>% paste0("-day moving average"),label = c("Positive rate","Fatality rate","Daily tests"))+
  scale_y_continuous(sec.axis = sec_axis(~ ./0.0005, name = "Daily tests", breaks = c(20000,40000,60000),labels = scales::label_number_si()))+
  coord_cartesian(ylim = c(0,35))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),legend.margin=margin(),legend.box="vertical",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14),legend.text = element_text(size=10))
plot(p2)

  ## new version p3 (cols for raw hospitalized and icu data)
## plot 7 daty moving average for Hospitalized, ICU
## Raw value for total Death.
## conversion factor for secondary axis
convertFactor = trunc(max(dataLongDailyTests %>%
                      filter(Variable %in% c("Date", "Total.deaths")) %>% 		
                      pull(Value), na.rm = T)/(max(dataLongDailyTests %>%
                                                     filter(Variable %in% c("Date", "New.ICU", "New.hospitalized")) %>% 		
                                                     pull(Value), na.rm = T))) %>% plyr::round_any(10)
 

p3 = ggplot(dataLongDailyTests %>% 
              filter(Variable %in% c("Total.deaths")) %>% 
              ## this value transformation is for plotting on the secondary y-axis (right) for ICU percentage of current hospitalized cases
              mutate(Variable = factor(Variable, levels = c("Total.deaths"))))+ 
  aes(Date, Value, color = Variable)+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4)+
  geom_vline(xintercept = dataLongDailyTests %>% 
               distinct(Date) %>% 
               filter(Date %in% vlineDf$Date) %>% 
               pull(Date), lty = 2, alpha = .4)+
  #geom_line(aes(group = Variable))+
  annotate("label", x = vlineDf  %>% pull(Date), y = p3LabelLoc, label = vlineDf %>% pull(Label), lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  geom_path(data = dataLongAvg %>% filter(Variable %in% c("Total.deaths")) %>% ungroup %>% mutate(Variable = factor(Variable, levels = c("Total.deaths"))), aes(Date, movAvgValue, color = (Variable), group = Variable), size = 1.3, alpha = .7)+
  geom_path(data = dataLongAvg %>% filter(Variable %in% c("New.hospitalized", "New.ICU")) %>% ungroup %>% mutate(Variable = factor(Variable, levels = c("New.hospitalized", "New.ICU"))), aes(Date, movAvgValue*convertFactor , color = (Variable), group = Variable), size = 1.3, alpha = .7)+
  # geom_text_repel(data=dataLongAvg %>% filter(Variable %in% c("New.hospitalized", "New.ICU"), Date == last(Date)), aes(x= Date, y = movAvgValue*convertFactor , label = Value, color = Variable), segment.color = NA, direction = "y", box.padding = .05, vjust = -.5, nudge_x= 5, hjust = -1, size = 3.5, show.legend = F, fontface = "bold")+
  # geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("Total.deaths"), Date == last(Date)), aes(x= Date, y = Value, label = Value, color = Variable), segment.color = NA, direction = "y", box.padding = .05, vjust = 3, nudge_x = 5, hjust = -2, size = 3.5, show.legend = F)+
  #geom_point(fill = "white", shape = 21, stroke = .8, size = .15, show.legend = F)+
  geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("New.ICU", "New.hospitalized")), aes(y = Value*convertFactor, fill = Variable), alpha = .3, color = NA, position = "dodge", width = .5)+
  labs(x = "", y = "Deaths", size = "Daily tests", fill = "", title = "Daily admitted hospitalized, ICU & total deaths")+
  #scale_size_continuous(range = c(.1,4))+
  scale_color_manual(values = RColorBrewer::brewer.pal(5, "Set1")[c(1,2,5)], name = "7-day moving average", labels = c("Hospitalized", "ICU", "Death"))+
  scale_fill_manual(values = alpha(RColorBrewer::brewer.pal(3, "Set1"), .3), labels  = c("Hospitalized","ICU"))+
  scale_y_continuous(labels = scales::label_number_si(), sec.axis = sec_axis(~ ./convertFactor, name = "Daily admitted cases"))+
  annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .2)+
  scale_x_date(date_breaks = "14 days", date_labels = "%b %d")+
  guides(color=guide_legend(nrow=1,byrow=TRUE, order = 2), fill = guide_legend(order = 1))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),axis.text.x = element_text(size=10, angle = 50, hjust = 1), axis.text.y.right =  element_text(colour = "black"), axis.title.y.right = element_text(colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal", legend.margin=margin(), legend.box="vertical",text=element_text(size=14), legend.text = element_text(size=12), legend.text.align = 0.5)
plot(p3)


## old version p3 (cols for percentage) ####
# p3 = ggplot(dataLongDailyTests %>% 
#               filter(Variable %in% c("ICU", "Currently.hospitalized", "Total.deaths"), Date > as.Date("2020-03-23")) %>% 
#               ## this value transformation is for plotting on the secondary y-axis (right) for ICU percentage of current hospitalized cases
#               mutate(Variable = factor(Variable, levels = c("Currently.hospitalized", "ICU", "Total.deaths"))))+ 
#   aes(Date, Value, color = Variable)+
#   geom_line(aes(group = Variable))+
#   geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("ICU", "Currently.hospitalized", "Total.deaths"), Date == last(Date)), aes(x= Date, y = Value, label = Value, color = Variable), segment.color = NA, direction = "y", box.padding = .05, nudge_y = 15, nudge_x = 2.5, size = 3.5, show.legend = F)+
#   geom_point(fill = "white", shape = 21, stroke = .8, size = .15, show.legend = F)+
#   geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4)+
#   geom_vline(xintercept = dataLongDailyTests %>% filter(Date > as.Date("2020-03-23")) %>% 
#                distinct(Date) %>% 
#                filter(Date %in% vlineDf$Date) %>% 
#                pull(Date), lty = 2, alpha = .4)+
#   #geom_point(inherit.aes = F, data = dataWide %>% filter(Date > as.Date("2020-03-23")), aes(x = Date, y = -15, size = Daily.testsPlot), shape = 21, stroke = 1, fill = "white")+
#   geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("HospitalizedPercent","ICUPercent"), Date > as.Date("2020-03-23")) %>% mutate(Value = Value*max(dataLongDailyTests %>% 
#                                                                                                                                                                   filter(Variable %in% c("Date", "Currently.hospitalized"), Date > as.Date("2020-03-23")) %>%
#                                                                                                                                                                   pull(Value)),
#                                                                                                                                               Variable = factor(Variable)), aes(y = Value, fill = Variable), alpha = .3, color = NA, position = "dodge", width = .5)+
#   labs(x = "", y = "Number of cases", size = "Daily tests", fill = "", title = "Cases (line): Hospitalized, ICU & Death\nPercentage (bar): Hospitalized & ICU")+
#   guides(color=guide_legend(nrow=1,byrow=TRUE, order = 3))+
#   scale_size_continuous(range = c(.1,4))+
#   scale_color_manual(values = c(RColorBrewer::brewer.pal(3, "Set1")[1:2], RColorBrewer::brewer.pal(5, "Set1")[5]), name = "", labels = c("Current hospitalized", "Current ICU","Total deaths"))+
#   scale_fill_manual(values = alpha(RColorBrewer::brewer.pal(3, "Set1"), .3), labels  = str_wrap(c("Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"), 26))+
#   scale_y_continuous(sec.axis = sec_axis(~ ./max(dataLongDailyTests %>%
#                                                    filter(Variable %in% c("Date", "Total.deaths", "ICU", "Currently.hospitalized"), Date > as.Date("2020-03-23")) %>% 		
#                                                    pull(Value))*100, 		
#                                          name = "Percentage (%)"))+
#   annotate("label", x = vlineDf %>% filter(Date > as.Date("2020-03-23")) %>% pull(Date), y = c(325, 615, 615, 800, 710, 400, 300, 400, 500), label = vlineDf %>% filter(Date > as.Date("2020-03-23")) %>% pull(Label), lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
#   #annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .15)+
#   scale_x_date(date_breaks = "7 days", date_labels = "%b %d")+
#   theme_minimal()+
#   theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=10, angle = 50, hjust = 1), axis.text.y.right =  element_text(colour = "black"), axis.title.y.right = element_text(colour = "black"),
#         legend.position = "bottom", legend.direction = "horizontal", legend.margin=margin(), legend.box="vertical",text=element_text(size=14), legend.text = element_text(size=12))
# 
## old version p3
# p3 = ggplot(dataLongDailyTests %>% 
#               filter(Variable %in% c("ICU", "Currently.hospitalized", "ICUPercent", "Total.deaths", "HospitalizedPercent"), Date > as.Date("2020-03-23")) %>% 
#               ## this value transformation is for plotting on the secondary y-axis (right) for ICU percentage of current hospitalized cases
#               mutate(Value = ifelse(str_detect(Variable, "Percent"), 
#                                     Value*max(dataLongDailyTests %>% 
#                                                 filter(Variable %in% c("Date", "Currently.hospitalized"), Date > as.Date("2020-03-23")) %>%
#                                                 pull(Value)), 
#                                     Value), 
#                      Variable = factor(Variable, levels = c("Currently.hospitalized", "ICU", "Total.deaths", "HospitalizedPercent","ICUPercent"))))+ 
#   aes(Date, Value, color = Variable)+
#   geom_line(aes(group = Variable, lty = Variable, alpha = Variable))+
#   geom_point(aes(fill = Variable), shape = 21, stroke = .8, size = .9)+
#   geom_vline(xintercept = dataLongDailyTests %>% filter(Date > as.Date("2020-03-23")) %>% 
#                distinct(Date) %>% 
#                filter(Date %in% as.Date(c("2020-03-28","2020-04-12"))) %>% 
#                pull(Date), lty = 2, alpha = .4)+
#   geom_point(inherit.aes = F, data = dataWide %>% filter(Date > as.Date("2020-03-23")), aes(x = Date, y = -15, size = Daily.tests), shape = 21, stroke = 1.2, fill = "white", alpha = .8)+
#   #geom_path(data = dataLongAvg %>% filter(Variable == "Currently.hospitalized", Date > as.Date("2020-03-23")), aes(y = movAvgValue, color = "5-day moving average"), size = 3, alpha = .6)+
#   labs(x = "", y = "Number of cases", size = "Daily tests", fill = "", title = str_wrap("Hospitalized, ICU, Death, Hospitalized percentage and ICU percentage",38))+
#   guides(color=guide_legend(nrow=2,byrow=TRUE))+
#   scale_color_manual(values = c(RColorBrewer::brewer.pal(3, "Dark2"), RColorBrewer::brewer.pal(3, "Dark2")), name = "", labels = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"), 30))+
#   scale_fill_manual(values = c("white","white", "white", alpha(RColorBrewer::brewer.pal(3, "Dark2"), .6)), labels  = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"),30))+
#   scale_y_continuous(sec.axis = sec_axis(~ ./max(dataLongDailyTests %>%
#                                                    filter(Variable %in% c("Date", "Currently.hospitalized"), Date > as.Date("2020-03-23")) %>% 		
#                                                    pull(Value))*100, 		
#                                          name = "Percentage (%)"))+
#   scale_linetype_manual(values= c(1,1,1,3,3), name = "", labels = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"),30))+
#   scale_alpha_manual(values= c(.6,.6,.6,.6,1), name = "", labels = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"),30))+
#   annotate("label", x = as.Date(c("2020-03-28","2020-04-12")), y = c(250,250), label = c("StayHomeOrder","8pm data"))+
#   scale_x_date(date_breaks = "3 days", date_labels = "%b %d")+
#   theme_minimal()+
#   theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=10, angle = 50, hjust = 1), axis.text.y.right =  element_text(colour = "black"), axis.title.y.right = element_text(colour = "black"),
#         legend.position = "bottom", legend.margin=margin(), legend.box="vertical",text=element_text(size=14), legend.text = element_text(size=12))
# plot(p3)
## plot hospital surge capacity ####
# edit 2020-06-23 to fix surge in use for ventilators
# edit 2020-06-26 with position_stack (cleaner text label)
# edit 2020-11-10 to remove "In Use for ICU surge"
# edit 2020-11-12 to match state website layout

p4 = ggplot(responseData)+
  aes(x = Detail2, y = Value, fill = Detail3, label = Value)+
  geom_text(aes(label = stat(y), group = Detail2), stat = 'summary', fun = sum, vjust = -.2, size = 2.8, fontface = "bold", color = "#36688D", show.legend = F)+
  geom_bar(position = "stack", stat = "identity", width = .8, size = .3)+
  geom_text(data = responseData %>% filter(!Detail3 %in% c("Surge","COVID+")|Detail1!="ICU"|Detail2!="In Use", count !=1),
            aes(x = Detail2, y = Value, label = Value),
            position = position_stack(vjust = 0.5), size = 3)+
  facet_wrap(~Detail1,scales="free")+
  labs(fill = "", x = "", y = "", title = paste("Hospital surge capacity: updated on", ifelse(responseData$Date %>% is.na(), format(last(mdy(responseData$Date.and.time.of.update)), "%b %d"), format(last(mdy(responseData$DateUpdate)), "%b %d"))))+
  #scale_fill_brewer(palette ="Pastel1")+
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Pastel1"), breaks = c("Capacity","Surge","COVID+","non-COVID+","In use","In warehouse"))+
  theme_minimal()+
  theme(title = element_text(size = 14), strip.text = element_text(size = 11, face = "bold"),
        axis.text.x =  element_text(size = 9, face = "bold", angle = 40, hjust = 1),
        plot.margin = margin(0, 0, .1, .1, "pt"))
plot(p4)


## active case over time

p5 = ggplot()+
  geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("Currently.sick","ICU", "Currently.hospitalized"), Date > as.Date("2020-03-23")) %>% 
             mutate(Variable = factor(Variable, levels = c("Currently.hospitalized","ICU","Currently.sick")),
                    Value = ifelse(Variable == "Currently.sick", Value, Value*secAxisConstant)), 
           aes(x = Date, y = Value, fill = Variable), width = .85, alpha = .3, show.legend = F, position = "dodge")+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4, show.legend = F)+
  geom_vline(xintercept = dataLongDailyTests %>%
               #drop_na(Daily.testsPlot) %>% 
               distinct(Date) %>% 
               filter(Date %in% vlineDf$Date, Date > as.Date("2020-03-23")) %>% 
               pull(Date), lty = 2, alpha = .4, show.legend = F)+
  annotate("label", x = vlineDf %>% filter(Date > as.Date("2020-03-23")) %>% pull(Date), y = p5LabelLoc, label = vlineDf %>% filter(Date > as.Date("2020-03-23")) %>% pull(Label), lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  # n day moving average
  geom_path(data = dataLongAvg %>% mutate(movAvgValue = ifelse(Variable == "Currently.sick", movAvgValue, movAvgValue*secAxisConstant)) %>% 
              filter(Variable %in% c("Currently.sick","ICU", "Currently.hospitalized")) %>% ungroup %>% 
              mutate(Variable = factor(Variable, levels = c("Currently.hospitalized","ICU","Currently.sick"))), 
            aes(Date, movAvgValue, color = Variable, group = Variable), size = 1.6, alpha = .6)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("Currently.sick","ICU", "Currently.hospitalized")) %>% ungroup %>%
                    mutate(Variable = factor(Variable, levels = c("Currently.hospitalized","ICU","Currently.sick")),
                           ValuePlot = ifelse(Variable == "Currently.sick", Value, Value*secAxisConstant)) %>%
                    filter(Date == last(Date)), aes(x= Date, y = ValuePlot, label = Value), segment.color = NA, direction = "y", box.padding = .05, vjust = -.5, size = 3.5, color = RColorBrewer::brewer.pal(4, "Set1")[c(2,1,4)], fontface = "bold")+
  annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .2)+
  labs(x = "", y = "Active cases", title = "Current hospitalized, ICU & active cases")+
  guides(color = guide_legend(nrow=1,byrow=TRUE, order = 1, override.aes=list(fill=NA)), fill = guide_legend(order = 1), linetype = guide_legend(override.aes=list(fill=NA)))+
  scale_x_date(date_breaks = "14 days", date_labels = "%b %d")+
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000), labels = scales::label_number_si(), sec.axis = sec_axis(~ ./secAxisConstant, 		
                                         name = "Hospitalizations", breaks = c(0,500,1000,1500,2000), labels = c("0","500","1k","1.5k","2k")))+
  scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), label = c("Hospitalized", "ICU", "Active"), values = (RColorBrewer::brewer.pal(4, "Set1")[c(1,2,4)]))+
  scale_fill_manual(name = "", label = "", values = c(RColorBrewer::brewer.pal(4, "Set1")[c(1,2,4)]))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),legend.margin=margin(),legend.box="horizontal",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14),legend.text = element_text(size=12))
plot(p5)

#### Other exploratory plots: not print####
# ggplot(dataWide,aes(x = Date))+
#   geom_point(aes(y = Total.cases))+
#   geom_point(aes(y = ICU), color = "red")+
#   #geom_line(aes(group = 1))+
#   #stat_smooth(aes(y = Total.cases, group = 1), method = "lm", formula = y~log2(x))+
#   scale_y_continuous(trans='log2')+
#   labs(y = "Total cases")+
#   theme(legend.position = "bottom", axis.text.x = element_text(size=11, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12))

## 3-day moving avg for currently hospitalized 
# ggplot(dataLongAvg %>% filter(Variable == "Currently.hospitalized", Date > as.Date("2020-03-23")))+
#   aes(x = Date)+
#   geom_path(aes(y = movAvgValue, color = "3-day moving average"), size = 3, alpha = .6)+
#   geom_point(data = dataWide %>% filter(Date > as.Date("2020-03-23")), aes(x = Date, y = Currently.hospitalized, color = "raw"), show.legend = F)+
#   scale_x_date(date_breaks = "3 days", date_labels = "%b %d")+
#   labs(title = "MN COVID-19", y = "Currently hospitalized", color = "")+
#   theme_minimal()+
#   theme(legend.position = "bottom",
#         axis.text.x = element_text(size=11, angle = 50, hjust = 1),
#         text=element_text(size=14), legend.text = element_text(size=12))


