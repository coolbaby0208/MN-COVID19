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
dataWide = read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  # format date string and compute values for plotting 
  mutate(Date = Date %>% ymd(),
         Daily.tests = Total.tested - lag(Total.tested),
         ## add on 2020-08-18
         Daily.tests.people = Total.tested.people - lag(Total.tested.people),
         New.cases = Total.cases - lag(Total.cases), 
         Currently.sick = Total.cases - Total.recovered - Total.deaths,
         New.deaths = Total.deaths - lag(Total.deaths),
         New.ICU = ICU - lag(ICU),
         New.hospitalized = Currently.hospitalized - lag(Currently.hospitalized),
         New.sick = Currently.sick - lag(Currently.sick),
         PositivePercent = New.cases/Daily.tests, 
         ICUPercent = ICU/Currently.hospitalized, 
         HospitalizedPercent = Currently.hospitalized/Currently.sick,
         DeathPercent = Total.deaths/Total.cases,
         # add on 2020-07-14
         DailyTestByReportDate = MDHTestsByReportDate+ExternalTestsByReportDate) %>%  
  #select(Date, Day, Daily.tests, starts_with("New"), starts_with("Current"), ICU, starts_with("Total"), ends_with("Percent"))
  drop_na(Daily.tests) %>% 
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
  pivot_longer(c(-Date, -Daily.tests, -DateReport, -DailyTestByReportDate, -Daily.testsPlot, -Daily.tests.people), names_to = "Variable", values_to = "Value")


## Long data for New cases and deaths with n day moving average
# set number of days for moving average 
# add moving average value weighted by number of daily tests on 2020-06-22
moveAvg = 7
dataLongAvg = dataLongDailyTests %>%
  group_by(Variable) %>%
  mutate(movAvgValue = RcppRoll::roll_mean(Value, moveAvg, fill = "left", align = "right"),
         movAvgValue2 = TTR::VWMA(Value, moveAvg, volume = Daily.testsPlot))

#### Plots ####
## set up vline dates and vline labels for important events
vlineDf = tibble(Date = as.Date(c("2020-03-17","2020-03-18","2020-03-28","2020-05-04","2020-05-18", "2020-05-26","2020-05-27", "2020-06-01", "2020-06-10", "2020-07-04", "2020-07-25")),
                Label = c("Bar\nclose","School\nclose","Stay\nHome", "Curbside\npickup", "Stay\nSafe", "Unrest","25%\nworship", "Outdoor\ndining", "Indoor\ndining", "Jul4th", "Mask\nmandate"))

# for p1 sec_axis
secAxisConstant = 12
## plot daily new cases and deaths
## edit on 2020-07-15 using the new variables for plotting new cases
p1 = ggplot(dataLongDailyTests %>% mutate(Value = ifelse(Variable == "New.deaths", Value*secAxisConstant, Value)) %>% filter(Variable %in% c("New.deaths","New.casesPlot")))+
  aes(Date, Value, fill = Variable, label = Value)+
  geom_col(position = "dodge", alpha = .5, width = .85)+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4)+
  geom_vline(xintercept = dataLongDailyTests %>%
               distinct(Date) %>% 
               filter(Date %in% vlineDf$Date) %>% 
               pull(Date), lty = 2, alpha = .4)+
  # n day moving average
  geom_path(data = dataLongAvg %>% mutate(movAvgValue = ifelse(Variable == "New.deaths", movAvgValue*secAxisConstant, movAvgValue)) %>% filter(Variable %in% c("New.deaths","New.casesPlot")) , aes(Date, movAvgValue, color = Variable, group = Variable), size = 1.6, alpha = .6)+
  #geom_path(data = dataLongAvg %>% mutate(movAvgValue2 = ifelse(Variable == "New.deaths", movAvgValue2*secAxisConstant, movAvgValue2)) %>% filter(Variable %in% c("New.deaths","New.casesPlot")) , aes(Date, movAvgValue2, group = Variable, lty = "DailyTestsWeighted"), size = .5, alpha = .8, color = "blue")+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("New.casesPlot")) %>% filter(Date == last(Date)), aes(x= Date, y = Value), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 3.5, vjust = -.5, size = 4, color = RColorBrewer::brewer.pal(3, "Dark2")[1], fontface = "bold")+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("New.deaths")) %>% filter(Date == last(Date)), aes(x= Date, y = Value*secAxisConstant), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 3.5, vjust = -.5, size = 4, ylim = c(0, Inf),color = RColorBrewer::brewer.pal(3, "Dark2")[2],fontface = "bold")+
  annotate("label", x = vlineDf$Date, y = c(200, 300, 400, 850, 850, 1030, 950, 850, 750, 850, 950), label = vlineDf$Label, lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .15)+
  labs(x = "", y = "New cases", title = "Daily new cases and deaths", fill = "")+
  guides(fill = guide_legend(order = 1))+
  scale_fill_brewer(name = "", palette = "Set2", labels = c("New case", "New death"))+
  scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), values = RColorBrewer::brewer.pal(3, "Dark2"), labels = c("New case", "New death"))+
  scale_linetype_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average weighted by daily tests"), values = c(1,1), labels = "")+
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d")+
  scale_y_continuous(sec.axis = sec_axis(~ ./secAxisConstant, 		
                                         name = "New deaths"))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), legend.margin=margin(),legend.box="vertical",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12),
        axis.title.y.left = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[1]), axis.title.y.right = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[2]),
        axis.text.y.left = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[1]),axis.text.y.right = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[2]))
plot(p1)

p2 = ggplot()+
  geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("PositivePercentPlot","DeathPercentPlot","PositivePercentPeoplePlot")) %>% mutate(Value = Value*100), 
           aes(x = Date, y = Value, fill = Variable), color = NA, position = "dodge", width = .85, alpha = .5)+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4)+
  geom_vline(xintercept = dataLongDailyTests %>%
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               filter(Date %in% vlineDf$Date) %>% 
               pull(Date), lty = 2, alpha = .4)+
  # n day moving average
  geom_path(data = dataLongAvg %>% filter(Variable %in% c("PositivePercentPlot","DeathPercentPlot","PositivePercentPeoplePlot")), aes(Date, movAvgValue*100, color = rev(Variable), group = Variable), size = 1.8, alpha = .7)+
  #geom_path(data = dataLongAvg %>% filter(Variable %in% c("PositivePercentPlot","DeathPercentPlot")), aes(Date, movAvgValue2*100, color = Variable, group = Variable, lty = "DailyTestsWeighted"), size = .5, alpha = .8, color = "blue")+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("PositivePercentPlot","PositivePercentPeoplePlot","DeathPercentPlot")) %>% filter(Date == last(Date)), aes(x= Date, y = Value*100, label = round(Value*100,2)), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 3.5, vjust = c(-.75, -.75, -.5), size = 3.5, color = RColorBrewer::brewer.pal(3, "Dark2")[c(1,3,2)], fontface = "bold")+
  #geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("DeathPercentPlot")) %>% filter(Date == last(Date)), aes(x= Date, y = Value*100, label = round(Value*100,2)), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 2.5, vjust = -.5, size = 3.5, ylim = c(0, Inf), color = RColorBrewer::brewer.pal(3, "Dark2")[2], fontface = "bold")+
  geom_point(inherit.aes = F, data = dataWide, aes(x = Date, y = -1, size = Daily.testsPlot), shape = 21, stroke = .6, fill = "white")+
  scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), values = (RColorBrewer::brewer.pal(3, "Dark2")[c(1,3,2)]), label = c("Positive rate","Positive rate (people)","Fatality rate"))+
  annotate("label", x = vlineDf$Date, y = c(9, 13, 17, 21, 21, 22, 17, 13, 10, 13, 13), label = vlineDf$Label, lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .15)+
  labs(x = "", y = "Percentage (%)", title = "Daily positive rate and case fatality rate", size = "Daily tests", fill = "Positive rate")+
  guides(color = guide_legend(nrow=1,byrow=TRUE, order = 2), lty = guide_legend(order = 2), size = guide_legend(order = 3), fill = guide_legend(order = 1))+
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d")+
  scale_fill_manual(name = "", label = c("Positive rate","Positive rate (people)","Fatality rate \n(total death/total case)"), values = (RColorBrewer::brewer.pal(3, "Set2")[c(1,3,2)]))+
  scale_linetype_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average weighted by daily tests"), values = c(1,1), labels = "")+
  scale_size_continuous(range = c(.05,2))+
  coord_cartesian(ylim = c(0,25))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),legend.margin=margin(),legend.box="vertical",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14),legend.text = element_text(size=10))
plot(p2)

## new version p3 (cols for raw hospitalized and icu data)
## plot 7 daty moving average for Hospitalized, ICU
## Raw value for total Death.
## conversion factor for secondary axis
convertFactor = max(dataLongDailyTests %>%
                      filter(Variable %in% c("Date", "Total.deaths"), Date > as.Date("2020-03-23")) %>% 		
                      pull(Value))/(max(dataLongDailyTests %>%
                                          filter(Variable %in% c("Date", "ICU", "Currently.hospitalized"), Date > as.Date("2020-03-23")) %>% 		
                                          pull(Value)))
p3 = ggplot(dataLongDailyTests %>% 
              filter(Variable %in% c("Total.deaths"), Date > as.Date("2020-03-23")) %>% 
              ## this value transformation is for plotting on the secondary y-axis (right) for ICU percentage of current hospitalized cases
              mutate(Variable = factor(Variable, levels = c("Currently.hospitalized", "ICU", "Total.deaths"))))+ 
  aes(Date, Value, color = Variable)+
  geom_line(aes(group = Variable))+
  geom_path(data = dataLongAvg %>% filter(Variable %in% c("Currently.hospitalized", "ICU"), Date > as.Date("2020-03-23")), aes(Date, movAvgValue*convertFactor, color = (Variable), group = Variable), size = 1.8, alpha = .7)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("ICU", "Currently.hospitalized"), Date == last(Date)), aes(x= Date, y = Value*convertFactor, label = Value, color = Variable), segment.color = NA, direction = "y", box.padding = .05, vjust = -1, nudge_x = 2.5, size = 3.5, show.legend = F)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("Total.deaths"), Date == last(Date)), aes(x= Date, y = Value, label = Value, color = Variable), segment.color = NA, direction = "y", box.padding = .05, vjust = -.5, nudge_x = 2.5, size = 3.5, show.legend = F)+
  geom_point(fill = "white", shape = 21, stroke = .8, size = .15, show.legend = F)+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4)+
  geom_vline(xintercept = dataLongDailyTests %>% filter(Date > as.Date("2020-03-23")) %>% 
               distinct(Date) %>% 
               filter(Date %in% vlineDf$Date) %>% 
               pull(Date), lty = 2, alpha = .4)+
  geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("ICU", "Currently.hospitalized"), Date > as.Date("2020-03-23")), aes(y = Value*convertFactor, fill = Variable), alpha = .3, color = NA, position = "dodge", width = .5)+
  labs(x = "", y = "Total deaths", size = "Daily tests", fill = "", title = "Current hospitalized, ICU cases & Total death")+
  guides(color=guide_legend(nrow=1,byrow=TRUE, order = 3))+
  scale_size_continuous(range = c(.1,4))+
  scale_color_manual(values = c(RColorBrewer::brewer.pal(3, "Set1")[1:2], RColorBrewer::brewer.pal(5, "Set1")[5]), name = "", labels = c("Total hospitalized\n7-day moving average", "ICU only\n7-day moving average","Total deaths"))+
  scale_fill_manual(values = alpha(RColorBrewer::brewer.pal(3, "Set1"), .3), labels  = c("Total hospitalized","ICU only"))+
  scale_y_continuous(sec.axis = sec_axis(~ ./convertFactor, name = "Current cases"))+
  annotate("label", x = vlineDf %>% filter(Date > as.Date("2020-03-23")) %>% pull(Date), y = c(325, 1500, 1500, 1300, 200, 500, 850, 600, 1000), label = vlineDf %>% filter(Date > as.Date("2020-03-23")) %>% pull(Label), lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d")+
  guides(fill = guide_legend(order = 1))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=10, angle = 50, hjust = 1), axis.text.y.right =  element_text(colour = "black"), axis.title.y.right = element_text(colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal", legend.margin=margin(), legend.box="vertical",text=element_text(size=14), legend.text = element_text(size=12), legend.text.align = 0.5)
plot(p3)
## old version p3 (cols for percentage)
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
## plot hospital surge capacity
# edit 2020-06-23 to fix surge in use for ventilators
# edit 2020-06-26 with position_stack (cleaner text label)
p4 = ggplot(responseData %>% filter (Value>0))+
  aes(x = Detail3, y = Value, fill = interaction(Detail1), label = Value)+
  geom_bar(position = "stack", stat = "identity", width = .8) + 
  geom_text(position = position_stack(vjust = 0.5), size = 3)+
  facet_wrap(~Metric)+
  labs(fill = "Capacity", x = "", y = "", title = paste("Hospital surge capacity: updated on", ifelse(responseData$Date %>% is.na(), format(mdy(responseData$Date.and.time.of.update), "%b %d"), format(mdy(responseData$Date), "%b %d"))), caption = paste("Source:", responseUrl))+
  scale_fill_brewer(palette ="Set2")+
  theme_minimal()+
  theme(title = element_text(size = 14), strip.text = element_text(size = 11, face = "bold"),
        axis.text.x =  element_text(size = 10, face = "bold"), plot.caption = element_text(hjust = -.0005))
plot(p4)


## active case over time

p5 = ggplot()+
  geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("Currently.sick"), Date > as.Date("2020-03-23")) %>% mutate(Value = Value), 
           aes(x = Date, y = Value, fill = Variable), color = NA, width = .85, alpha = .3, show.legend = T)+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4, show.legend = F)+
  geom_vline(xintercept = dataLongDailyTests %>%
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               filter(Date %in% vlineDf$Date, Date > as.Date("2020-03-23")) %>% 
               pull(Date), lty = 2, alpha = .4, show.legend = F)+
  # n day moving average
  geom_path(data = dataLongAvg %>% filter(Variable %in% c("Currently.sick"), Date > as.Date("2020-03-23")), aes(Date, movAvgValue, color = Variable, group = Variable), size = 1.8, alpha = .7)+
  #geom_path(data = dataLongAvg %>% filter(Variable %in% c("Currently.sick"), Date > as.Date("2020-03-23")), aes(Date, movAvgValue2, color = Variable, group = Variable, lty = "DailyTestsWeighted"), size = .5, alpha = .8, color = "blue", show.legend = FALSE)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("Currently.sick")) %>% filter(Date == last(Date)), aes(x= Date, y = Value, label = Value), segment.color = NA, direction = "y", box.padding = .05, vjust = -6, size = 3.5, color = RColorBrewer::brewer.pal(4, "Dark2")[4], fontface = "bold")+
  annotate("label", x = vlineDf %>% filter(Date > as.Date("2020-03-23")) %>% pull(Date), y = c(4000, 4850, 5500, 1500, 500, 2500, 1750, 2500, 1950), label = vlineDf %>% filter(Date > as.Date("2020-03-23")) %>% pull(Label), lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .15)+
  labs(x = "", y = "Active cases", title = "Daily active cases")+
  guides(color = guide_legend(nrow=1,byrow=TRUE, order = 1, override.aes=list(fill=NA)), fill = guide_legend(order = 1), linetype = guide_legend(override.aes=list(fill=NA)))+
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d")+
  scale_color_manual(label = moveAvg %>% as.character() %>% paste0("-day moving average"), values = (RColorBrewer::brewer.pal(4, "Dark2")[4]), name = c(""))+
  scale_fill_manual(name = "", label = c("Raw"), values = c(RColorBrewer::brewer.pal(4, "Dark2")[4]))+
  scale_linetype_manual(name = "", values = c(1), labels = "")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),legend.margin=margin(),legend.box="horizontal",legend.position = c(0.5, -.25), axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14),legend.text = element_text(size=12))
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


