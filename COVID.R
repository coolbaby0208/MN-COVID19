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

## Data preprocessing
## tidy up code on 2020-04-17
dataWide = read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  distinct(Date, .keep_all = T) %>% 
  # format date string and compute values for plotting 
  mutate(Date = Date %>% as.Date(format = "%m/%d/%y"), 
         Daily.tests = Total.tested - lag(Total.tested), 
         New.cases = Total.cases - lag(Total.cases), 
         Currently.sick = Total.cases - Total.deaths - Total.recovered,
         New.deaths = Total.deaths - lag(Total.deaths),
         New.ICU = ICU - lag(ICU),
         New.hospitalized = Currently.hospitalized - lag(Currently.hospitalized),
         New.sick = Currently.sick - lag(Currently.sick),
         PositivePercent = New.cases/Daily.tests, 
         ICUPercent = ICU/Currently.hospitalized, 
         HospitalizedPercent = Currently.hospitalized/Currently.sick,
         DeathPercent = Total.deaths/Total.cases) %>% 
  # drop rows with NA in daily tests
  drop_na(Daily.tests)

## Convert to long format for p1 and p3 plots
dataLongDailyTests = dataWide %>% 
  pivot_longer(c(-Date, -Daily.tests), names_to = "Variable", values_to = "Value")


## Long data for New cases and deaths with n day moving average
# set number of days for moving average 
moveAvg = 5
dataLongAvg = dataLongDailyTests %>%
  group_by(Variable) %>%
  mutate(movAvgValue = RcppRoll::roll_mean(Value, moveAvg, fill = "left", align = "right"))


#### Plots ####
## plot daily new cases and deaths
p1 = ggplot(dataLongDailyTests %>% filter(Variable %in% c("New.deaths","New.cases")))+
  aes(Date, Value, fill = Variable, label = Value)+
  geom_col(position = "dodge", alpha = .5, width = .85)+
  geom_vline(xintercept = dataLongDailyTests %>%
               distinct(Date) %>% 
               filter(Date %in% as.Date(c("2020-03-17","2020-03-18","2020-03-28","2020-04-12"))) %>% 
               pull(Date), lty = 2, alpha = .4)+
  # n day moving average
  geom_path(data = dataLongAvg %>% filter(Variable %in% c("New.deaths","New.cases")), aes(Date, movAvgValue, color = Variable, group = Variable), size = 1.6, alpha = .9)+
  #geom_text(data=dataLongDailyTests %>% filter(Variable %in% c("New.cases")) %>% filter(Value > 0), aes(x= Date, y = Value), nudge_y = 4, size = 2.5)+
  #geom_text(data=dataLongDailyTests %>% filter(Variable %in% c("New.deaths")) %>% filter(Value > 0), aes(x= Date, y = Value), nudge_y = 2, size = 2.5)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("New.cases")) %>% filter(Date == last(Date)), aes(x= Date, y = Value), segment.color = NA, direction = "y", box.padding = .05, nudge_x = -.25, nudge_y = 1, size = 4)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("New.deaths")) %>% filter(Date == last(Date)), aes(x= Date, y = Value), segment.color = NA, direction = "y", box.padding = .05, nudge_x = .25, nudge_y = 1, size = 4, ylim = c(0, Inf))+
  annotate("label", x = as.Date(c("2020-03-16","2020-03-20","2020-03-29","2020-04-12")), y = c(100, 150, 200, 250), label = c("Bar close","School close","StayHomeOrder","8pm data"))+
  labs(x = "", y = "Number of new cases", title = "Daily new cases and deaths", fill = "")+
  guides(fill = guide_legend(order = 1))+
  scale_fill_brewer(name = "", palette = "Set2", labels = c("New case", "New death"))+
  scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), values = RColorBrewer::brewer.pal(3, "Dark2"), labels = c("New case", "New death"))+
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), legend.margin=margin(),legend.box="vertical",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12))
plot(p1)

## plot daily positive percentage with data point size indicating number of daily tests
p2 = ggplot()+
  geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("PositivePercent","DeathPercent")) %>% mutate(Value = Value*100), 
           aes(x = Date, y = Value, fill = rev(Variable)), color = NA, position = "dodge", width = .85, alpha = .5)+
  geom_vline(xintercept = dataLongDailyTests %>%
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               filter(Date %in% as.Date(c("2020-03-17","2020-03-18","2020-03-28","2020-04-12"))) %>% 
               pull(Date), lty = 2, alpha = .4)+
  # n day moving average
  geom_path(data = dataLongAvg %>% filter(Variable %in% c("PositivePercent","DeathPercent")), aes(Date, movAvgValue*100, color = rev(Variable), group = Variable), size = 1.8, alpha = .8)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("PositivePercent")) %>% filter(Date == last(Date)), aes(x= Date, y = Value*100, label = round(Value*100,2)), segment.color = NA, direction = "y", box.padding = .05, nudge_x = -.05, nudge_y = 1, size = 3.5)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("DeathPercent")) %>% filter(Date == last(Date)), aes(x= Date, y = Value*100, label = round(Value*100,2)), segment.color = NA, direction = "y", box.padding = .05, nudge_x = .5, nudge_y = 1, size = 3.5, ylim = c(0, Inf))+
  geom_point(inherit.aes = F, data = dataWide, aes(x = Date, y = -1, size = Daily.tests), shape = 21, stroke = 1, fill = "white")+
  scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), values = (RColorBrewer::brewer.pal(3, "Dark2")[1:2]), label = c("Positive rate","Fatality rate"))+
  annotate("label", x = as.Date(c("2020-03-16","2020-03-20","2020-03-29","2020-04-12")), y = c(11, 13, 15, 17), label = c("Bar close","School close","StayHomeOrder","8pm data"))+
  labs(x = "", y = "Percentage (%)", title = "Daily positive rate and fatality rate", size = "Daily tests", fill = "Positive rate")+
  guides(color = guide_legend(nrow=1,byrow=TRUE, order = 2), size = guide_legend(order = 3), fill = guide_legend(order = 1))+
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d")+
  scale_fill_manual(name = "", label = c("Positive rate","      Fatality rate \n(total death/total case)"), values = (RColorBrewer::brewer.pal(3, "Set2")[1:2]))+
  scale_size_continuous(range = c(.1,4))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),legend.margin=margin(),legend.box="vertical",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14),legend.text = element_text(size=12))
plot(p2)

# p2 = ggplot(dataWide)+
#   aes(Date, PositivePercent*100, size = Daily.tests, label = Date)+
#   geom_line(aes(group=1), size = .5, color = "gray50")+
#   geom_point(shape = 21, stroke = 1, fill = scales::alpha(RColorBrewer::brewer.pal(3,"Set2")[1], 0.7), color = "gray50")+
#   geom_vline(xintercept = dataLongDailyTests %>%
#                drop_na(Daily.tests) %>% 
#                distinct(Date) %>% 
#                filter(Date %in% as.Date(c("2020-03-17","2020-03-18","2020-03-28","2020-04-12"))) %>% 
#                pull(Date), lty = 2, alpha = .4)+
#   # n day moving average
#   geom_path(data = dataLongAvg %>% filter(Variable %in% c("PositivePercent")), aes(Date, movAvgValue*100, color = Variable, group = Variable), size = 1.8, alpha = .8)+
#   #scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
#   scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), values = RColorBrewer::brewer.pal(3, "Set1"), label = "Positive case percentage")+
#   annotate("label", x = as.Date(c("2020-03-16","2020-03-20","2020-03-29","2020-04-12")), y = c(11, 13, 15, 17), label = c("Bar close","School close","StayHomeOrder","8pm data"))+
#   guides(fill = guide_legend(nrow=1,byrow=TRUE), size = guide_legend())+
#   labs(x = "", y = "Percentage (%)", title = "MN COVID-19: daily positive case percentage", 
#        size = "Daily tests", fill = "Daily tests")+
#   scale_x_date(date_breaks = "3 days", date_labels = "%b %d")+
#   scale_size_continuous(range = c(.1,4))+
#   theme_minimal()+
#   theme(panel.grid.major.x = element_blank(),legend.margin=margin(),legend.box="vertical",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14),legend.text = element_text(size=12))
# plot(p2)
## plot Hospitalized, ICU, total Death, Hospitalized percentage and ICU percentage
## new version p3 (cols for percentage)
p3 = ggplot(dataLongDailyTests %>% 
              filter(Variable %in% c("ICU", "Currently.hospitalized", "Total.deaths"), Date > as.Date("2020-03-23")) %>% 
              ## this value transformation is for plotting on the secondary y-axis (right) for ICU percentage of current hospitalized cases
              mutate(Variable = factor(Variable, levels = c("Currently.hospitalized", "ICU", "Total.deaths"))))+ 
  aes(Date, Value, color = Variable)+
  geom_line(aes(group = Variable))+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("ICU", "Currently.hospitalized", "Total.deaths"), Date == last(Date)), aes(x= Date, y = Value, label = Value, color = Variable), segment.color = NA, direction = "y", box.padding = .05, nudge_y = 15, nudge_x = .9, size = 3.5, show.legend = F)+
  geom_point(fill = "white", shape = 21, stroke = .8, size = .9, show.legend = F)+
  geom_vline(xintercept = dataLongDailyTests %>% filter(Date > as.Date("2020-03-23")) %>% 
               distinct(Date) %>% 
               filter(Date %in% as.Date(c("2020-03-28","2020-04-12"))) %>% 
               pull(Date), lty = 2, alpha = .4)+
  geom_point(inherit.aes = F, data = dataWide %>% filter(Date > as.Date("2020-03-23")), aes(x = Date, y = -15, size = Daily.tests), shape = 21, stroke = 1, fill = "white")+
  geom_col(data = dataLongDailyTests %>% filter(Variable %in% c("HospitalizedPercent","ICUPercent"), Date > as.Date("2020-03-23")) %>% mutate(Value = Value*max(dataLongDailyTests %>% 
                                                                                                                                                                  filter(Variable %in% c("Date", "Currently.hospitalized"), Date > as.Date("2020-03-23")) %>%
                                                                                                                                                                  pull(Value)),
                                                                                                                                              Variable = factor(Variable)), aes(y = Value, fill = Variable), alpha = .3, color = NA, position = "dodge", width = .5)+
  labs(x = "", y = "Number of cases", size = "Daily tests", fill = "", title = str_wrap("Hospitalized, ICU, Death, Hospitalized percentage and ICU percentage",38))+
  guides(color=guide_legend(nrow=1,byrow=TRUE, order = 3))+
  scale_size_continuous(range = c(.1,4))+
  scale_color_manual(values = c(RColorBrewer::brewer.pal(3, "Set1")[1:2], RColorBrewer::brewer.pal(5, "Set1")[5]), name = "", labels = c("Current hospitalized", "Current ICU","Total deaths"))+
  scale_fill_manual(values = alpha(RColorBrewer::brewer.pal(3, "Set1"), .3), labels  = str_wrap(c("Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"), 26))+
  scale_y_continuous(sec.axis = sec_axis(~ ./max(dataLongDailyTests %>%
                                                   filter(Variable %in% c("Date", "Currently.hospitalized"), Date > as.Date("2020-03-23")) %>% 		
                                                   pull(Value))*100, 		
                                         name = "Percentage (%)"))+
  annotate("label", x = as.Date(c("2020-03-28","2020-04-12")), y = c(200,250), label = c("StayHomeOrder","8pm data"))+
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=10, angle = 50, hjust = 1), axis.text.y.right =  element_text(colour = "black"), axis.title.y.right = element_text(colour = "black"),
        legend.position = "bottom", legend.margin=margin(), legend.box="vertical",text=element_text(size=14), legend.text = element_text(size=12))
plot(p3)
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
p4 = ggplot(responseData)+
  aes(x = interaction(Detail3), y = Value, fill = Detail1, label = Value)+
  geom_bar(position = "stack", stat = "identity", width = .8) + 
  geom_text(position = "stack", vjust = 2)+
  facet_wrap(~Metric)+
  labs(fill = "Capacity", x = "", y = "", title = paste("Hospital surge capacity: updated on", format(mdy(responseData$Date), "%b %d")), caption = paste("Data source:", responseUrl))+
  scale_fill_brewer(palette ="Set2")+
  theme_minimal()+
  theme(title = element_text(size = 14), strip.text = element_text(size = 11, face = "bold"),
        axis.text.x =  element_text(size = 10, face = "bold"))
#plot(p4)
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


