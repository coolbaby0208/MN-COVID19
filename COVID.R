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
library(gridExtra)

## Data preprocessing
dataWide = read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y"), 
         Daily.tests = Total.tested - lag(Total.tested), 
         New.cases = Total.cases - lag(Total.cases), 
         Currently.sick = Total.cases - Total.deaths - Total.recovered,
         New.deaths = Total.deaths - lag(Total.deaths),
         New.ICU = ICU - lag(ICU),
         New.hospitalized = Currently.hospitalized - lag(Currently.hospitalized),
         New.sick = Currently.sick - lag(Currently.sick)) %>% 
  mutate(Date = factor(str_remove(Date,"2020-")), 
         PositivePercent = New.cases/Daily.tests, 
         ICUPercent = ICU/Currently.hospitalized, 
         HospitalizedPercent = Currently.hospitalized/Currently.sick) 

## Convert to long format for p1 and p3 plots
dataLongDailyTests = dataWide %>% 
  pivot_longer(c(-Date, -Daily.tests), names_to = "Variable", values_to = "Value")

#### Plots ####
## plot daily new cases and deaths
p1 = ggplot(dataLongDailyTests %>% drop_na(Daily.tests) %>% 
              filter(Variable %in% c("New.deaths","New.cases")))+
  aes(Date, Value, fill = Variable, label = Value)+
  geom_col(position = "identity")+
  geom_text(data=dataLongDailyTests %>% drop_na(Daily.tests) %>% 
              filter(Variable %in% c("New.cases")) %>% filter(Value > 0), aes(x= Date, y = Value), nudge_y = 2, size = 3)+
  geom_text(data=dataLongDailyTests %>% drop_na(Daily.tests) %>% 
              filter(Variable %in% c("New.deaths")) %>% filter(Value > 0), aes(x= Date, y = Value), nudge_y = 1)+
  theme(legend.position = "bottom", axis.text.x = element_text(size=11, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12))+
  labs(y = "Number of new cases", title = "MN COVID-19: daily new cases and deaths", fill = "")+
  geom_vline(xintercept = dataLongDailyTests %>%
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               mutate(Loc = as.integer(factor(Date))) %>% 
               filter(Date %in% c("03-17","03-18","03-28","04-12")) %>% 
               pull(Loc), lty = 2)+
  annotate("label", x = c("03-16","03-20","03-29","04-12"), y = c(70, 90, 110, 150), label = c("Bar close","School close","StayHomeOrder","8pm data"))+
  scale_fill_brewer(name = "", palette = "Set2", labels = c("New case", "New death"))

## plot daily positive percentage with data point size indicating number of daily tests
p2 = ggplot(dataWide %>% drop_na(Daily.tests))+
  aes(Date, PositivePercent*100, size = Daily.tests, label = Date, fill = Daily.tests)+
  geom_line(aes(group=1), size = 1, color = "gray20")+
  geom_point(shape = 21, stroke = 1.5)+
  geom_vline(xintercept = dataLongDailyTests %>%
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               mutate(Loc = as.integer(factor(Date))) %>% 
               filter(Date %in% c("03-17","03-18","03-28","04-12")) %>% 
               pull(Loc), lty = 2)+
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
  annotate("label", x = c("03-16","03-20","03-29","04-12"), y = c(12, 15, 15, 15), label = c("Bar close","School close","StayHomeOrder","8pm data"))+
  guides(fill = guide_legend(nrow=2,byrow=TRUE), size = guide_legend())+
  labs(x = "Date", y = "Percentage (%)", title = "MN COVID-19: daily positive case percentage", 
       size = "Number of daily tests", fill = "Number of daily tests")+
  theme(legend.position = "bottom", axis.text.x = element_text(size=11, angle = 50, hjust = 1), text=element_text(size=14),legend.text = element_text(size=12))

## plot Hospitalized, ICU, total Death, Hospitalized percentage and ICU percentage
p3 = ggplot(dataLongDailyTests %>% drop_na(Daily.tests) %>% 
              filter(Variable %in% c("ICU", "Currently.hospitalized", "ICUPercent", "Total.deaths", "HospitalizedPercent"), as.numeric(Date)>18) %>% 
              mutate(Value = ifelse(str_detect(Variable, "Percent"), Value*max(dataLongDailyTests %>%
                                                                                     drop_na(Daily.tests) %>% 		
                                                                                     filter(Variable %in% c("Date", "Currently.hospitalized"), as.numeric(Date)>18) %>% 		
                                                                                     pull(Value)), Value), Variable = factor(Variable, levels = c("Currently.hospitalized", "ICU", "Total.deaths", "HospitalizedPercent","ICUPercent"))), 
            aes(Date, Value, color = Variable))+
  geom_line(aes(group = Variable, lty = Variable), size = 1.2)+
  geom_point(aes(fill = Variable),shape = 21, stroke = 1.2)+
  geom_vline(xintercept = dataLongDailyTests %>% filter(as.numeric(Date)>18) %>% 
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               mutate(Loc = as.integer(factor(Date))) %>% 
               filter(Date %in% c("03-28","04-12")) %>% 
               pull(Loc), lty = 2)+
  geom_point(inherit.aes = F, data = dataWide %>% filter(as.numeric(Date)>18), aes(x = Date, y = -15, size = Daily.tests), shape = 21, stroke = 1.2, fill = "white")+
  labs(y = "Number of cases", size = "Number of daily tests", fill = "", title = str_wrap("Hospitalized, ICU, Death, Hospitalized percentage and ICU percentage",38))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))+
  scale_color_brewer(palette = "Dark2", name = "", labels = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"), 30))+
  scale_fill_manual(values = c("white","white", "white", alpha(c("#E7298A", "#66A61E"), .5)), labels  = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"),30))+
  scale_y_continuous(sec.axis = sec_axis(~ ./max(dataLongDailyTests %>%
                                                   drop_na(Daily.tests) %>% 		
                                                   filter(Variable %in% c("Date", "Currently.hospitalized"), as.numeric(Date)>18) %>% 		
                                                   pull(Value))*100, 		
                                         name = "Percentage (%)"))+
  scale_linetype_manual(values= c(1,1,1,2,2), name = "", labels = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"),30))+
  annotate("label", x = c("03-28","04-12"), y = c(180,180), label = c("StayHomeOrder","8pm data"))+
  theme(axis.text.x = element_text(size=12, angle = 50, hjust = 1), axis.text.y.right =  element_text(colour = "black"), axis.title.y.right = element_text(colour = "black"),
        legend.position = "bottom", legend.margin=margin(), legend.box="vertical",text=element_text(size=14), legend.text = element_text(size=12))

## plot hospital surge capacity
p4 = ggplot(responseData)+
  aes(x = interaction(Detail3), y = Value, fill = Detail1, label = Value)+
  geom_bar(position = "stack", stat = "identity", width = .8) + 
  geom_text(position = "stack", vjust = 2)+
  facet_wrap(~Metric)+
  labs(fill = "Capacity", x = "", y = "", title = paste("Hospital surge capacity: updated on", format(mdy(responseData$Date), "%Y-%m-%d")), caption = "Data source: https://mn.gov/covid19/data/response")+
  scale_fill_brewer(palette ="Set2")+
  theme_minimal()+
  theme(title = element_text(size = 14), strip.text = element_text(size = 11, face = "bold"),
        axis.text.x =  element_text(size = 10, face = "bold"))

#### Other exploratory plots: not print####
# ggplot(dataWide,aes(x = Date))+
#   geom_point(aes(y = Total.cases))+
#   geom_point(aes(y = ICU), color = "red")+
#   #geom_line(aes(group = 1))+
#   stat_smooth(aes(y = Total.cases, group = 1), method = "lm", formula = y~log2(x))+
#   scale_y_continuous(trans='log2')+
#   labs(y = "Total cases")+
#   theme(legend.position = "bottom", axis.text.x = element_text(size=11, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12))

# 
# ## New cases with 3 day moving average ####
# 
# dataLongAvg = dataLongDailyTests %>%
#   group_by(Variable) %>% 
#   mutate(movAvgValue = RcppRoll::roll_mean(Value, 3, fill = "left", align = "right"))
# 
# ggplot(dataLongAvg %>% drop_na(Daily.tests) %>% 
#          filter(Variable %in% c("New.deaths","New.cases")))+
#   aes(Date, movAvgValue, fill = Variable, label = round(movAvgValue))+
#   geom_col(position = "identity")+
#   geom_text(data=dataLongAvg %>% drop_na(Daily.tests) %>% 
#               filter(Variable %in% c("New.cases")) %>% filter(movAvgValue > 0), aes(x= Date, y = movAvgValue), nudge_y = 2, size = 3)+
#   geom_text(data=dataLongAvg %>% drop_na(Daily.tests) %>% 
#               filter(Variable %in% c("New.deaths")) %>% filter(movAvgValue > 0), aes(x= Date, y = movAvgValue), nudge_y = 1)+
#   theme(legend.position = "bottom", axis.text.x = element_text(size=11, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12))+
#   labs(y = "Number of new cases", title = "MN COVID-19: daily new cases and deaths: \n3 day moving average", fill = "")+
#   geom_vline(xintercept = dataLongAvg %>%
#                drop_na(Daily.tests) %>% 
#                distinct(Date) %>% 
#                mutate(Loc = as.integer(factor(Date))) %>% 
#                filter(Date %in% c("03-17","03-18","03-28","04-12")) %>% 
#                pull(Loc), lty = 2)+annotate("label", x = c("03-16","03-20","03-29","04-12") ,y = c(70, 90, 110, 150), label = c("Bar close","School close","StayHomeOrder","8pm data"))+
#   scale_fill_brewer(name = "", palette = "Set2", labels = c("New case", "New death"))
