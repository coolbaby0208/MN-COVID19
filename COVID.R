## data source: https://www.twincities.com/coronavirus-mn/cases-map/
## data source: https://www.health.state.mn.us/diseases/coronavirus/situation.html
## Credit: coolbaby0208

library(tidyverse)
library(gridExtra)

## Remove existing variables ####
rm(list=ls(all=TRUE))

## data preprocessing
dataWide = read.csv("MNCovidData.csv", na.strings = c("", "NA")) %>% 
  mutate(Date = as.Date(Date,format = "%m/%d/%y"), 
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

#### Old Plots: not printing ####
# p1 = ggplot(dataLong %>% filter(!Variable %in% c("Total.tested", "Daily.tests", "Total.cases")), 
#        aes(Date, Value, color = Variable))+
#   geom_line(aes(group = Variable),size= 1.2)+geom_point(shape = 21, fill = "white")+
#   theme(legend.position = "right", axis.text.x = element_text(size=12, angle = 90))+
#   geom_vline(xintercept = c(12,23), lty = 2)+annotate("text", x = c(11,21) ,y = 350:351, label = c("Bar close", "StayHomeOrder"))+
#   ggtitle("MN COVID cases")
# plot(p1)
# 
# 
# 
# p2 = ggplot(dataLong %>% filter(Variable %in% c("PositivePercent", "Daily.tests")) %>% mutate(Value = ifelse(Variable == "PositivePercent", Value*10000, Value)), 
#             aes(Date, Value, color = Variable))+
#   geom_line(aes(group = Variable), size= 1.2)+geom_point(shape = 21, fill = "white")+
#   theme(legend.position = "top", axis.text.x = element_text(size=12, angle = 90))+labs(y = "Number of daily tests")+
#   geom_vline(xintercept = c(12,23), lty = 2)+annotate("text", x = c(11,24), y = 4600:4601, label = c("Bar close", "StayHomeOrder"))+
#   ggtitle("Number of daily tests vs Positive case percentage")+ scale_y_continuous(sec.axis = sec_axis(~ ./100, name = "Positive case percentage (%)"))
# plot(p2)


# p3 = ggplot(dataWide %>% drop_na(Daily.tests), aes(Daily.tests, PositivePercent*100, label = Date))+
#   geom_point(aes(color = Daily.tests > 900), size = 2, shape = 21, fill = "white", stroke = 1.5)+
#   scale_colour_manual(name = 'Number of daily tests', 
#                       values = setNames(c("#E69F00", "#56B4E9"), c(T, F)), labels = c("< 900", "> 900"))+
#   labs(x = "Number of daily tests", y = "Positive case percentage (%)", title = "Positive case rate vs Number of daily tests")+
#   geom_smooth(data = dataWide %>% drop_na(Daily.tests) %>% filter(Daily.tests >900, Daily.tests <4000), method = "glm", se = T, color = "darkgray", size = 1.1)+
#   theme(legend.position = "top")
# plot(p3)
# plotly::ggplotly(p3)

# p4 = ggplot(dataLong %>% filter(Variable %in% c("ICU", "Currently.hospitalized", "ICUPercent", "Total.deaths"), as.numeric(Date)>18)%>% mutate(Value = ifelse(Variable == "ICUPercent", Value*100, Value)), 
#             aes(Date, Value, color = Variable))+
#   geom_line(aes(group = Variable, size = Variable))+geom_point(shape = 21, fill = "white")+
#   theme(legend.position = "right", axis.text.x = element_text(size=12, angle = 90))+labs(y = "Number of cases")+
#   geom_vline(xintercept = c(1), lty = 2)+annotate("text", x = c(3), y = c(90), label = c("StayHomeOrder"))+
#   ggtitle("Hospitalized cases")+
#   scale_color_brewer(palette = "Set1")+
#   scale_y_continuous(sec.axis = sec_axis(~ ./1, name = "ICU percentage"))+
#   scale_size_manual(values= c(1,1,2,1))+
#   theme(axis.text.y.right =  element_text(colour = "darkgreen"), axis.title.y.right = element_text(colour = "darkgreen"))
# 

#### Current plots ####
## plot daily new cases and deaths
p1 = ggplot(dataLongDailyTests %>% drop_na(Daily.tests) %>% 
              filter(Variable %in% c("New.deaths","New.cases")))+
  aes(Date, Value, fill = Variable, label = Value)+
  geom_col(position = "identity")+
  geom_text(data=dataLongDailyTests %>% drop_na(Daily.tests) %>% 
              filter(Variable %in% c("New.deaths")) %>% filter(Value > 0), aes(x= Date, y = Value), nudge_y = 1)+
  theme(legend.position = "bottom", axis.text.x = element_text(size=11, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12))+
  labs(y = "Number of new cases", title = "MN COVID-19: daily new cases and deaths", fill = "")+
  geom_vline(xintercept = dataLongDailyTests %>%
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               mutate(Loc = as.integer(factor(Date))) %>% 
               filter(Date %in% c("03-17","03-18","03-28")) %>% 
               pull(Loc), lty = 2)+annotate("label", x = c("03-16","03-20","03-29") ,y = c(70, 75, 80), label = c("Bar close","School close","StayHomeOrder"))+
  scale_fill_brewer(name = "", palette = "Set2", labels = c("New case", "New death"))

plot(p1)

## plot daily positive percentage with data point size indicating number of daily tests
p2 = ggplot(dataWide %>% drop_na(Daily.tests))+
  aes(Date, PositivePercent*100, size = Daily.tests, label = Date, fill = Daily.tests)+
  geom_line(aes(group=1), size = 1, color = "gray20")+
  geom_point(shape = 21, stroke = 1.5)+
  labs(x = "Date", y = "Percentage (%)", title = "MN COVID-19: daily positive case percentage", 
       size = "Number of daily tests", fill = "Number of daily tests")+
  theme(legend.position = "bottom", axis.text.x = element_text(size=11, angle = 50, hjust = 1), text=element_text(size=14),legend.text = element_text(size=12))+
  geom_vline(xintercept = dataLongDailyTests %>%
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               mutate(Loc = as.integer(factor(Date))) %>% 
               filter(Date %in% c("03-17","03-18","03-28")) %>% 
               pull(Loc), lty = 2)+
  annotate("label", x = c("03-16","03-20","03-29") ,y = c(12, 15, 15), label = c("Bar close","School close","StayHomeOrder"))+
  guides(fill = guide_legend(), size = guide_legend()) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

## plot Hospitalized, ICU, total Death, Hospitalized percentage and ICU percentage
p3 = ggplot(dataLongDailyTests %>% drop_na(Daily.tests) %>% 
              filter(Variable %in% c("ICU", "Currently.hospitalized", "ICUPercent", "Total.deaths", "HospitalizedPercent"), as.numeric(Date)>18) %>% 
              mutate(Value = ifelse(str_detect(Variable, "Percent"), Value*100, Value), Variable = factor(Variable, levels = c("Currently.hospitalized", "ICU", "Total.deaths", "HospitalizedPercent","ICUPercent"))), 
            aes(Date, Value, color = Variable))+
  geom_line(aes(group = Variable, lty = Variable), size = 1.2)+
  geom_point(aes(size = Daily.tests, fill = Variable),shape = 21, stroke = 1.2)+
  theme(axis.text.x = element_text(size=12, angle = 50, hjust = 1))+
  labs(y = "Number of cases", size = "Number of daily tests", fill = "")+
  ggtitle(str_wrap("Hospitalized, ICU, Death, Hospitalized percentage and ICU percentage",38))+
  geom_vline(xintercept = dataLongDailyTests %>% filter(as.numeric(Date)>18) %>% 
               drop_na(Daily.tests) %>% 
               distinct(Date) %>% 
               mutate(Loc = as.integer(factor(Date))) %>% 
               filter(Date %in% c("03-28")) %>% 
               pull(Loc), lty = 2)+
  annotate("label", x = c("03-28"), y = 100, label = c("StayHomeOrder"))+
  #scale_alpha_manual(values = c(1,1,1,.75,.75), labels  = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"),30))+
  scale_color_brewer(palette = "Dark2", name = "", labels = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"), 30))+
  scale_fill_manual(values = c("white","white", "white", alpha(c("#E7298A", "#66A61E"), .5)), labels  = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"),30))+
  scale_y_continuous(sec.axis = sec_axis(~ ./max(dataLongDailyTests %>% 
                                                   drop_na(Daily.tests) %>% 
                                                   filter(Variable %in% c("Date", "Currently.hospitalized"), as.numeric(Date)>18) %>% 
                                                   pull(Value))*100, 
                                                 name = "Percentage (%)"))+
  scale_linetype_manual(values= c(1,1,1,3,3), name = "", labels = str_wrap(c("Current hospitalized", "Current ICU","Total deaths", "Hospitalized percentage (of current active cases)","ICU percentage (of current hospitalized cases)"),30))+
  theme(axis.text.y.right =  element_text(colour = "black"), axis.title.y.right = element_text(colour = "black"),
        legend.position = "bottom", legend.margin=margin(), legend.box="vertical",text=element_text(size=14), legend.text = element_text(size=12))+
  guides(color=guide_legend(nrow=2,byrow=TRUE))
plot(p3)
#grid.arrange(p1, p2,p3, nrow = 3)
## Save 3 plots as a png file ##
ggsave(paste0("MN-COVID-19_", Sys.Date(), ".png"), grid.arrange(p1,p2,p3, nrow = 3), height = 12, width = 8)

#### other exploratory plots ####
# ggplot(dataWide,aes(x = Date))+
#   geom_point(aes(y = Total.cases))+
#   geom_point(aes(y = ICU), color = "red")+
#   #geom_line(aes(group = 1))+
#   stat_smooth(aes(y = Total.cases, group = 1), method = "lm", formula = y~log2(x))+
#   scale_y_continuous(trans='log2')+
#   labs(y = "Total cases")+
#   theme(legend.position = "bottom", axis.text.x = element_text(size=11, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12))
