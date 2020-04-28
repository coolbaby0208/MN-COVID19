library(tidyverse)
library(ggpubr)
library(usmap)


## function for preprocess county data from NYtimes ####
## x = dataset from NYtime, y = state name
preprocessCountyData = function(x, y) {
  result = x %>% 
    filter(state == y, cases > 0) %>%
    arrange(desc(cases)) %>% 
    pivot_longer(cols = c(cases,deaths)) %>% 
    group_by(county, name) %>% 
    mutate(diff = value -lead(value)) %>% 
    ungroup() %>% 
    filter(date == last(date))
  return(result)
}


## Source: https://github.com/nytimes/covid-19-data
## Pull data for NY-TImes github 

## read in data ####
counties = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") 

states = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

countyMNPopulation = read_csv("MNCountyPopulation.csv") %>% 
  mutate(county = str_remove(County," County")) %>% 
  select(-County)

countyMN = preprocessCountyData (counties, "Minnesota") %>%
  left_join(countyMNPopulation, by = "county") %>% 
  mutate(value = ifelse(name == "deaths", -1*value, value)) %>% 
  mutate(value10KPerCapita = value/Population*10000) 

## bars
# p1 = ggplot(countyMN, aes(x= reorder(county, value), value, fill = name, label = value))+
#   geom_col() +
#   ## case number today
#   geom_text(data=countyMN %>% filter(value > 0), aes(x= reorder(county, value)), nudge_y = 6)+
#   ## death number today
#   geom_text(data=countyMN %>% filter(value < 0) %>% mutate(valueLabel = abs(value)), aes(x= reorder(county, value), label = valueLabel), nudge_y = -10)+
#   ## increase case from previous day
#   geom_text(data=countyMN %>% filter(diff > 0, name == "cases") %>% mutate(valueLabel = paste0("(+", as.character(diff),")")), aes(x= reorder(county, value), label = valueLabel), nudge_y = .1*max(countyMN %>% filter(name == "cases") %>% pull(value)))+
#   ## increase death from previous day
#   geom_text(data=countyMN %>% filter(diff > 0, name == "deaths") %>% mutate(valueLabel = paste0("(+", as.character(diff),")")), aes(x= reorder(county, value), label = valueLabel), nudge_y = -25)+
#   labs(x = "County", y = "Number of cases", fill = "")+
#   coord_flip()+
#   theme_minimal()+
#   ggtitle(last(countyMN$date))
fontsize = 3  

## lollipop 
p1 = ggplot(countyMN, aes(x= reorder(county, value), value,  color = name, fill = name, label = value))+
  geom_segment(aes(x = reorder(county, value), xend=reorder(county, value), y=0, yend=value), color = "gray60")+
  geom_point(data=countyMN %>% filter(value != 0), aes(x= reorder(county, value)), size=2, shape=21, stroke=2)+
  ## case number today
  geom_text(data=countyMN %>% filter(value > 0), aes(x= reorder(county, value)), nudge_y = 25, color = "gray20", size = fontsize)+
  ## death number today
  geom_text(data=countyMN %>% filter(value < 0) %>% mutate(valueLabel = abs(value)), aes(x= reorder(county, value), label = valueLabel), nudge_y = .5*min(countyMN %>% filter(name == "deaths") %>% pull(value)), color = "gray20", size = fontsize )+
  ## increase case from previous day
  geom_text(data=countyMN %>% filter(diff > 0, name == "cases") %>% mutate(valueLabel = paste0("(+", as.character(diff),")")), aes(x= reorder(county, value), label = valueLabel), nudge_y = .1*max(countyMN %>% filter(name == "cases") %>% pull(value)), color = "gray20", size = fontsize )+
  ## increase death from previous day
  geom_text(data=countyMN %>% filter(diff > 0, name == "deaths") %>% mutate(valueLabel = paste0("(+", as.character(diff),")")), aes(x= reorder(county, value), label = valueLabel), nudge_y = 1.2*min(countyMN %>% filter(name == "deaths") %>% pull(value)), color = "gray20", size = fontsize)+
  labs(x = "", y = "Number of cases", fill = "", color = "")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.y=element_blank(), plot.margin = unit(c(.75,0,.1, 1), "cm"))+
  ggtitle(paste(last(countyMN$date), ":", "Cases by MN county"))+
  scale_fill_manual(name="", values = alpha(c("#F8766D","#00BFC4"),.5))

## bars
# p2 = ggplot(countyMN %>% filter(name == "cases"), aes(x= reorder(county, value), value10KPerCapita, fill = name, label = sprintf("%0.2f", round(value10KPerCapita, digits = 2))))+
#   geom_col() +
#   ## case number today
#   geom_text(data=countyMN %>% filter(value > 0), aes(x= reorder(county, value)), nudge_y = 1)+
#   labs(x = "", y = "Number of cases per 10k capita", fill = "")+
#   coord_flip()+
#   theme_minimal()+
#   ggtitle("source: https://github.com/nytimes/covid-19-data")

## lollipop 
p2 = ggplot(countyMN, aes(x= reorder(county, value), value10KPerCapita, color = name, fill = name, label = sprintf("%0.2f", round(value10KPerCapita, digits = 2))))+
  geom_segment(aes(x=reorder(county, value), xend=reorder(county, value), y=0, yend=value10KPerCapita), color = "gray60") +
  geom_point(data=countyMN %>% filter(value != 0), size=2, shape=21, stroke=2)+
  geom_text(data=countyMN %>% filter(value > 0), aes(x= reorder(county, value)), nudge_y = 1, color = "gray20", size = fontsize)+
  geom_text(data=countyMN %>% filter(value < 0) %>% mutate(valueLabel = abs(value10KPerCapita)), aes(x= reorder(county, value), label = sprintf("%0.2f", round(valueLabel, digits = 2))), nudge_y = -1, color = "gray20", size = fontsize)+
  labs(x = "", y = "Number of cases per 10k capita", fill = "", color = "")+
  coord_flip()+
  theme_minimal()+
  ggtitle("source: https://github.com/nytimes/covid-19-data")+
  scale_fill_manual(name="", values = alpha(c("#F8766D","#00BFC4"),.5))+
  theme(plot.margin = unit(c(.75, 1.5, .1, -.5), "cm"))
#plot(p2)
fig1 = ggarrange(p1,p2, common.legend = T, legend = ("bottom"))
#grid.arrange(p1,p2, nrow = 1)
#ggarrange(p1,p2, common.legend = T)
ggsave("MNcounty_COVID-19_Lollipop.png",fig1, width = 14, height = 8)


## Add county map

p3 = plot_usmap(data = countyMN %>% filter(name == "cases"), values = "value", include = c("MN"), color = "black")+ 
  scale_fill_continuous(low = "peachpuff", high = "red", name = "Total cases", na.value = 'white')+ 
  #labs(title = "COVID-19 data by county", subtitle = last(countyMN$date)) +
  theme(legend.position = "right", plot.margin = unit(c(1, 0.5, .1, 1), "cm"))

p4 = plot_usmap("counties",data = countyMN %>% filter(name == "cases"), values = "value10KPerCapita", include = c("MN"), color = "black")+ 
  scale_fill_continuous(low = "peachpuff", high = "red", name = "Cases 10K per capita", na.value = 'white')+
  #labs(title = "COVID-19 cases per 10k capita by county", subtitle = last(countyMN$date)) +
  theme(legend.position = "right", plot.margin = unit(c(1, 1, .1, -.5), "cm"))

p5 = plot_usmap(data = countyMN %>% 
                  filter(name == "deaths") %>% 
                  mutate(value = abs(value)), values = "value", include = c("MN"), color = "black")+ 
  scale_fill_continuous(low = "snow2", high = "black", name = "Total deaths", na.value = 'white')+ 
  #labs(title = "COVID-19 data by county", subtitle = last(countyMN$date)) +
  theme(legend.position = "right", plot.margin = unit(c(.5, .5, 1, 1), "cm"))

p6 = plot_usmap(data = countyMN %>% 
                  filter(name == "deaths") %>% 
                  mutate(value10KPerCapita = abs(value10KPerCapita)), values = "value10KPerCapita", include = c("MN"), color = "black")+ 
  scale_fill_continuous(low = "snow2", high = "black", name = "Deaths 10K per capita", na.value = 'white')+
  #labs(title = "COVID-19 deaths per 10k capita by county", subtitle = last(countyMN$date)) +
  theme(legend.position = "right", plot.margin = unit(c(.5, 1.5, 1, -.5), "cm"))
fig2 = ggarrange(p3,p4,p5,p6, nrow = 2, ncol = 2)

ggsave("MNcounty_COVID-19_Map.png", 
       annotate_figure(fig2, fig.lab = paste("COVID-19 by MN county:", last(countyMN$date)), bottom = "Data source: https://github.com/nytimes/covid-19-data"), width = 10, height = 6)

## Combine both figures
ggsave("MNcounty_COVID-19_Combined.png", gridExtra::grid.arrange(fig1, fig2, nrow = 2 , ncol = 1, heights = c(1.5,1)), width = 12, height = 14)

#### Just for Ken (Utah data) ####
countyUT = preprocessCountyData (counties, "Utah")
p7 = plot_usmap(data = countyUT %>% filter(name == "cases"), values = "value", include = c("UT"), color = "black")+ 
  scale_fill_continuous(low = "peachpuff", high = "red", name = "Total cases", na.value = 'white')+ 
  #labs(title = "COVID-19 data by county", subtitle = last(countyMN$date)) +
  theme(legend.position = "right", plot.margin = unit(c(1, 0.5, .1, 1), "cm"))

p8 = plot_usmap(data = countyUT %>% 
                  filter(name == "deaths") %>% 
                  mutate(value = abs(value)), values = "value", include = c("UT"), color = "black")+ 
  scale_fill_continuous(low = "snow2", high = "black", name = "Total deaths", na.value = 'white')+ 
  #labs(title = "COVID-19 data by county", subtitle = last(countyMN$date)) +
  theme(legend.position = "right", plot.margin = unit(c(.5, .5, 1, 1), "cm"))

ggsave("UTcounty_COVID-19_Map.png", 
       annotate_figure(ggarrange(p7, p8, nrow = 1, ncol = 2), fig.lab = paste("COVID-19 by UT county:", last(countyUT$date)), bottom = "Data source: https://github.com/nytimes/covid-19-data"), width = 10, height = 6)
