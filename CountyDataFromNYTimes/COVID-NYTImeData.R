library(tidyverse)
library(gridExtra)
library(ggpubr)

## Source: https://github.com/nytimes/covid-19-data
## Pull data for NY-TImes github 
counties = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
states = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
countyMNPopulation = read_csv("MNCountyPopulation.csv") %>% 
  mutate(county = str_remove(County," County")) %>% 
  select(-County)

countyMN = counties %>% 
  filter(state == "Minnesota", cases > 0) %>%
  arrange(desc(cases)) %>% 
  pivot_longer(cols = c(cases,deaths)) %>% 
  group_by(county, name) %>% 
  mutate(diff = value -lead(value)) %>% 
  ungroup() %>% 
  filter(date == last(date)) %>%
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
  

## lollipop 
p1 = ggplot(countyMN, aes(x= reorder(county, value), value,  color = name, fill = name, label = value))+
  geom_segment(aes(x = reorder(county, value), xend=reorder(county, value), y=0, yend=value), color = "gray60")+
  geom_point(data=countyMN %>% filter(value != 0), aes(x= reorder(county, value)), size=3, shape=21, stroke=2)+
  geom_text(data=countyMN %>% filter(value > 0), aes(x= reorder(county, value)), nudge_y = 15, color = "gray20")+
  ## death number today
  geom_text(data=countyMN %>% filter(value < 0) %>% mutate(valueLabel = abs(value)), aes(x= reorder(county, value), label = valueLabel), nudge_y = -12, color = "gray20")+
  ## increase case from previous day
  geom_text(data=countyMN %>% filter(diff > 0, name == "cases") %>% mutate(valueLabel = paste0("(+", as.character(diff),")")), aes(x= reorder(county, value), label = valueLabel), nudge_y = .12*max(countyMN %>% filter(name == "cases") %>% pull(value)), color = "gray20")+
  ## increase death from previous day
  geom_text(data=countyMN %>% filter(diff > 0, name == "deaths") %>% mutate(valueLabel = paste0("(+", as.character(diff),")")), aes(x= reorder(county, value), label = valueLabel), nudge_y = -30, color = "gray20")+
  labs(x = "", y = "Number of cases", fill = "", color = "")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.y=element_blank(), plot.margin = unit(c(.75,0,.15,.5), "cm"))+
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
  geom_point(data=countyMN %>% filter(value != 0), size=3, shape=21, stroke=2)+
  geom_text(data=countyMN %>% filter(value > 0), aes(x= reorder(county, value)), nudge_y = 1, color = "gray20")+
  geom_text(data=countyMN %>% filter(value < 0) %>% mutate(valueLabel = abs(value10KPerCapita)), aes(x= reorder(county, value), label = sprintf("%0.2f", round(valueLabel, digits = 2))), nudge_y = -1, color = "gray20")+
  labs(x = "", y = "Number of cases per 10k capita", fill = "", color = "")+
  coord_flip()+
  theme_minimal()+
  ggtitle("source: https://github.com/nytimes/covid-19-data")+
  scale_fill_manual(name="", values = alpha(c("#F8766D","#00BFC4"),.5))+
  theme(plot.margin = unit(c(.75, 1, .15, -.5), "cm"))
plot(p2)

#grid.arrange(p1,p2, nrow = 1)
ggarrange(p1,p2, common.legend = T)
ggsave("county.png",ggarrange(p1,p2, common.legend = T, legend = ("bottom")), width = 14, height = 8)

