p1 = dataWide %>% 
  mutate(test = (Daily.tests-lag(Daily.tests))/lag(Daily.tests)) %>% 
  filter(Date > "2020-03-19", test < 30) %>% 
  ggplot()+aes(x = Day, y = test, label = Date)+
  geom_boxplot(width = .55, outlier.colour = NA, alpha = .5)+
  geom_jitter(height = 0, width = .25, shape = 21, stroke = 1.1, color = RColorBrewer::brewer.pal(3, "Dark2")[1])+
  labs(y = "Test growth rate (%)", title = "MN COVID-19 testing", subtitle = "Date since Mar 20, excluding Mar 25 (5663 daily tests)",
       caption = "Formula: (current tests - past tests) / past tests", x = "")+
  theme_minimal() 
  
  ggsave(filename = "covidTestGrowthRate.png", plot = p1, device = "png", width = 4.5, height = 4)
  