dataWide %>% 
  mutate(test = (Daily.tests-lag(Daily.tests))/lag(Daily.tests)) %>% 
  filter(Date > "2020-03-19", test < 30) %>% 
  ggplot()+aes(x = Day, y = test, label = Date)+
  geom_boxplot(width = .55)+
  labs(y = "Test growth rate (%)", title = "MN COVID-19 testing", subtitle = "Date since Mar 20, excluding Mar 25 (5663 daily tests)",
       caption = "Formula: (current tests - past tests) / past tests", x = "")+
  theme_minimal()
  