p1 = dataWide %>% 
  mutate(Date = Date %>% as.Date(format = "%m/%d/%y"), 
    Day = Date %>% wday(label = TRUE),
    test = (Daily.tests-lag(Daily.tests))/lag(Daily.tests)) %>% 
  filter(Date > "2020-03-19", test < 30) %>% 
  ggplot()+aes(x = Day, y = test, label = Date)+
  geom_boxplot(width = .55, outlier.colour = NA, alpha = .5, size = .25)+
  geom_jitter(height = 0, width = .25, shape = 21, stroke = 1, size = .8, color = RColorBrewer::brewer.pal(3, "Dark2")[1])+
  labs(y = "Test growth rate (%)", title = "MN COVID-19 testing", subtitle = "Date since Mar20, excluding Mar25 (5663 daily tests)",
       caption = "Formula: (current tests - past tests) / past tests", x = "")+
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 8))

p2 = dataWide %>% 
  mutate(Date = Date %>% as.Date(format = "%m/%d/%y"), 
         Day = Date %>% wday(label = TRUE)) %>% 
  filter(Date > "2020-03-19") %>% 
  ggplot()+aes(x = Day, y = Daily.tests, label = Date)+
  geom_boxplot(width = .55, outlier.colour = NA, alpha = .5, size = .25)+
  geom_jitter(height = 0, width = .25, shape = 21, stroke = 1, size = .8, color = RColorBrewer::brewer.pal(3, "Dark2")[1])+
  labs(y = "Daily tests", title = "MN COVID-19 testing", subtitle = "Date since Mar20", x = "")+
  theme_minimal()+
  theme(plot.subtitle = element_text(size = 8))


  ggsave(filename = "covidTestGrowthRate.pdf", plot = gridExtra::grid.arrange(p2, p1, nrow = 1, ncol = 2), device = "pdf", width = 1.5, height = 1, scale = 4.5, units = "in")
  

  