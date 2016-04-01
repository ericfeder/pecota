projections.summary <- projections.combined %>% 
  group_by(team) %>% 
  summarize(mean = mean(wins), 
            perc90 = mean(wins > 90), 
            perc72 = mean(wins < 72),
            sd = sd(wins),
            iqr = IQR(wins),
            skewness = skewness(wins),
            kurtosis = kurtosis(wins))
