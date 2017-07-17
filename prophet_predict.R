


## Making Predictions 

con$ds <- as.Date(as.character(con$date_received), format = "%m/%d/%Y")
prophet <- con %>% group_by(ds) %>% summarise(y = n())
m <- prophet(prophet)

future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)

ggplot(forecast, aes(x= ds, y= yhat)) + 
  geom_line(aes(y = yhat), size =.25) +
  xlab("Date") + ylab("Projected Number of Complaints") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=22, family="KerkisSans")) +
  theme(panel.background = element_rect(fill = "gray77")) +
  ggtitle("Using Facebook's Prophet to make Predictions") + geom_smooth(se = FALSE)

ggsave(file="full_prophet_time.png", type = "cairo-png", width = 20, height =12)

forecast$year <- format(as.Date(forecast$ds, format="%Y/%m/%d"),"%Y")
forecast %>% group_by(year) %>% summarise(mean = mean(yhat))  

forecast$date <- format(as.Date(forecast$ds, format="%Y/%m/%d"))
forecast %>% filter(date > "2017-07-17" & date <"2018-07-17") %>% summarise(sum = sum(yhat)) %>% mutate(perday = sum/365)

## total 222397.8 
## per day 609.3092

forecast %>% 
  filter(date > "2017-07-17" & date <"2018-07-17") %>% 
  ggplot(., aes(x= ds, y= yhat)) + 
  geom_line(aes(y = yhat), size =.5) +
  xlab("Date") + ylab("Projected Number of Complaints") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=22, family="KerkisSans")) +
  theme(panel.background = element_rect(fill = "gray77")) +
  ggtitle("Using Facebook's Prophet to make Predictions") + geom_smooth(se = FALSE)

ggsave(file="prophet_next_year.png", type = "cairo-png", width = 20, height =12)

con$year <- format(as.Date(con$date_received, format="%d/%m/%Y"),"%Y")

yearplot <- con %>% group_by(year) %>% 
  summarise(diff = mean(diff_date, na.rm= TRUE)) %>% 
  mutate(diff = as.numeric((round(diff,2)*-1))) %>% 
  na.omit()

ggplot(yearplot, aes(x=year, y=diff)) +  geom_col()