library(tidyverse)
library(janitor)
library(scales)
library(extrafont)
library(ggjoy)
library(prophet)
library(leaflet)
library(viridis)
library(spdplyr)
library(ggalt)
library(stringi)

## Read in the Data and Clean it
con <- read_csv("D://cfpb/Consumer_Complaints.csv") %>%  
  clean_names() %>% 
  glimpse()

## WHich Companies Show up the Most? 
p1 <- con %>% count(company) %>% arrange(-n) %>% head(10)

p1 %>% ggplot(., aes(x=reorder(company,n), y=n)) + 
  geom_col(fill = "firebrick1", color = "black") + coord_flip() +
  labs(x="Company", y= "Number of Complaints", title = "Which Companies Receive the Most Complaints?") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=22, family="KerkisSans"))

ggsave(file="most_per_company.png", type = "cairo-png", width = 24, height =12)


con %>% group_by(company) %>% count(product) %>% filter(n >100) %>% arrange(-n)

con$diff_date <- as.Date(as.character(con$date_received), format = "%m/%d/%Y")-
  as.Date(as.character(con$date_sent_to_company), format = "%m/%d/%Y")

plot <- data.frame("label" = c("Two Days or Less", "Three Days or More"), pct = c(68.9, 31.1))

ggplot(plot, aes(x=reorder(label, -pct), y=pct/100)) + geom_col(fill = "firebrick1", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "", y= "Percent of Complaints", title= "Claims Sent to Company")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="overall_time_to_respond.png", type = "cairo-png", width = 24, height =12)


time <- c("5 Days", "15 Days", "30 Days", "50 Days", "100 Days", "500 Days", "1000 Days")
claims <- c(127692, 50299, 28740, 15022, 10527, 7216, 7055)

tail <- data.frame(time, claims)
tail$time <- factor(tail$time, levels=unique(tail$time))

tail <- tail %>% mutate(new =claims/808618)


ggplot(tail, aes(x=reorder(time, -new), y=new)) + geom_col(fill = "firebrick1", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "Time Being Held", y= "% of Complaints Still Being Processed", title= "The Distribution Is Long Tailed")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="time_being_held.png", type = "cairo-png", width = 24, height =12)