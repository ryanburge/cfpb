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

ggsave(file="most_per_company.png", type = "cairo-png", width = 20, height =12)


con %>% group_by(company) %>% count(product) %>% filter(n >100) %>% arrange(-n)

con$diff_date <- as.Date(as.character(con$date_received), format = "%m/%d/%Y")-
                           as.Date(as.character(con$date_sent_to_company), format = "%m/%d/%Y")



plot <- data.frame("label" = c("Two Days or Less", "Three Days or More"), pct = c(68.9, 31.1))

ggplot(plot, aes(x=reorder(label, -pct), y=pct/100)) + geom_col(fill = "firebrick1", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x= "", y= "Percent of Complaints", title= "Claims Sent to Company")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="overall_time_to_respond.png", type = "cairo-png", width = 20, height =12)


count <- con %>% count(company) %>%  arrange(-n) 

plot2 <- con %>% 
  group_by(company) %>%
  summarise(mean = mean(diff_date)) %>% 
  arrange(mean) %>% 
  left_join(count) %>% 
  arrange(-n) %>% 
  mutate(mean = as.numeric((round(mean,0)*-1)))

plot2 %>% na.omit() %>% 
  filter(n >1000 & mean <10) %>%  
  ggplot(., aes(x=mean, y=n)) + 
  geom_point() + 
  geom_smooth(method = lm) +  
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) +
  labs(x= "Number of Days", y= "Total Number of Complaints", title="Do Larger Companies Get Complaints Faster?") +
  geom_segment(aes(x = 6, y = 60000, xend = 6.95, yend = 67000), arrow = arrow(length = unit(.5, "cm"))) + annotate("text", x = 5.75, y = 59000, label = "Bank of America")

ggsave(file="bofa_outlier.png", type = "cairo-png", width = 20, height =12)

plot3 <- con %>% 
  filter(company == "BANK OF AMERICA, NATIONAL ASSOCIATION") %>% 
  group_by(product) %>% 
  summarise(mean = mean(diff_date)) %>% 
  mutate(mean = as.numeric((round(mean,0)*-1))) %>% 
  arrange(-mean) %>% head(10)

ggplot(plot3, aes(x=reorder(product, mean), y=mean)) + geom_col(fill = "firebrick1", color = "black") + 
  labs(x= "", y= "Number of Days", title= "Which Complaints Take the Longest?")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + coord_flip() + 
  geom_hline(yintercept = mean((con$diff_date*-1), na.rm= TRUE), linetype = "longdash")

ggsave(file="bofa_by_product.png", type = "cairo-png", width = 20, height =12)

## Making a Joyplot

plot4 <- con %>% 
  filter(company == "BANK OF AMERICA, NATIONAL ASSOCIATION") %>% 
  group_by(product) %>% 
  count(diff_date)


plot4 <-   plot4 %>% filter(product == "Mortgage" | product == "Consumer Loan" | product == "Credit reporting" | product == "Prepaid card" | product == "Payday Loan" | product == "Student loan" | product == "Bank account or service" | product == "Credit card" | product == "Debt collection" | product == "Other financial service") 

ggplot(plot4,aes(x=(diff_date*-1), y=product, group=product,  height=..density.., fill = product)) +
  geom_joy(scale=4, alpha =0.6) +
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_continuous(expand=c(0, 0), limits = c(0,400)) + 
  labs(x= "Days to Transfer Claim", y= "Type of Product", title= "Which Complaints Take the Longest?")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.position="none")

ggsave(file="bofa_joyplot.png", type = "cairo-png", width = 20, height =12)


plot5 <- con %>% 
  filter(company != "BANK OF AMERICA, NATIONAL ASSOCIATION") %>%
  group_by(product) %>% 
  summarise(Overall = mean(diff_date, na.rm=TRUE))


plot6 <- con %>% 
  filter(company == "BANK OF AMERICA, NATIONAL ASSOCIATION") %>% 
  group_by(product) %>% 
  summarise(BofA = mean(diff_date, na.rm=TRUE)) %>% left_join(plot5) %>% 
  mutate(BofA = as.numeric((round(BofA,0)*-1))) %>% 
  mutate(Overall = as.numeric((round(Overall,0)*-1))) 

gg <- ggplot(data = plot6, aes(y=product, x=BofA, xend= Overall)) + 
  geom_dumbbell(colour = "#686868", colour_x = "purple1", colour_xend = "firebrick1", size_x = 3, size_xend = 3, size = 1)

gg <- gg + labs(x="Days Between Complaint Received and Passed On", y=NULL,
                title="Bank of America's Differences from the Rest",
                caption="Data from CFPB")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.title.x=element_text(hjust=1, face="italic", margin=margin(t=-24)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=24)))

gg <- gg + geom_text(data=filter(plot6, product=="Mortgage"),
                     aes(x=BofA, y=product, label="Bank of America"),
                     color="purple1", size=3, vjust=-2, fontface="bold", family="Calibri")

gg <- gg + geom_text(data=filter(plot6, product=="Mortgage"),
                     aes(x=Overall, y=product, label="All Others"),
                     color="firebrick1", size=3, vjust=-2, fontface="bold", family="Calibri")

gg <- gg +  theme(text=element_text(size=18, family="KerkisSans"))

gg

ggsave(file="barbell_plot.png", type = "cairo-png", width = 20, height =12)


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



## Making a Map

st <- rgdal::readOGR("state.json", "OGRGeoJSON")
states <- read_csv("C:/Users/Ryan Burge/Desktop/states.csv")

state <- con %>% group_by(state) %>% count() %>% filter(n >500) %>% as.data.frame()
state <- state %>% filter(state != "PR") %>% filter(state != "DC")
state <- state %>% select(NAME, n)
#plot <- plot %>% mutate(mean = round(mean*100,2))

bind <- bind_cols(state, states) 
bind$NAME <- as_factor(bind$state)

pop2016 <- read_csv("C:/Users/Ryan Burge/Desktop/pop2016.csv") %>% select(NAME, pop2016)

pop <- left_join(bind, pop2016) %>% mutate(pct = n/pop2016)
 
map <- left_join(st, pop)

pal <- colorNumeric("viridis", NULL)

myLabelFormat = function(..., reverse_order = FALSE){ 
  if(reverse_order){ 
    function(type = "numeric", cuts){ 
      cuts <- sort(cuts, decreasing = T)
    } 
  }else{
    labelFormat(...)
  }
}

leaflet(map) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, weight = 1, smoothFactor = 0.3, fillOpacity = 1, color = "black",
              fillColor = ~pal(map$pct), 
              label = ~paste0(map$pct, ""))%>%
  addLegend(pal = pal, values = rev(map$pct), opacity = 1.0, labFormat = myLabelFormat(reverse_order = F)) %>% 
  setView(-98.690940, 39.651426, zoom = 4)



table1 <- con %>% 
  filter(company == "BANK OF AMERICA, NATIONAL ASSOCIATION") %>% 
  tabyl(product) %>% mutate(pct =percent*100) %>% select(-n, -percent)


t1 <- data.frame("Everything Else", 6.05)
names(t1) <- c("product", "pct")
t2 <- rbind(table1, t1)


t2 %>% filter(pct >5) %>% ggplot(., aes(x=reorder(product, -pct), y= pct)) + geom_col() 

