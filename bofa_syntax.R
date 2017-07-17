## Bank of America Outlier Graph

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

## Bank of America by Product 

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


## Bank of America Barbell Plot 

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

gg <- gg +  theme(text=element_text(size=22, family="KerkisSans"))

gg

ggsave(file="barbell_plot.png", type = "cairo-png", width = 20, height =12)

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
  labs(x= "Days to Transfer Claim", y= "Type of Product", title= "Distribution of Processing Time for Bank of America")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.position="none")

ggsave(file="bofa_joyplot.png", type = "cairo-png", width = 24, height =12)


table1 <- con %>% 
  filter(company == "BANK OF AMERICA, NATIONAL ASSOCIATION") %>% 
  tabyl(product) %>% mutate(pct =percent*100) %>% select(-n, -percent)


t1 <- data.frame("Everything Else", 6.05)
names(t1) <- c("product", "pct")
t2 <- rbind(table1, t1)


t2 %>% filter(pct >5) %>% ggplot(., aes(x=reorder(product, -pct), y= pct/100)) + geom_col(fill = "firebrick1", color = "black") + 
  labs(x= "Product", y= "Percent of Complaints", title= "What Products Receive the Most Complaints?")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + 
  scale_y_continuous(labels = scales::percent)

ggsave(file="bofa_most_complaints_product.png", type = "cairo-png", width = 24, height =12)



con %>% 
  group_by(company, product) %>% 
  count() %>% mutate(lower = tolower(company), capital= stri_trans_totitle(lower)) %>% select(-company, -product)

t3 <- con %>% 
  group_by(company, product) %>% 
  count() %>% arrange(-n) %>% 
  mutate(lower = tolower(company), capital= stri_trans_totitle(lower), new = paste(capital, product, sep = ", ")) %>%
  head(5)

t3 %>%  ggplot(., aes(x=reorder(new, n), y= n)) + geom_col(fill = "firebrick1", color = "black") + 
  labs(x= "", y= "Number of Complaints", title= "What Products Receive the Most Complaints?")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=22, family="KerkisSans")) + coord_flip()

ggsave(file="top_five_complaint.png", type = "cairo-png", width = 24, height =12)

 

con %>% 
  filter(company == "EQUIFAX, INC." & product == "Credit reporting") %>% 
  summarise(mean = mean(diff_date, na.rm = TRUE))
## 3.66 days

con %>% 
  filter(company == "EXPERIAN DELAWARE GP" & product == "Credit reporting") %>% 
  summarise(mean = mean(diff_date, na.rm = TRUE))

## 4.13 days

con %>% 
  filter(company == "BANK OF AMERICA, NATIONAL ASSOCIATION" & product == "Mortgage") %>% 
  summarise(mean = mean(diff_date, na.rm = TRUE))

## 10.3

con %>% 
  filter(company == "TRANSUNION INTERMEDIATE HOLDINGS, INC." & product == "Credit reporting") %>% 
  summarise(mean = mean(diff_date, na.rm = TRUE))

## 4.15 days

con %>% 
  filter(company == "WELLS FARGO BANK, NATIONAL ASSOCIATION" & product == "Mortgage") %>% 
  summarise(mean = mean(diff_date, na.rm = TRUE))

## 3.03 days


product <- t3$new
days <- c(3.66, 4.13, 10.3, 4.15, 3.03)
t4 <- data.frame(product, days)

t4$product <- factor(t4$product, levels=unique(t4$product))


ggplot(t4, aes(x=product, y=days)) + geom_col(fill = "firebrick1", color = "black") + 
  labs(x= "Company and Product", y= "Median Time to Process", title= "Bank of America, Mortgage is an Outlier")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + coord_flip()

ggsave(file="top_five_process.png", type = "cairo-png", width = 24, height =12)

con$year <- format(as.Date(con$date_received, format="%m/%d/%Y"),"%Y")

year <- con %>% 
  filter(company == "BANK OF AMERICA, NATIONAL ASSOCIATION" & product == "Mortgage") %>% 
  group_by(year) %>%  summarise(mean = mean(diff_date, na.rm = TRUE)) %>% 
  mutate(mean = as.numeric((round(mean,0)*-1)))

ggplot(year, aes(x=year, y=mean)) + geom_col(fill = "firebrick1", color = "black") + 
  labs(x= "Year", y= "Median Time to Process (Days)", title= "2013 is the Root Cause")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans"))

ggsave(file="bofa_year.png", type = "cairo-png", width = 24, height =12)



joyplot <- con %>% 
  filter(company == "BANK OF AMERICA, NATIONAL ASSOCIATION" & product == "Mortgage" ) %>% 
  group_by(year) %>% 
  count(diff_date)


ggplot(joyplot,aes(x=(diff_date*-1), y=year, group=year,  height=..density.., fill = year)) +
  geom_joy(scale=4, alpha =0.6) +
  scale_y_discrete(expand=c(0.01, 0)) +
  scale_x_continuous(expand=c(0, 0), limits = c(0,400)) + 
  labs(x= "Days to Transfer Claim", y= "Year", title= "Distribution of Processing Time for Mortgages at Bank of America")  +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(size=28, family="KerkisSans")) + theme(legend.position="none")

ggsave(file="joyplot_year.png", type = "cairo-png", width = 24, height =12)
