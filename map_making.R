


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