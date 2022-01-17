library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(Rcpp)
library(sf)
library(tigris)
library(leaflet)

ca <- counties("California", cb = TRUE)
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

#Adding 0 to front of county id
broadband$`COUNTY ID` <- str_pad(broadband$`COUNTY ID`, 5, pad = "0")

#Merge data and shapefile
merge <- ca %>% full_join(broadband, by = c("GEOID" = "COUNTY ID"))

merge$`BROADBAND AVAILABILITY PER FCC` <- as.numeric(merge$`BROADBAND AVAILABILITY PER FCC`)

#The plot
plot <- merge %>%
  mutate(broadbandbinned = cut_interval(`BROADBAND AVAILABILITY PER FCC`, n = 5)) %>%
  ggplot() +
  geom_sf(aes(fill=broadbandbinned), color="white") +
  scale_fill_manual(values=c("#30545F","#34778D", "#59B1A5", "#89C592", "#cfe0c3"), 
                    labels=c("<20%","20-40%","40-60%","60-80%","80-100%"), na.translate=FALSE) +
  theme_void() +
# geom_sf_text(data=filter(merge,NAME=="Siskiyou" | NAME=="Alpine" | NAME=="Plumas" | NAME== "Sierra" | NAME== "Modoc"), aes(label=NAME), family="Helvetica") +  
  labs(
    title="Broadband availability in California", 
    subtitle=" Even in a state known for tech advancement, there are jarring disparities in broadband availability \n In 5 counties, less than 20% of the residents have access to broadband", 
    caption="Data from Microsoft via The Verge \n Graphic by Ilena Peng for #TidyTuesday") +
  theme(
    text=element_text(family="Helvetica"), 
    plot.title=element_text(face="bold"), 
    plot.subtitle=element_text(size=9), 
    plot.margin=margin(2,2,2,2, "cm"), 
    legend.title=element_blank())

print(plot)
ggsave("w20_broadband2.png", width=7, height=7, unit="in")



