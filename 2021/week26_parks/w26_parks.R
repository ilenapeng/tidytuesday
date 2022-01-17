library(tidyverse)
library(extrafont)

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')
view(parks)

#remove $ sign from front
parks$spend <- str_sub(parks$spend_per_resident_data, 2, -1)
parks$spend <- as.numeric(parks$spend)
#remove % sign from back
parks$pct_city <- str_sub(parks$park_pct_city_data, 1, -2)
parks$pct_city <- as.numeric(parks$pct_city)

parks$pct_near <- str_sub(parks$pct_near_park_data, 1, -2)
parks$pct_near <- as.numeric(parks$pct_near)

plot <- parks %>% filter(year=="2020") %>%
  ggplot(aes(x=med_park_size_data, y=pct_near)) +
  geom_point(alpha=0.7, size=3, color="#9DBF9E") +
  geom_smooth(method=lm , color="#003E1F", se=FALSE) +
  labs(
    title="Having smaller parks makes green space accessible to more residents",
    subtitle="Cities with small median park sizes had a higher percentage of residents who lived within a 10-minute walk to a park",
    caption="2020 data from The Trust for Public Land \n Graphic by Ilena Peng for #TidyTuesday",
    x="Median park size in acres",
    y="% of population living near a park"
  ) +
  theme_minimal() +
  theme(text=element_text(family="Helvetica"), 
        plot.title=element_text(face="bold", color="#003E1F"), 
        plot.caption=element_text(size=6),
        axis.text=element_text(color="black", size=10),
        axis.title=element_text(face="bold"))

print(plot)
ggsave("w26_parks.png")


