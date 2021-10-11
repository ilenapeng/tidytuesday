library(tidyverse)
library(extrafont)

fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
view(fishing)

ustotal <- fishing %>% filter(region=="U.S. Total")
ustotal$values[is.na(ustotal$values)] <- 0
ustotal$species <-  str_replace(ustotal$species, "Cisco and Chub", "Cisco and Chubs")
ustotal$species <-  str_replace(ustotal$species, "Cisco and chubs", "Cisco and Chubs")

ustotalsum <- ustotal %>% 
  group_by(species) %>% 
  summarise(count = sum(values))

ustotalsum <- ustotalsum[order(-ustotalsum$count),]

top15fish <- c("Cisco", "Alewife",	"Cisco and Chubs", "Lake Whitefish",	"Chubs",	"Yellow Perch", "Blue Pike", "Carp", "Rainbow Smelt", "Freshwater Drum",	"Walleye",	"Suckers",	"Sauger",	"Walleye and Blue Pike")
top15 <- ustotal %>% filter(species %in% top15fish)

#dotplot
plot <- top15 %>%
  mutate(species = fct_rev (factor(species, levels=top15fish))) %>% 
  ggplot() + 
  geom_point(aes(x=year, y=species, size=values), color="#08203E", alpha=0.2) +
  scale_size(name="Number of fish (,000)", breaks=c(0,24000,48000), labels=c("0","24","48")) +
  scale_x_continuous(breaks=seq(1865, 2015, by = 15)) +
  labs(
    title="U.S. Commercial Fish Catch Data, 1867-2015",
    subtitle="Looking at production for the 15 fish species with the highest cumulative counts",
    caption="Data from the Great Lakes Fishery Commission | Graphic by Ilena Peng"
  ) +
  theme_minimal() +
  theme(
    plot.background=element_rect(fill="#557C93"),
    panel.grid = element_blank(),
    text=element_text(family="Raleway", color="white", size=14),
    legend.position="top",
    legend.title=element_text(size=12, face="bold"),
    legend.text=element_text(face="bold"),
    legend.key.width=unit(0.25,"in"),
    axis.title=element_blank(),
    axis.text=element_text(color="white"),
    plot.caption=element_text(hjust=0.5, size=8,color="#08203E"),
    plot.title=element_text(hjust=0.5, face="bold"),
    plot.subtitle=element_text(hjust=0.5, size=12),
    plot.margin=margin(1,1,1,1, unit="cm")
  )

print(plot)
ggsave("week24_fishing.png",width=10,height=9,unit="in")
