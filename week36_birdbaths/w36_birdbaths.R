library(tidyverse)
library(ggtext)
library(extrafont)

bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')
view(bird_baths)

bbaths <- bird_baths %>% group_by(urban_rural, bioregions) %>% summarise(bird_count = sum(bird_count)) 
bbaths <- bbaths[!is.na(bbaths$urban_rural),]

plot <- ggplot(data = bbaths, 
    mapping = aes(
    x = bioregions, 
    y = ifelse(urban_rural == "Rural",  yes = -bird_count, no = bird_count), 
    fill = urban_rural,
    label= bird_count)) +
  geom_bar(stat = "identity") +
  geom_text(hjust=ifelse(bbaths$urban_rural == "Rural",  yes = 1.1, no = -0.1), size=5, colour="black", family="HK Grotesk") +
# this centers the chart: scale_y_continuous(labels = abs, limits = max(bbaths$bird_count) * c(-1,1) * 1.1) +
  scale_y_continuous(labels = abs, limits=c(-600,1620)) +
  scale_fill_manual(values=as.vector(c("#9CE37D", "#B07156"))) +
  coord_flip() + 
  labs(
    title="Number of Birds at Australian <b><span style = 'color:#B07156;'>Urban</span></b> and <b><span style = 'color:#9CE37D;'>Rural</span></b> Bird Baths",
    subtitle="",
    caption="Data from Australian Summer 2014 and Winter 2015 via Avian Assemblages at Bird Baths (Clearly et al., 2016) \n Graphic by Ilena Peng for #TidyTuesday"
  ) +
  theme_minimal() +
  theme(
    text=element_text(family="HK Grotesk"),
    plot.margin=unit(c(1,1,1,1), "cm"),
    axis.title=element_blank(),
    axis.text=element_text(color="black", size=12),
    legend.position="none",
    plot.title=element_markdown(size=16),
    plot.caption=element_text(size=8)
  )

print(plot)
ggsave("w36_birdbaths.png", width=9, height=6, unit="in")

