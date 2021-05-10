library(tidyverse)
library(extrafont)
library(RColorBrewer)
library(ggtext)

brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')

#Create total variable
brazil_loss$total=brazil_loss$commercial_crops + brazil_loss$flooding_due_to_dams + brazil_loss$natural_disturbances + brazil_loss$pasture + brazil_loss$selective_logging + brazil_loss$fire + brazil_loss$mining + brazil_loss$other_infrastructure + brazil_loss$roads + brazil_loss$tree_plantations_including_palm + brazil_loss$small_scale_clearing

#Create percent variables
brazil_loss$commercial_crops_pct = (brazil_loss$commercial_crops/brazil_loss$total)*100
brazil_loss$flooding_pct = (brazil_loss$flooding_due_to_dams/brazil_loss$total)*100
brazil_loss$natural_disturbances_pct = (brazil_loss$natural_disturbances/brazil_loss$total)*100
brazil_loss$pasture_pct = (brazil_loss$pasture/brazil_loss$total)*100
brazil_loss$logging_pct = (brazil_loss$selective_logging/brazil_loss$total)*100
brazil_loss$fire_pct = (brazil_loss$fire/brazil_loss$total)*100
brazil_loss$mining_pct = (brazil_loss$mining/brazil_loss$total)*100
brazil_loss$infrastructure_pct = (brazil_loss$other_infrastructure/brazil_loss$total)*100
brazil_loss$roads_pct = (brazil_loss$roads/brazil_loss$total)*100
brazil_loss$tree_plantations_pct = (brazil_loss$tree_plantations_including_palm/brazil_loss$total)*100
brazil_loss$clearing_pct = (brazil_loss$small_scale_clearing/brazil_loss$total)*100

view(brazil_loss)

#Chart
plot <- ggplot(brazil_loss, aes(x=year)) +
  xlim(2001, 2014) +
  geom_line(aes(y=commercial_crops_pct), color="#53B3CB", size=1.2) +
  geom_line(aes(y=clearing_pct), color="#CBFF4D", size=1.2) +
  geom_line(aes(y=pasture_pct), color="#109648", size=1.2) +
  geom_line(aes(y=logging_pct), color="#A2E3C4", size=1.2) +
  geom_line(aes(y=fire_pct), color="#9C6615",size=1.2) +
  geom_line(aes(y=flooding_pct), color="#7E8D85") +
  geom_line(aes(y=natural_disturbances_pct), color="#7E8D85") +
  geom_line(aes(y=mining_pct), color="#7E8D85") +
  geom_line(aes(y=infrastructure_pct), color="#7E8D85") +
  geom_line(aes(y=roads_pct), color="#7E8D85") +
  geom_line(aes(y=tree_plantations_pct), color="#7E8D85") +
#Labels
  labs (title="Causes of forest loss in Brazil from 2001-2013, by percent of total forest loss",
        subtitle="Variables that play a minor role in forest loss are pictured in gray:  mining, flooding due to dams, natural disturbances, tree plantations, infrastructure and roads",
        caption ="Data from Hannah Ritchie and Max Roser via Our World in Data. \n Graphic by Ilena Peng for #TidyTuesday")+
  ylab("Percent of total deforestation") +
#Theme
  theme(
    text=element_text(family="Lato"),
    axis.text=element_text(color="black", size=12),
    axis.title.x=element_blank(),
    axis.ticks=element_blank(),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_line(color="#DCDCDC"),
    plot.title=element_text(size=22, face="bold", color="#53B3CB"),
    plot.margin= margin(1, 1, 1, 1, "cm")
  ) +
#Annotations
  annotate("text", x = 2013.2, y = 52, label = "Pasture for livestock", hjust = 0, family="Lato") +
  annotate("text", x = 2013.2, y = 21, label = "Small-scale clearing", hjust = 0, family="Lato") +
  annotate("text", x = 2013.2, y = 10, label = "Selective logging", hjust = 0, family="Lato") +
  annotate("text", x = 2013.2, y = 2, label = "Fire loss", hjust = 0, family="Lato") +
  annotate("text", x = 2013.2, y = 6.5, label = "Commercial crops", hjust = 0, family="Lato")
    
print(plot)

ggsave("w15_forests.png", width = 15, height = 8, units = "in")

