install.packages("cowplot")
install.packages("magick")

library(tidyverse)
library(extrafont)
library(statebins)
library(cowplot)
library(magick)

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

#Preparing data
post_offices$state <-  str_replace(post_offices$state, "VAy", "VA")
post_offices$state <-  str_replace(post_offices$state, "MI/OH", "OH")

StStampIndex <- post_offices %>%
  group_by(state) %>%
  summarise_at(vars(stamp_index), mean, na.rm=TRUE)

view(StStampIndex)

#Stamp image
img <- "https://i.dlpng.com/static/png/13715_preview.png" %>% image_read() %>% image_rotate(degrees=90) %>% image_colorize(50, "white")

#limits=c(2, 6), breaks=c(2,4,6), labels=c(6,4,2)) +

#Making plot
plot <- ggplot(StStampIndex) +
  geom_statebins(aes(state=state, fill=stamp_index),border_col = "#FBFBFB") +
  coord_equal() +
  scale_fill_gradient(low = "#85C7F2", high = "#111D4A", breaks=c(2,4,6)) +
  labs(
    title="Average Scarcity of Post Office Postmarks", 
    subtitle="Scarcity measured by Richard Helbock's Stamp Scarcity Index",
    caption="Data from Cameron Blevins and Richard W. Helbock via Harvard Dataverse. \n Plot by @ilenapeng for #TidyTuesday") +
  theme(
    text=element_text(family="Raleway", face="bold.italic"),
    plot.title=element_text(size=20, hjust=0.5, color="#47638C"), 
    plot.subtitle=element_text(hjust=0.5),
    plot.caption=element_text(size=9),
    legend.title=element_blank(), 
    legend.position="right",
    legend.background=element_blank(),
    plot.margin = margin(40,30,30,30), 
    panel.background=element_blank(), 
    plot.background=element_blank(), 
    panel.grid=element_blank(),
    axis.text=element_blank(), 
    axis.ticks=element_blank())

print(plot)

ggdraw() + 
  draw_image(img, scale=1.1) + 
  draw_plot(plot)

ggsave("w16_postoffice.png", width = 9, height = 6, units = "in")
ggsave("w16_postoffice2.png", width = 9, height = 12, units = "in")