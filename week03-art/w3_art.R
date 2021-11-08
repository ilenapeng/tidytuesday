library(tidyverse)
library(extrafont)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
view(artwork)

art <- artwork %>% drop_na(acquisitionYear, height, width)
range(art$acquisitionYear)

#Inspired by James Davenport https://ifweassume.blogspot.com/2013/11/the-dimensions-of-art.html, Os Keyes https://ironholds.org/art-as-data-as-art/ and Joseph Lewis https://josephlewis.github.io/aspect.html#aspect-ratio-through-time

plot <- art %>% filter(artist=="Turner, Joseph Mallord William") %>%
  ggplot(aes(x=0, y=0, color=acquisitionYear)) +
  geom_tile(aes(width=width, height=height), alpha=0, lwd = 0.5, linetype=6) +
  scale_color_gradient(low="#FFA686", high="#7972A0") +
  labs(
    title="Dimensions of Joseph Mallord William Turner's Art",
    subtitle="A depiction of Tate Art Museum's 39,389-piece collection of the English Romantic painter's works",
    caption="2014 data from the Tate Art Museum | Graphic by Ilena Peng for #TidyTuesday",
    color="Year acquired"
  ) +
  theme_minimal() +
  theme(
    plot.background=element_rect(fill="#FED0AE"),
    plot.margin=margin(1,1,1,1, unit="cm"),
    panel.grid=element_blank(),
    axis.text=element_blank(),
    axis.title=element_blank(),
    legend.position="top",
    plot.title=element_text(family="Georgia", hjust=0.5, size=14),
    plot.subtitle=element_text(family="Raleway", hjust=0.5, size=10),
    plot.caption=element_text(family="Raleway", hjust=0.5),
    legend.title=element_text(family="Raleway", size=10),
    legend.text=element_text(family="Raleway", size=10))

print(plot)
ggsave("week3_art.png", width=9, height=6, unit="in")
