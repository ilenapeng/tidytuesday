library(tidyverse)
library(extrafont)

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
netflix <- netflix_titles[c(1,2,3,7,8,9)]
netflix <- filter(netflix, type=="Movie")
netflix$added_year <- str_sub(netflix$date_added, start= -4)
netflix$added_year <- as.numeric(netflix$added_year)
netflix$release_year <- as.numeric(netflix$release_year)
netflix$yeardiff <- (netflix$added_year - netflix$release_year)

#substituting ratings that are equivalent
netflix$newrating <- netflix$rating
netflix$newrating <-  str_replace(netflix$newrating, "TV-G", "G")
netflix$newrating <-  str_replace(netflix$newrating, "TV-PG", "PG")
netflix$newrating <-  str_replace(netflix$newrating, "TV-Y7-FV", "TV-Y7")

netflixaggreg <- aggregate(netflix[, 8], list(netflix$newrating), mean)
netflixaggreg <- netflixaggreg %>% arrange(Group.1) %>% mutate(Group.1 = factor(Group.1, levels=c("UR","NR","NC-17","TV-MA","R","TV-14","PG-13","PG","G","TV-Y7","TV-Y")))

p <- ggplot(netflixaggreg, aes(x=Group.1, y=yeardiff)) +
  geom_point(color="#E50914",size=8) + 
  geom_segment(aes(x=Group.1, xend=Group.1, y=0, yend=yeardiff),color="#f5f5f1") +
  coord_flip()+
  labs(
    title="From movie screen to computer screen",
    subtitle="Average years between movie release date and date added to Netflix,\n by TV Parental Guidelines and Motion Picture Association ratings",
    caption="Data from Shivam Bansal via Kaggle. \n Plot by @ilenapeng for #TidyTuesday"
  ) +
  ylab("Years")+
  theme(
    text=element_text(family="Bebas"),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text=element_text(color="#f5f5f1"),
    axis.title.y=element_blank(),
    axis.title.x=element_text(color="#f5f5f1"),
    plot.title=element_text(color="#E50914",size=30),
    plot.subtitle=element_text(color="#f5f5f1", family="Roboto"),
    plot.caption=element_text(color="#f5f5f1", family="Roboto",size=8),
    panel.background = element_rect(fill="#292f33"),
    plot.background = element_rect(fill="#292f33"),
    panel.grid.minor=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x = element_line(color="#383838"),
    plot.margin	= margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")
  )

print(p)

ggsave("w17_netflix.png", width = 8, height = 6, units = "in")

