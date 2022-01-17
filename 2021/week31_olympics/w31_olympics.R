library(tidyverse)
library(extrafont)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

# replace -1 that was added onto countries
olympics$team <- gsub('[0-9]+', '', olympics$team)
olympics$team <- gsub('-', '', olympics$team)

# Mens vs womens figure skating medals
# figure_skating <- olympics %>% filter(sport == 'Figure Skating')
figure_skating_singles <- olympics %>% filter(event == "Figure Skating Men's Singles" | event == "Figure Skating Women's Singles")

# Combine Soviet Union and Russia
figure_skating_singles$team <- str_replace(figure_skating_singles$team, 'Soviet Union', 'Russia')

# count number of medals by country and men's or women's singles
medals_count <- figure_skating_singles %>% group_by(team, event) %>% count()

# create two separate columns for men's and women's
medals_count_pivot <- medals_count %>%
  pivot_wider(names_from = event, values_from = n)

# Replace NAs with 0, no medals
medals_count_pivot[is.na(medals_count_pivot)] <- 0

# Rename columns
medals_count_pivot <- medals_count_pivot %>% rename("Men" =  "Figure Skating Men's Singles", "Women" = "Figure Skating Women's Singles")

# Make a total column so we can sort by total number of medals
medals_count_pivot$total = medals_count_pivot$Men + medals_count_pivot$Women

# top 15
medals_count_pivot_top <- medals_count_pivot %>% 
  arrange(desc(total))
medals_count_pivot_top <- head(medals_count_pivot_top, 10)

# create new column with color
medals_count_pivot_top <- medals_count_pivot_top %>% 
mutate(
  col_color = case_when(
    Men > Women ~ "#F5F5FF",
    Women > Men ~ "#88CDFF"))

# reshape back to longer
medals_count_top <- medals_count_pivot_top %>%
  pivot_longer(cols = c('Men', 'Women'), names_to = "event", values_to = "count")

# make column to place labels with team names only for summer values
medals_count_top <- medals_count_top %>% mutate(men_count = replace(count, event=='Women', ''))
medals_count_top <- medals_count_top %>% mutate(women_count = replace(count, event=='Men', ''))
medals_count_top$men_count <- as.integer(medals_count_top$men_count)
medals_count_top$women_count <- as.integer(medals_count_top$women_count)

# slope chart
plot <- ggplot(medals_count_top, aes(group=team)) +
  geom_line(aes(x=event, y=count, alpha = 1, color=col_color), size = 1) +
  # Point
  geom_point(aes(x=event, y=count, color=col_color), size = 8, alpha=0.8) +
  # For the colors to change by whether Men's medals > Women
  scale_color_identity() +
  # Snowflake to go on top of the circle
  geom_point(aes(x=event, y=count), color="#374E6C", size = 5, shape = 8, alpha = 0.2) +
  # Count label
  geom_text(aes(x=event, y=count, label=count), size=3.5, color="black", family="Bebas", fontface = "bold") +
  # Team name label
  geom_text(aes(x=0.9, y=men_count, label=team), hjust=1, size=3, family="HK Grotesk", color="white") +
  scale_x_discrete(limits = c("Men", "Women"), position="top") +
  scale_y_continuous(breaks=seq(0, 70, by=10)) +
  labs(
    title="O l y m p i c  S i n g l e s  F i g u r e  S k a t i n g  M e d a l s",
    subtitle="Total medals from men's and women's singles figure skating",
    caption="Data from sports-reference.com via rgriffin/heesoo37 on Kaggle\nGraphic by Ilena Peng for #TidyTuesday"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#234D6C'),
    panel.grid = element_blank(),
    legend.position="none", 
    plot.title=element_text(hjust=0.5, color="white", family="Bebas"),
    plot.subtitle=element_text(hjust=0.5, color="#92AAC8", family="HK Grotesk"),
    plot.caption=element_text(hjust=0.5, color="#92AAC8", family="HK Grotesk"),
    axis.title=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(color="white", family="HK Grotesk"),
    plot.margin=margin(1,1,1,1, unit="cm"))

print(plot)
ggsave("w31_olympics.png", width=5, height=10, unit="in")