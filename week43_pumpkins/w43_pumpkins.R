library(tidyverse)
library(extrafont)
library(GGally)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')
view(pumpkins)

#separate year and type into two different columns
pumpkins <- pumpkins %>% separate(id, c('year', 'type'))
#filter to only first place
first <- pumpkins %>% filter(place=="1")

df <- first[c(1,2,4)]
#Remove commas in order to convert to numeric
df$weight_lbs <- str_replace(df$weight_lbs, ",", "")
df$weight_lbs <- as.numeric(df$weight_lbs)
df$year <- as.numeric(df$year)
#rename types
df$type <- str_replace(df$type, "P", "Giant Pumpkin")
df$type <- str_replace(df$type, "F", "Field Pumpkin")
df$type <- str_replace(df$type, "S", "Giant Squash")
df$type <- str_replace(df$type, "W", "Giant Watermelon")
df$type <- str_replace(df$type, "L", "Long Gourd")
df$type <- str_replace(df$type, "T", "Tomato")
#remove gourd bc it's measured in lengh
df <- df %>% filter(df$type != "Long Gourd")

#max values for geom_point
df_max <- df %>% 
  group_by(type) %>% 
  mutate(max = max(weight_lbs)) %>% 
  ungroup() %>% 
  filter(weight_lbs==max)

view(df_max)

#end values for geom_text
df_end <- df %>% 
  filter(year==2021)

plot <- ggplot(df, aes(x=year, y=weight_lbs, group=type, color=type)) +
  scale_color_manual(values=c("#5E0B15", "#F95738", "#EDD83D", "#84DD63", "#BC8034")) +
  geom_line(size=1.2) +
  scale_x_continuous(limits = c(2013,2022), breaks=seq(2013,2022, by=1)) +
  #max point & label
  geom_point(data=df_max, aes(x=year, y=weight_lbs), size=16) +
  geom_text(data=df_max, aes(x=year, y=weight_lbs, label=round(weight_lbs, digits = 0)), color="white", family="Roboto", fontface = "bold") +
  #year label
  geom_label(aes(label = year, y = 1000), fill = "white", size = 3, color = "black", label.size = NA, family="Roboto", fontface = "bold") +
  #type label
  geom_text(data=df_end, aes(x=2021.3, y=weight_lbs, label=type), color="black", hjust=0, family="PT Serif") +
  labs(
    title="Heavyweight Produce Champions",
    subtitle="Weight in pounds of produce that won first prize, highlighted are the heaviest in each category from 2013 to 2022",
    caption="Data from the Great Pumpkin Commonwealth | Graphic by Ilena Peng for #TidyTuesday",
    y="Weight in pounds") +
  theme_minimal() +
  theme(
    text=element_text(family="Roboto"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=10, color="gray80"),
    axis.title=element_blank(),
    plot.title = element_text(family="PT Serif",face="bold", size=18),
    plot.subtitle = element_text(size=12),
    plot.margin=margin(1,1,1,1, "cm"),
    panel.grid.minor = element_blank(),
    legend.position = "none")
print(plot)

ggsave("w43_pumpkins.png", width = 12, height = 8, unit = "in")