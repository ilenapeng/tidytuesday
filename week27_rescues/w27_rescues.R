library(tidyverse)
library(extrafont)
library(waffle)

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

#Slight changes to groups 
animal_rescues$animal_group_parent <- str_replace(animal_rescues$animal_group_parent,"Budgie","Bird")
animal_rescues$animal_group_parent <- str_replace(animal_rescues$animal_group_parent,"Pigeon","Bird")
animal_rescues$animal_group_parent <- str_replace(animal_rescues$animal_group_parent,"cat","Cat")

#Top 5 groups, adding percentages
rescue_count <- animal_rescues %>% 
  group_by() %>% 
  count(animal_group_parent) %>% 
  arrange(desc(n)) %>% 
  slice(1:5)
rescue_count$pct <- (rescue_count$n/(sum(rescue_count$n)) )*100
#Rearranging so values go from low to high - needs to be in this order for highest % to show up at top of pictogram
rescue_count <- rescue_count %>% arrange(n)
view(rescue_count)

#Pictogram
plot <- rescue_count %>% 
  ggplot(aes(label = reorder(animal_group_parent, -n), values = n)) +
  geom_pictogram(n_rows = 10, aes(colour = reorder(animal_group_parent, -n)), flip = TRUE, make_proportional = TRUE) +
  scale_color_manual(
    name = "Animal rescues (%, real values)",
    values = c("#26A96C", "#73FBD3", "#FDCA40", "#EF3E36", "#FF7700"),
    labels = c("Cat (52.8%, 3666)", "Bird (22.1%, 1536)", "Dog (17.2%, 1194)", "Fox (5.03%, 349)", "Horse (2.78%, 193)")
  ) +
  scale_label_pictogram(
    name = "Animal rescues (%, real values)",
    values = c("paw", "feather-alt", "bone", "burn", "carrot"),
    labels = c("Cat (52.8%, 3666)", "Bird (22.1%, 1536)", "Dog (17.2%, 1194)", "Fox (5.03%, 349)", "Horse (2.78%, 193)")
  ) +
  coord_equal() +
  labs(
    title="Animal Rescues by the London Fire Brigade, 2009-2020",
    caption="Data from London.gov via Data is Plural and Georgios Karamanis\nGraphic by Ilena Peng for #TidyTuesday"
  ) +
  theme_enhance_waffle() +
  theme_void() +
  theme(
      text=element_text(family="Arial Rounded MT Bold"),
      plot.title=element_text(size=15),
      plot.margin=margin(1,1,1,1, unit="cm"),
      legend.key.height = unit(2.5, "line"),
      legend.position="left",
      legend.text = element_text(hjust = 0, vjust = 0.75)) 

print(plot)
#Exported from plot zoom because dimensions were not working with me
ggsave("w27_rescues.png")