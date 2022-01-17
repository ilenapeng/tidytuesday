library(tidyverse)
library(extrafont)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# ongoing projects only
ongoing_df <- transit_cost %>% filter(transit_cost$end_year > 2021)

# look at number of projects in each country
projects_count <- ongoing_df %>% group_by(country) %>% count()

# filter to India only
india_df <- ongoing_df %>% filter(ongoing_df$country == "IN")

# new variable with city and line name
india_df$label <- paste(india_df$city, india_df$line, sep=", ")

# text colors
textcolors <- c('#F9F148','#F9F148','#F9F148','#F9F148','#F9F148','#F9F148',
                '#3BCEAC',
                '#F9F148','#F9F148',
                '#EBE3FF',
                '#C0F5FA', '#C0F5FA',
                '#F9F148','#F9F148',
                '#ABFF4F')

# plot
plot <- india_df %>%
  mutate(label = fct_reorder(label, desc(end_year))) %>%
  ggplot() + 
  geom_segment(aes(x=start_year, xend=end_year, y=label, yend=label), size=5, color="#CACECE", lineend='round') +
#  from the old version of this chart
#  scale_color_manual(values=c('#3BCEAC','#822DCD','#06BA63','#F02943','#F9F148')) +
  labs(
    title="Timelines of Ongoing Transit Projects in India",
    subtitle="Start and projected end years for public transit projects",
    caption="Data from the Transit Costs Project | Graphic by Ilena Peng for #TidyTuesday"
  ) +
  theme_minimal() +
  theme(
    plot.background=element_rect(fill="#666B6A"),
    panel.grid=element_line(color="#787D7C"),
    text=element_text(family="Orator Std", color="white"),
    plot.title=element_text(size=16),
    plot.subtitle=element_text(size=12),
    plot.caption=element_text(size=8),
    axis.title=element_blank(),
    axis.text.x=element_text(size=12, color="white"),
    axis.text.y=element_text(size=12, color=rev(textcolors)),
    plot.margin=margin(1,1,1,1,unit="cm")
  )

print(plot)
ggsave("w02_transitcosts.png", width=10, height=6, unit="in")

