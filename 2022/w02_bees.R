library(tidyverse)
library(extrafont)
library(ggstar)

stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# filter to mites only
mites_only <- stressor %>% filter(stressor == "Varroa mites")
# filter out other states and united states
mites_only <- mites_only %>% filter(state != "United States" & state != "Other States")

# add regions
# make dataframe of states and their regions
states_formerge <- data.frame(state.name, state.region)
colnames(states_formerge)[1] <- "state"
colnames(states_formerge)[2] <- "region"

df_for_plot <- merge(mites_only, states_formerge,by="state")

# group by region
df_for_plot <- df_for_plot %>% group_by(region, months, year) %>% summarise(stress_pct_avg = mean(stress_pct, na.rm=TRUE))

# turn months into numbers
df_for_plot <- df_for_plot %>%
  mutate(months_num = case_when(months == "January-March" ~ '1',
                                months == "April-June" ~ '2',
                                months == "July-September" ~ '3',
                                months == "October-December" ~ '4')) 
df_for_plot$months_num = as.numeric(df_for_plot$months_num)

# shift every other row to create a beehive
df_for_plot <- df_for_plot %>%
  mutate(months_num_shift = case_when(region == "West" ~ months_num + 0,
                                    region == "North Central" ~ months_num + 0.5,
                                    region == "South" ~ months_num + 0,
                                    region == "Northeast" ~ months_num + 0.5
                                )) 

# plot
plot <- ggplot(df_for_plot, aes(months_num_shift, region, fill=stress_pct_avg)) + 
  geom_star(starshape = "hexagon", size=5, color="#50372B") +
  scale_fill_gradient2(low="#FAF1AF", mid="#FFD94F", high="#FF831F", midpoint=25, na.value = "white", limits=c(0,60), breaks=seq(0,60, by=20)) +
  scale_x_continuous(breaks=c(1.25, 2.25, 3.25, 4.25), labels=c("Q1","Q2","Q3","Q4"), limits=c(0,5)) +
  scale_y_discrete(breaks=c("West", "North Central", "South", "Northeast"), labels=c("West", "Midwest", "South", "Northeast")) +
  coord_equal() +
  facet_grid(cols = vars(year)) +
  labs(
    title="The Bees Are Stressed",
    subtitle="Varroa mites, which are parasites that feed on honey bees, are a top stressor for bee colonies\nPercent of colonies stressed by Varroa mites over time, averaged by region",
    caption="\nData from USDA | Chart by Ilena Peng for #TidyTuesday"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color="#F5E9E0"),
    plot.title = element_text(hjust=0.5, family="Cooper Std Black", size=20, color="#50372B"),
    plot.subtitle = element_text(hjust=0.5, lineheight = 1.2, family="Avenir", color="#50372B", size=12),
    plot.caption = element_text(hjust=0.5, family="Avenir", color="#50372B", size=9),
    strip.text.x = element_text(family="Cooper Std Black", size=16, color="#50372B"),
    axis.title = element_blank(),
    axis.text.y = element_text(color="#50372B", family="Avenir", size=12),
    axis.text.x = element_text(color="#50372B", family="Avenir", size=9),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(color="#50372B", family="Avenir"),
    plot.margin = margin(1,1,1,1,"cm")
  )
  
print(plot)
ggsave("w02_bees.png", width=10, height=6, unit="in")