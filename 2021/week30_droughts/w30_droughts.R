library(tidyverse)
library(geofacet)
library(extrafont)

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

#Extract only the year and month
drought$date = str_sub(drought$valid_start,1,7) 

#Collapse average for year and month, per state
#Percent of people who live in drought currently
drought_pop <- drought %>%
  group_by(state_abb, drought_lvl, date) %>%
  summarise_at(vars(pop_pct), mean, na.rm=TRUE)
drought_now <- drought_pop %>% filter(date=="2021-07")
drought_now <- drought_now %>% filter(drought_lvl!="None")

#Calculating totals for labels
totals[, 4][drought_now[, 4] == 0] <- NA

totals <- drought_now %>%
  na.omit(drought_now) %>%
  group_by(state_abb) %>%
  summarise(pop_sum = sum(pop_pct)) %>%
  ungroup() 

totals$pop_sum <- round(totals$pop_sum, digits = 0)
totals$pop_sum[totals$pop_sum == 101] <- 100
totals$drought_lvl <- "D0"
totals$pop_sum <- paste(totals$pop_sum, "%", sep="")

drought_plot <- merge(drought_now, totals, by = c("state_abb","drought_lvl"), all.x = TRUE)
drought_plot[, 4][drought_plot[, 4] == 0] <- NA

#Plot (exported and finished outside of R)
plot <- drought_plot %>% filter(state_abb!="PR") %>%
  ggplot(aes(x=1, y=1)) +
  geom_point(aes(size=pop_pct, color=drought_lvl), shape=21, stroke=1.5, alpha=0.7, na.rm=TRUE) +
  geom_text(aes(label=pop_sum, x=-Inf, y=Inf, hjust=0, vjust=1, size=4), na.rm=TRUE) +
  scale_colour_manual(
    values = c("#F2C849", "#F2921D", "#D95204", "#731702", "#2C0D0D"), 
    labels=c("Abnormally dry", "Moderate", "Severe", "Extreme", "Exceptional")) +
  scale_size(range = c(0, 15)) +
  facet_geo(~ state_abb, grid = "us_state_grid2") +
  theme_void() +
  labs (
    title="Percentage of U.S. States Living in Drought", 
    caption="Data from the U.S. Drought Monitor \n Graphic by Ilena Peng for #TidyTuesday",
    col="Drought severity", size="% of Population") +
  theme(
    text=element_text(family="Roboto", color="black", size=12),
    plot.title=element_text(face="bold"),
    strip.text.x = element_text(angle = 0, hjust = 0, face="bold"),
    legend.position="top",
    plot.margin = unit(c(1,1,1,1), "cm"))
print(plot)
ggsave("w30_droughts.png", width=10, height=9, unit="in")