library(tidyverse)
library(geofacet)
library(extrafont)
library(svglite)

# TidyTuesday data
drought_tidytues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')

# filter to just April 2022
drought_22 <- drought_tidytues %>% filter(DATE == 'd_20220401')

# keep only wet conditions
df_wet = drought_22[c("state","W0", "W1", "W2", "W3", "W4")]

# find the percent of state that is in some sort of drought - so the max value
df_wet <- df_wet %>%
  mutate(max_wet = case_when(
    # if % of state at W0 is bigger than at W1, then assign that percent to our max_drought
    df_wet$W0 > df_wet$W1 ~ paste0(df_wet$W0, '%'),
    # if W0 is less than W1, then see if W2 is bigger than W1
    df_wet$W1 > df_wet$W2 ~ paste0(df_wet$W1, '%'),
    # if W2 is less than W1, then see if W3 is bigger than W2
    df_wet$W2 > df_wet$W3 ~ paste0(df_wet$W2, '%'),
    # if W3 is less than W2, then see if W4 is bigger than W3
    df_wet$W3 > df_wet$W4 ~ paste0(df_wet$W3, '%'),
    # else
    TRUE ~ '0%'
  ))

# find the highest level of drought and the percent of the state experiencing that level of drought
# df_wet <- df_wet %>%
#   mutate(most_severe = case_when(
#                               # if W4 is greater than 0, use W4 label
#                               df_wet$W4 > 0 ~ paste0('W4 - ', df_wet$W4, '%'),
#                               # if W4 is 0, then see if W3 is greater than 0. if yes, use W3 label
#                               df_wet$W3 > 0 ~ paste0('W3 - ', df_wet$W3, '%'),
#                               # and so on
#                               df_wet$W2 > 0 ~ paste0('W2 - ', df_wet$W2, '%'),
#                               df_wet$W1 > 0 ~ paste0('W1 - ', df_wet$W1, '%'),
#                               df_wet$W0 > 0 ~ paste0('W0 - ', df_wet$W0, '%'),
#                               # not adding an else statement here because we don't need a duplicate "No drought level" label
#                               ))

# clean state names
# first replace dashes
df_wet$state <- str_replace_all(df_wet$state, '-', ' ')
# make title case
df_wet$state <- str_to_title(df_wet$state)

# reshape data for plot
# gather(df, new column name, new value name, columns to combine)
df_wet_long <- gather(df_wet, wet_condition, pct_wet, W0:W4)

# This creates multiple versions of labels that will overlap - use check_overlap=TRUE to only have one show up

# retrieve state abbreviations
# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/state
# then make column with state abbreviations
df_wet_long$state_abb <- state.abb[match(df_wet_long$state,state.name)]

# replace NA with 0 for plotting
df_wet_long$pct_wet <- replace(df_wet_long$pct_wet, df_wet_long$pct_wet == 0, NA)

# plot (exported and finished outside of R)
plot <- df_wet_long %>%
  ggplot(aes(x=1, y=1, label=max_wet)) +
  # change shape to 15 to make a square chart
  geom_point(aes(size=pct_wet, color=wet_condition), shape=21, stroke=1.5, alpha=0.7, na.rm=TRUE) +
  # Setting y to Inf will place it right below the state abbv
  geom_text(x=-Inf, y=1.045, check_overlap=TRUE, hjust=0, vjust=1, size=4) +  
  scale_color_manual(
    values = c("#ade8f4", "#48cae4", "#0096c7", "#023e8a", "#03045e"), 
    labels=c("Abnormally wet", "Moderate", "Severe", "Extreme", "Exceptional")) +
  scale_size(range = c(0, 15)) +
  facet_geo(~ state_abb, grid = "us_state_grid1") +
  theme_void() +
  labs (
    title="U.S. Wet Conditions", 
    subtitle="Labels show the percent of each state that is experiencing unusually wet weather",
    caption="\nApril 2022, data from the U.S. Drought Monitor | Graphic by Ilena Peng for #TidyTuesday",
  ) +
  theme(
    text=element_text(family="Roboto", color="black", size=12),
    plot.title=element_text(face="bold", size=16),
    plot.subtitle=element_text(size=14),
    plot.caption=element_text(hjust=0.5, size=10),
    strip.text.x = element_text(angle = 0, hjust = 0, face="bold"),
    legend.position="top",
    plot.margin = unit(c(1,1,1,1), "cm"))
print(plot)
ggsave("w24_droughts_wet_raw.svg", width=10, height=9, unit="in")