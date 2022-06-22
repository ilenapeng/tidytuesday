library(tidyverse)
library(geofacet)
library(extrafont)
library(svglite)

# My data, downloaded from 6/14/22 from https://droughtmonitor.unl.edu/DmData/GISData.aspx

drought <- readr::read_csv('dm_export_state_2022-06-14.csv')

# keep only columns we need
df_dry = drought[c("State","D0", "D1", "D2", "D3", "D4")]

# find the percent of state that is in some sort of drought - so the max value
df_dry <- df_dry %>%
  mutate(max_drought = case_when(
    # if % of state at D0 is bigger than at D1, then assign that percent to our max_drought
    df_dry$D0 > df_dry$D1 ~ paste0(df_dry$D0, '%'),
    # if D0 is less than D1, then see if D2 is bigger than D1
    df_dry$D1 > df_dry$D2 ~ paste0(df_dry$D1, '%'),
    # if D2 is less than D1, then see if D3 is bigger than D2
    df_dry$D2 > df_dry$D3 ~ paste0(df_dry$D2, '%'),
    # if D3 is less than D2, then see if D4 is bigger than D3
    df_dry$D3 > df_dry$D4 ~ paste0(df_dry$D3, '%'),
    # else
    TRUE ~ '0%'
  ))

# find the highest level of drought and the percent of the state experiencing that level of drought
# df_dry <- df_dry %>%
#   mutate(most_severe = case_when(
#                               # if D4 is greater than 0, use D4 label
#                               df_dry$D4 > 0 ~ paste0('D4 - ', df_dry$D4, '%'),
#                               # if D4 is 0, then see if D3 is greater than 0. if yes, use D3 label
#                               df_dry$D3 > 0 ~ paste0('D3 - ', df_dry$D3, '%'),
#                               # and so on
#                               df_dry$D2 > 0 ~ paste0('D2 - ', df_dry$D2, '%'),
#                               df_dry$D1 > 0 ~ paste0('D1 - ', df_dry$D1, '%'),
#                               df_dry$D0 > 0 ~ paste0('D0 - ', df_dry$D0, '%'),
#                               # not adding an else statement here because we don't need a duplicate "No drought level" label
#                               ))

# reshape data for plot
# gather(df, new column name, new value name, columns to combine)
df_dry_long <- gather(df_dry, dry_condition, pct_dry, D0:D4, -max_drought)

view(df_dry_long)

# This creates multiple versions of labels that will overlap - use check_overlap=TRUE to only have one show up

# replace 0 with NA for plotting
df_dry_long$pct_dry <- replace(df_dry_long$pct_dry, df_dry_long$pct_dry == 0, NA)

# plot (exported and finished outside of R)
plot <- df_dry_long %>%
  ggplot(aes(x=1, y=1, label=max_drought)) +
  # change shape to 15 to make a square chart
  geom_point(aes(size=pct_dry, color=dry_condition), shape=21, stroke=1.5, alpha=0.7, na.rm=TRUE) +
  # Setting y to Inf will place it right below the state abbv
  geom_text(x=-Inf, y=1.045, check_overlap=TRUE, hjust=0, vjust=1, size=4) +
  scale_colour_manual(
    values = c("#F2C849", "#F2921D", "#D95204", "#731702", "#2C0D0D"), 
    labels=c("Abnormally dry", "Moderate", "Severe", "Extreme", "Exceptional")) +
  scale_size(range = c(0, 15)) +
  facet_geo(~ State, grid = "us_state_grid1") +
  theme_void() +
  labs (
    title="U.S. Drought Conditions", 
    subtitle="Labels show the percent of each state that is in some form of drought",
    caption="\nData as of June 14, 2022 from the U.S. Drought Monitor | Graphic by Ilena Peng for #TidyTuesday",
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
ggsave("w24_droughts_dry_raw.svg", width=10, height=9, unit="in")