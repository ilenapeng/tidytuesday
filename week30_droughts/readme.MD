<h1>Mapping the percentage of U.S. states in a drought</h1>
After taking a few weeks off from participating in <a href="https://github.com/rfordatascience/tidytuesday">#TidyTuesday</a>, I found some time last week to make this proportional area cartogram heatmap of the percentage of each U.S. state living in a drought, as of July 2021.

<p align="center">
<img src="https://github.com/ilenapeng/tidytuesday/blob/main/week30_droughts/w30_droughts.png?raw=true" width="600" alt="Map of percentage of U.S. population in a drought">
</p>

<h2>How to read the chart</h2>

The size of each circle corresponds to the percentage of people in that state living in a particular severity level of a drought, which is indicated by the circle’s color.

So in Arizona, we can see that only 0.75% of the population is living in the lowest drought level, “Abnormally dry” and that the greatest share of the population — 48% — is living in a severe drought.

The number above each state represents the total percentage of that state living in a drought (all those circles added up together). So 100% of Arizona’s population is living in some form of a drought.

<p align="center">
<img src="https://user-images.githubusercontent.com/30391956/131103994-17b31992-aa08-462f-8517-f15ef1e69b76.png" width="400" style="text-align: center" alt="Diagram showing how to read nested circle chart on U.S. droughts">
</p>

<h2>How I made the chart</h2>

I first collapsed the data to get the average percentage of people who lived in each drought level in each state. I then filtered to look specifically at the most recent month, July 2021, and calculated the total percentage of each state’s population that was in a drought. 

I merged the sums into the rest of the data using the shared state variable, but since there were several rows for each state corresponding to the different levels of drought, I merged thee sums into the row that corresponded with the lowest level of drought. It doesn’t really matter which row, but picking one prevents from stacking four blocks of “100%” text on top of each other.

And here’s the plot itself. The circles are scaled based on the population percentage, labels were added using geom_text() – and all of it, of course, was faceted using the <a href="https://cran.r-project.org/web/packages/statebins/index.html">statebins</a> package. I also added the legends and annotations in Adobe Photoshop.

My major struggle with this chart was trying to make nested circles in R. It had me going in circles, so I ended up opting for manually moving them all in Adobe Photoshop later. Time-consuming, yes, but somehow still much faster than making it work in R – please let me know if you have any suggestions or solutions! I love proportional area charts and would greatly appreciate an easier way to make charts like this in the future.

```
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
ggsave("w30_droughts_raw2.png", width=10, height=9, unit="in")
```
