library(tidyverse)
library(extrafont)
library(taylor)

df <- taylor_album_songs

# df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
# view(df)

# Replace (Taylor's Version) text with * to be included in notes
df$album_name <- gsub(" (Taylor's Version)", "*", df$album_name, fixed=TRUE)

# Reorder axis
df$album_name_fct <- factor(df$album_name, levels=c('Taylor Swift', 'Fearless*', 'Speak Now*', 'Red*', '1989*', 'reputation', 'Lover', 'folklore', 'evermore', 'Midnights'))
# Level track numbers
df$track_number <- factor(df$track_number, levels=c(1:30))

# Convert milliseconds to minutes
df <- df %>% mutate(duration_min = (duration_ms/1000)/60)

# Plot
# Note -- are three NAs in the acousticness metrics from Spotify
plot <- df %>% ggplot(aes(fill=acousticness, x=duration_min, y=fct_rev(album_name_fct))) + 
  geom_bar(position="stack", stat="identity", color='white', size=1, width=0.7) +
  # colors from Lover https://asteves.github.io/tayloRswift/
  scale_fill_gradientn(colours = c('#b8396b', '#76bae0', '#D1C4DA', '#F2B6D7', '#ffd1d7', '#fff5cc')) +
  annotate("segment", x = 0, xend = 5, y = 10.7, yend = 10.7, linewidth=1) +
  labs(
    title="Taylor Swift",
    subtitle="Spotify acousticness scores across Taylor Swift's discography",
    caption="Note: Fearless, Speak Now, Red, and 1989 are Taylor's Version\nData from the taylor R package | Chart by Ilena Peng for #TidyTuesday"
  ) +
  theme_minimal() +
  theme(
    plot.background=element_rect(fill="white", color="white"),
    panel.grid = element_blank(),
    text=element_text(family="Avenir", size=16, color='black'),
    axis.title=element_blank(),
    axis.text.x=element_blank(),
    legend.position="top",
    plot.caption=element_text(hjust=0, size=10),
    plot.title=element_text(family="Satisfaction", size=24),
    plot.subtitle=element_text(size=14),
    plot.margin=margin(1,1,1,1, unit="cm")
  )

print(plot)

ggsave('~/Documents/github/tidytuesday/2024/w01_taylorswift/w01_taylorswift_acousticness_raw.png', height=12, width=12, unit="in")