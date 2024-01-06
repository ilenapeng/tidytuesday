library(tidyverse)
library(extrafont)
library(tidytext)
library(waffle)
library(taylor)
# testing 
# taylor_album_songs$lyrics

df_raw <- taylor_album_songs

# Replace (Taylor's Version) text with * to be included in notes
df_raw$album_name <- gsub(" (Taylor's Version)", "*", df_raw$album_name, fixed=TRUE)

# ok here is an ... interesting way of getting into this lyrics data, partly drawing from https://github.com/aaumaitre/taylor_swift

# unnest the data. this gives us one row for each line of each lyric
df1 <- tidyr::unnest(df_raw, cols = lyrics)

# retrieve words from the lyrics
df <- df1%>%
  #word is the new column, lyric the column to retrieve the information from
  unnest_tokens(word, lyric) 
view(df)

# subset columns
df <- df[c('album_name', 'track_name', 'word')]

# ok I cheated a bit here and used Genius to see what colors to search for
# https://genius.com/Genius-users-colors-in-taylor-swift-songs-annotated
# and also this https://taylorswiftandx.tumblr.com/post/177059355810/taylor-swift-and-colors-sorted-by-color
match_list <- paste(c('amber', 'aquamarine', 'black', 'blue', 'brown', 'gold', 'green', 'indigo', 'lavender', 'maroon', 'orange', 'pink', 'purple', 'red', 'gray', 'rose', 'tan', 'tangerine', 'teal', 'white', 'violet', 'rosy', 'cherry', 'burgundy', 'scarlet', 'rust', 'sapphire', 'silver', 'crimson'))

# filter dataset based on colors we know are in the songs
df_color <- subset(df_count, word %in% match_list)

# and then group by album  and count the colors -- which is what i ended up visualizing 
df_color_by_album <- df %>%
  group_by(album_name) %>%
  count(word, sort = TRUE) %>%
  subset(word %in% match_list)

match_list <- paste(c( 'rosy', 'cherry', 'burgundy', 'scarlet', 'rust', 'sapphire', 'silver', 'crimson'))

# take forever assigning custom colors
df_color_by_album <- df_color_by_album %>% 
  mutate(hexcode = case_when(word == 'amber'  ~ '#FFBF00', 
                             word == 'aquamarine' ~ '#76D7C4',
                             word == 'black' ~ '#000000',
                             word == 'blue' ~ '#85C1E9',
                             word == 'brown' ~ '#BA4A00',
                             word == 'gold' ~ '#F4D03F',
                             word == 'green' ~ '#7DCEA0',
                             word == 'indigo' ~ '#4B0082',
                             word == 'lavender' ~ '#D2B4DE',
                             word == 'maroon' ~ '#A93226',
                             word == 'orange' ~ '#EB984E',
                             word == 'pink' ~ '#FFC0CB',
                             word == 'purple' ~ '#AF7AC5',
                             word == 'red' ~ '#FF0000',
                             word == 'gray' ~ '#CACFD2',
                             word == 'rose' ~ '#F7879A', 
                             word == 'tan' ~ '#D2B48C',
                             word == 'tangerine' ~ '#F28500',
                             word == 'teal' ~ '#008080',
                             word == 'white' ~ '#d3d3d3',
                             word == 'violet' ~ '#AF69EE',
                             word == 'rosy' ~ '#F7879A',
                             word == 'cherry' ~ '#E74C3C', 
                             word == 'burgundy' ~ '#f00000',
                             word == 'scarlet' ~ '#FF2400',
                             word == 'rust' ~ '#b7410e',
                             word == 'sapphire' ~ '#0f52ba',
                             word == 'silver' ~ '#C0C0C0',
                             word == 'crimson' ~ '#DC143C',
                             ))

# Reorder axis
df_color_by_album$album_name_fct <- factor(df_color_by_album$album_name, levels=c('Taylor Swift', 'Fearless*', 'Speak Now*', 'Red*', '1989*', 'reputation', 'Lover', 'folklore', 'evermore', 'Midnights'))

# plot
plot <- df_color_by_album %>% ggplot(aes(fill = hexcode, values = n)) +
  geom_waffle(n_rows = 10, size = 0.33, color='white') +
  scale_fill_identity() +
  coord_equal() +
  facet_wrap(vars(album_name_fct), nrow=2) +
  labs(
    title="Taylor Swift",
    subtitle="Colors in Taylor Swift lyrics\n",
    caption="*Taylor's Version\nData from the taylor R package | Chart by Ilena Peng for #TidyTuesday"
  ) +
  theme_void() +
  theme(
    plot.background=element_rect(fill="white", color="white"),
    panel.grid = element_blank(),
    text=element_text(family="Avenir", size=16, color='black'),
    axis.title=element_blank(),
    axis.text.x=element_blank(),
    legend.position="none",
    plot.caption=element_text(hjust=0, size=10),
    plot.title=element_text(family="Satisfaction", size=24),
    plot.subtitle=element_text(size=14),
    plot.margin=margin(1,1,1,1, unit="cm")
  )

print(plot)
ggsave('~/Documents/github/tidytuesday/2024/w01_taylorswift/w01_taylorswift_colors_raw.png', height=6, width=10, unit="in")
