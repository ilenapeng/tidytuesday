library(tidyverse)
library(extrafont)
library(patchwork)

studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')

# Manually export and add in blank rows for gaps in the circular chart
# write.csv(studio_album_tracks, "studio_album_tracks.csv", row.names = FALSE)
studio_album_tracks <- read_csv("studio_album_tracks.csv")

# Keep only variables needed
df_for_plot <- studio_album_tracks[c(6,7,11,12,13,14,15,20,22)]

# Reshape data 
df_for_plot <- gather(df_for_plot, key = "characteristic", value = "value", -c(album_name, track_name))

# Making separate charts for each because I couldn't reorder bars within facet_grid()
# https://community.rstudio.com/t/how-can-we-arrange-the-bars-for-each-facets-grid-in-ggplot/31512/8

# Make dataframes for each one
danceability <- df_for_plot %>% filter(characteristic == "danceability")
energy <- df_for_plot %>% filter(characteristic == "energy")
speechiness <- df_for_plot %>% filter(characteristic == "speechiness")
instrumentalness <- df_for_plot %>% filter(characteristic == "instrumentalness")
acousticness <- df_for_plot %>% filter(characteristic == "acousticness")
liveness <- df_for_plot %>% filter(characteristic == "liveness")
valence <- df_for_plot %>% filter(characteristic == "valence")

plot_theme <- theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  axis.text=element_blank(),
  axis.title=element_blank(),
  panel.grid=element_blank(),
  legend.title=element_blank())

# danceability
danceability_plt <-
  ggplot(danceability, aes(fct_reorder(track_name, album_name), value, fill=album_name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#B93245", "#C2DB20", "#3B8AA2")) +
  ylim(-1,1) +
  coord_polar(start = 0) +
  annotate(geom = 'text', x = 0.5, y = -1, label = "Dance", family="Bebas", color="#43358D", size=3) +
  theme_minimal() +
  theme(legend.position="none") +
  plot_theme

# energy
energy_plt <-
  ggplot(energy, aes(fct_reorder(track_name, album_name), value, fill=album_name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#B93245", "#C2DB20", "#3B8AA2")) +
  ylim(-1,1) +
  coord_polar(start = 0) +
  annotate(geom = 'text', x = 0.5, y = -1, label = "Energy", family="Bebas", color="#43358D", size=3) +
  theme_minimal() +
  theme(legend.position="top", legend.text=element_text(family="Roboto", color="#43358D", size=10)) +
  plot_theme

# speechiness
speechiness_plt <-
  ggplot(speechiness, aes(fct_reorder(track_name, album_name), value, fill=album_name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#B93245", "#C2DB20", "#3B8AA2")) +
  ylim(-1,1) +
  coord_polar(start = 0) +
  annotate(geom = 'text', x = 0.5, y = -1, label = "Speech", family="Bebas", color="#43358D", size=3) +
  theme_minimal() +
  theme(legend.position="none") +
  plot_theme

# instrumentalness
instrumentalness_plt <-
  ggplot(instrumentalness, aes(fct_reorder(track_name, album_name), value, fill=album_name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#B93245", "#C2DB20", "#3B8AA2")) +
  ylim(-1,1) +
  coord_polar(start = 0) +
  annotate(geom = 'text', x = 0.5, y = -1, label = "Instrumental", family="Bebas", color="#43358D", size=3) +
  theme_minimal() +
  theme(legend.position="none") +
  plot_theme

# acousticness
acousticness_plt <-
  ggplot(acousticness, aes(fct_reorder(track_name, album_name), value, fill=album_name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#B93245", "#C2DB20", "#3B8AA2")) +
  ylim(-1,1) +
  coord_polar(start = 0) +
  annotate(geom = 'text', x = 0.5, y = -1, label = "Acoustic", family="Bebas", color="#43358D", size=3) +
  theme_minimal() +
  theme(legend.position="none") +
  plot_theme

# liveness
liveness_plt <-
  ggplot(liveness, aes(fct_reorder(track_name, album_name), value, fill=album_name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#B93245", "#C2DB20", "#3B8AA2")) +
  ylim(-1,1) +
  coord_polar(start = 0) +
  annotate(geom = 'text', x = 0.5, y = -1, label = "Live", family="Bebas", color="#43358D", size=3) +
  theme_minimal() +
  theme(legend.position="none") +
  plot_theme

#valence
valence_plt <-
  ggplot(valence, aes(fct_reorder(track_name, album_name), value, fill=album_name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#B93245", "#C2DB20", "#3B8AA2")) +
  ylim(-1,1) +
  coord_polar(start = 0) +
  annotate(geom = 'text', x = 0.5, y = -1, label = "Valence", family="Bebas", color="#43358D", size=3) +
  theme_minimal() +
  theme(legend.position="none") +
  plot_theme

# Stack charts together
patchwork <- (danceability_plt|energy_plt|valence_plt)/(acousticness_plt|liveness_plt|speechiness_plt|instrumentalness_plt)

plot <- patchwork + plot_annotation(
  title = "The Spice Girls' Discography",
  subtitle = "Characteristics of Spice Girls songs, as determined by Spotify",
  caption = "Data from Spotify and Genius | Graphic by Ilena Peng for #TidyTuesday\n") & 
  theme(
    plot.title=element_text(family="Bebas", hjust=0.5, color="#43358D", size=18),
    plot.subtitle=element_text(family="Roboto", hjust=0.5, color="#43358D", size=14),
    plot.caption=element_text(family="Roboto", hjust=0.5, color="#43358D", size=8),
    plot.margin=margin(.2,.2,.2,.2,"cm"))

print(plot)
ggsave("w51_spicegirls.png", width=9, height=6, unit="in")