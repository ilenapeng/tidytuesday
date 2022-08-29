library(tidyverse)
library(extrafont)
library(sf)
library(tigris)
library(svglite)

df_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# convert to capital case
df_raw$county <- str_to_title(df_raw$county)
df_raw$nhood <- str_to_title(df_raw$nhood)

# load shapefiles
ca <- counties("California", cb = TRUE)

# filter to most recent year
df_18 <- df_raw %>% filter(year==2018)

# filter to one beds -- there are 61k 1 beds and 71k beds in this data across all years
df_18_onebd <- df_18 %>% filter(beds==1)

# group by county
df_18_onebd_avg <- df_18_onebd %>% group_by(county) %>% 
  summarise_at(vars(price), list(name = mean))

# remove NA
df_18_onebd_avg <- na.omit(df_18_onebd_avg)

# rename 'name' column to 'price'
df_18_onebd_avg <- rename(df_18_onebd_avg, price=name)
view(df_18_onebd_avg)

merge <- ca %>% full_join(df_18_onebd_avg, by = c("NAME" = "county"))
# omit NAs (other counties outside of the SF Bay Area)
merge <- na.omit(merge)
# rename "name" column tha
view(merge)

# paste labels
merge$label <- paste0(merge$NAME, "\n$", round(merge$price,2))

# plot
plot <- merge %>%
  ggplot() +
  geom_sf(aes(fill=price), color="white", size=0.1) +
  scale_fill_gradient(low='#EA98DA', high='#2514E2', limits=c(500, 3500), breaks=seq(500, 3500, by=500)) +
  theme_void() +
  geom_sf_text(aes(label = label), size=3, colour = "white", family="Avenir") +
  labs(
    title="2018 Rent Prices in the San Francisco Bay Area", 
    subtitle="Rental Prices from Craiglist Listings for One-Bedroom Units in the Bay\n", 
    caption="Data from Kate Pennington \nGraphic by Ilena Peng for #TidyTuesday") +
  theme(
    text=element_text(family="Avenir"), 
    plot.title=element_text(face="bold"), 
    plot.caption=element_text(hjust=0),
    plot.subtitle=element_text(size=9), 
    plot.margin=margin(1,1,1,1, "cm"),
    plot.background=element_rect(color="white", fill="white"),
    legend.title=element_blank())

print(plot)
ggsave("w27_sf_rent_raw.svg", width=6, height=6, unit="in")