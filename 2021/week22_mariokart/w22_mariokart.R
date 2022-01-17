install.packages("wesanderson")
library(tidyverse)
library(extrafont)
library(wesanderson)
library(RColorBrewer)

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')

#Fastest for each track
fastest <- records %>% filter(type=="Three Lap")
fastest <- fastest %>% group_by(track) %>% slice(which.min(time))

#Thanks to this post: https://stackoverflow.com/questions/15751442/making-a-circular-barplot-with-a-hollow-center-aka-race-track-plot
#Creating empty rows for circle center
emptyrows <- data.frame(track = letters[1:4], time = rep(0, 4), 
                  Category2 = rep("", 4))

fastest$Category2 <- paste0(fastest$track," - ",fastest$time,"  ")

fastsubset <- fastest[c(1,8,10)]

emptyrows <- as.data.frame(emptyrows)
fastsubset <- as.data.frame(fastsubset)

finaldf <- rbind(fastsubset, emptyrows)

#Creating plot
plot <- ggplot(finaldf, aes(x = reorder(track,time), y = time,
                               fill = Category2)) + 
  geom_bar(width = 0.9, stat="identity") + 
  scale_fill_manual(values = c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2",n=4),wes_palette("FantasticFox1"),wes_palette("Darjeeling1"))) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,150)) +
  labs(
    title="MARIO KART WORLD RECORDS",
    subtitle="Record times (in seconds) for three-lap Nintendo 64 tracks",
    caption="Data from Bendikt Claus via Mario Kart World Records | Graphic by Ilena Peng for #TidyTuesday") +
  geom_text(data = finaldf, hjust = 1, size = 3, family="Roboto",
            aes(x = track, y = 0, label = Category2)) +
  theme_minimal() +
  theme(text=element_text(family="Roboto"),
        plot.title=element_text(family="Dela Gothic One", size=22, hjust=0.5),
        plot.subtitle=element_text(hjust=0.5, size=14),
        plot.caption=element_text(hjust=0.5, size=8),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


print(plot)
ggsave("w22_mariokart.png",width=9,height=9, units="in")

#Reordering for a rainbow chart
reorderdf <- finaldf %>% arrange(desc(time))

reorderdf$track<-factor(reorderdf$track, levels=c("Bowser's Castle", "Banshee Boardwalk", "Kalimari Desert", "Royal Raceway", 
                                                   "Koopa Troopa Beach", "Sherbet Land", "Moo Moo Farm", "Mario Raceway",
                                                   "Rainbow Road","Yoshi Valley","Toad's Turnpike","Luigi Raceway",
                                                   "Frappe Snowland","D.K.'s Jungle Parkway","Choco Mountain","Wario Stadium",
                                                   "a","b","c","d"))

plot <- ggplot(reorderdf, aes(x=rev(track), y = time, fill = track)) + 
  geom_bar(width = 0.9, stat="identity",alpha=0.9) + 
  scale_color_gradientn(colours = rainbow(3)) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,150)) +
  labs(
    title="MARIO KART WORLD RECORDS",
    subtitle="Record times (in seconds) for three-lap Nintendo 64 tracks",
    caption="Data from Bendikt Claus via Mario Kart World Records | Graphic by Ilena Peng for #TidyTuesday") +
  geom_text(data = reorderdf, hjust = 1, size = 3, family="Roboto",
            aes(x = rev(track), y = 0, label = Category2)) +
  theme_minimal() +
  theme(text=element_text(family="Roboto"),
        plot.title=element_text(family="Dela Gothic One", size=22, hjust=0.5),
        plot.subtitle=element_text(hjust=0.5, size=14),
        plot.caption=element_text(hjust=0.5, size=8),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


print(plot)
ggsave("w22_mariokartrainbow.png",width=9,height=9, units="in")


