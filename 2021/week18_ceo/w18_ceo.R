install.packages("patchwork","ggtext")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggtext)
library(extrafont)

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

departnewop <- filter(departures, departure_code=="6")
departCt <- departnewop %>% count(fyear)
departCt<- filter(departCt,fyear!=c("1992","1993"))

#Statista data: https://www.statista.com/statistics/235494/new-entrepreneurial-businesses-in-the-us/
year <- c(1994:2018)
startup <- c(569419,504415,609638,639114,643070,650730,674644,671383,659236,662543,653887,679925,715734,703834,678095,608769,560688,582569,631817,629078,652780,678135,733085,733490,733825)
startupall<-as.data.frame(cbind(year,startup))
startupall$transf<-startupall$startup/10000

p1 <- ggplot(departCt, aes(x=fyear, y=n)) +
  geom_line(color="#0081A7", size=1.5) +
  labs (title="CEOs leaving for <b><span style = 'color:#0081A7;'>new opportunities</span></b> vs <b><span style = 'color:#FC814A;'>number of U.S. startups</span></b>, over time",
        subtitle="Startups are defined as businesses less than a year old")+
  ylab("CEO departures")+
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015), labels=c(1995,2000,2005,2010,2015))+
  theme(
    text=element_text(family="Raleway"),
    axis.text=element_text(color="black", size=12),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_line(color="#DCDCDC"),
    plot.title=element_markdown(size=16)
  )

print(p1)

p2 <- ggplot(startupall, aes(x=year, y=transf)) +
  geom_line(color="#FC814A",size=1.5) +
  labs(caption="CEO data from Gentry et al. via DataIsPlural. Startup data via Statista. \n Plot by @ilenapeng for #TidyTuesday")+
  ylab("Startups, in tens of thousands")+
  scale_y_continuous(breaks=c(50,60,70),labels=c(50,60,70))+
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015), labels=c(1995,2000,2005,2010,2015))+
  theme(
    text=element_text(family="Raleway"),
    axis.text=element_text(color="black", size=12),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_line(color="#DCDCDC")
  )
  
print(p2)

p1/p2

ggsave("w18_ceo.png", width = 10, height = 6, units = "in")



