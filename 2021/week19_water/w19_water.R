library(tidyverse)
library(extrafont)
library(ggtext)
library(RColorBrewer)

#Data
water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')
waterSource <- as.data.frame(water)
waterSource$country_name <-  str_replace(waterSource$country_name, "Congo - Brazzaville", "Congo")
waterSource$country_name <-  str_replace(waterSource$country_name, "Congo - Kinshasa", "Congo")
waterSource <- waterSource %>% filter(country_name!="Timor-Leste"&country_name!="Peru")

#Find number of NAs
sum(!complete.cases(waterSource[1:13]))
#Returned value is 424734

472304 - 424734

NAct <- c(424734, 47570)
Labels <- c("Missing","Complete")
NAdata <- as.data.frame(NAct, Labels)

NAdata$fraction = NAdata$NAct / sum(NAdata$NAct)

count(unique(waterSource$country_name))
view(NAdata)

# top of each rectangle
NAdata$ymax = cumsum(NAdata$fraction)
# bottom of each rectangle
NAdata$ymin = c(0, head(NAdata$ymax, n=-1))

# plot
plot <- ggplot(NAdata, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=NAct)) +
  geom_rect(fill=c("#A78A7F","#00798C")) +
  coord_polar(theta="y") + 
  theme_void() +
  theme(text=element_text(family="Lato"),legend.position = "none", plot.margin=margin(1,1,1,1,unit="cm")) +
  labs(caption="Data from Water Point Data Exchange. \n Graphic by Ilena Peng for #TidyTuesday")+
  xlim(c(-1, 4))

print(plot)

ggsave("w19_water.png", width = 8, height = 8, units = "in")
