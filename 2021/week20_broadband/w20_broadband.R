install.packages("zipcodeR")
install.packages("geofacet")
library(zipcodeR)
library(geofacet)
library(tidyverse)
library(extrafont)

broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

#Prepare data
zipcodedb <- zip_code_db
broadband$zipcode <- broadband$`COUNTY ID`
broadband$zipcode <- as.double(broadband$zipcode)
zipcodedb$zipcode <- as.double(zipcodedb$zipcode)

broadband$`BROADBAND AVAILABILITY PER FCC` <- as.numeric(broadband$`BROADBAND AVAILABILITY PER FCC`)
broadband$`BROADBAND USAGE` <- as.numeric(broadband$`BROADBAND USAGE`)

bbandCty <- broadband %>% inner_join(zipcodedb, by = "zipcode")
#Convert % to real #s. Note missing values (-) forced to NAs
bbandCty$bbandUse <- bbandCty$`BROADBAND USAGE` * bbandCty$population
bbandCty$bbandAvail <- bbandCty$`BROADBAND AVAILABILITY PER FCC` * bbandCty$population

#Collapse by state, get sums of actual broadband use, availability and population
bbandState <- bbandCty %>% group_by(ST) %>% summarise_at(vars(bbandUse, bbandAvail, population), sum, na.rm=TRUE)
#Get state %
bbandState$bbandUsePct <- bbandState$bbandUse / bbandState$population
bbandState$bbandAvailPct <- bbandState$bbandAvail / bbandState$population
bbandState$code <- bbandState$ST

longDf <- bbandState %>% gather(Var, Value, bbandUse:population)

view(bbandState)

plot <-  ggplot(longDf, aes(area = Value, fill = Var, label = Value)) +
geom_treemap(layout = "fixed") +
scale_fill_manual(values=c("#F0EC57","#748067","#BBCEA8")) +
theme_minimal() +
theme(text=element_text(family="Roboto"), legend.position="none", plot.title=element_markdown(size=18, family="Roboto"), plot.subtitle=element_markdown(lineheight=1.5), plot.caption=element_markdown(hjust = 0)) +
facet_geo(~ code) +
labs(title="U.S. broadband <span style='color: #F0EC57'><b><strong>access</strong></b></span> and <span style='color: #748067'><b>usage</b></span>",
subtitle="Treemap displays real values for access and use, within state <span style='color: #BBCEA8'><b>population</b></span>. Dotplot displays percentages of state populations.  \nBroadband access is defined as having access to fixed terrestrial broadband with 25 Mbps upload / 3 Mbps download speeds at the end of 2017",
caption="Data from Microsoft via The Verge. Graphic by Ilena Peng for #TidyTuesday")
print(plot)

plotNE <- ggplot(bbandNE) +
geom_segment( aes(x=ST, xend=ST, y=bbandUsePct, yend=bbandAvailPct), color="grey") +
geom_point( aes(x=ST, y=bbandUsePct), size=3, color="#748067") +
geom_point( aes(x=ST, y=bbandAvailPct), size=3, color="#F0EC57") +
coord_flip()+
scale_y_continuous(limits = c(0,1)) +
theme_minimal() +
theme(text=element_text(family="Roboto"), legend.position = "none") +
xlab("") + ylab("") +
labs(title="Northeast")

print(plotNE)

plotMid <- ggplot(bbandMid) +
geom_segment( aes(x=ST, xend=ST, y=bbandUsePct, yend=bbandAvailPct), color="grey") +
geom_point( aes(x=ST, y=bbandUsePct), size=3, color="#748067") +
geom_point( aes(x=ST, y=bbandAvailPct), size=3, color="#F0EC57") +
coord_flip()+
scale_y_continuous(limits = c(0,1)) +
theme_minimal() +
theme(text=element_text(family="Roboto"), legend.position = "none") +
xlab("") + ylab("") +
labs(title="Midwest")

print(plotMid)

plotW <- ggplot(bbandW) +
geom_segment( aes(x=ST, xend=ST, y=bbandUsePct, yend=bbandAvailPct), color="grey") +
geom_point( aes(x=ST, y=bbandUsePct), size=3, color="#748067") +
geom_point( aes(x=ST, y=bbandAvailPct), size=3, color="#F0EC57") +
coord_flip()+
scale_y_continuous(limits = c(0,1)) +
theme_minimal() +
theme(text=element_text(family="Roboto"), legend.position = "none") +
xlab("") + ylab("") +
labs(title="West")

print(plotW)

plotS <- ggplot(bbandS) +
geom_segment( aes(x=ST, xend=ST, y=bbandUsePct, yend=bbandAvailPct), color="grey") +
geom_point( aes(x=ST, y=bbandUsePct), size=3, color="#748067") +
geom_point( aes(x=ST, y=bbandAvailPct), size=3, color="#F0EC57") +
coord_flip()+
scale_y_continuous(limits = c(0,1)) +
theme_minimal() +
theme(text=element_text(family="Roboto"), legend.position = "none") +
xlab("") + ylab("Percent of population") +
labs(title="South", caption="Data from Microsoft via The Verge. Graphic by Ilena Peng for #TidyTuesday")

print(plotS)

(plot | (plotNE / plotMid / plotW / plotS)) + plot_layout(ncol=2, widths=c(3,1))
ggsave("w20_broadband.png", width=16, height=10, unit="in")





