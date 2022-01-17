library(tidyverse)
library(extrafont)
library(patchwork)
library(ggtext)

nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')
view(nominees)

#Cleaning data
#HBO
nominees$distributor <-  str_replace(nominees$distributor , "HBO (discoverwestworld.com)", "HBO")
nominees$distributor <-  str_replace(nominees$distributor , "HBO Max", "HBO")
nominees$distributor <-  str_replace(nominees$distributor , "HBO.com", "HBO")

#NBC
nominees$distributor <-  str_replace(nominees$distributor , "NBC (nbc.com)", "NBC")
nominees$distributor <-  str_replace(nominees$distributor , "NBC (YouTube)", "NBC")
nominees$distributor <-  str_replace(nominees$distributor , "NBC.com", "NBC")

#CBS
nominees$distributor <-  str_replace(nominees$distributor , "CBS (CBS All Access)", "CBS")
nominees$distributor <-  str_replace(nominees$distributor , "CBS (CBS on Snapchat)", "CBS")
nominees$distributor <-  str_replace(nominees$distributor , "CBS All Access", "CBS")

#ABC
nominees$distributor <-  str_replace(nominees$distributor , "ABC (abc.go.com)", "ABC")
nominees$distributor <-  str_replace(nominees$distributor , "ABC (ABCd/ABC.com)", "ABC")
nominees$distributor <-  str_replace(nominees$distributor , "ABC (Oscar.com)", "ABC")
nominees$distributor <-  str_replace(nominees$distributor , "ABC.com", "ABC")
nominees$distributor <-  str_replace(nominees$distributor , "abc.com/lost", "ABC")

#Data for plotting
distrib <- nominees %>% count(distributor, type, year)
distrib <- spread(distrib, type, n)

#Data for analyzing
total_ct <- nominees %>% count(distributor)
type_ct <- nominees %>% count(distributor, type)

#Plot theme
plottheme <- theme(
  panel.grid.minor=element_blank(),
  plot.margin=margin(0.5,0.5,0.5,0.5,unit="cm"),
  text=element_text(family="Helvetica"),
  plot.title=element_markdown(size=22, lineheight=1.2),
  plot.subtitle=element_text(size=20, face="bold"),
  axis.title=element_blank(),
  axis.text=element_text(size=16, color="black"))

#HBO
hbo <- distrib %>% filter(distributor=="HBO") %>%
  ggplot(aes(x=year)) + 
  geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
  geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
  scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
  scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
  labs(
    title="<b>Networks & Platforms' Emmy <span style='color:#820B8A;'>Wins</span> & <span style='color:#ADFFE8;'>Nominations</span></b> <br>
    Distributors with the most Emmy recognitions <br>",
    subtitle="HBO - 4,558 total (nominations & wins combined)"
  ) +
  theme_minimal() +
  plottheme

print(hbo)

#NBC
nbc <- distrib %>% filter(distributor=="NBC") %>%
  ggplot(aes(x=year)) + 
  geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
  geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
  scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
  scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
  labs(
    subtitle="NBC - 3,683 total"
  ) +
  theme_minimal() +
  plottheme

print(nbc)

#CBS
cbs <- distrib %>% filter(distributor=="CBS") %>%
  ggplot(aes(x=year)) + 
  geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
  geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
  scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
  scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
  labs(
    subtitle="CBS - 3,002 total"
  ) +
  theme_minimal() +
  plottheme

print(cbs)

#ABC
abc <- distrib %>% filter(distributor=="ABC") %>%
  ggplot(aes(x=year)) + 
  geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
  geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
  scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
  scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
  labs(
    subtitle="ABC - 2,962 total"
  ) +
  theme_minimal() +
  plottheme

print(abc)

#Netflix
net <- distrib %>% filter(distributor=="Netflix") %>%
  ggplot(aes(x=year)) + 
  geom_bar(aes(y=Winner), stat="identity", fill="#820B8A") +
  geom_bar(aes(y=-Nominee), stat="identity", fill="#ADFFE8") +
  scale_y_continuous(limits=c(-300,100), breaks=c(-300, -200, -100, 0, 100), labels=c(300,200,100,0,100)) +
  scale_x_continuous(limits=c(1952,2026), breaks=c(1960,1970,1980,1990,2000,2010,2020), labels=c(1960,1970,1980,1990,2000,2010,2020)) +
  labs(
    subtitle="Netflix - 2,144 total",
    caption="Data from emmys.com | Graphic by Ilena Peng for #TidyTuesday"
  ) +
  theme_minimal() +
  plottheme

print(net)

hbo / nbc / cbs / abc / net
ggsave("w39_emmys.png",height=15,width=9, unit="in")