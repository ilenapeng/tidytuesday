library(tidyverse)
library(extrafont)

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')
view(sports)

# keep only values with division 1
df_div1 <- sports %>% filter(classification_code==1 | classification_code==2 | classification_code==3)

# remove NAs if they are across both men and women teams
df_div1 <- df_div1[!with(df_div1,is.na(exp_women) & is.na(exp_men) & is.na(rev_men) & is.na(rev_women)),]

# fill in remaining NAs with 0
df_div1[c("exp_women", "exp_men", "rev_women", "rev_men")][is.na(df_div1[c("exp_women", "exp_men", "rev_women", "rev_men")])] <- 0

# create var subtracting difference
# if positive, university is making a profit
df_div1$diff_women <- df_div1$rev_women - df_div1$exp_women
df_div1$diff_men <- df_div1$rev_men - df_div1$exp_men
df_div1$diff_menwomen <- df_div1$total_rev_menwomen - df_div1$total_exp_menwomen

view(df_div1)

# average these differences by sport
df_div1_avg <- df_div1 %>% group_by(sports) %>% 
  summarise_at(vars(diff_men, diff_women), median, na.rm=TRUE) %>%
# filter out 0s in both diff_men and diff_women
  filter(!(diff_men == 0 & diff_women == 0)) %>%
# reshape
  gather('type', 'value', diff_men, diff_women)
view(df_div1_avg)

# plot
plot <- df_div1_avg %>%
  # filter out track and field individually, because we have it combined already
  filter(sports != 'Track and Field, X-Country' & sports != 'Track and Field, Indoor' & sports != 'Track and Field, Outdoor') %>%
ggplot(aes(fill=type, y=value, x=reorder(sports, value, sum))) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip() +
  scale_y_continuous(breaks=c(0, 2000000, 4000000, 6000000), label=c('0', '2M', '4M', '6M')) +
  scale_fill_manual(values=c('#023e8a', '#fb8500')) +
  labs(
    title='Nearly all US college sports operate at a loss, except for football',
    subtitle='Median difference between revenue and expenditures for Division 1 teams\n',
    caption='\nData from the Equity in Athletics Data Analysis | Chart by Ilena Peng for #TidyTuesday'
  ) +
  theme_minimal() +
  theme(
    plot.title=element_text(family="Bebas", size=16),
    plot.subtitle=element_text(family="Roboto"),
    plot.caption=element_text(family="Roboto", hjust=0, size=9),
    legend.position="none",
    axis.title=element_blank(),
    axis.text=element_text(family="Roboto", color="black"),
    plot.background=element_rect(color='white', fill="white"),
    plot.margin=margin(.5,.5,.5,.5, unit="cm")
  )

print(plot)
ggsave('w13_college_sports_raw.png', width=9, height=6, unit='in')

