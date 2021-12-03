library(tidyverse)
library(extrafont)

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')
view(scoobydoo)

unique(scoobydoo$jeepers)

df <- select(scoobydoo, c('jeepers':'rooby_rooby_roo'))

# convert all to numeric
df[c(1:8)] <- lapply(df, as.numeric)

# summarise across all rows -- total # of times each is said in the show
df <- df%>%summarise_all(funs(sum), na.rm=TRUE)

# pivot 
df <- df %>%
  pivot_longer(c(1:8), names_to = "word", values_to = "count")

# reorder & keep only top 5 to match Spotify Wrapped
df <- df %>% arrange(desc(count))
df_top5 <- head(df, 5)

# make rank variable
df_top5$rank <- c('#1', '#2', '#3', '#4', '#5')

# cchange text in the catchphrases
df_top5$word <- str_replace_all(df_top5$word, "_", " ")
df_top5$word <- str_to_title(df_top5$word)

# make plot
plot <- df_top5 %>%
  mutate(rank = fct_reorder(rank, count)) %>%
  ggplot(aes(x=count, y=rank, fill=rank)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(x=count+50, y=rank, label=word), hjust=0, family = 'Helvetica', fontface='bold', size=6, color='#282C70') +
  scale_fill_manual(values = c('#99EBB8', '#CBF16A', '#F7D1D9', '#4343E3', '#503658')) +
  labs(
    title = "Scooby Doo's Catchphrases",
    subtitle = 'Number of times each phrase was said in Scooby Doo',
    caption = 'Data from plummye via Kaggle\nChart by Ilena Peng for #TidyTuesday'
  ) +
  xlim(0, 1500) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#E96CBD'),
    panel.grid = element_blank(),
    plot.title = element_text(family = 'Helvetica', face='bold', size=30, color='#282C70'),
    plot.subtitle = element_text(family = 'Helvetica', face='bold', size=18, color='#282C70'),
    axis.title = element_blank(), 
    axis.text.y = element_text(family = 'Helvetica', face='bold', size=30, color='#282C70'),
    axis.text.x = element_text(family = 'Helvetica', face='bold', size=12, color='#E928AF'),
    plot.caption = element_text(family = 'Helvetica', face='bold', color='#E928AF'),
    plot.margin = margin(1,1,1,1, "cm"),
    legend.position = "none")

print(plot)
ggsave('w29_scoobydoo.png', width=8, height=12, unit="in")

# 5.94 2.98 in // 1792 x 828



