library(tidyverse)
library(extrafont)
# library(tidygeocoder)
# library(maps)
library(RColorBrewer)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# all the different foods you can taste in chocolate, apparently
freq <- sort(table(unlist(strsplit(chocolate$most_memorable_characteristics, ", "))),      # Create frequency table
                   decreasing = TRUE)
view(freq)

# convert to dataframe
df_freq <- as.data.frame(freq)
view(df_freq)

# capital case
df_freq$Var1 <- str_to_title(df_freq$Var1)

# loosely replace duplicates
# replace Grape and Mild Grapes with Grapes
df_freq$Var1 <- str_replace(df_freq$Var1, 'Grape$', 'Grapes')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Mild Grapes', 'Grapes')
# replace Black Licorice, Burnt Black Licorice with Licorice
df_freq$Var1 <- str_replace(df_freq$Var1, 'Black Licorice', 'Licorice')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Burnt Black Licorice', 'Licorice')
# replace Roasted Nuts with Nuts
df_freq$Var1 <- str_replace(df_freq$Var1, 'Roasted Nuts', 'Nuts')
# replace Rich Brownie, Burnt Brownie with Brownie
df_freq$Var1 <- str_replace(df_freq$Var1, 'Rich Brownie', 'Brownie')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Burnt Brownie', 'Brownie')
# replace sour banana, intense banana, mild banana with banana
df_freq$Var1 <- str_replace(df_freq$Var1, 'Sour Banana', 'Banana')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Intense Banana', 'Banana')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Mild Banana', 'Banana')
# replace mild grape with grape
df_freq$Var1 <- str_replace(df_freq$Var1, 'Mild Grape', 'Grape')
# replace mild lemon with lemon
df_freq$Var1 <- str_replace(df_freq$Var1, 'Mild Lemon', 'Lemon')
# replace mild orange, intense orange, oranges, orange citrus, sour orange with orange
df_freq$Var1 <- str_replace(df_freq$Var1, 'Mild Orange', 'Orange')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Intense Orange', 'Orange')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Orange$', 'Orange')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Orange Citrus', 'Orange')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Sour Orange', 'Orange')
# replace mild strawberry, strawberries with strawberry
df_freq$Var1 <- str_replace(df_freq$Var1, 'Mild Strawberry', 'Strawberry')
df_freq$Var1 <- str_replace(df_freq$Var1, 'Strawberries', 'Strawberry')
# replace spoiled milk with sour milk
df_freq$Var1 <- str_replace(df_freq$Var1, 'Spoiled Milk', 'Sour Milk')
# replace burnt black pepper with black pepper
df_freq$Var1 <- str_replace(df_freq$Var1, 'Burnt Black Pepper', 'Black Pepper')

# aggregate them back together so that the Black Licorice and Licorice values all go into Licorice now!
df_freq <- aggregate(Freq~.,df_freq,FUN=sum)

# keep only the foods of interest! > foods that appear more than 15 times
df_freq_top <- df_freq %>% filter(Var1 %in% 
                                    c(
                                  'Banana',
                                  'Brownie',
                                  'Cherry',
                                  'Tobacco',
                                  'Honey',
                                  'Melon',
                                  'Red Berry',
                                  'Strawberry',
                                  'Nuts',
                                  'Licorice',
                                  'Black Pepper',
                                  'Marshmallow',
                                  'Orange',
                                  'Coconut',
                                  'Grapes',
                                  'Raisins',
                                  'Sour Milk',
                                  'Lemon',
                                  'Fig')
                                    )

view(df_freq_top)

plot <- ggplot(df_freq_top, aes(x=reorder(Var1, Freq), y=Freq)) + 
  geom_point(color='#462521', size=1, shape=15) + 
  geom_segment(aes(x=Var1, xend=Var1, y=0, yend=Freq), color='#462521') +
  coord_flip() +
  labs(
    title="Different Foods You Can Taste in Chocolate, Apparently",
    subtitle="I doubt I could taste most of these, but here are some common food flavors\nthat were used to describe chocolates. Vanilla, coffee and caramel were\nalso on this list, but aren't visualized because that's unsurprising!\n",
    caption="\nData from Flavors of Cacao | Chart by Ilena Peng for #TidyTuesday"
  ) +
  theme_minimal() +
  theme(
    text=element_text(color="#462521", family="Raleway"),
    plot.title=element_text(face="bold", size=18),
    plot.subtitle=element_text(size=16),
    axis.title=element_blank(),
    axis.text=element_text(color="#462521", size=14),
    legend.position='none',
#    plot.background=element_rect(fill="#FFE0B5", color="#FFE0B5"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x = element_line(color="#F5E7E0"),
    plot.margin = margin(.5, .5, .5, .5, "cm")
  )

print(plot)

# ggsave("w3_chocolate_raw.png", width=9, height=6, "in")

# couldn't figure out the export, am getting 'Error: $ operator is invalid for atomic vectors', so just exported manually with 900x900 dimensions


