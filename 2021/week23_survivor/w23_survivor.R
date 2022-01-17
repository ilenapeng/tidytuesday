library(tidyverse)
library(waffle)
library(wesanderson)
library(extrafont)

castaways <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')

#Categories from https://www.16personalities.com/personality-types
mbtiCt <- castaways %>% count(personality_type)
mbtiCt <- na.omit(mbtiCt)
unique(mbtiCt$personality_type)

mbtiCt$trait <- recode(mbtiCt$personality_type, 
     'ENFJ' = 'Extrovert', 'ENFP' = 'Extrovert', 'ENTJ' = 'Extrovert', 'ENTP' = 'Extrovert', 'ESFJ' = 'Extrovert', 'ESFP' = 'Extrovert', 'ESTJ' = 'Extrovert', 'ESTP' = 'Extrovert',
      'INFJ' = 'Introvert', 'INFP' = 'Introvert', 'INTJ' = 'Introvert', 'INTP' = 'Introvert', 'ISFJ' = 'Introvert', 'ISFP' = 'Introvert', 'ISTJ' = 'Introvert', 'ISTP' = 'Introvert'
)

mbtiCt$groups <- recode(mbtiCt$personality_type, 
    'INTJ' = 'Analysts (_NT_)', 'INTP' = 'Analysts (_NT_)', 'ENTJ' = 'Analysts (_NT_)', 'ENTP' = 'Analysts (_NT_)', 
    'INFJ' = 'Diplomats (_NF_)', 'INFP' = 'Diplomats (_NF_)', 'ENFJ' = 'Diplomats (_NF_)', 'ENFP' = 'Diplomats (_NF_)',
    'ISTJ' = 'Sentinels (_S_J)', 'ISFJ' = 'Sentinels (_S_J)', 'ESTJ' = 'Sentinels (_S_J)', 'ESFJ' = 'Sentinels (_S_J)',
    'ISTP' = 'Explorers (_S_P)', 'ISFP' = 'Explorers (_S_P)', 'ESTP' = 'Explorers (_S_P)', 'ESFP' = 'Explorers (_S_P)',
)

mbtiCt$type <- recode(mbtiCt$personality_type, 
  'INFJ' = 'I__J', 'INTJ' = 'I__J', 'ISFJ' = 'I_F_', 'ISTJ' = 'I_T_',
  'INFP' = 'I__P', 'INTP' = 'I__P', 'ISFP' = 'I_F_', 'ISTP' = 'I_T_', 
  'ENFJ' = 'E__J', 'ENTJ' = 'E__J', 'ESFJ' = 'E_F_', 'ESTJ' = 'E_T_',
  'ENFP' = 'E__P', 'ENTP' = 'E__P', 'ESFP' = 'E_F_', 'ESTP' = 'E_T_', 
)

#barplot
plot <- ggplot(mbtiCt, aes(fill=groups, y=n, x=reorder(personality_type, n))) + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  scale_fill_manual(values = wes_palette("Cavalcanti1")) +
  labs (
    title="MBTI Personality Types of Survivor TV Show Participants",
    subtitle="
    The Myers–Briggs Type Indicator has 16 personality types, which can be grouped into four categories - Analysts, Diplomats, Explorers and Sentinels. \n \n
    A brief rundown of what each letter means : Introvert (I) or extravert (E) | Observant (S) or Intuitive (I) | Thinking (T) or Feeling (F) | Judging (J) or Prospecting (P) \n \n
    ENFPs, which fit into the Diplomat personality category, appeared the most on the show, followed perhaps unsurprisingly by several Explorer types.",
    caption="Data from Daniel Oehm & the survivorR R package | MBTI information from 16Personalities | Graphic by Ilena Peng for #TidyTuesday"
    ) +
  theme_minimal() +
  theme(
    text=element_text(family='Roboto Condensed'),
    axis.title=element_blank(), 
    axis.text=element_text(color="black",size=14),
    legend.title=element_blank(), 
    legend.text = element_text(size=14),
    legend.position="top", 
    panel.grid.major.y = element_blank(),
    plot.title=element_text(color="#02401B",size=25, face="bold", hjust=0),
    plot.subtitle=element_text(lineheight = 0.5, hjust=0, size=12, color="#494A30"), 
    plot.caption=element_text(hjust=0.5, color="#494A30"),
    plot.margin=margin(0.5,0.5,0.5,0.5, unit="cm"))

print(plot)
ggsave("w23_survivor.png",width=15, height=9, units="in")

#Not using: Waffle bar chart
plot <- ggplot(mbtiCt, aes(fill=personality_type, values=n)) +
  geom_waffle(color = "white", size = 0.33, flip=TRUE) +
  facet_wrap(~groups, nrow=1, strip.position = "bottom") +
  coord_equal() +
  scale_fill_manual(values = c(wes_palette("Moonrise2"),wes_palette("IsleofDogs2"),wes_palette("Moonrise1"),wes_palette("Cavalcanti1"))) +
  theme(text=element_text(family='Zwizz'),strip.text.x = element_text(hjust = 0.5), strip.background=element_rect(fill="white"), panel.background=element_rect(fill="white"), axis.text.x = element_blank(), axis.ticks.x=element_blank()
  )

print(plot)


