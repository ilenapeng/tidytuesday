library(tidyverse)
library(extrafont)
library(ggtext)

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

#Creating numeric variables
df <- filter(survey, currency == "USD")
df <- filter(df, annual_salary < 3e+05)

df$gender[df$gender=='Prefer not to answer'] <- NA
df$gender[df$gender=='Other or prefer not to answer'] <- NA
df <- df[!is.na(df$gender),]
df <- df[!is.na(df$highest_level_of_education_completed),]

df <- df %>% arrange(highest_level_of_education_completed) %>% mutate(highest_level_of_education_completed = factor(highest_level_of_education_completed, 
              levels=c("High School","Some college", "College degree","Master's degree","PhD","Professional degree (MD, JD, etc.)")))
df <- df %>% arrange(highest_level_of_education_completed) %>% mutate(gender = factor(gender, 
              levels=c("Man","Woman","Non-binary")))

#Making plot
plot <- ggplot(data=df, aes(x=highest_level_of_education_completed, y=annual_salary, fill=gender, color=gender)) +
  geom_boxplot(outlier.colour = NULL, show.legend = FALSE) +
  scale_y_continuous(labels=c('0'='0k','100000'='100k','200000'='200k','300000'='300k')) +
  scale_color_manual(values=c("#346F5B","#364196","#B9A513")) +
  scale_fill_manual(values=c("#74BEA4","#9CA3DB","#EAD637")) +
  labs(title="Breaking down US salaries under $300k by education and gender",
       subtitle="Self-reported earnings for <b><span style='color:#346F5B'>male</span></b>, <b><span style='color:#364196'>female</span></b> and <b><span style='color:#B9A513'>non-binary</span></b> individuals",
       caption="Data from Ask The Manager blog \n Graphic by Ilena Peng for #TidyTuesday") +
  theme_minimal()+
  theme(text=element_text(family="Roboto"), axis.title=element_blank(), axis.text=element_text(color='Black'), panel.grid.major.x = element_line(color="white"),
        plot.title=element_text(size=20), plot.subtitle=element_markdown(size=16), plot.caption=element_text(size=8), plot.margin=margin(0.5,0.5,0.5,0.5, unit = "cm"))

print(plot)
ggsave("w21_salaries.png",width=12, height=9, unit="in")


