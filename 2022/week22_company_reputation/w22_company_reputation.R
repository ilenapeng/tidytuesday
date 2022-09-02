library(tidyverse)
library(extrafont)
#library(fmsb)
# library(gridExtra)

# https://r-graph-gallery.com/web-radar-chart-with-R.html
library(ggradar)

reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

# How do people view fb & twitter & tiktok
telecom <- reputation %>% filter(industry=='Telecom')
# unique(tech$company)

# social_list <- c("Twitter", "Facebook", "TikTok")
# social <- tech %>% filter(company %in% social_list)

# drop industry and rank columns
telecom <- telecom[-c(2, 5)]

# capital case the company and name columns
telecom$company <- str_to_title(telecom$company)
telecom$name <- str_to_title(telecom$name)
                                 
# reshape long to wide
telecom_wide <- telecom %>%
 pivot_wider(names_from = name, values_from = score)

# documentation: https://rdrr.io/github/ricardo-bion/ggradar/man/ggradar-package.html
plot <- telecom_wide %>%
#  mutate_at(vars(-company), rescale) %>%
  ggradar(
    values.radar = c("50", "75", "100"),
    grid.min = 50,
    grid.mid = 75,
    grid.max = 100,
    group.line.width = .8,
    group.point.size = 1,
    group.colours = c("#00A1E4", "#F5B700", "#0CCA4A", "#B744B8"),
    axis.label.size = 5,
    grid.label.size = 5,
    legend.text.size = 12,
    legend.position = "top",
    plot.title="Reputations of US Telecom Companies",
    background.circle.colour = "#ECECE9",
    font.radar = "Roboto",
    
  ) 

print(plot)
ggsave("w22_company_reputation_raw.png", width=9, height=9, unit="in")
# caption and subtitles manually added in
