library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2020-01-14')
tuesdata2 <- tidytuesdayR::tt_load(2020, week = 3)

passwords <- passwords %>%
          filter(!is.na(password)) %>%
          unite(online_crack, value, time_unit)

rank_diff <- passwords %>%
          filter(rank != rank_alt) %>%
          mutate(diff = abs(rank - rank_alt)) %>%
          filter(diff>=2)

category <- passwords %>%
          group_by(category) %>%
          summarise(avg_rank = mean(rank), avg_strength = mean(strength))

ggplot(passwords, aes(offline_crack_sec, strength)) + geom_point() #+ geom_smooth('lm', strength ~ rank)
