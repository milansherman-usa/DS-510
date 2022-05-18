meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

nrow(subset(meteorites, is.na(meteorites$mass)))
# 131
nrow(subset(meteorites, is.na(meteorites$year)))
# 291
length(unique(meteorites$class))
# 455
ggplot(data = meteorites, aes(name_type)) + geom_bar()
nrow(subset(meteorites, meteorites$name_type == "Relict"))
# 75
ggplot(data = meteorites, aes(fall)) + geom_bar()
nrow(subset(meteorites, meteorites$fall == "Fell"))
# 1107

meteorites <- subset(meteorites, !is.na(meteorites$year))
meteorites <- meteorites %>%
  mutate(
    year = case_when(year == "2101" ~ "2010", TRUE ~ as.character(year))
  ) 
meteorites$year <- as.numeric(meteorites$year)

meteorites_mass <- subset(meteorites, !is.na(meteorites$mass))
meteorites_mass <- meteorites_mass %>%
  mutate(
    median_mass = median(mass)
  )

meteorites_tokens <- meteorites_mass %>%
  unnest_tokens(type, class) %>%
  # remove duplicate words in variety name
  distinct() %>%
  group_by(type) %>%
  summarise(
    n_type = n(),
    avg_mass = mean(mass)
    
    #avg_diff = mean(star_diff),
    #Rating = case_when(avg_diff < 0 ~ "Worse than average", avg_diff > 0 ~ "Better than average", TRUE ~ "Don't know")
  )

year <- meteorites_mass %>%
  group_by(year) %>%
  filter(year > 1800) %>%
  summarise(
    number = n(),
    mass = mean(mass)
  ) %>%
  filter(number >9)
 
ggplot(data = year, aes(x = year, y = mass)) + geom_point()

class <- meteorites_mass %>% 
  group_by(class) %>%
  summarise(
    number = n(),
    avg_mass = mean(mass)
  )%>%
  filter(number > 100) %>%
  mutate(
    median = median(avg_mass)
  )

class$size <- case_when(class$avg_mass < class$median ~ "Smaller than average", class$avg_mass > class$median ~ "Larger than average", TRUE ~ "Average")

class %>%
  arrange(avg_mass) %>%
  ggplot(aes(factor(class, levels = class), avg_mass, fill = size)) +
  geom_col() +
  coord_flip() +
  scale_fill_tableau() +
  ylab("Average mass by classification") +
  xlab("Classification") +
  ggtitle("Mass by Class")
