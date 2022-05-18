ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

ramen_ratings <- ramen_ratings %>%
  mutate (
  # missing review number
  review_number = replace_na(review_number, 2991),
  # looked up varity and brand
  style = replace_na(style, "Pack"),
  # typo fixes
  country = case_when(country == "Phlippines" ~ "Philippines", country == "USA" ~ "United States", TRUE ~ country)
  ) %>%
  # missing stars are either noodles-only (no flavor), 2 products mixed together, or not food
  filter(!is.na(stars))

 ramen_ratings <- filter(ramen_ratings, !is.na(stars))
 ramen_ratings <- ramen_ratings %>%
   mutate(star_diff = stars - mean(ramen_ratings$stars))
 
 ramen_tokens <- ramen_ratings %>%
   unnest_tokens(words, variety) %>%
   # remove duplicate words in variety name
   distinct() %>%
   group_by(words) %>%
   summarise(
     avg_stars = mean(stars),
     n_words = n(),
     avg_diff = mean(star_diff),
     Rating = case_when(avg_diff < 0 ~ "Worse than average", avg_diff > 0 ~ "Better than average", TRUE ~ "Don't know")
   )
   
 # plot of average star rating for ramen varieties associated with a word vs. word count
 # in all variety names (sanity check):
 ramen_tokens %>%
   #filter out some noise from rare words
   filter(n_words > 10) %>%
   ggplot(aes(n_words, avg_stars, color = Rating)) +
   geom_point(size = 2, alpha = 0.5) +
   scale_color_tableau() +
   ylab("Average star rating") +
   xlab("Number of time word appears in variety name")
 
 ramen_tokens %>%
   # most common words, whith count above 50 in dataset:
   filter(n_words >= 50) %>%
   arrange(avg_diff) %>%
   # 10 best and worst rated words
   slice(c(1:10, 37:46)) %>%
   ggplot(aes(factor(words, levels = words), avg_diff, fill = Rating)) +
   geom_col() +
   coord_flip() +
   scale_fill_tableau() +
   ylab("Average rating difference from mean rating") +
   xlab("Word in variety name") +
   ggtitle("Variety name words associated with better or worse ramen ratings")



 
 
 
 
 
 
 boxplot(ramen_ratings$stars)
hist(ramen_ratings$stars)
style <- ramen_ratings_clean %>% 
  group_by(style) %>%
  summarise(number = n(), avg = mean(stars))

brand <- ramen_ratings_clean %>%
  group_by(brand)  %>%
  summarise(number = n(), avg = mean(stars), median = median(stars))

country <- ramen_ratings_clean %>% 
  group_by(country) %>%
  summarise(number = n(), avg = mean(stars))

pack <- subset(ramen_ratings, ramen_ratings$style == "Pack")
ramen_ratings1 <- subset(ramen_ratings, !is.na(ramen_ratings$stars))
ramen_ratings_clean <- subset(ramen_ratings1, !is.na(ramen_ratings1$style))

ggplot(ramen_ratings_clean, aes(stars)) + geom_histogram(binwidth = 1) + facet_wrap("country")
