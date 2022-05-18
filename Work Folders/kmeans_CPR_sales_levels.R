library(dplyr)
library(ggplot2)
library(purrr)
library(cluster)

# Read in results from Excel sheet
CPs <- read.csv("Historical_CPs.csv")
sales <- unique(CPs$total_sales)
sales <- as.data.frame(sales)

tot_withinss <- map_dbl(1:15, function(k) {
  model <- kmeans(x = sales, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:15, 
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + 
  scale_x_continuous(limits = c(1, 15),
                     breaks = seq(1, 15, by = 1),
                     labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14","15"))

# elbow plot suggest k = 2, which is not practical

sil_width <- map_dbl(2:10, function(k){
  model <- pam(x = sales, k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame(
  k= 2:10,
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width)) + 
  geom_line() +
  scale_x_continuous(breaks = 2:10)

# silhouette width also suggest k = 2, but k = 5 also looks reasonable and is more practical

model_km5 <- kmeans(sales, centers = 5)
clust_km5 <- model_km5$cluster
sales <- mutate(sales, cluster = clust_km5)
table(sales$cluster)
sales %>% group_by(cluster) %>% summarise(avg_sales = mean(sales), n = n())

############################ kmeans with classIntervals ################################

CPs$week <- as.character(CPs$week)
CPs$change_point <- as.character(CPs$change_point)
CP_only <- CPs %>% filter(week == change_point)

library(classInt)
bins <- classIntervals(CP_only$total_sales, 
                       style = "kmeans", 
                       largeN = length(CP_only$total_sales),
                       iter.max = 1000)[["brks"]]

CP_only <- CP_only %>%
  mutate(SalesGroup = cut(total_sales, 
                          breaks = bins, 
                          labels = as.character(1:(length(bins) - 1)), 
                          include.lowest = TRUE))

table(CP_only$SalesGroup)
sales_levels <- CP_only %>% select(nielsenCatSubcat, total_sales, SalesGroup)

sales_levels %>% group_by(SalesGroup) %>% summarise(avg_sales = mean(total_sales), n= n())

                                                    