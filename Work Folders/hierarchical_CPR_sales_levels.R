library(dplyr)
library(ggplot2)
library(factoextra)
library(dbscan)

# Read in results from Excel sheet
CPs <- read.csv("Historical_CPs.csv")
sales <- unique(CPs$total_sales)
sales <- as.data.frame(sales)
sales_dist <- dist(sales, method = 'euclidean')

# using the complete linkage (not good)
hc_levels <- hclust(sales_dist, method = 'complete')
clusters_k5 <- cutree(hc_levels, k = 4)
sales_complete <- mutate(sales, cluster = clusters_k5)
table(sales_complete$cluster)
sales_complete %>% group_by(cluster) %>% summarise(avg_sales = mean(sales))

# using the single linkage (worse)
hc_levels <- hclust(sales_dist, method = 'single')
clusters_k5 <- cutree(hc_levels, k = 5)
sales_single <- mutate(sales, cluster = clusters_k5)
table(sales_single$cluster)

# using the average linkage (slightly better than complete)
hc_levels <- hclust(sales_dist, method = 'average')
clusters_k5 <- cutree(hc_levels, k = 5)
sales_average<- mutate(sales, cluster = clusters_k5)
table(sales_average$cluster)
