library(dplyr)
library(ggplot2)
library(factoextra)
library(dbscan)

# Read in results from Excel sheet
CPs <- read.csv("Historical_CPs.csv")
sales <- unique(CPs$total_sales)
sales <- as.data.frame(sales)
sales_dist <- dist(sales, method = 'euclidean')
sales_knn <- sales %>% mutate(distance = kNNdist(sales_dist, 50))
kNNdistplot(sales_dist, 5)
ggplot(sales_knn, aes(distance)) + geom_density() #+ scale_x_continuous(limits = c(0, 5000),
                                                   #                    breaks = seq(0, 5000, by = 1000),
                                                    #                   labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14","15"))
sales_cluster <- dbscan(sales_knn, 2500)
fviz_cluster(sales_cluster, sales_knn)
sales <- cbind(sales, sales_cluster[[1]])

