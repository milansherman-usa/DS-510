library(dbscan)
library(ggplot2)
library(dplyr)
library(factoextra)

sales <- unique(CP_df$total_sales)
norm_sales <- unique(CP_df$norm_percent)
