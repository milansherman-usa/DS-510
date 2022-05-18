# extract the item dataframe and the clusters dataframe
itemdf <- item_df[[1]]
clusters <- item_df[[2]]

# create a vector of unique sales groups to index on
salesgrp <- sort(unique(itemdf$SalesGroup), decreasing = T)

# a list of dataframes by sales group
cluster_list <- vector("list", length(salesgrp))
for (i in seq_along(salesgrp)) {
  cluster_list[[i]] <- itemdf %>% filter(SalesGroup == i)
}

# starting with cluster with the highest sales (41), combine dataframes if the dateframe has less than
# 1560 rows (30 x 52 weeks)
for (i in seq_along(salesgrp)) {
  if (nrow(cluster_list[[salesgrp[i]]]) < 1560) {
    cluster_list[[salesgrp[i] - 1]] <- bind_rows(cluster_list[[salesgrp[i]-1]], cluster_list[[salesgrp[i]]])
    cluster_list[[salesgrp[i] - 1]]$SalesGroup <- (salesgrp[i] - 1)
  }
}

# remove any dataframes with less than 1560 rows (they've already been combined with other sales groups)
cluster_logical <- lapply(cluster_list, function(x) (nrow(x) >= 1560))
new_cluster_list <- cluster_list[unlist(cluster_logical)]

# make one dataframe out of all of the dataframes in the list
new_clusters <- bind_rows(new_cluster_list)

# check results
clusters <- new_clusters %>%
  group_by(SalesGroup) %>%
  summarise(n = n(),
            AvgSales = mean(TotalSales),
            MinSales = min(TotalSales),
            MaxSales = max(TotalSales),
            Diff_perc = (max(TotalSales) - min(TotalSales))/min(TotalSales))
