packages <- c("dplyr", "tidyverse", "lubridate", "RODBC", "reshape", "rlist", "RDCOMClient", "purrr", "dbscan","ggplot2", "factoextra", "classInt", "purr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
.pkgs <- c("dplyr", "tidyverse", "lubridate", "RODBC", "reshape", "rlist", "RDCOMClient", "purrr", "dbscan","ggplot2", "factoextra", "classInt", "purr")
.inst <- lapply(.pkgs, library, character.only = TRUE)

# Query data
con <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};Server=10.200.199.35,17001;Database=T_DATASCIENCE;Trusted_Connection=Yes;")
ss_query <- "SELECT * FROM StoreSimilarity"
df_ss <- sqlQuery(channel = con, query = ss_query, stringsAsFactors = FALSE)
odbcClose(con)

# clean data and create a separate dataframe for each store
ss <- df_ss %>% select(Store1, Store2, AverageScore, WeightedScore)
stores <- unique(ss$Store1)
store_list <- vector("list", 229)
for (i in 1:length(stores)) {
  store_list[[i]] <- ss %>% filter(Store1 == stores[i] | Store2 == stores[i])
}

# explore distribution of similarity scores for each store
# order each dataframe by WeightedScore descending
ranked_list <- lapply(store_list, function(x) x%>% arrange(-WeightedScore))

######################## kmeans clustering #############################

# extract column for clustering
weights <- lapply(ranked_list, function(x) x %>% select(WeightedScore))
weights <- lapply(weights, function(x) unlist(x))

above <- lapply(weights, function(x) length(x[x>.53]))
above <- unlist(above)
table(above)
length(above[above<5])
length(above[above>20])

# create 5 clusters
#fits <- lapply(weights, function(x) kmeans(x, 5))
fits <- lapply(weights, function(x) Ckmeans.1d.dp(x, k = c(3,20)))

# pick off cluster assignment and save as a vector
clusters <- lapply(fits, function(x) x$cluster)
num <- lapply(clusters, function(x) length(unique(x)))
num <- unlist(num)
table(num)

# append cluster vector to store dataframe
ranked_stores <- map2(ranked_list, clusters, function(x,y) data.frame(x,y))
ranked_stores <- lapply(ranked_stores, function(x) setnames(x, old = c("y"), new = c("cluster")))

# visually inspect clusters
for (i in 1:length(ranked_stores)) {
  mypath <- file.path("Q:", "Business Analytics", "Data Analytics", "DATA-632 Similar Stores", "plots", paste("plot_", i, ".pdf", sep = ""))
  pdf(file = mypath)
  print(ggplot(ranked_stores[[i]], aes(WeightedScore, cluster)) + geom_point())
  dev.off()
}

ggplot(ranked_stores[[3]], aes(WeightedScore, cluster)) + geom_point()

# determine the number of the cluster of most similar stores
top_clusters <- lapply(ranked_stores, function(x) x[1,5])

# collect most similar stores into a dataframe
top_stores <- map2(ranked_stores, top_clusters, function(x,z) x %>% filter(y == z))

# examine the size of the dataframes
lengths <- lapply(top_stores, function(x) nrow(x))
lengths <- unlist(lengths)
summary(lengths)

# how many have fewer than 5 similar stores?
small <- lapply(top_stores, function(x) nrow(x) < 5)
small <- unlist(small)
table(small) #0

# find the stores with fewer than 5 similar stores and use the top 5 most similar stores
for (i in 1:229) {
  if (nrow(top_stores[[i]]) < 5) {
    top_stores[[i]] <- top5[[i]]
  }
}

top5_df <- bind_rows(top5)

str(top5_df)
str(final_df)

both <- inner_join(top5_df, final_df, by = c("Store1" = "Store", "Store2" = "Similar_Stores"))

# repeat using classIntervals in order to find a unique k for each store

bins <- lapply(store_list, function(x) classIntervals(x$WeightedScore, style = "kmeans", iter.max = 1000)[["brks"]])
k <- lapply(bins, function(x) length(x))


df <- ranked_stores[[1]] %>% group_by(cluster) %>% summarise(cluster_avg = mean(WeightedScore))

#############################################################################################
library(Ckmeans.1d.dp)

x <- weights[[3]]
result <- Ckmeans.1d.dp(x, k = c(1,20))
plot(result)
k <- max(result$cluster)
plot(x, col=result$cluster, pch=result$cluster, cex=1.5,
     main="Optimal univariate clustering with k estimated",
     sub=paste("Number of clusters is estimated to be", k))
abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
legend("topleft", paste("Cluster", 1:k), col=1:k, pch=1:k, cex=1.5, bty="n")



