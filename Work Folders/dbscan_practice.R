.pkgs <- c("plyr", "tidyverse", "lubridate", "reshape", "ggplot2", "dbscan", "factoextra")
.inst <- lapply(.pkgs, library, character.only = TRUE)
options(scipen = 999)

iris<- iris
iris <- iris %>% select(-c(Species))
iris_scaled <- scale(iris)
iris_scaled <- as.data.frame(iris_scaled)
iris_scaled <- iris_scaled %>% mutate(distance = kNNdist(iris_scaled, 5))
ggplot(iris_scaled, aes(distance)) + geom_density()
iris_cluster <- dbscan(iris_scaled, 0.75)
fviz_cluster(iris_cluster, iris_scaled)
