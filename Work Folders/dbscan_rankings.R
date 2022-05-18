# Read in results from Excel sheet
CPs <- read_excel("Q:/Business Analytics/Data Analytics/DATA-554 Change Point Ranking/Revised ranking report/Revised CPR Ranking Again Aggregated 822.xlsx")
CPs <- rename(CPs, c("Median % difference" = "diff"))
CPs <- CPs %>% select(Mean, Rank, diff)
CPs <- CPs %>% filter(!is.na(Mean))
CPs_scaled <- scale(CPs)
CPs_scaled <- as.data.frame(CPs_scaled)
CPs_scaled <- CPs_scaled %>% mutate(distance = kNNdist(CPs_scaled, 6))
ggplot(CPs_scaled, aes(distance)) + geom_density()
CPs_cluster <- dbscan(CPs_scaled, 0.75)
fviz_cluster(CPs_cluster, CPs_scaled)
CPs <- cbind(CPs, CPs_cluster[[1]])

for (i in 1:nrow(CPs)) {
  if (CPs$cluster[i] == "1") {
    CPs$rank2[i] == "1"
  }
  else if (CPs$cluster[i] == "2") {
    CPs$rank2[i] == "0"
  }
  else if (CPs$cluster[i] == "3") {
    CPs$rank2[i] == "2"
  }
  else if (CPs$cluster[i] == "4") {
    CPs$rank2[i] == "3"
  }
  else if (CPs$cluster[i] == "5") {
    CPs$rank2[i] == "4"
  }
  else CPs$rank2[i] == "0"
}
