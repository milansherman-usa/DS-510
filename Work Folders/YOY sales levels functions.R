#checks if packages are installed and if not, installs them
packages <- c("dplyr", "lubridate", "RODBC", "testthat", "classInt", "Ckmeans.1d.dp", "tictoc")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(dplyr)
library(lubridate)
library(RODBC)
library(testthat)
library(classInt)
library(Ckmeans.1d.dp)
library(tictoc)

getYOYData <- function() {
  #This function retrieves the appropriate data from POSMart
  #The result will be a list with 3 data frames
  #The first data frame is the location data
  #The second is the subcategory data
  #the third is the childbrand data
  
  con <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};Server=10.200.199.35,17001;Database=P_POS_MART;Trusted_Connection=Yes;")
  
  # Create temp tables to be used in final query
  queryChild_temp1 <- "DECLARE @LastCompletedWeek DATE =
  (
  SELECT MAX(FYWeekEndingDate)
  FROM dbo.DateDimension
  WHERE FYWeekEndingDate < GETDATE()
  )
  
  IF OBJECT_ID('tempdb..#ChildBrand') IS NOT NULL
  DROP TABLE #ChildBrand
  SELECT sid.NielsenChildBrand
  INTO #ChildBrand
  FROM dbo.SalesDailyItemFact sdif
  JOIN dbo.DateDimension dd ON dd.DateDimensionID = sdif.DateDimensionID
  JOIN dbo.StoreItemDimension sid ON sid.StoreItemDimensionID = sdif.StoreItemDimensionID 
  WHERE dd.FYWeekEndingDate BETWEEN DATEADD(WEEK, -51, @LastCompletedWeek) AND @LastCompletedWeek
  AND sid.NielsenChildBrand NOT IN ('Unknown','Department Sale')
  GROUP BY sid.NielsenChildBrand
  HAVING SUM(sdif.SaleAmount) >= 130000
  ORDER BY sid.NielsenChildBrand"
  
  queryChild_temp2 <- "DECLARE @LastCompletedWeek DATE =
  (
  SELECT MAX(FYWeekEndingDate)
  FROM dbo.DateDimension
  WHERE FYWeekEndingDate < GETDATE()
  )
  
  IF OBJECT_ID('tempdb..#ChildBrandSales') IS NOT NULL
	DROP TABLE #ChildBrandSales
  SELECT sid.NielsenChildBrand,
  dd.FYYearNumber,
  dd.FYWeekNumber,
  dd.FYWeekEndingDate,
  SUM(sdif.SaleAmount) AS Sales,
  COUNT(DISTINCT(ld.StoreNumber)) as StoreCount
  INTO #ChildBrandSales
  FROM dbo.SalesDailyItemFact sdif
  JOIN dbo.StoreItemDimension sid ON sid.StoreItemDimensionID = sdif.StoreItemDimensionID
  JOIN #ChildBrand ON #ChildBrand.NielsenChildBrand = sid.NielsenChildBrand
  JOIN dbo.DateDimension dd ON dd.DateDimensionID = sdif.DateDimensionID
  JOIN dbo.LocationDimension ld on ld.LocationDimensionID = sdif.LocationDimensionID
  WHERE dd.FYWeekEndingDate BETWEEN DATEADD(WEEK, -103, @LastCompletedWeek) AND @LastCompletedWeek
  GROUP BY sid.NielsenChildBrand,
  dd.FYYearNumber,
  dd.FYWeekNumber,
  dd.FYWeekEndingDate
  ORDER BY sid.NielsenChildBrand, dd.FYWeekEndingDate"  
  
  queryChild <- "SELECT c1.NielsenChildBrand,
	   c1.FYWeekEndingDate,
  SUM(c1.Sales) AS Sales,
  SUM(c2.Sales) AS PYSales,
  (SUM(c1.Sales) - SUM(c2.Sales))/NULLIF(SUM(c2.Sales), 0) AS [PercentChange],
  sum(c1.StoreCount) as 'CYStoreCount',
  sum(c2.StoreCount) as 'PYStoreCount'
  FROM #ChildBrandSales c1
  JOIN #ChildBrandSales c2
  ON c2.NielsenChildBrand = c1.NielsenChildBrand
  AND c2.FYWeekNumber = c1.FYWeekNumber
  AND c2.FYYearNumber = c1.FYYearNumber - 1
  GROUP BY c1.NielsenChildBrand,
  c1.FYWeekEndingDate
  ORDER BY c1.NielsenChildBrand,
  c1.FYWeekEndingDate"
  
  querySubcategory_temp1 <- "DECLARE @LastCompletedWeek DATE =
  (
  SELECT MAX(FYWeekEndingDate)
  FROM dbo.DateDimension
  WHERE FYWeekEndingDate < GETDATE()
  )
  IF OBJECT_ID('tempdb..#Subcategory') IS NOT NULL
  DROP TABLE #Subcategory
  
  SELECT sid.NielsenSubcategory
  INTO #Subcategory
  FROM dbo.SalesDailyItemFact sdif
  JOIN dbo.DateDimension dd ON dd.DateDimensionID = sdif.DateDimensionID
  JOIN dbo.StoreItemDimension sid ON sid.StoreItemDimensionID = sdif.StoreItemDimensionID 
  WHERE dd.FYWeekEndingDate BETWEEN DATEADD(WEEK, -51, @LastCompletedWeek) AND @LastCompletedWeek
  AND sid.NielsenSubcategory NOT IN ('Unknown','Department Sale')
  GROUP BY sid.NielsenSubcategory
  HAVING SUM(sdif.SaleAmount) >= 130000
  ORDER BY sid.NielsenSubcategory"
  
  querySubcategory_temp2 <- "DECLARE @LastCompletedWeek DATE =
  (
  SELECT MAX(FYWeekEndingDate)
  FROM dbo.DateDimension
  WHERE FYWeekEndingDate < GETDATE()
  )
  
  IF OBJECT_ID('tempdb..#SubcategorySales') IS NOT NULL
	DROP TABLE #SubcategorySales
  SELECT sid.NielsenSubcategory,
  dd.FYWeekEndingDate,
  dd.FYYearNumber,
  dd.FYWeekNumber,
  SUM(sdif.SaleAmount) AS Sales,
  COUNT(DISTINCT(ld.StoreNumber)) as StoreCount
  INTO #SubcategorySales
  FROM dbo.SalesDailyItemFact sdif
  JOIN dbo.StoreItemDimension sid ON sid.StoreItemDimensionID = sdif.StoreItemDimensionID
  JOIN #Subcategory ON #Subcategory.NielsenSubcategory = sid.NielsenSubcategory
  JOIN dbo.DateDimension dd ON dd.DateDimensionID = sdif.DateDimensionID
  JOIN dbo.LocationDimension ld on ld.LocationDimensionID = sdif.LocationDimensionID
  WHERE dd.FYWeekEndingDate BETWEEN DATEADD(WEEK, -103, @LastCompletedWeek) AND @LastCompletedWeek
  GROUP BY sid.NielsenSubcategory,
  dd.FYWeekEndingDate,
  dd.FYYearNumber,
  dd.FYWeekNumber
  ORDER BY sid.NielsenSubcategory, dd.FYWeekEndingDate"
  
  querySubcategory <- "SELECT s1.NielsenSubcategory,
	   s1.FYWeekEndingDate,
	   SUM(s1.Sales) AS Sales,
	   SUM(s2.Sales) AS PYSales,
	   (SUM(s1.Sales) - SUM(s2.Sales))/NULLIF(SUM(s2.Sales), 0) AS [PercentChange],
	   sum(s1.StoreCount) as 'CYStoreCount',
     sum(s2.StoreCount) as 'PYStoreCount'
FROM #SubcategorySales s1
JOIN #SubcategorySales s2
ON s2.NielsenSubcategory = s1.NielsenSubcategory
AND s2.FYWeekNumber = s1.FYWeekNumber
AND s2.FYYearNumber = s1.FYYearNumber - 1
GROUP BY s1.NielsenSubcategory,
         s1.FYWeekEndingDate
ORDER BY s1.NielsenSubcategory,
	     s1.FYWeekEndingDate"
  
  queryLocation_temp <- "DECLARE @LastCompletedWeek DATE =
  (
  SELECT MAX(FYWeekEndingDate)
  FROM dbo.DateDimension
  WHERE FYWeekEndingDate < GETDATE()
  )
  
  IF OBJECT_ID('tempdb..#LocationSales') IS NOT NULL
  DROP TABLE #LocationSales
  SELECT ld.DistrictName,
  CASE 
  WHEN ld.MarketDescription = 'All Other Locations' THEN (ld.MarketDescription + ' - ' + ld.DistrictName)
  ELSE ld.MarketDescription
  END AS [Market],
  ld.StoreName,
  dd.FYYearNumber,
  dd.FYWeekNumber,
  dd.FYWeekEndingDate,
  SUM(sdif.SaleAmount) AS Sales
  INTO #LocationSales
  FROM dbo.SalesDailyItemFact sdif
  JOIN dbo.LocationDimension ld ON ld.LocationDimensionID = sdif.LocationDimensionID
  JOIN dbo.StoreItemDimension sid ON sid.StoreItemDimensionID = sdif.StoreItemDimensionID
  JOIN dbo.DateDimension dd ON dd.DateDimensionID = sdif.DateDimensionID
  WHERE dd.FYWeekEndingDate BETWEEN DATEADD(WEEK, -103, @LastCompletedWeek) AND @LastCompletedWeek
  AND ld.New_Key = 1
  AND ld.Closed_Key = 3
  GROUP BY CASE
  WHEN ld.MarketDescription = 'All Other Locations' THEN
  (ld.MarketDescription + ' - ' + ld.DistrictName)
  ELSE
  ld.MarketDescription
  END,
  ld.DistrictName,
  ld.StoreName,
  dd.FYYearNumber,
  dd.FYWeekNumber,
  dd.FYWeekEndingDate
  ORDER BY ld.DistrictName,
  Market,
  ld.StoreName,
  dd.FYWeekEndingDate"
  
  queryLocation <- "SELECT l1.DistrictName,
  l1.Market,
  l1.StoreName,
  l1.FYWeekEndingDate,
  SUM(l1.Sales) AS Sales,
  SUM(l2.Sales) AS PYSales,
  (SUM(l1.Sales) - SUM(l2.Sales))/NULLIF(SUM(l2.Sales), 0) AS [PercentChange]
  FROM #LocationSales l1
  JOIN #LocationSales l2
  ON l2.DistrictName = l1.DistrictName
  AND l2.Market = l1.Market
  AND l2.StoreName = l1.StoreName
  AND l2.FYWeekNumber = l1.FYWeekNumber
  AND l2.FYYearNumber = l1.FYYearNumber - 1
  GROUP BY l1.DistrictName,
  l1.Market,
  l1.StoreName,
  l1.FYWeekEndingDate
  ORDER BY l1.DistrictName,
  l1.Market,
  l1.StoreName,
  l1.FYWeekEndingDate"    
  
  # sqlQuery(channel = con, query = prep_query)
  sqlQuery(channel = con, query = queryChild_temp1, stringsAsFactors = FALSE)
  sqlQuery(channel = con, query = queryChild_temp2, stringsAsFactors = FALSE)
  
  sqlQuery(channel = con, query = querySubcategory_temp1, stringsAsFactors = FALSE)
  sqlQuery(channel = con, query = querySubcategory_temp2, stringsAsFactors = FALSE)
  
  sqlQuery(channel = con, query = queryLocation_temp, stringsAsFactors = FALSE)
  
  child <- sqlQuery(channel = con, query = queryChild, stringsAsFactors = FALSE)
  subcategory <- sqlQuery(channel = con, query = querySubcategory, stringsAsFactors = FALSE)
  location <- sqlQuery(channel = con, query = queryLocation, stringsAsFactors = FALSE)
  odbcClose(con)
  
  return(list(location, subcategory, child))
}

NARemove <- function(df) {
  #removes NAs from the PercentChange column
  #input : data frame
  #output : data frame
  #note: df should only have three columns in this order Name, FYWeekEndingDate, Sales
  
  df1 <- df %>%
    group_by_at(1) %>%
    filter(is.na(PercentChange) == FALSE)
  return(df1)
}

remove_LT52 <- function(df) {
  #removes groups that have less than 52 weeks of data
  #input : data frame
  #output : data frame
  #note: df should only have three columns in this order Name, FYWeekEndingDate, Sales
  
  df1 <- df %>%
    group_by_at(1) %>%
    filter(n() == 52) %>%
    ungroup()
  
  return(df1)
}

cleanData <- function(df) {
  #combines the NARemove() function and the remove_LT52() function
  #removes and NAs and any groups with less than 52 weeks of data
  #input : data frame
  #output : data frame
  #note: df should only have three columns in this order Name, FYWeekEndingDate, Sales
  df <- NARemove(df)
  df <- remove_LT52(df)
  
  return(df)
}

prepDataFrame <- function(df) {
  #adds a column "Level" and takes the value of the first column name
  #Ex. If first column name = StoreName then it adds a column "Level" and gives it the value of "StoreName"
  #output: data frame with an extra column 'Name'
  df$Level <- colnames(df)[1]
  colnames(df)[1] <- 'Name'
  
  
  #move Level to first column of df, make Name a character and not a factor
  df <- df %>%
    select(Level, everything()) %>%
    mutate(Name = as.character(Name))
  
  return(df)
}

extractDataFrames <- function(list_dfs) {
  #takes the list of data frames from the getYOYData() function
  #separates the location data frame into three distinct dfs (Store, Market, Region)
  #input : list of data frames
  #output : list of data frames
  
  location_df <- list_dfs[[1]]
  
  store_df <- location_df %>%
    select(StoreName, FYWeekEndingDate, Sales, PYSales, PercentChange) %>%
    mutate(CYStoreCount = 1,
           PYStoreCount = 1)
  
  market_df <- location_df %>%
    select(Market, StoreName, FYWeekEndingDate, Sales, PYSales) %>%
    group_by(Market, FYWeekEndingDate) %>%
    summarise(Sales = sum(Sales),
              PYSales = sum(PYSales),
              CYStoreCount = n_distinct(StoreName),
              PYStoreCount = n_distinct(StoreName)) %>%
    mutate(PercentChange = (Sales - PYSales)/PYSales) %>%
    ungroup()
  
  region_df <- location_df %>%
    select(DistrictName, StoreName, FYWeekEndingDate, Sales, PYSales) %>%
    group_by(DistrictName, FYWeekEndingDate) %>%
    summarise(Sales = sum(Sales),
              PYSales = sum(PYSales),
              CYStoreCount = n_distinct(StoreName),
              PYStoreCount = n_distinct(StoreName)) %>%
    mutate(PercentChange = (Sales - PYSales)/PYSales) %>%
    ungroup()
  
  subcategory_df <- list_dfs[[2]]
  childBrand_df <- list_dfs[[3]]
  
  return(list(store_df, market_df, region_df, subcategory_df, childBrand_df))
}

makeSalesGroups <- function(df) {
  #The purpose of this function is to group by the column 'Name'
  #and get the total sales for each group in 'Name'. It uses
  #the Ckmeans.1d.dp function to determine the breaks in the data automatically
  
  #output: data frame that has an extra column denoting sales group
  df1 <- df %>%
    group_by(Name) %>%
    summarise(TotalSales = sum(Sales))
  
  df1 <- df1 %>%
    mutate(SalesGroup = Ckmeans.1d.dp(df1$TotalSales, k = c(1, 100))$cluster)
  
  final_df <- inner_join(df, df1, by = "Name")
  
  clusters <- df1 %>%
          group_by(SalesGroup) %>%
          summarise(n = n(),
                    AvgSales = mean(TotalSales),
                    MinSales = min(TotalSales),
                    MaxSales = max(TotalSales))

  list(final_df, clusters)
}

convertSmallClusters <- function(df, min_n = 5) {
  loop <- 1
  df_final <- df
  
  #group and get n counts
  n1 <- df_final %>% 
    group_by(SalesGroup) %>%
    summarize(n1 = n_distinct(Name))
  
  #this checks if any of the n counts are smaller than the min_n
  #it's used in the while loop so when there are zero n counts smaller than min_n, it exits the while loop
  min_cluster <- sum(seq(min_n - 1) %in% n1$n1) > 0
  
  while (min_cluster) {
    #group and get n counts
    n1 <- df_final %>% 
      group_by(SalesGroup) %>%
      summarize(n1 = n_distinct(Name))
    
    #if other than the first loop, remove the n count column from df_final
    if (loop > 1) {
      df_final <- df_final %>%
        subset(select = -c(n1))
    }
    
    #gets all groups that have n counts smaller than min_n
    groups <- n1 %>%
      filter(n1 < min_n) %>%
      pull(SalesGroup)
    
    #gets last group. want to collapse the last group first then repeat
    group <- last(groups)
    print(groups)
    #want to check if the group in question is the first group
    #if it is the first cluster then merge it into the second cluster (SalesGroup 1 -> SalesGroup 2)
    #if it is not the first cluster then collapse down into next cluster (SalesGroup 6 -> SalesGroup 5)
    if (group == 1) {
      df_final <- df_final %>%
        mutate(SalesGroup = ifelse(SalesGroup == group, SalesGroup + 1, SalesGroup))
    } else {
      df_final <- df_final %>%
        mutate(SalesGroup = ifelse(SalesGroup == group, SalesGroup - 1, SalesGroup))
      
      #increase loop    
      loop <- loop + 1
      
      #agg and get n counts again
      n1 <- df_final %>% 
        group_by(SalesGroup) %>%
        summarize(n1 = n_distinct(Name))
      
      #join back to data frame
      df_final <- inner_join (df_final, n1, by = "SalesGroup")
      
      #check if any of the groups are smaller than min_n
      #if yes then continue loop
      min_cluster <- sum(seq(min_n - 1) %in% n1$n1) > 0
    }
  }
  
  #clusters will most likely not be in order
  #this just renames the clusters 1 through n-clusters
  rerank <- df_final %>% 
    group_by(SalesGroup) %>%
    summarize(n1 = n_distinct(Name)) %>%
    mutate(SalesGroup_New = row_number()) %>%
    select(SalesGroup, SalesGroup_New)
  
  #join new cluster names to data frame
  df_final <- inner_join(df_final, rerank, by = "SalesGroup")
  
  #replace old SalesGroup with new
  df_final <- df_final %>%
    mutate(SalesGroup = SalesGroup_New) %>%
    select(-c(SalesGroup_New))
  
  return(df_final)
}

calculateMetrics <- function(df, type) {
  #compute z scores for each SalesGroup for every week
  #type determines how z scores are calculated
  #'location' groups by level and fyweekendingdate
  #'item' groups by salesgroup and fyweekendingdate
  
  #compute LOBF, Area, Velocity and Acceleration
  seq_week <- seq(ymd("1989-10-16") - weeks(103),ymd("1989-10-16"),by='weeks')
  df1 <- df %>%
    #group_by(Level, Name) %>%
    mutate(CYSalesPerStore = Sales/CYStoreCount,
           PYSalesPerStore = PYSales/PYStoreCount) %>%
    mutate(PercentChange_SalesPerStore = (CYSalesPerStore - PYSalesPerStore)/PYSalesPerStore) %>%
    #LOBF = as.numeric(coef(lm( c(PYSales, Sales) ~ seq_week ))[2]),
    #LOBFScaled = as.numeric(coef(lm( c(PYSales, Sales) ~ seq_week ))[2]) / mean(Sales),
    #Area = Sales - PYSales,
    #AreaScaled = (Sales - PYSales) / mean(Sales),
    #Velocity = c(NA, diff(Sales)),
    #VelocityScaled = c(NA, diff(Sales)) / mean(Sales),
    #Acceleration = c(NA, NA, diff(diff(Sales))),
    #AccelerationScaled = c(NA, NA, diff(diff(Sales))/mean(Sales))) %>%
    ungroup()
  
  if (type == "location") {
    df1 <- df1 %>%
      group_by(Level, FYWeekEndingDate) %>%
      mutate(ZScore = scale(PercentChange),
             ZScore_PerStore = scale(PercentChange_SalesPerStore)) %>%
      ungroup()
  } else if (type == "item") {
    df1 <- df1 %>%
      group_by(SalesGroup, FYWeekEndingDate) %>%
      mutate(ZScore = scale(PercentChange),
             ZScore_PerStore = scale(PercentChange_SalesPerStore)) %>%
      ungroup()
  }
  

  #this computes the recency metric
  #compute the median YOY percent change for the latest 6 weeks
  df_6 <- df1 %>%
    filter(between(FYWeekEndingDate, max(FYWeekEndingDate) - weeks(5), max(FYWeekEndingDate))) %>%
    group_by(Level, Name) %>%
    summarise(Med_PercentChange_Last6 = median(PercentChange),
              Med_PercentChange_Last6_PerStore = median(PercentChange_SalesPerStore))
  
  #compute the median YOY percent change for the 13 weeks before the latest 6 weeks
  df_13 <- df1 %>%
    filter(between(FYWeekEndingDate, max(FYWeekEndingDate) - weeks(18), max(FYWeekEndingDate) - weeks(6))) %>%
    group_by(Level, Name) %>%
    summarise(Med_PercentChange_Previous13 = median(PercentChange),
              Med_PercentChange_Previous13_PerStore = median(PercentChange_SalesPerStore))
  
  #combine the two data frames and compute the difference between the medians of the latest 6 and previous 13
  df_6_13 <- inner_join(df_6, df_13, by = c("Level", "Name")) %>%
    mutate(Diff_Med_Percent_6v13 = Med_PercentChange_Last6 - Med_PercentChange_Previous13,
           Diff_Med_Percent_6v13_PerStore = Med_PercentChange_Last6_PerStore - Med_PercentChange_Previous13_PerStore)
  
  #join the recency metric back to original data frame
  final_df <- inner_join(df1, df_6_13, by = c("Level", "Name"))
  
  return(final_df)
}

aggResults <- function(df) {
  #aggregate by Level and Name and get results
  df_results <- df %>%
    group_by(Level, Name) %>%
    summarise(AVG_PercentChange = mean(PercentChange, na.rm = TRUE),
              AVG_PercentChange_PerStore = mean(PercentChange_SalesPerStore, na.rm = TRUE),
              ZScoreMedian = median(ZScore, na.rm = TRUE),
              ZScoreMin = min(ZScore, na.rm = TRUE),
              ZScore_PerStore_Median = median(ZScore_PerStore, na.rm = TRUE),
              ZScore_PerStoreMin = min(ZScore_PerStore, na.rm = TRUE),
              Diff_Med_Percent_6v13_PerStore = min(Diff_Med_Percent_6v13_PerStore),
              Diff_Med_Percent_6v13 = min(Diff_Med_Percent_6v13)) %>%
              #LOBFScaled = max(LOBFScaled))
              # Area = sum(Area, na.rm = TRUE),
              # AreaScaled = sum(AreaScaled, na.rm = TRUE),
              # Velocity = sum(Velocity, na.rm = TRUE),
              # VelocityScaled = sum(VelocityScaled, na.rm = TRUE),
              # Acceleration = sum(Acceleration, na.rm = TRUE),
              # AccelerationScaled = sum(AccelerationScaled, na.rm = TRUE)) %>%
    ungroup()
  
  return(df_results)
}
