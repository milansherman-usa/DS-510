
# Setup -------------------------------------------------------------------
library(tidyverse)
library(testthat)
library(lubridate)
library(RVAideMemoire)
library(Ckmeans.1d.dp)
library(classInt)
library(reshape)
library(odbc)
library(DBI)
library(rlang)



getSubcatData <- function(docker = TRUE) {
  #This function retrieves chidbrand/subcategory data from POSMart
  #The data is collected at the childbrand level and aggregated to subcategory 
  #for use with the YOY and SalesPerItem changepoints, while the unaggregated 
  #data is used to identify changepoints at the childbrand level
  if (docker == TRUE) {
    con <- dbConnect(odbc(), 
                     Driver = "ODBC Driver 17 for SQL Server", 
                     Server = "hy-vee-dw-sql-server-p.database.windows.net",
                     Database = "hy-vee-dw-db",
                     uid = P_UID,
                     pwd = P_PWD)
  } else {
    con <- dbConnect(odbc(), 
                     Driver = "ODBC Driver 17 for SQL Server", 
                     Server = "hy-vee-dw-sql-server-p.database.windows.net",
                     Database = "hy-vee-dw-db",
                     Authentication = "Azure Active Directory-Universal with MFA")
  }
  
  
  on.exit(dbDisconnect(con))

# Create temp tables to be used in final query
dbGetQuery(con, "DECLARE @EDate DATE = (
           SELECT MAX(FYWeekEndingDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate < GETDATE());

           DECLARE @BDate DATE = (
           SELECT MIN(ActualDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate)
           );
           DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate);
           DECLARE @ChildBrandCutoff INT = 130000;
           
           IF OBJECT_ID('tempdb..#Dates') IS NOT NULL
           DROP TABLE #Dates;
           SELECT DISTINCT
           FYWeekEndingDate,
           DATEADD(DAY, -1, FYWeekEndingDate) AS WeekEndingDate,
           FYYearNumber,
           FYWeekNumber
           INTO #Dates
           FROM POS_MART.DateDimension
           WHERE ActualDate
           BETWEEN @PriorBDate AND @EDate;"
           )

dbGetQuery(con, "DECLARE @EDate DATE = (
           SELECT MAX(FYWeekEndingDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate < GETDATE());

           DECLARE @BDate DATE = (
           SELECT MIN(ActualDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate)
           );
           DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate);
           DECLARE @ChildBrandCutoff INT = 130000;
           
           IF OBJECT_ID('tempdb..#ChildBrand') IS NOT NULL
           DROP TABLE #ChildBrand;
           SELECT sid.NielsenChildBrand
           INTO #ChildBrand
           FROM POS_MART.SalesDailyItemFact sdif
           JOIN POS_MART.DateDimension dd
           ON dd.DateDimensionID = sdif.DateDimensionID
           JOIN POS_MART.StoreItemDimension sid
           ON sid.StoreItemDimensionID = sdif.StoreItemDimensionID
           WHERE dd.FYWeekEndingDate BETWEEN @BDate AND @EDate
           AND sid.NielsenChildBrand NOT IN ('Unknown', 'Department Sale')
           AND Category NOT IN ('Department Sale', 'Corporate Number', 'Income')
           AND Category NOT LIKE '%don% use%'
           AND Category NOT IN ('Unknown & Store Created LU Numbers', 'Unknown & Store Created Scale Numbers', 'Fuel & Car Wash')
           AND SubCategory NOT IN ('Items Flagged to remove')
           AND Category NOT IN ('Link Code Numbers')
           AND Category NOT IN ('Customer Service')
           AND CorporateDepartment NOT LIKE '%Pharmacy%'
           AND Category NOT IN ('Lottery', 'Store Supplies', 'Pallet/Shipper/Display')
           AND SubCategory NOT IN ('Lotto', 'Lottery Tickets')
           AND Description NOT LIKE ('%Catering%')
           GROUP BY sid.NielsenChildBrand
           HAVING SUM(sdif.SaleAmount) >= @ChildBrandCutoff"
           )

dbGetQuery(con, "DECLARE @EDate DATE = (
           SELECT MAX(FYWeekEndingDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate < GETDATE());
           DECLARE @BDate DATE = (
           SELECT MIN(ActualDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate)
           );
           DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate);
           DECLARE @ChildBrandCutoff INT = 130000;
           
           IF OBJECT_ID('tempdb..#NewItemsDates') IS NOT NULL
           DROP TABLE #NewItemsDates;
           SELECT DISTINCT
           FYWeekEndingDate,
           ActualDate,
           FYYearNumber,
           FYWeekNumber
           INTO #NewItemsDates
           FROM POS_MART.DateDimension
           WHERE ActualDate
           BETWEEN @PriorBDate AND @EDate;"
           )

dbGetQuery(con, "DECLARE @EDate DATE = (
           SELECT MAX(FYWeekEndingDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate < GETDATE());
           DECLARE @BDate DATE = (
           SELECT MIN(ActualDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate)
           );
           DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate);
           DECLARE @ChildBrandCutoff INT = 130000;
           
           IF OBJECT_ID('tempdb..#NewNonSeasonal') IS NOT NULL
           DROP TABLE #NewNonSeasonal;
           SELECT i.StoreItemDimensionID,
           i.NielsenCategory,
           i.NielsenSubCategory,
           i.NielsenChildBrand,
           i.FirstSoldDate,
           i.LastSoldDate
           INTO #NewNonSeasonal
           FROM POS_MART.StoreItemDimension i
           JOIN #NewItemsDates d
           ON d.ActualDate = i.FirstSoldDate
           JOIN #ChildBrand c
           ON c.NielsenChildBrand = i.NielsenChildBrand
           WHERE 
           Category NOT IN ('Department Sale', 'Corporate Number', 'Income')
           AND Category NOT LIKE '%don% use%'
           AND Category NOT IN ('Unknown & Store Created LU Numbers', 'Unknown & Store Created Scale Numbers', 'Fuel & Car Wash')
           AND SubCategory NOT IN ('Items Flagged to remove')
           AND Category NOT IN ('Link Code Numbers')
           AND Category NOT IN ('Customer Service')
           AND CorporateDepartment NOT LIKE '%Pharmacy%'
           AND Category NOT IN ('Lottery', 'Store Supplies', 'Pallet/Shipper/Display')
           AND SubCategory NOT IN ('Lotto', 'Lottery Tickets')
           AND Description NOT LIKE ('%Catering%');"
)

dbGetQuery(con, "DECLARE @EDate DATE = (
           SELECT MAX(FYWeekEndingDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate < GETDATE());
           DECLARE @BDate DATE = (
           SELECT MIN(ActualDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate)
           );
           DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate);
           DECLARE @ChildBrandCutoff INT = 130000;
           
           IF OBJECT_ID('tempdb..#NewSales') IS NOT NULL
           DROP TABLE #NewSales;
           SELECT d.FYWeekEndingDate,
           n.NielsenCategory,
           n.NielsenSubCategory,
           n.NielsenChildBrand,
           COUNT(DISTINCT n.StoreItemDimensionID) AS NewNonSeasonalItems,
           SUM(sd.SaleAmount) AS NewSales
           INTO #NewSales
           FROM POS_MART.SalesDailyItemFact sd
           JOIN #NewNonSeasonal n
           ON n.StoreItemDimensionID = sd.StoreItemDimensionID
           JOIN POS_MART.DateDimension d
           ON d.DateDimensionID = sd.DateDimensionID
           WHERE sd.DateDimensionID
           BETWEEN CONVERT(INT, CONVERT(CHAR(8), @BDate, 112)) AND CONVERT(INT,
           CONVERT(CHAR(8), @EDate, 112)
           )
           AND n.FirstSoldDate >= DATEADD(WEEK, -52, d.ActualDate)
           GROUP BY d.FYWeekEndingDate,
           n.NielsenCategory,
           n.NielsenSubCategory,
           n.NielsenChildBrand;"
)


dbGetQuery(con, "DECLARE @EDate DATE = (
           SELECT MAX(FYWeekEndingDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate < GETDATE());
           DECLARE @BDate DATE = (
           SELECT MIN(ActualDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate)
           );
           DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate);
           DECLARE @ChildBrandCutoff INT = 130000;
           
           IF OBJECT_ID('tempdb..#Sales') IS NOT NULL
           DROP TABLE #Sales;
           SELECT i.NielsenCategory AS nielsenCategory,
           i.NielsenSubcategory AS nielsenSubcategory,
           i.NielsenChildBrand AS nielsenChildBrand,
           d.FYWeekEndingDate AS week,
           d.FYWeekNumber AS weekNum,
           d.FYYearNumber AS yearNum,
           SUM(s.SaleAmount) AS 'sales',
           COUNT(DISTINCT s.StoreItemDimensionID) AS ItemCt --Item Count is what causes query slowness - remove if we decide we don't need sales per item change points
           INTO #Sales
           FROM POS_MART.SalesDailyItemFact s
           JOIN POS_MART.DateDimension d
           ON d.DateDimensionID = s.DateDimensionID
           JOIN POS_MART.LocationDimension l
           ON l.LocationDimensionID = s.LocationDimensionID
           JOIN POS_MART.POSDepartmentDimension dep
           ON dep.POSDepartmentDimensionID = s.POSDepartmentDimensionID
           JOIN POS_MART.StoreItemDimension i
           ON i.StoreItemDimensionID = s.StoreItemDimensionID
           JOIN #Dates da
           ON da.FYWeekEndingDate = d.FYWeekEndingDate
           JOIN #ChildBrand c
           ON c.NielsenChildBrand = i.NielsenChildBrand
           WHERE l.New_Key = 1
           AND Category NOT IN ('Department Sale', 'Corporate Number', 'Income')
           AND Category NOT LIKE '%don% use%'
           AND Category NOT IN ('Unknown & Store Created LU Numbers', 'Unknown & Store Created Scale Numbers', 'Fuel & Car Wash')
           AND SubCategory NOT IN ('Items Flagged to remove')
           AND Category NOT IN ('Link Code Numbers')
           AND Category NOT IN ('Customer Service')
           AND CorporateDepartment NOT LIKE '%Pharmacy%'
           AND Category NOT IN ('Lottery', 'Store Supplies', 'Pallet/Shipper/Display')
           AND SubCategory NOT IN ('Lotto', 'Lottery Tickets')
           AND Description NOT LIKE ('%Catering%')
           GROUP BY i.NielsenCategory,
           i.NielsenSubcategory,
           i.NielsenChildBrand,
           d.FYWeekEndingDate,
           d.FYWeekNumber,
           d.FYYearNumber"
)

dbGetQuery(con, "DECLARE @EDate DATE = (
           SELECT MAX(FYWeekEndingDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate < GETDATE());
           DECLARE @BDate DATE = (
           SELECT MIN(ActualDate)
           FROM POS_MART.DateDimension
           WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate)
           );
           DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate);
           DECLARE @ChildBrandCutoff INT = 130000;
           
           IF OBJECT_ID('tempdb..#SalesComp') IS NOT NULL
           DROP TABLE #SalesComp;
           SELECT s1.nielsenCategory,
           s1.nielsenSubcategory,
           s1.nielsenChildBrand,
           s1.week,
           SUM(s1.sales) AS sales,
           SUM(s2.sales) AS PYsales,
           (SUM(s1.sales) - SUM(s2.sales)) / NULLIF(SUM(s2.sales), 0) AS PYchg,
           SUM(s1.ItemCt) AS ItemCt,
           SUM(s2.ItemCt) AS PYItemCt,
           SUM(s1.sales / NULLIF(s1.ItemCt, 0)) AS salesPerItem,
           SUM(s2.sales) / SUM(NULLIF(s2.ItemCt, 0)) AS PYsalesPerItem,
           ((SUM(s1.sales) / SUM(NULLIF(s1.ItemCt, 0))) - SUM(s2.sales / NULLIF(s2.ItemCt, 0))) / (NULLIF(SUM(s2.sales / NULLIF(s2.ItemCt, 0)), 0)) AS PYchgPerItem
           INTO #SalesComp
           FROM #Sales s1
           JOIN #Sales s2
           ON s2.nielsenSubcategory = s1.nielsenSubcategory
           AND s2.nielsenCategory = s1.nielsenCategory
           AND s2.nielsenChildBrand = s1.nielsenChildBrand
           AND s2.week = DATEADD(WEEK, -52, s1.week)
           GROUP BY s1.nielsenCategory,
           s1.nielsenSubcategory,
           s1.nielsenChildBrand,
           s1.week"
)

final_query <- dbGetQuery(con,
                          "SELECT 
                          s.nielsenCategory,
                          s.nielsenSubcategory,
                          s.nielsenChildBrand,
                          s.nielsenCategory + '-' + s.nielsenSubcategory AS nielsenCatSubcat,
                          s.nielsenChildBrand + '-' + s.nielsenCategory AS nielsenBrandCat,
                          s.week,
                          s.sales,
                          s.PYsales,
                          s.PYchg,
                          s.ItemCt,
                          s.PYItemCt,
                          s.salesPerItem,
                          s.PYsalesPerItem,
                          s.PYchgPerItem,
                          ISNULL(n.NewSales, 0) AS NewSales,
                          ISNULL(n.NewSales, 0) / NULLIF(s.sales, 0) AS NewPctOfSales,
                          ISNULL(n.NewNonSeasonalItems, 0) AS NewNonSeasonalItems,
                          ISNULL(n.NewSales / NULLIF(ISNULL(n.NewNonSeasonalItems, 0), 0), 0) AS NewSalesPerItem
                          FROM #SalesComp s
                          LEFT JOIN #NewSales n
                          ON n.NielsenCategory = s.nielsenCategory
                          AND n.NielsenSubCategory = s.nielsenSubcategory
                          AND n.NielsenChildBrand = s.NielsenChildBrand
                          AND n.FYWeekEndingDate = s.week"
  )
  
  dbDisconnect(con)
  
  return(final_query)
}

getROMData <- function(docker = TRUE) {

  # Return dataset of total sales, new item sales, and ROM sales by Nielsen category
  # and subcategory for General dept to identify ROM change points
  
  if (docker == TRUE) {
    con <- dbConnect(odbc(), 
                     Driver = "ODBC Driver 17 for SQL Server", 
                     Server = "hy-vee-dw-sql-server-p.database.windows.net",
                     Database = "hy-vee-dw-db",
                     uid = P_UID,
                     pwd = P_PWD)
  } else {
    con <- dbConnect(odbc(), 
                     Driver = "ODBC Driver 17 for SQL Server", 
                     Server = "hy-vee-dw-sql-server-p.database.windows.net",
                     Database = "hy-vee-dw-db",
                     Authentication = "ActiveDirectoryIntegrated")
  }
  
  
  on.exit(dbDisconnect(con))
  
  # Create temp tables to be used in final query
  dbGetQuery(con, "DECLARE @EDate DATE = (SELECT MAX(FYWeekEndingDate)
             FROM POS_MART.DateDimension
             WHERE FYWeekEndingDate < DATEADD(WEEK, -1, GETDATE()))
             
             DECLARE @BDate DATE = (SELECT MIN(ActualDate)
             FROM POS_MART.DateDimension
             WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate))
             
             DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate)
             
             IF OBJECT_ID('tempdb..#Dates') IS NOT NULL
             DROP TABLE #Dates
             SELECT DISTINCT FYWeekEndingDate,
             DATEADD(DAY, -1, FYWeekEndingDate) AS WeekEndingDate,
             FYYearNumber,
             FYWeekNumber
             INTO #Dates
             FROM POS_MART.DateDimension
             WHERE ActualDate BETWEEN @PriorBDate AND @EDate"
             )
  
  dbGetQuery(con, "DECLARE @EDate DATE = (SELECT MAX(FYWeekEndingDate)
             FROM POS_MART.DateDimension
             WHERE FYWeekEndingDate < DATEADD(WEEK, -1, GETDATE()))
             
             DECLARE @BDate DATE = (SELECT MIN(ActualDate)
             FROM POS_MART.DateDimension
             WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate))
             
             DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate)
             
             IF OBJECT_ID('tempdb..#UPC') IS NOT NULL
             DROP TABLE #UPC
             SELECT StoreItemDimensionID,
             LEFT(UPC, 12) AS UPC,
             NielsenCategory,
             NielsenSubcategory
             INTO #UPC
             FROM POS_MART.StoreItemDimension
             WHERE NielsenCategory NOT IN ('Unknown')
             AND NielsenSubcategory NOT IN ('Unknown')"
             )
  
  dbGetQuery(con, "DECLARE @EDate DATE = (SELECT MAX(FYWeekEndingDate)
             FROM POS_MART.DateDimension
             WHERE FYWeekEndingDate < DATEADD(WEEK, -1, GETDATE()))
             
             DECLARE @BDate DATE = (SELECT MIN(ActualDate)
             FROM POS_MART.DateDimension
             WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate))
             
             DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate)

             IF OBJECT_ID('tempdb..#NielsenSales') IS NOT NULL
             DROP TABLE #NielsenSales
             SELECT d.FYWeekEndingDate AS week,
             d.WeekEndingDate AS CWeek,
             d.FYYearNumber,
             d.FYWeekNumber,
             u.NielsenCategory AS nielsenCategory,
             u.NielsenSubcategory AS nielsenSubcategory,
             SUM(nmd.Dollars) AS sales
             INTO #NielsenSales
             FROM DIM_ODS.Nielsen_MarketData nmd
             JOIN #Dates d
             ON d.WeekEndingDate = nmd.WeekEndingDate
             JOIN #UPC u
             ON u.UPC = nmd.UPC
             WHERE nmd.Market_Display_Name = 'Hy-Vee Total xAOC Rem'
             GROUP BY d.FYWeekEndingDate,
             d.WeekEndingDate,
             d.FYYearNumber,
             d.FYWeekNumber,
             u.NielsenCategory,
             u.NielsenSubcategory"
  )
  
 dbGetQuery(con, "DECLARE @EDate DATE = (SELECT MAX(FYWeekEndingDate)
             FROM POS_MART.DateDimension
             WHERE FYWeekEndingDate < DATEADD(WEEK, -1, GETDATE()))
             
             DECLARE @BDate DATE = (SELECT MIN(ActualDate)
             FROM POS_MART.DateDimension
             WHERE FYWeekEndingDate = DATEADD(WEEK, -51, @EDate))
             
             DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate)
            
             IF OBJECT_ID('tempdb..#NielsenSalesComp') IS NOT NULL
             DROP TABLE #NielsenSalesComp
             SELECT c1.week,
             c1.FYYearNumber,
             c1.FYWeekNumber,
             c1.nielsenCategory,
             c1.nielsenSubcategory,
             c1.sales,
             c2.sales AS PYsales,
             ISNULL((c1.sales - c2.sales) / NULLIF(c2.sales, 0), 0) AS PYchg
             INTO #NielsenSalesComp
             FROM #NielsenSales c1
             JOIN #NielsenSales c2
             ---Join on cat/subcat instead of keys
             ON c2.nielsenCategory = c1.nielsenCategory
             AND c2.nielsenSubcategory = c1.nielsenSubcategory
             AND c2.week = DATEADD(WEEK, -52, c1.week)"
  )

  
  final_query <- dbGetQuery(con,
                            "SELECT nielsenCategory,
                            nielsenSubcategory,
                            nielsenCategory + '-' + nielsenSubcategory AS nielsenCatSubcat,
                            week,
                            ISNULL(sales, 0) AS sales_ROM,
                            ISNULL(PYsales, 0) AS PYsales_ROM,
                            ISNULL(PYchg, 0) AS PYchg_ROM
                            FROM #NielsenSalesComp
                            WHERE nielsenCategory <> '#N/A' AND nielsenSubcategory <> '#N/A'"
  )
  
  dbDisconnect(con)
  
  return(final_query)
}

normEffect <- function(df, field, test_field1) {
  # Calculate normalized effect size 
  #
  # Inputs:
  # subset_df   (df of change point)
  #
  # Output:
  # subset_df (df with norm_effect added)
  field <- enquo(field)
  test_field1 <- enquo(test_field1)
  df1 <- filter(df, !is.na(!!test_field1))
  
  df_sd <- df1 %>% group_by(!!field) %>% mutate(st_dev = sd(!!test_field1))
  df_med <- df_sd %>% group_by(!!field, period) %>% summarise(med = median(!!test_field1))
  df_med <- spread(df_med, period, med)
  df_fsize <- inner_join(df_sd, df_med, by = as_name(field))
  df_fsize <- df_fsize %>% mutate(effect_size = (current - previous)/st_dev)
  df_fsize <- df_fsize %>% mutate(global_effect = abs((abs(effect_size)-0.02829984)/(2.852710-0.02829984)))
  
  return(df_fsize)
}

levelEffect <- function(df) {
  # calculate effect size by SalesGroup
  #
  # Inputs:
  # df    (df of change points with global effect size)
  
  # Outputs:
  # final_df  df of change point with effect size normalized by sales level (level_effect)
  
  # find the min and max global effect size for each sales group and append to dataframe
  
  df1 <- df %>% 
    group_by(SalesGroup) %>% 
    summarise(min_effect = min(global_effect), 
              max_effect = max(global_effect))
  
  final_df <- inner_join(df, df1, by = "SalesGroup") %>%
    mutate(level_effect = (global_effect - min_effect)/(max_effect - min_effect))
  
  return(final_df)
}

columnAggregation <- function(df, ...) {
  # Aggregate sales data by different column
  # For example, queried sales by subcategory but want to aggregate by category
  #
  # Inputs:
  # df          (df of all sales data)
  # ... (field names used for grouping)
  #
  # Output:
  # df (df grouped by specified column)
  
  column_name <- quos(...)
  
  df <- df %>%
    group_by(!!!column_name, week) %>%
    summarise_if(is.numeric, sum, na.rm = FALSE) %>%
    filter(sales >= 1000) %>%
    mutate(PYchg = (sales - PYsales) / PYsales,
           PYchgPerItem = (salesPerItem - PYsalesPerItem) / PYsalesPerItem,
           NewPctOfSales = (NewSales) / (sales))
}

addColumn <- function(changepoint, changetype) {
  # Return dataframe with column for change point type and datetime
  #
  # Inputs:
  # changepoint (dataframe being modified)
  # changetype  (string of the change point type being added, i.e. "YOY") 
  #
  # Output:
  # changepoint (dataframe with column to specify the change point type)
  # If there is no change point, nothing will be returned
  
  if(nrow(changepoint) > 0){
    changepoint$changepoint <- changetype
    changepoint$datetime <- as.character(max(changepoint$week))
  }
  changepoint <- as.data.frame(changepoint)
  return(changepoint)
}

makeSalesGroups <- function(df, field, test_field2) {
  #The purpose of this function is to group by the column 'nielsenCatSubcat'
  #and get the total sales for each group in 'nielsenCatSubcat'. It uses
  #the Ckmeans.1d.dp function to determine the breaks in the data automatically
  
  #output: data frame that has an extra column denoting sales group
  field <- enquo(field)
  test_field2 <- enquo(test_field2)
  
  df1 <- df %>%
    group_by(!!field) %>%
    summarise(TotalSales = sum(!!test_field2))
  
  df1 <- df1 %>%
    mutate(SalesGroup = Ckmeans.1d.dp(df1$TotalSales, k = c(1, 100))$cluster)
  
  final_df <- inner_join(df, df1, by = as_name(field))
  
  #print(df1 %>%
  #        group_by(SalesGroup) %>%
  #        summarise(n = n(),
  #                  AvgSales = mean(TotalSales),
  #                  MinSales = min(TotalSales),
  #                  MaxSales = max(TotalSales))
  #)
  return(final_df)
}

convertSmallClusters <- function(df, field, min_n = 7) {
  field <- enquo(field)
  loop <- 1
  df_final <- df
  
  #group and get n counts
  n1 <- df_final %>% 
    group_by(SalesGroup) %>%
    summarize(n1 = n_distinct(!!field))
  
  #this checks if any of the n counts are smaller than the min_n
  #it's used in the while loop so when there are zero n counts smaller than min_n, it exits the while loop
  min_cluster <- sum(seq(min_n - 1) %in% n1$n1) > 0
  
  while (min_cluster) {
    #group and get n counts
    n1 <- df_final %>% 
      group_by(SalesGroup) %>%
      summarize(n1 = n_distinct(!!field))
    
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
        summarize(n1 = n_distinct(!!field))
      
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
    summarize(n1 = n_distinct(!!field)) %>%
    mutate(SalesGroup_New = row_number()) %>%
    select(SalesGroup, SalesGroup_New)
  
  #join new cluster names to data frame
  df_final <- inner_join(df_final, rerank, by = "SalesGroup")
  
  #replace old SalesGroup with new
  df_final <- df_final %>%
    mutate(SalesGroup = SalesGroup_New) %>%
    select(-c(SalesGroup_New))
  
  #View(df_final %>%
  #        group_by(SalesGroup) %>%
  #        summarise(n = n_distinct(nielsenCatSubcat),
  #                  AvgSales = mean(TotalSales),
  #                  MinSales = min(TotalSales),
  #                  MaxSales = max(TotalSales),
  #                  Diff_perc = (max(TotalSales) - min(TotalSales))/min(TotalSales)))
  
  return(df_final)
}

effectRank <- function(df, column) {
  column <- enquo(column)
  rank_name <- paste0(quo_name(column), "_rank")
  my_var <- pull(df, !!column)
  bins = classIntervals(my_var,
                        n = 5,
                        style = "quantile",
                        largeN = length(my_var))[["brks"]]
  
  #this will apply the breaks found to the data frame
  df <- df %>%
    mutate(!!rank_name := cut(!!column,
                              breaks = bins,
                              labels = as.character(1:(length(bins) - 1)),
                              include.lowest = TRUE))
  return(df)
}

assignPeriods <- function(df, period = 52, current = 8) {
  # Subset df according to period and current
  #
  # Inputs:
  # df      (data.frame with weekly data)
  # period  (total time period in weeks to be used for analysis)
  # current (most recent time period to be used, compared to the previous time period, i.e., previous = period - current)
  #
  # Output:
  # df_short  (data.frame with previous and current periods indicated, only for period # of weeks)

  # create two periods: current = 8 weeks, previous = 44 weeks
  lastweek <- floor_date(max(df$week), "week")
  df_short <- subset(df, df$week > lastweek - weeks(period))
  df_short$period <- if_else(df_short$week <= lastweek - weeks(current), "previous", "current")
  df_short$period <- as.factor(df_short$period)
 
  return(df_short)
}

createChgPoint <- function(field, field_str, test_field1, test_field2, df, pval = .05) {
  # Create change point list based on specified field string
  #
  # Inputs:
  # field         (string of field name used for filtering)
  # field_str     (string to search for in field)
  # test_field1   (string of field name used for part 1 of median test)
  # test_field2   (string of field name used for part 2 of median test)
  # df            (data.frame with weekly data)
  # pval          (p-value for the statistical test comparing medians)
 
  #
  # Output:
  # list of dataframes by field; the element of the list is null if the field_str does not have a change point
  #df_short <- assignPeriods(df = df, period = period, current = current)
  
  chg_pts <- data.frame()
  subset_df <- filter(df, !!sym(field) == field_str & !is.na(!!sym(test_field1)) & !!sym(test_field1) != 0)
  # check to see if filtering produced dfs with less than 52 weeks
  if (nrow(subset_df) == 52) {
    if ((mood.medtest(subset_df[[test_field1]], subset_df[["period"]], exact = TRUE))$p.value < pval) {
      #if ((mood.medtest(subset_df[[test_field2]], subset_df[["period"]], exact = TRUE))$p.value < pval) {
            chg_pts <- bind_rows(chg_pts, subset_df)
          #}
        }
      }
  return(chg_pts)
}

applyChgPoints <- function(field, test_field1, test_field2, df, pval = .05) {
  # Create dataframe with change points from all field_strs
  #
  # Inputs:
  # field         (string of field name used for filtering)
  # test_field1   (string of field name used for part 1 of median test)
  # test_field2   (string of field name used for part 2 of median test)
  # df            (data.frame with weekly data)
  # pval          (p-value for the statistical test comparing medians)

  #
  # Output:
  # cpts_data   (data.frame of all change points and weekly data)
  
  #df1 <- filter(df, !is.na(!!sym(test_field1)))
  #allfield <- unique(pull(df1, nielsenCatSubcat))
  allfield <- sort(unique(filter(df, !is.na(!!sym(test_field1)))[[field]]))
  
  cpts_data <- sapply(allfield, createChgPoint,
                      field = field,
                      test_field1 = test_field1,
                      test_field2 = test_field2,
                      df = df,
                      pval = pval)
  
  cpts_vec <- names(unlist(sapply(cpts_data, function(x) x[2,1])))
  cpts_df <- bind_rows(cpts_data)

  cpts_data <- list(cpts_df, cpts_vec)
}

runChgPoints <- function(df, field, test_field1, test_field2, period = 52, current = 8, pval = 0.05) {
  # Run all functions for change points and returns file paths
  #
  # Inputs:
  # df            (data.frame of weekly data)
  # field         (string of column name from df used for filtering)
  # test_field1   (string of field name used for part 1 of median test)
  # test_field2   (string of field name used for part 2 of median test)
  # period        (total time period in weeks to be used for analysis)
  # current       (most recent time period to be used, compared to the previous time period, i.e., previous = period - current)
  # pval          (p-value for the statistical test comparing medians)
  
  #
  # Output: cpts_data (data.frame of change points)
  
  df_short <- assignPeriods(df = df, period = period, current = current)
  cpts_data <- applyChgPoints(field = field,
                              test_field1 = test_field1,
                              test_field2 = test_field2,
                              df = df_short,
                              pval = pval)
  
  return(cpts_data[[1]])
}

updateTable_YOY <- function(df,docker = TRUE) {
  if (docker == TRUE) {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  } else {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  }
  
  
  on.exit(dbDisconnect(write_con))
  
  
  # escape any single quotes in Name with double single quotes
  df$nielsenCatSubcat <- gsub("'", "''", df$nielsenCatSubcat)
  df$nielsenCategory <- gsub("'", "''", df$nielsenCategory)
  df$nielsenSubcategory <- gsub("'", "''", df$nielsenSubcategory)
  
  chunks <- seq(from = 1, to = nrow(df), by = 1000)
  loop <- 1
  for (i in chunks) {
    if (loop != length(chunks)) {
      i2 <- i + 999
      temp_df <- df[i:i2, ]
      
      vals <- paste0("SELECT ",
                     "'", temp_df$nielsenCatSubcat, "'",
                     ", ",
                     "'", temp_df$nielsenCategory, "'",
                     ", ",
                     "'", temp_df$nielsenSubcategory, "'",
                     ", ",
                     "'", temp_df$week, "'",
                     ",",
                     temp_df$sales,
                     ", ",
                     temp_df$PYsales,
                     ", ",
                     temp_df$PYchg,
                     ", ",
                     "'", temp_df$period, "'",
                     ", ",
                     "'", temp_df$changepoint, "'",
                     ", ",
                     temp_df$effect_size,
                     ", ",
                     temp_df$global_effect,
                     ", ",
                     temp_df$level_effect,
                     ", ",
                     "'", temp_df$sales_tier,"'",
                     ", ",
                     temp_df$global_effect_rank,
                     ",",
                     temp_df$level_effect_rank,
                     ",",
                     temp_df$sig_score,
                     ",",
                     "'", temp_df$datetime, "'")
      
      vals <- paste(vals, collapse = " UNION ALL ")
      
      write_query <- paste0("INSERT INTO ", "DATASCIENCE.CPR_YOY", " ", vals)
      
      dbExecute(con = write_con, statement = write_query)
      
    } else {
      remainder <- nrow(df) - i
      i2 <- i + remainder
      
      temp_df <- df[i:i2, ]
      
      vals <- paste0("SELECT ",
                     "'", temp_df$nielsenCatSubcat, "'",
                     ", ",
                     "'", temp_df$nielsenCategory, "'",
                     ", ",
                     "'", temp_df$nielsenSubcategory, "'",
                     ", ",
                     "'", temp_df$week, "'",
                     ",",
                     temp_df$sales,
                     ", ",
                     temp_df$PYsales,
                     ", ",
                     temp_df$PYchg,
                     ", ",
                     "'", temp_df$period, "'",
                     ", ",
                     "'", temp_df$changepoint, "'",
                     ", ",
                     temp_df$effect_size,
                     ", ",
                     temp_df$global_effect,
                     ", ",
                     temp_df$level_effect,
                     ", ",
                     "'", temp_df$sales_tier,"'",
                     ", ",
                     temp_df$global_effect_rank,
                     ",",
                     temp_df$level_effect_rank,
                     ",",
                     temp_df$sig_score,
                     ",",
                     "'", temp_df$datetime, "'")
      
      vals <- paste(vals, collapse = " UNION ALL ")
      
      write_query <- paste0("INSERT INTO ", "DATASCIENCE.CPR_YOY", " ", vals)
      
      dbExecute(con = write_con, statement = write_query)
    }
    loop <- loop + 1
  }
}

writeToTable_YOY <- function(docker = TRUE) {
  #' checks if there are results from this week
  #' 
  #' @param docker boolean if this is being ran in docker
  #' @return boolean    
  if (docker == TRUE) {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  } else {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  }
  on.exit(dbDisconnect(write_con))
  
  latest_week <- floor_date(today(), unit = "week")
  
  latest_runDate <- dbGetQuery(write_con, paste0(
    "SELECT TOP (1)
    week
    FROM ", "DATASCIENCE.CPR_YOY",
    " ORDER BY week DESC;")
  )
  # if running for first time, latest_runDate will be empty
  if (nrow(latest_runDate) == 0) {
    run <- TRUE
  } else if (latest_week == latest_runDate[[1]]) {
    run <- FALSE
  } else {
    run <- TRUE
  }
  
  return(run)
}

updateTable_SalesPerItem <- function(df,docker = TRUE) {
  if (docker == TRUE) {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  } else {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  }
  
  
  on.exit(dbDisconnect(write_con))
  
  
  # escape any single quotes in Name with double single quotes
  df$nielsenCatSubcat <- gsub("'", "''", df$nielsenCatSubcat)
  df$nielsenCategory <- gsub("'", "''", df$nielsenCategory)
  df$nielsenSubcategory <- gsub("'", "''", df$nielsenSubcategory)
  
  chunks <- seq(from = 1, to = nrow(df), by = 1000)
  loop <- 1
  for (i in chunks) {
    if (loop != length(chunks)) {
      i2 <- i + 999
      temp_df <- df[i:i2, ]
      
      vals <- paste0("SELECT ",
                     "'", temp_df$nielsenCatSubcat, "'",
                     ", ",
                     "'", temp_df$nielsenCategory, "'",
                     ", ",
                     "'", temp_df$nielsenSubcategory, "'",
                     ", ",
                     "'", temp_df$week, "'",
                     ",",
                     temp_df$sales,
                     ", ",
                     temp_df$PYsales,
                     ", ",
                     temp_df$PYchg,
                     ", ",
                     temp_df$salesPerItem,
                     ", ",
                     temp_df$PYsalesPerItem,
                     ", ",
                     temp_df$PYchgPerItem,
                     ", ",
                     "'", temp_df$period, "'",
                     ", ",
                     "'", temp_df$changepoint, "'",
                     ", ",
                     temp_df$effect_size,
                     ", ",
                     temp_df$global_effect,
                     ", ",
                     temp_df$level_effect,
                     ", ",
                     "'", temp_df$sales_tier,"'",
                     ", ",
                     temp_df$global_effect_rank,
                     ",",
                     temp_df$level_effect_rank,
                     ",",
                     temp_df$sig_score,
                     ",",
                     "'", temp_df$datetime, "'")
      
      vals <- paste(vals, collapse = " UNION ALL ")
      
      write_query <- paste0("INSERT INTO ", "DATASCIENCE.CPR_SalesPerItem", " ", vals)
      
      dbExecute(con = write_con, statement = write_query)
      
    } else {
      remainder <- nrow(df) - i
      i2 <- i + remainder
      
      temp_df <- df[i:i2, ]
      
      vals <- paste0("SELECT ",
                     "'", temp_df$nielsenCatSubcat, "'",
                     ", ",
                     "'", temp_df$nielsenCategory, "'",
                     ", ",
                     "'", temp_df$nielsenSubcategory, "'",
                     ", ",
                     "'", temp_df$week, "'",
                     ",",
                     temp_df$sales,
                     ", ",
                     temp_df$PYsales,
                     ", ",
                     temp_df$PYchg,
                     ", ",
                     temp_df$salesPerItem,
                     ", ",
                     temp_df$PYsalesPerItem,
                     ", ",
                     temp_df$PYchgPerItem,
                     ", ",
                     "'", temp_df$period, "'",
                     ", ",
                     "'", temp_df$changepoint, "'",
                     ", ",
                     temp_df$effect_size,
                     ", ",
                     temp_df$global_effect,
                     ", ",
                     temp_df$level_effect,
                     ", ",
                     "'", temp_df$sales_tier,"'",
                     ", ",
                     temp_df$global_effect_rank,
                     ",",
                     temp_df$level_effect_rank,
                     ",",
                     temp_df$sig_score,
                     ",",
                     "'", temp_df$datetime, "'")
      
      vals <- paste(vals, collapse = " UNION ALL ")
      
      write_query <- paste0("INSERT INTO ", "DATASCIENCE.CPR_SalesPerItem", " ", vals)
      
      dbExecute(con = write_con, statement = write_query)
    }
    loop <- loop + 1
  }
}

writeToTable_SalesPerItem <- function(docker = TRUE) {
  #' checks if there are results from this week
  #' 
  #' @param docker boolean if this is being ran in docker
  #' @return boolean    
  if (docker == TRUE) {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  } else {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  }
  on.exit(dbDisconnect(write_con))
  
  latest_week <- floor_date(today(), unit = "week")
  
  latest_runDate <- dbGetQuery(write_con, paste0(
    "SELECT TOP (1)
    week
    FROM ", "DATASCIENCE.CPR_SalesPerItem",
    " ORDER BY week DESC;")
  )
  # if running for first time, latest_runDate will be empty
  if (nrow(latest_runDate) == 0) {
    run <- TRUE
  } else if (latest_week == latest_runDate[[1]]) {
    run <- FALSE
  } else {
    run <- TRUE
  }
  
  return(run)
}

updateTable_ROM <- function(df,docker = TRUE) {
  if (docker == TRUE) {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  } else {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  }
  
  
  on.exit(dbDisconnect(write_con))
  
  
  # escape any single quotes in Name with double single quotes
  df$nielsenCatSubcat <- gsub("'", "''", df$nielsenCatSubcat)
  df$nielsenCategory <- gsub("'", "''", df$nielsenCategory)
  df$nielsenSubcategory <- gsub("'", "''", df$nielsenSubcategory)
  
  chunks <- seq(from = 1, to = nrow(df), by = 1000)
  loop <- 1
  for (i in chunks) {
    if (loop != length(chunks)) {
      i2 <- i + 999
      temp_df <- df[i:i2, ]
      
      vals <- paste0("SELECT ",
                     "'", temp_df$nielsenCatSubcat, "'",
                     ", ",
                     "'", temp_df$nielsenCategory, "'",
                     ", ",
                     "'", temp_df$nielsenSubcategory, "'",
                     ", ",
                     "'", temp_df$week, "'",
                     ",",
                     temp_df$sales_ROM,
                     ", ",
                     temp_df$PYsales_ROM,
                     ", ",
                     temp_df$PYchg_ROM,
                     ", ",
                     "'", temp_df$period, "'",
                     ", ",
                     "'", temp_df$changepoint, "'",
                     ", ",
                     temp_df$effect_size,
                     ", ",
                     temp_df$global_effect,
                     ", ",
                     temp_df$level_effect,
                     ", ",
                     "'", temp_df$sales_tier,"'",
                     ", ",
                     temp_df$global_effect_rank,
                     ",",
                     temp_df$level_effect_rank,
                     ",",
                     temp_df$sig_score,
                     ",",
                     "'", temp_df$datetime, "'")
      
      vals <- paste(vals, collapse = " UNION ALL ")
      
      write_query <- paste0("INSERT INTO ", "DATASCIENCE.CPR_ROM", " ", vals)
      
      dbExecute(con = write_con, statement = write_query)
      
    } else {
      remainder <- nrow(df) - i
      i2 <- i + remainder
      
      temp_df <- df[i:i2, ]
      
      vals <- paste0("SELECT ",
                     "'", temp_df$nielsenCatSubcat, "'",
                     ", ",
                     "'", temp_df$nielsenCategory, "'",
                     ", ",
                     "'", temp_df$nielsenSubcategory, "'",
                     ", ",
                     "'", temp_df$week, "'",
                     ",",
                     temp_df$sales_ROM,
                     ", ",
                     temp_df$PYsales_ROM,
                     ", ",
                     temp_df$PYchg_ROM,
                     ", ",
                     "'", temp_df$period, "'",
                     ", ",
                     "'", temp_df$changepoint, "'",
                     ", ",
                     temp_df$effect_size,
                     ", ",
                     temp_df$global_effect,
                     ", ",
                     temp_df$level_effect,
                     ", ",
                     "'", temp_df$sales_tier,"'",
                     ", ",
                     temp_df$global_effect_rank,
                     ",",
                     temp_df$level_effect_rank,
                     ",",
                     temp_df$sig_score,
                     ",",
                     "'", temp_df$datetime, "'")
      
      vals <- paste(vals, collapse = " UNION ALL ")
      
      write_query <- paste0("INSERT INTO ", "DATASCIENCE.CPR_ROM", " ", vals)
      
      dbExecute(con = write_con, statement = write_query)
    }
    loop <- loop + 1
  }
}

writeToTable_ROM <- function(docker = TRUE) {
  #' checks if there are results from this week
  #' 
  #' @param docker boolean if this is being ran in docker
  #' @return boolean    
  if (docker == TRUE) {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  } else {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  }
  on.exit(dbDisconnect(write_con))
  
  latest_week <- floor_date(today(), unit = "week") - weeks(1)
  
  latest_runDate <- dbGetQuery(write_con, paste0(
    "SELECT TOP (1)
    week
    FROM ", "DATASCIENCE.CPR_ROM",
    " ORDER BY week DESC;")
  )
  # if running for first time, latest_runDate will be empty
  if (nrow(latest_runDate) == 0) {
    run <- TRUE
  } else if (latest_week == latest_runDate[[1]]) {
    run <- FALSE
  } else {
    run <- TRUE
  }
  
  return(run)
}

updateTable_childbrand <- function(df,docker = TRUE) {
  if (docker == TRUE) {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  } else {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  }
  
  
  on.exit(dbDisconnect(write_con))
  
  
  # escape any single quotes in Name with double single quotes
  df$nielsenBrandCat <- gsub("'", "''", df$nielsenBrandCat)
  df$nielsenCategory <- gsub("'", "''", df$nielsenCategory)
  df$nielsenSubcategory <- gsub("'", "''", df$nielsenSubcategory)
  
  chunks <- seq(from = 1, to = nrow(df), by = 1000)
  loop <- 1
  for (i in chunks) {
    if (loop != length(chunks)) {
      i2 <- i + 999
      temp_df <- df[i:i2, ]
      
      vals <- paste0("SELECT ",
                     "'", temp_df$nielsenCategory, "'",
                     ", ",
                     "'", temp_df$nielsenSubcategory, "'",
                     ", ",
                     "'", temp_df$nielsenBrandCat, "'",
                     ", ",
                     "'", temp_df$week, "'",
                     ",",
                     temp_df$sales,
                     ", ",
                     temp_df$PYsales,
                     ", ",
                     temp_df$PYchg,
                     ", ",
                     "'", temp_df$period, "'",
                     ", ",
                     "'", temp_df$changepoint, "'",
                     ", ",
                     temp_df$effect_size,
                     ", ",
                     temp_df$global_effect,
                     ", ",
                     temp_df$level_effect,
                     ", ",
                     "'", temp_df$sales_tier,"'",
                     ", ",
                     temp_df$global_effect_rank,
                     ",",
                     temp_df$level_effect_rank,
                     ",",
                     temp_df$sig_score,
                     ",",
                     "'", temp_df$datetime, "'")
      
      vals <- paste(vals, collapse = " UNION ALL ")
      
      write_query <- paste0("INSERT INTO ", "DATASCIENCE.CPR_childbrand", " ", vals)
      
      dbExecute(con = write_con, statement = write_query)
      
    } else {
      remainder <- nrow(df) - i
      i2 <- i + remainder
      
      temp_df <- df[i:i2, ]
      
      vals <- paste0("SELECT ",
                     "'", temp_df$nielsenCategory, "'",
                     ", ",
                     "'", temp_df$nielsenSubcategory, "'",
                     ", ",
                     "'", temp_df$nielsenBrandCat, "'",
                     ", ",
                     "'", temp_df$week, "'",
                     ",",
                     temp_df$sales,
                     ", ",
                     temp_df$PYsales,
                     ", ",
                     temp_df$PYchg,
                     ", ",
                     "'", temp_df$period, "'",
                     ", ",
                     "'", temp_df$changepoint, "'",
                     ", ",
                     temp_df$effect_size,
                     ", ",
                     temp_df$global_effect,
                     ", ",
                     temp_df$level_effect,
                     ", ",
                     "'", temp_df$sales_tier,"'",
                     ", ",
                     temp_df$global_effect_rank,
                     ",",
                     temp_df$level_effect_rank,
                     ",",
                     temp_df$sig_score,
                     ",",
                     "'", temp_df$datetime, "'")
      
      vals <- paste(vals, collapse = " UNION ALL ")
      
      write_query <- paste0("INSERT INTO ", "DATASCIENCE.CPR_childbrand", " ", vals)
      
      dbExecute(con = write_con, statement = write_query)
    }
    loop <- loop + 1
  }
}

writeToTable_childbrand <- function(docker = TRUE) {
  #' checks if there are results from this week
  #' 
  #' @param docker boolean if this is being ran in docker
  #' @return boolean    
  if (docker == TRUE) {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  } else {
    write_con <- dbConnect(odbc(), 
                           Driver = "ODBC Driver 17 for SQL Server", 
                           Server = "hy-vee-dw-sql-server-p.database.windows.net",
                           Database = "hy-vee-dw-db",
                           uid = P_UID,
                           pwd = P_PWD)
  }
  on.exit(dbDisconnect(write_con))
  
  latest_week <- floor_date(today(), unit = "week")
  
  latest_runDate <- dbGetQuery(write_con, paste0(
    "SELECT TOP (1)
    week
    FROM ", "DATASCIENCE.CPR_childbrand",
    " ORDER BY week DESC;")
  )
  # if running for first time, latest_runDate will be empty
  if (nrow(latest_runDate) == 0) {
    run <- TRUE
  } else if (latest_week == latest_runDate[[1]]) {
    run <- FALSE
  } else {
    run <- TRUE
  }
  
  return(run)
}
