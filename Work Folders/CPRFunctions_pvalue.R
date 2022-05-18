# install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
.pkgs <- c("plyr", "tidyverse", "lubridate", "RODBC", "reshape", "rlist", "RVAideMemoire", "coin", "rcompanion", "RDCOMClient", "testthat", "glue")
.inst <- lapply(.pkgs, library, character.only = TRUE)
options(scipen = 999)

prodDRSyncQuery <- function() {
  con <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};Server=10.200.199.35,17001;Database=P_POS_MART;Trusted_Connection=Yes;")
  prod_con <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};Server=10.247.250.28,17001;Database=P_POS_MART;Trusted_Connection=Yes;")
  
  query <- "
  DECLARE @start INT = (SELECT MIN(DateDimensionID)
					FROM P_POS_MART.dbo.DateDimension
					WHERE FYWeekEndingDate = (SELECT MAX(FYWeekEndingDate) FROM dbo.DateDimension WHERE FYWeekEndingDate < GETDATE()))
  DECLARE @end INT = (SELECT MAX(DateDimensionID)
  						FROM P_POS_MART.dbo.DateDimension
  						WHERE FYWeekEndingDate = (SELECT MAX(FYWeekEndingDate) FROM dbo.DateDimension WHERE FYWeekEndingDate < GETDATE()))
  
  SELECT SUM(SaleAmount) AS Sales
  FROM dbo.SalesDailyItemFact
  WHERE DateDimensionID BETWEEN @start AND @end"
  
  dr_sales <- sqlQuery(channel = con, query = query)
  odbcClose(con)
  
  prod_sales <- sqlQuery(channel = prod_con, query = query)
  odbcClose(prod_con)
  
  sales_lst <- list(dr_sales, prod_sales)
  return(sales_lst)
}

getData <- function() {
  # Return dataset of total sales, new item sales, and ROM sales by NielsenCat and Subcat for General dept
  #
  # Inputs: none
  #
  # Outputs:
  # df  (data.frame of sales by Nielsen subcategory)
  #
  
  # Query data
  con <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};Server=10.200.199.35,17001;Database=P_POS_MART;Trusted_Connection=Yes;")
  
  # Create temp tables to be used in final query
  prep_query <- "
  DECLARE @EDate DATE = (SELECT MAX(FYWeekEndingDate)
  FROM dbo.DateDimension
  WHERE FYWeekEndingDate < GETDATE())
  DECLARE @BDate DATE = (SELECT MIN(ActualDate)
  FROM dbo.DateDimension
  WHERE FYWeekEndingDate = DATEADD(WEEK, -259, @EDate))
  DECLARE @PriorBDate DATE = DATEADD(WEEK, -52, @BDate)
  
  IF OBJECT_ID('tempdb..#Dates') IS NOT NULL
  DROP TABLE #Dates
  SELECT DISTINCT FYWeekEndingDate,
  DATEADD(DAY, -1, FYWeekEndingDate) AS WeekEndingDate,
  FYYearNumber,
  FYWeekNumber
  INTO #Dates
  FROM dbo.DateDimension
  WHERE ActualDate BETWEEN @PriorBDate AND @EDate

 IF OBJECT_ID('tempdb..#SalesPrep') IS NOT NULL
  DROP TABLE #SalesPrep
  SELECT i.NielsenCategory,
  i.NielsenSubcategory,
  i.NielsenCategory_Key,
  i.NielsenSubcategory_Key,
  d.FYWeekEndingDate,
  d.FYWeekNumber,
  d.FYYearNumber,
  SUM(CASE WHEN dep.POSDepartmentDimensionID = 1
  THEN s.SaleAmount
  END) AS General,
  SUM(CASE WHEN dep.POSDepartmentDimensionID <> 1
  THEN s.SaleAmount
  END) AS NonGeneral
  INTO #SalesPrep
  FROM dbo.SalesDailyItemFact s
  JOIN dbo.DateDimension d
  ON d.DateDimensionID = s.DateDimensionID
  JOIN dbo.LocationDimension l
  ON l.LocationDimensionID = s.LocationDimensionID
  JOIN dbo.POSDepartmentDimension dep
  ON dep.POSDepartmentDimensionID = s.POSDepartmentDimensionID
  JOIN dbo.StoreItemDimension i
  ON i.StoreItemDimensionID = s.StoreItemDimensionID
  JOIN #Dates da
  ON da.FYWeekEndingDate = d.FYWeekEndingDate
  WHERE l.New_Key = 1
  AND i.Category_Key NOT IN (0, 156, 241, 270, 269, 254, 85)
  AND i.SubCategory_Key NOT IN (1028, 619, 1420, 716, 481, 482, 1712, 566)
  AND i.NielsenCategory_Key > -1
  AND i.NielsenSubcategory NOT LIKE '% SEASONAL%'
  AND i.NielsenCategory <> 'SEASONAL'
  AND i.Description NOT LIKE '%seasonal%'
  GROUP BY i.NielsenCategory,
  i.NielsenSubcategory,
  i.NielsenCategory_Key,
  i.NielsenSubcategory_Key,
  d.FYWeekEndingDate,
  d.FYWeekNumber,
  d.FYYearNumber
  HAVING SUM(s.SaleAmount) > 1000


  --Keeping only subcategories with majority of sales in general
  IF OBJECT_ID('tempdb..#Sales') IS NOT NULL
  DROP TABLE #Sales;
  SELECT NielsenCategory AS nielsenCategory,
  NielsenSubcategory AS nielsenSubcategory,
  NielsenCategory_Key AS nielsenCategory_Key,
  NielsenSubcategory_Key AS nielsenSubcategory_Key,
  FYWeekEndingDate AS week,
  General + NonGeneral AS sales
  INTO #Sales
  FROM #SalesPrep
  WHERE General > NonGeneral
  
  --Calculating prior year sales
  IF OBJECT_ID('tempdb..#SalesComp') IS NOT NULL
  DROP TABLE #SalesComp;
  SELECT s1.nielsenCategory,
  s1.nielsenSubcategory,
  s1.week,
  SUM(s1.sales) AS sales,
  SUM(s2.sales) AS PYsales,
  (SUM(s1.sales) - SUM(s2.sales)) / SUM(s2.sales) AS PYchg
  INTO #SalesComp
	FROM #Sales s1
    JOIN #Sales s2
    ON s2.nielsenSubcategory_Key = s1.nielsenSubcategory_Key
    AND s2.nielsenCategory_Key = s1.nielsenCategory_Key
    AND s2.week = DATEADD(WEEK, -52, s1.week)
  GROUP BY s1.nielsenCategory,
  s1.nielsenSubcategory,
  s1.week"
  
  final_query <- "SELECT s.nielsenCategory + '-' + s.nielsenSubcategory AS nielsenCatSubcat,
  s.week,
  s.sales,
  s.PYsales,
  s.PYchg
  FROM #SalesComp s"
  
  sqlQuery(channel = con, query = prep_query)
  df <- sqlQuery(channel = con, query = final_query, stringsAsFactors = FALSE)
  odbcClose(con)
  
  return(df)
}

list_extractor <- function(df_list) {
  big_logic <- vector(length = length(df_list))
  
  big_logic <- lapply(df_list, function(x) if (nrow(x) == 52) {TRUE} else {FALSE})
  big_logic <- unlist(big_logic)
  ext_list <- df_list[big_logic]
  return(ext_list)
}

aggSales <- function(df, field) {
  #df1 <- df
  
  #firstdate <- max(df1$week)-weeks(52)
  #df1 <- df1 %>%
  #  filter(df1$week >= firstdate) 
  
  df4 <- aggregate(df$sales, by = list(df[[field]]), FUN = sum)
  names(df4)[colnames(df4)=="Group.1"] <- field
  names(df4)[colnames(df4)=="x"] <- "total_sales"
  df4$all_the_sales <- sum(df4$total_sales)
  df4 <- df4 %>%
    mutate(percent_sales = total_sales/all_the_sales)
  
  minp <- min(df4$percent_sales)
  maxp <- max(df4$percent_sales)
  
  df4$norm_percent <- (df4$percent_sales-minp)/(maxp-minp)
  
  df <- merge(df, df4, by = field)
  
  return(df)
}


createDataframes <- function(df) {
  # Subset df into 52 week dataframes
  #
  # Inputs:
  # df      (data.frame with sales data)
  #
  # Outputs:
  # df_list  (data.frame with previous and current periods indicated, only for period # of weeks)
  
  lastweek <- floor_date(max(df$week), "week")
  firstweek <- floor_date(min(df$week), "week")
  num_weeks <- length(unique(df$week)) - 51
  
  begin_date <- firstweek + weeks(51)
  
  df_list <- vector("list", length = num_weeks)
  for (i in 1:num_weeks) {
    df_list[[i]] <- subset(df, df$week <= begin_date + 7*(i-1) & df$week >= firstweek + 7*(i-1))
  }
  return(df_list)
}

createSubcatDataframes <- function(df, field) {
  # create dataframes based on specified subcategory
  #
  # Inputs:
  # field       (string of field name used for filtering)

  # Outputs:
  # list of dataframes by field
  dfs <- vector("list", length = 209)
  for (i in 1:209) {
    df_temp <- df[[i]]
    allfield <- sort(unique(df_temp[[field]]))
    dfs[[i]] <- lapply(allfield, function(x) subset(df_temp, df_temp[[field]] == x))
  }
  return(dfs)
}

assignPeriods <- function(df, current = 6) {
  # Subset df according to period and current
  #
  # Inputs:
  # df      (data.frame with sales data)
  # period  (total time period to be used for analysis, usually a subset of the dataframe)
  # current (the most recent time period to be used, compared to the previous time period, i.e., previous = period - current)
  #
  # Outputs:
  # df_short  (data.frame with previous and current periods indicated, only for period # of weeks)
  
  lastweek <- floor_date(max(df$week), "week")
  
  df$period <- if_else(df$week <= lastweek - weeks(current), "previous", "current")
  df$period <- as.factor(df$period)
  
  return(df)
}


createChgPoint <- function(df, field, test_field1, test_field2, pval = .05) {
  # Create change point list based on specified subcategory
  #
  # Inputs:
  # field       (string of field name used for filtering)
  # field_str   (string to search for in field)
  # test_field1 (string of field name used for part 1 of median test, usually %)
  # test_field2 (string of field name used for part 2 of median test, usually dollars)
  # df          (data.frame with sales data)
  # pval        (the p-value for the statistical test comparing medians)
  #
  # Outputs:
  # list of dataframes by field; the element of the list is null
  # if the field_str does not have a change point

  chg_pts <- data.frame()

    if ((mood.medtest(df[[test_field1]], df[["period"]], exact = TRUE))$p.value < pval) {
      df$pvalue_PYchg <- ((mood.medtest(df[[test_field1]], df[["period"]], exact = TRUE))$p.value)
      #print((mood.medtest(df[[test_field1]], df[["period"]], exact = TRUE))$p.value)
      if ((mood.medtest(df[[test_field2]], df[["period"]], exact = TRUE))$p.value < pval) {
        df$pvalue_sales <- ((mood.medtest(df[[test_field2]], df[["period"]], exact = TRUE))$p.value)
        #print((mood.medtest(df[[test_field2]], df[["period"]], exact = TRUE))$p.value)
        previous <- subset(df, df$period == "previous")
        current <- subset(df, df$period == "current")
        p_median <- median(previous$sales)
        c_median <- median(current$sales)
        df$effect_size <- (c_median - p_median)/sd(df$sales)
        
        effect <- abs(df$effect_size)
        
        df$norm_effect <- abs((effect-0.02676861)/(3.360491-0.02676861))
        
        df$significant <- ((df$norm_effect *0.25) + (df$norm_percent * 0.75))/1
        
        chg_pts <- bind_rows(chg_pts, df)  
      }
    }
  return(chg_pts)
}

applyChgPoints <- function(df, field, test_field1, test_field2, pval = .05) {
  # Create dataframe with change points from all field_strs
  #
  # Inputs:
  # field       (string of field name used for filtering)
  # test_field1 (string of field name used for part 1 of median test, usually %)
  # test_field2 (string of field name used for part 2 of median test, usually dollars)
  # df          (data.frame with sales data)
  # pval        (the p-value for the statistical test comparing medians)
  #
  # Outputs:
  # cpts_data   (data.frame of all change points and sales data)
  
  
  cpts_data <- sapply(df, createChgPoint, field = field, test_field1 = test_field1, test_field2 = test_field2, pval = pval)
  
  #cpts_vec <- names(unlist(sapply(cpts_data, function(x) x[2,1])))
  #cpts_df <- bind_rows(cpts_data)
  #cpts_data <- list(cpts_df, cpts_vec)
  
  return(cpts_data)
}

format_numeric_dpd <- function(x, scientific = FALSE, ...) {
  numeric_cols <- vapply(x, is.numeric, logical(1))
  x[numeric_cols] <- lapply(x[numeric_cols], format, scientific = scientific, ...)
  x
}

writeSQLTable <- function(cpr_df, table_name){
  # tdata variable is the connection to T_DATASCIENCE SQL Server
  # df variable is the combined dataframe
  # table_name is the name of the table being wrote to in SQL Server. Must be in 'quotes'.
  
  #Expected result: A dataframe that is written to the respective SQL table specified.
  
  date <- today()
  file_path <- tempfile(fileext = ".csv")
 
  write_csv(format_numeric_dpd(cpr_df), file_path)
  
  server <- "10.200.199.35"
  sql_table <- glue("T_DATASCIENCE.dbo.{table_name}")
  reject_path <- glue("\\\\corpfs-v\\Departments\\Business Analytics\\Data Analytics\\Breakout Detection Alerts\\BadRows\\{date}BadRows.bad")
  
  cmd <- glue('dwloader.exe -S {server} -M append -i "{file_path}" -fh 1 -T {sql_table} -rv 9999999999 -R "{reject_path}" -t \",\" -r \\n -W')
  shell(cmd)
  unlink(file_path)
}
