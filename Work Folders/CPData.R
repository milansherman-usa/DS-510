CPData <- function(df1, df2, field, ...) {
  # Takes a dataframe with 52 weeks of data for each CP and connects it with previous 52 weeks of data
  # for the given subcategory to generate a dataframe with 104 weeks for each CP
  #
  # Inputs:
  # df1     (dataframe with 52 weeks of data for each CP)
  # df2     (dataframe that was fed to CPR with 104 weeks data for all subcategories, not just CPs)
  # field   (unit of analysis for CPs, generally nielsenCatSubcat)
  # ...     (columns that need to be included in both dataframes, mostly sales data)
  #
  # Output:
  # df  (dataframe with 104 weeks of data for each CP)
  
  column_name <- quos(...)
  field <- enquo(field)
  
  # generate a list of subcategories with CPs
  CPs <- df1 %>%
    pull(!!field) %>%
    unique()
  
  # filter the original dataframe fed to CPR by CPs and the previous 52 weeks
  lastweek <- max(df2$week)
  df_cat_CP <- df2 %>% 
    filter(!!field %in% CPs, week <= lastweek - weeks(52)) %>%
    # get sales data for previous 52 weeks for each CP
    select(!!field, !!!column_name)
  
  # take one observation of each CP in order to get the columns needed for output, 
  # e.g., effect size, SalesGroup, etc. (note: the choice of lastweek was arbitrary and was used 
  # for convenience. This works because these measures are the same for every observation of the CP) 
  
  metrics <- df1 %>% 
    filter(week == lastweek) %>%
    # remove the CP sales data 
    select(-c(!!!column_name))
  
  # add the CP columns to the sales data
  df_data <- inner_join(df_cat_CP, metrics, by = as_name(field))
  
  # bind into a single dataframe and group by subcategory
  df <- bind_rows(df_data, df1)
  
  df <- df %>% group_by(!!field) 
  df <- as.data.frame(df)
  return(df)
}

