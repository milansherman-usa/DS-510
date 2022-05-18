# Use Change Point Recognition to find change points by Nielsen Category/Subcategory and send CSV/PDF via email
# Author: Milan Sherman & Lainey Johnson

source("Analysis/CPRFunctions_pvalue.R")

df <- getData()

# create 52 week dataframes from the entire 259 week dataset
df_list <- createDataframes(df)

# calculate normalized percent of overall sales for each subcategory
norm_list <- lapply(df_list, aggSales, "nielsenCatSubcat")

# create subcategory dataframes from each 52 week dataframes
dfs <- createSubcatDataframes(norm_list, "nielsenCatSubcat")

# make one big list of all the 52 week subcategory dataframes
big_df_list <- unlist(dfs, recursive = FALSE)

# remove any dataframes without 52 weeks
final_df_list <- list_extractor(big_df_list)

# split each 52 week dataframes into 6 and 46 week periods
df_list_periods <- lapply(final_df_list, assignPeriods)

#Run Subcategory Change Points
all_change_points <- applyChgPoints(df_list_periods, "nielsenCatSubcat", "PYchg", "sales" )

# remove any empty dataframes (dataframes are empty from the above step if there was no change point)
change_points <- list_extractor(all_change_points)

CP_ordered <- lapply(change_points, function(x) x %>% arrange(week))

CP_ordered <- bind_rows(CP_ordered)

#################################################################################
# Checking PYchg CPs

CP_ordered_PY <- lapply(CP_ordered, function(x) mutate(x, global_median = median(PYchg)))

CP_ordered_above_PY <- lapply(CP_ordered_PY, function(x) filter(x, PYchg > global_median))

previous_PY <- lapply(CP_ordered_above_PY, function(x) subset(x, x$period == "previous"))
previous_PY <- lapply(previous_PY, function(x) mutate(x, n = n()))
previous_PY <- bind_rows(previous_PY)
table(previous_PY$n)
table(previous_PY$pvalue_PYchg)



current_PY <- lapply(CP_ordered_above_PY, function(x) subset(x, x$period == "current"))
current_PY <- lapply(current_PY, function(x) mutate(x, n = n()))
current_PY <- bind_rows(current_PY)
table(current_PY$n)
table(current_PY$pvalue_PYchg)

##################################################################################
# Checking Sales CPs

CP_ordered_sales <- lapply(CP_ordered, function(x) mutate(x, global_median = median(sales)))

CP_ordered_above_sales <- lapply(CP_ordered_sales, function(x) filter(x, sales > global_median))

previous_sales <- lapply(CP_ordered_above_sales, function(x) subset(x, x$period == "previous"))
previous_sales <- lapply(previous_sales, function(x) mutate(x, n = n()))
previous_sales <- bind_rows(previous_sales)
table(previous_sales$n)
table(previous_sales$pvalue_sales)

current_sales <- lapply(CP_ordered_above_sales, function(x) subset(x, x$period == "current"))
current_sales <- lapply(current_sales, function(x) mutate(x, n = n()))
current_sales <- bind_rows(current_sales)
table(current_sales$n)
table(previous_sales$pvalue_sales)


##################################################################################
# Lainey's Solution

change_points <- lapply(change_points, function(x) mutate(x, chg_week = max(week)))

cp_df <- bind_rows(change_points)

cp_df <- cp_df %>%
  group_by(nielsenCatSubcat, chg_week) %>%
  mutate(global_med_sales = median(sales),
         global_med_chg = median(PYchg),
         above_sales = ifelse(sales > global_med_sales, 1, 0),
         above_chg = ifelse(PYchg > global_med_chg, 1, 0))

spread_mult <- function(df, key, value) { # https://community.rstudio.com/t/spread-with-multiple-value-columns/5378/2
  keyq <- enquo(key)
  valueq <- enquo(value)
  s <- quos(!!valueq)
  
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

cp_med_prop <- cp_df %>%
  group_by(nielsenCatSubcat, chg_week, period) %>%
  summarize(above_sales = sum(above_sales),
            above_chg = sum(above_chg)) %>%
  spread_mult(period, c(above_sales, above_chg))

cp_final <- cp_df %>%
  inner_join(cp_med_prop, by = c("nielsenCatSubcat", "chg_week"))

table(cp_final$current_above_chg)
table(cp_final$previous_above_chg)
table(cp_final$current_above_sales)
table(cp_final$previous_above_sales)
