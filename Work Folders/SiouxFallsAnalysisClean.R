library(dplyr)
library(tidyr)
library(RODBC)
library(ggplot2)
library(scales)
library(purrr)
library(survival)
library(survminer)
library(ranger)
library(ggfortify)
library(ggpubr)
library(coin)
library(rcompanion)
library(FSA)
library(lattice)
library(psych)

#read in category, loyalty files
#hvcorp11 <- odbcConnect("hvcorp11")
con <- odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};Server=10.247.250.28,17001;Database=P_SONATA;Trusted_Connection=Yes;")

weekly_sales_df <- sqlQuery(con,"
         ;with customers as (
         SELECT DISTINCT c.CustomerDimensionID, c.FactsWeekSegment_Key, oh.ControlGroup
         FROM p_Sonata.dbo.SON_AllocationOfferHistory oh
         JOIN P_POS_MART.dbo.CustomerDimension c ON c.clubcardid = cast(oh.HouseholdID as varchar(24))
         WHERE JobID = 272 --@jobID
         )
         select 
         dateadd(dd, 7 - (7 + 3 + datepart(dw, actualdate)) % 7, actualdate) WeekEnd
         , FactsWeekSegment_Key
         , ControlGroup
         , slif.CustomerDimensionID
         , sum(slif.SaleAmount-discountamount)/count(distinct slif.CustomerDimensionID) as WeeklySales
         from P_POS_MART.dbo.SalesBasketFact slif
         join P_POS_MART.dbo.DateDimension DD on DD.DateDimensionID = slif.DateDimensionID
         join customers on customers.CustomerDimensionID = slif.CustomerDimensionID
         where slif.DateDimensionID between 20190605 and 20190625
         and slif.CustomerDimensionID <> 1
         and FactsWeekSegment_Key = 1
         group by dateadd(dd, 7 - (7 + 3 + datepart(dw, actualdate)) % 7, actualdate),slif.CustomerDimensionID,FactsWeekSegment_Key, ControlGroup
         ")

######################################################################################################################################
# compare spending of test and control group during promo week

# filter to promo week
r6.19df <- weekly_sales_df %>% filter(WeekEnd == '2019-06-26')
r6.19df$ControlGroup <- as.factor(r6.19df$ControlGroup)

# boxplot with outliers removed
ggplot(r6.19df, aes(ControlGroup, WeeklySales)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = quantile(r6.19df$WeeklySales, c(0,0.8)))

# density plot of two groups
ggplot(r6.19df, aes(WeeklySales, fill = ControlGroup)) + geom_density(alpha = 0.5)

# create separate dataframes for groups
test <- r6.19df %>% filter(ControlGroup == 0)
control <- r6.19df %>% filter(ControlGroup == 1)

# look at summary stats
summary(test$WeeklySales)
summary(control$WeeklySales)


#test for normality in test week
test_sample <- as.vector(unlist(r6.19df %>% filter(ControlGroup == 0) %>% sample_n(500) %>% select(WeeklySales)))
cntrl_sample <- as.vector(unlist(r6.19df %>% filter(ControlGroup == 1) %>% sample_n(250) %>% select(WeeklySales)))
ggdensity(test_sample)
ggdensity(cntrl_sample)

#if p-value >= 0.05 then we assume the sample has come from a normal distribution
shapiro.test(test_sample)
shapiro.test(cntrl_sample)

# transform weekly sales with cube root
weekly_sales_tr <- weekly_sales_df %>% mutate(transWeeklySales = WeeklySales^(1/3))

r6.19tr <- weekly_sales_tr %>% filter(WeekEnd == '2019-06-26')
r6.19tr$ControlGroup <- as.factor(r6.19tr$ControlGroup)

test_sample <- as.vector(unlist(r6.19tr %>% filter(ControlGroup == 0) %>% sample_n(500) %>% select(transWeeklySales)))
cntrl_sample <- as.vector(unlist(r6.19tr%>% filter(ControlGroup == 1) %>% sample_n(250) %>% select(transWeeklySales)))
ggdensity(test_sample)
ggdensity(cntrl_sample)

#if p-value >= 0.05 then we assume the sample has come from a normal distribution
shapiro.test(test_sample)
shapiro.test(cntrl_sample)
# NOTE: cube root of Weekly Sales is normally distributed

# conduct two-sample t-test on transformed data  (note: by default R uses Welch's t-test which does not assume equal variances)
t.test(transWeeklySales~ControlGroup, data = r6.19tr)

#################################################################################################################################
# check test group to see if they spent more during promo week

# add 0's for customers who did not shop
customers_info <- unique(weekly_sales_df[,c('CustomerDimensionID','FactsWeekSegment_Key','ControlGroup')])
customers <- unique(customers_info$CustomerDimensionID)
WeekEnd <- unique(weekly_sales_df$WeekEnd)

cmplt_df <- expand.grid(WeekEnd = WeekEnd, CustomerDimensionID = customers)
cmplt_df <- cmplt_df %>% inner_join(customers_info, by = 'CustomerDimensionID')

weekly_sales_cmplt <- cmplt_df %>% left_join(weekly_sales_df[,c('CustomerDimensionID','WeekEnd','WeeklySales')], by= c('CustomerDimensionID','WeekEnd'))

weekly_sales_cmplt[is.na(weekly_sales_cmplt)] <- 0

# look at test group pre/post
test2 <- weekly_sales_cmplt %>% filter(WeekEnd > '2019-06-12' & ControlGroup == 0)

# to use the test below, data must grouped by time period, with customers in the same order in each group
test2 <- arrange (test2, WeekEnd)

# check stats
Summarize(WeeklySales~WeekEnd, data = test2)

# two-sample signed-rank test for paired data
# tests for whether the paired differences are different from 0; if p-value is less then 0.05, then yes
# computes differences as (time 1 - time 2)

# this version provides 95% confidence interval and estimate of median
wilcox.test(WeeklySales ~ WeekEnd, data = test2, paired = TRUE, conf.int = TRUE, conf.level = 0.95)

# this version handles 
wilcoxsign_test(WeeklySales ~ WeekEnd, data = test2)


#################################################################################################################################
# check proportion of customers who shopped by control group

r6.19cmplt <- weekly_sales_cmplt %>% filter(WeekEnd == '2019-06-26')
r6.19cmplt$ControlGroup <- as.factor(r6.19cmplt$ControlGroup)

r6.19cmplt$shopped <- if_else(r6.19cmplt$WeeklySales == 0, "No", "Yes")
r6.19cmplt$shopped <- as.factor(r6.19cmplt$shopped)

# look at number of customers who shopped by control group
table(r6.19cmplt$ControlGroup, r6.19cmplt$shopped)

# prop.test uses chi-square statistic to check for equality of proportions
# if p-value is less than 0.05, then proportions are not equal

prop.test(table(r6.19cmplt$ControlGroup, r6.19cmplt$shopped))


#check proportion of test customers who shopped by week
test2$shopped <- if_else(test2$WeeklySales == 0, "No", "Yes")
test2$shopped <- as.factor(test2$shopped)

# look at number of test customers who shopped by week
table(test2$WeekEnd, test2$shopped)

# prop.test uses chi-square statistic to check for equality of proportions
# if p-value is less than 0.05, then proportions are not equal

prop.test(table(test2$WeekEnd, test2$shopped))
