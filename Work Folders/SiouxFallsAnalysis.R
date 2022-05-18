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
pdw <- odbcConnect("PDW_PROD")


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

weekly_sales_df <- weekly_sales_df %>% mutate(transWeeklySales = WeeklySales^(1/3))

r6.19df <- weekly_sales_df %>% filter(WeekEnd == '2019-06-26')
r6.19df$ControlGroup <- as.factor(r6.19df$ControlGroup)
ggplot(r6.19df, aes(ControlGroup, WeeklySales)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = quantile(r6.19df$WeeklySales, c(0,0.8)))

ggplot(r6.19df, aes(WeeklySales, fill = ControlGroup)) + geom_density(alpha = 0.5)


# add 0's for customers who did not shop
customers_info <- unique(weekly_sales_df[,c('CustomerDimensionID','FactsWeekSegment_Key','ControlGroup')])
customers <- unique(customers_info$CustomerDimensionID)
WeekEnd <- unique(weekly_sales_df$WeekEnd)

cmplt_df <- expand.grid(WeekEnd = WeekEnd, CustomerDimensionID = customers)
cmplt_df <- cmplt_df %>% inner_join(customers_info, by = 'CustomerDimensionID')

weekly_sales_cmplt <- cmplt_df %>% left_join(weekly_sales_df[,c('CustomerDimensionID','WeekEnd','WeeklySales')], by= c('CustomerDimensionID','WeekEnd'))

weekly_sales_cmplt[is.na(weekly_sales_cmplt)] <- 0

# create dataframes for use with ggplot
test <- r6.19df %>% filter(ControlGroup == 0)
control <- r6.19df %>% filter(ControlGroup == 1)

# look at summary stats
summary(test$WeeklySales)
summary(control$WeeklySales)

# check density plots for distribution shape

ggplot(test, aes(WeeklySales)) + geom_density()
ggplot(control, aes(WeeklySales)) + geom_density()

#test for normality in test week
test_sample <- as.vector(unlist(r6.19df %>% filter(ControlGroup == 0) %>% sample_n(500) %>% select(transWeeklySales)))
cntrl_sample <- as.vector(unlist(r6.19df %>% filter(ControlGroup == 1) %>% sample_n(250) %>% select(transWeeklySales)))
ggdensity(test_sample)
ggdensity(cntrl_sample)

#if p-value >= 0.05 then we assume the sample has come from a normal distribution
shapiro.test(test_sample)
shapiro.test(cntrl_sample)

# NOTE: cube root of Weekly Sales is normally distributed

x <- as.vector(unlist(r6.19df %>% filter(ControlGroup == 0) %>% select(transWeeklySales)))
y <- as.vector(unlist(r6.19df %>% filter(ControlGroup == 1) %>% select(transWeeklySales)))

t.test(transWeeklySales~ControlGroup, data = r6.19df)

#Using Kolmogorov-Smirnov Test due to non-parametric distribution and unequal variances present between two groups
ks.test(x,y) #two-sided
ks.test(x,y,alternative="greater") #is the test sample inherently larger than control sample?

plotdf <- as.data.frame(c(x,y), col = c('x','y'))
ggplot(x) + geom_density()


#looking at difference of just the test group pre/post
pre <- as.vector(unlist(weekly_sales_df %>% filter(WeekEnd < '2019-06-26' & ControlGroup == 0) %>% select(WeeklySales)))
dur <- as.vector(unlist(weekly_sales_df %>% filter(WeekEnd == '2019-06-26' & ControlGroup == 0) %>% select(WeeklySales)))

ks.test(dur,pre)
ks.test(dur,pre, alternative = "greater")

mean(pre)
mean(dur)

# look at test group pre/post
test2 <- weekly_sales_cmplt %>% filter(WeekEnd > '2019-06-12' & ControlGroup == 0)
test2 <- arrange (test2, WeekEnd)

Time.1 <- test2$WeeklySales[test2$WeekEnd == '2019-06-19']
Time.2 <- test2$WeeklySales[test2$WeekEnd == '2019-06-26']
Difference <- Time.2 - Time.1
Summarize(WeeklySales~WeekEnd, data = test2)

wilcox.test(WeeklySales ~ WeekEnd, data = test2, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
wilcoxsign_test(WeeklySales ~ WeekEnd, data = test2)


pre <- test2 %>% filter(WeekEnd == '2019-06-19')
post <- test2 %>% filter(WeekEnd == '2019-06-26')
cmplt_df2 <- pre%>% inner_join(post, by = 'CustomerDimensionID')
cmplt_df2 <- cmplt_df2[-c(3:4,6,8:9,11)]
cmplt_df2 <- mutate(cmplt_df2, diff = WeeklySales.y - WeeklySales.x)
ggplot(cmplt_df2, aes(diff)) + geom_density()
ggplot(cmplt_df2, aes(diff)) + geom_boxplot()
summary(cmplt_df2$diff)