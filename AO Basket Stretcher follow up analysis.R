library(dplyr)
library(tidyverse)
library(RODBC)
library(readxl)
library(odbc)
library(janitor)
library(scales)
library(formattable)

conn <-  odbcDriverConnect("Driver={ODBC Driver 17 for SQL Server};Server=ecomsql;Database=HyveeCom;Trusted_Connection=Yes;")


final_query <- "select
ord_ordID,
CAST(ord_orderdate AS DATE) AS order_date,
ord_estimatedtotal,
ord_fuelsavercardnumber
, opt_RTCMPromoCode
FROM OnlineGroceryShopping.dbo.OGS_Order o
JOIN OnlineGroceryShopping.dbo.OGS_OrderPromo p ON p.opr_ordID = o.ord_ordID
JOIN OnlineGroceryShopping.dbo.OGS_OrderPromoType pt ON pt.opt_optID = p.opr_optID
JOIN HyveeCom.dbo.Promotion on Id = pt.opt_RTCMPromoID
WHERE opt_RTCMPromoCode LIKE ('%SPOOKY%')"


promo <- sqlQuery(channel = conn, query = final_query, stringsAsFactors = FALSE, as.is = TRUE) %>% 
  mutate(ClubCardID = ord_fuelsavercardnumber,
         promo = opt_RTCMPromoCode,
         promo_sales = ord_estimatedtotal)

promo$ClubCardID <- as.numeric(promo$ClubCardID)

final_query2 <- "select
ord_ordID,
CAST(ord_orderdate AS DATE) AS order_date,
ord_estimatedtotal,
ord_fuelsavercardnumber
FROM OnlineGroceryShopping.dbo.OGS_Order o
WHERE CAST(ord_orderdate AS DATE) BETWEEN '2021-10-21' AND '2021-10-24'
AND ord_fuelsavercardnumber <> ' ' "

AO <- sqlQuery(channel = conn, query = final_query2, stringsAsFactors = FALSE, as.is = TRUE) %>% 
  mutate(ClubCardID = ord_fuelsavercardnumber,
         AO_sales = ord_estimatedtotal)

AO$ClubCardID <- as.numeric(AO$ClubCardID)
AO$AO_sales <- as.numeric(AO$AO_sales)



con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "hy-vee-dw-sql-server-p.database.windows.net",
                 Database = "hy-vee-dw-db",
                 UID = "MiSherman@hy-vee.net",
                 Authentication = "ActiveDirectoryInteractive")

target <- dbGetQuery(con, 
           "
select ClubCardID
, basket_size AS pre_basket
, tier
, promotion
, condition
, customer_type
FROM MARKETING.LOY_AOBasketStretcher")

control <- target %>%
  filter(condition == 'Control')

bad <- control[which(control$ClubCardID %in% promo$ord_fuelsavercardnumber),]
bad2 <- promo[which(!(promo$ClubCardID %in% target$ClubCardID)),]

AO2 <- left_join(target %>% filter(customer_type == 'AO'), AO %>% select(ClubCardID, AO_sales), by = "ClubCardID")
bad2 <- control[which(control$ClubCardID %in% AO$ord_fuelsavercardnumber),]

target_promo <- left_join(target %>% filter(customer_type == 'AO'), AO %>% select(ClubCardID, AO_sales), by = "ClubCardID") %>% 
  left_join(promo %>% select(ClubCardID, promo_sales), by = "ClubCardID") %>% 
  mutate(AO_flag = if_else(is.na(AO_sales), 0, 1),
         promo_flag = if_else(is.na(promo_sales),0, 1),
         AO_sales1 = if_else(is.na(AO_sales), 0, AO_sales),
         qualified = if_else((tier == '100' & AO_sales1 >= 100) |
                               (tier == '150' & AO_sales1 >= 150) |
                               (tier == '200' & AO_sales1 >= 200), 1, 0)) %>% 
  ungroup()

target_promo$promotion <- as.factor(target_promo$promotion)
target_promo$tier <- as.factor(target_promo$tier)
setwd("Q:/Business Analytics/MARKETING - OTHERS KEEP OUT/AO Analysis/AO Basket Stretcher October")
write.csv(target_promo, "qualified.csv")

target_promo <- target_promo %>% 
  mutate(condition2 = if_else((condition == 'Control' & promo_flag == 1), 'Test', condition))

summary <- target_promo %>% 
  group_by(tier, promotion, condition2) %>% 
  summarise(proportion = mean(qualified))

basket_size <- target_promo %>% 
  filter(qualified == 1) %>% 
  group_by(tier, promotion, condition) %>% 
  summarise(average_order = mean(AO_sales),
            average_basket = mean(pre_basket))


# check the effect of channel on propensity to buy for each tier

# tier = 100
glm1 <- glm(qualified ~ promotion, 
            data = target_promo %>% filter(tier == 100), 
            family = "binomial")
summary(glm1)

# tier = 150
glm2 <- glm(qualified ~ promotion, 
                    data = target_promo %>% filter(tier == 150), 
                    family = "binomial")
summary(glm2)

# tier = 200
glm3 <- glm(qualified ~ promotion, 
            data = target_promo %>% filter(tier == 200), 
            family = "binomial")
summary(glm3)

#check the effect of channel on basket size among those who purchased
lm1 <- lm(AO_sales ~ promotion,
          data = target_promo %>% filter(tier == 100 & qualified == 1))
summary(lm1)

lm2 <- lm(AO_sales ~ promotion,
          data = target_promo %>% filter(tier == 150 & qualified == 1))
summary(lm2)

lm3 <- lm(AO_sales ~ promotion,
          data = target_promo %>% filter(tier == 200 & qualified == 1))
summary(lm3)

write.csv(summary, "proportion.csv")
write.csv(basket_size, "basket_size.csv")


# item analysis

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "hy-vee-dw-sql-server-p.database.windows.net",
                 Database = "hy-vee-dw-db",
                 UID = "MiSherman@hy-vee.net",
                 Authentication = "ActiveDirectoryInteractive")


items <- dbGetQuery(con, "SELECT ClubCardID
, count(upc) As items
from pos_mart.saleslineitemfact slf
JOIN POS_MART.CustomerDimension cd ON cd.CustomerDimensionID = slf.CustomerDimensionID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionID = slf.DateDimensionID
where CheckoutTypeDimensionID = 3
and cd.CustomerDimensionID <> 1
and actualdate between '2021-10-21' and '2021-10-24'
group by ClubCardID")

items$ClubCardID <- as.numeric(items$ClubCardID)
target_items <- left_join(target_promo, items, by = "ClubCardID")

write.csv(target_items, "items.csv")

item_count <- target_items %>% 
  filter(qualified == 1) %>% 
  group_by(tier, promotion, condition) %>% 
  summarise(average_items = mean(items, na.rm = T))



lm4 <- lm(items ~ promotion,
          data = target_items %>% filter(tier == 100 & AO_flag == 1))
summary(lm4)

lm5 <- lm(items ~ promotion,
          data = target_items %>% filter(tier == 150 & AO_flag == 1))
summary(lm5)

lm6 <- lm(items ~ promotion,
          data = target_items %>% filter(tier == 200 & AO_flag == 1))
summary(lm6)


# post hoc analysis

dbGetQuery(con, 
           "
IF OBJECT_ID('tempdb..#target') IS NOT NULL
drop table #target
select cd.ClubCardID
, condition
into #target
FROM MARKETING.LOY_AOBasketStretcher a
JOIN POS_MART.CustomerDimension cd ON cd.CustomerUUID = a.CustomerUUID
where customer_type = 'AO'
")


dbGetQuery(con,
          "
DECLARE @EndPre DATE = (SELECT MAX(FYWeekEndingDate)
					   from pos_mart.DateDimension
					   where FYWeekEndingDate < '2021-10-20')

DECLARE @BeginPre DATE = (SELECT dateadd(week, -9, @EndPre))

IF OBJECT_ID('tempdb..#pre') IS NOT NULL
drop table #pre
SELECT DISTINCT 
FYWeekEndingDate
, t.ClubCardID
, condition
, SUM(SaleAmount) AS sales
, COUNT(DISTINCT BasketKey) AS baskets
, COUNT(UPC) AS items
INTO #pre
FROM  #target t
JOIN POS_MART.CustomerDimension cd ON cd.ClubCardID = t.ClubCardID
JOIN POS_MART.SalesLineItemFact slf ON cd.CustomerDimensionID = slf.CustomerDimensionID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionId = slf.DateDimensionID
WHERE ActualDate BETWEEN @BeginPre AND @EndPre
AND CheckoutTypeDimensionID = 3
GROUP BY FYWeekEndingDate, t.ClubCardID,  condition           ")

dbGetQuery(con, "
DECLARE @EndPre DATE = (SELECT MAX(FYWeekEndingDate)
					   from pos_mart.DateDimension
					   where FYWeekEndingDate < '2021-10-20')

DECLARE @BeginPre DATE = (SELECT dateadd(week, -9, @EndPre))

IF OBJECT_ID('tempdb..#pre_weeks') IS NOT NULL
drop table #pre_weeks
SELECT DISTINCT FYWeekEndingDate
INTO #pre_weeks
FROM POS_MART.DateDimension 
where fyweekendingdate BETWEEN @BeginPre AND @EndPre
           ")

dbGetQuery(con, "
        IF OBJECT_ID('tempdb..#pre_target') IS NOT NULL
drop table #pre_target
select FYWeekEndingDate
, ClubCardID
, condition
, 0 as [period]
INTO #pre_target
FROM #target 
CROSS JOIN #pre_weeks   ")

dbGetQuery(con, "
IF OBJECT_ID('tempdb..#pre_all') IS NOT NULL
drop table #pre_all
SELECT t.FYWeekEndingDate
, t.ClubCardID
, t.condition
, sales
, baskets
, items
, [period]
INTO #pre_all
FROM #pre_target t
LEFT JOIN #pre p ON p.FYWeekEndingDate = t.FYWeekEndingDate
and p.ClubCardID = t.ClubCardID
and p.condition = t.condition
           ")


dbGetQuery(con, "
           DECLARE @BeginPost DATE = (SELECT MIN(FYWeekEndingDate)
					   from pos_mart.DateDimension
					   where FYWeekEndingDate > '2021-10-25')

DECLARE @EndPost DATE = (SELECT DATEADD(WEEK, 2, @BeginPost))


IF OBJECT_ID('tempdb..#post') IS NOT NULL
drop table #post
SELECT DISTINCT 
FYWeekEndingDate
, t.ClubCardID
, condition
, SUM(SaleAmount) AS sales
, COUNT(DISTINCT BasketKey) AS baskets
, COUNT(UPC) AS items
INTO #post
FROM  #target t
JOIN POS_MART.CustomerDimension cd ON cd.ClubCardID = t.ClubCardID
JOIN POS_MART.SalesLineItemFact slf ON cd.CustomerDimensionID = slf.CustomerDimensionID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionId = slf.DateDimensionID
WHERE ActualDate BETWEEN @BeginPost AND @EndPost
AND CheckoutTypeDimensionID = 3
GROUP BY FYWeekEndingDate, t.ClubCardID,  condition ")

dbGetQuery(con, "
           DECLARE @BeginPost DATE = (SELECT MIN(FYWeekEndingDate)
					   from pos_mart.DateDimension
					   where FYWeekEndingDate > '2021-10-25')

DECLARE @EndPost DATE = (SELECT DATEADD(WEEK, 2, @BeginPost))

           IF OBJECT_ID('tempdb..#post_weeks') IS NOT NULL
drop table #post_weeks
SELECT DISTINCT FYWeekEndingDate
INTO #post_weeks
FROM POS_MART.DateDimension 
where fyweekendingdate BETWEEN @BeginPost AND @EndPost")

dbGetQuery(con, "
IF OBJECT_ID('tempdb..#post_target') IS NOT NULL
drop table #post_target
select FYWeekEndingDate
, ClubCardID
, condition
, 1 as [period]
INTO #post_target
FROM #target 
CROSS JOIN #post_weeks           ")

dbGetQuery(con, "
IF OBJECT_ID('tempdb..#post_all') IS NOT NULL
drop table #post_all
SELECT t.FYWeekEndingDate
, t.ClubCardID
, t.condition
, sales
, baskets
, items
, [period]
INTO #post_all
FROM #post_target t
LEFT JOIN #post p ON p.FYWeekEndingDate = t.FYWeekEndingDate
and p.ClubCardID = t.ClubCardID
and p.condition = t.condition           ")

pre_post<- dbGetQuery(con, "
SELECT FYWeekEndingDate
, ClubCardID
, condition
, ISNULL(sales, 0) as sales
, ISNULL(baskets, 0) as baskets
, ISNULL(items, 0) as items
, [period]
FROM #pre_all
UNION 
SELECT
FYWeekEndingDate
, ClubCardID
, condition
, ISNULL(sales, 0) as sales
, ISNULL(baskets, 0) as baskets
, ISNULL(items, 0) as items
, [period]
FROM #post_all
ORDER BY ClubCardID, FYWeekEndingDate, condition ")

pre_post$ClubCardID <-as.numeric(pre_post$ClubCardID)

diff <- inner_join(target_promo, pre_post, by = c("ClubCardID", "condition")) %>% 
  filter(qualified == 1 & (condition2 == 'Control' | promo_flag == 1) & FYWeekEndingDate > '2021-08-15') %>% 
  mutate(period2 = if_else(period == 0, 'Pre', 'Post')) %>% 
  select(FYWeekEndingDate, ClubCardID, tier, condition2, period2, sales, baskets, items, promo_flag) %>% 
  unique() %>% 
  mutate(group = case_when(condition2 == 'Control' ~ 'Control',
                           promo_flag == 1 ~ 'Redeemed')) %>% 
  select(FYWeekEndingDate, ClubCardID, tier, group, period2, sales, baskets, items) %>% 
  rename(period = period2)

diff$group <- as.factor(diff$group)

write.csv(diff, "pre_post.csv")

require(reshape2)
pre_post_all <- diff %>% 
  group_by(FYWeekEndingDate, period,tier, group) %>% 
  summarise(PotentialCustomers = n()) %>%
  dplyr::left_join(diff %>%
                     dplyr::filter(sales >0) %>%
                     group_by(FYWeekEndingDate, period,tier, group) %>%
                     dplyr::summarize(customers = n(),
                                      sales = mean(sales),
                                      baskets = mean(baskets),
                                      items = mean(items))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(period,tier, group) %>%
  dplyr::summarize(PotentialCustomers = mean(PotentialCustomers),
                   customers = mean(customers),
                   sales = mean(sales),
                   baskets = mean(baskets),
                   items = mean(items)) 

# %>%
#   melt(id.vars = c("period", "tier",  "group")) %>%
#   dcast(variable + period ~  group + tier,
#         value.var = "value") %>%
#   dplyr::select(variable,
#                 period,
#                 Control_100,
#                 Redeemed_100,
#                 Control_150,
#                 Redeemed_150,
#                 Control_200,
#                 Redeemed_200) %>%
#   dplyr::arrange(period)


#             sales = mean(sales),
#             baskets = mean(baskets),
#             items = mean(items))
# 
# pre_post_summary <- diff %>% 
#   group_by(period, tier, group) %>% 
#   summarise(customers = n(),
#             sales = mean(sales),
#             baskets = mean(baskets),
#             items = mean(items))

#sales plot

ggplot(pre_post_all %>% filter(FYWeekEndingDate <= '2021-10-17' & tier == '100'), aes(x = FYWeekEndingDate,
                           y = sales,
                           color = group)) +
  geom_line()

ggplot(pre_post_all %>% filter(FYWeekEndingDate > '2021-10-17' & tier == '100'), aes(x = FYWeekEndingDate,
                                                                        y = sales,
                                                                        color = group)) +
  geom_line()

#baskets plot

ggplot(pre_post_all %>% filter(FYWeekEndingDate <= '2021-10-17'), aes(x = FYWeekEndingDate,
                                                                        y = baskets,
                                                                        color = group)) +
  geom_line()

ggplot(pre_post_all %>% filter(FYWeekEndingDate > '2021-10-17'), aes(x = FYWeekEndingDate,
                                                                       y = baskets,
                                                                       color = group)) +
  geom_line()


#items plot

ggplot(pre_post_all %>% filter(FYWeekEndingDate <= '2021-10-17'), aes(x = FYWeekEndingDate,
                                                                        y = items,
                                                                        color = group)) +
  geom_line()

ggplot(pre_post_all %>% filter(FYWeekEndingDate > '2021-10-17'), aes(x = FYWeekEndingDate,
                                                                       y = items,
                                                                       color = group)) +
  geom_line()


# difference in differences models
sales_model <- lm(sales ~ group*period, data = diff %>% filter(tier=='200'))
summary(sales_model)

baskets_model <- lm(baskets ~ group*period, data = diff)
summary(baskets_model)

items_model <- lm(items ~ group*period, data = diff)
summary(items_model)

# RFM analysis

con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "hy-vee-dw-sql-server-p.database.windows.net",
                 Database = "hy-vee-dw-db",
                 UID = "MiSherman@hy-vee.net",
                 Authentication = "ActiveDirectoryInteractive")


customers <- dbGetQuery(con, "IF OBJECT_ID('tempdb..#target') IS NOT NULL
drop table #target
select distinct cd.ClubCardID
, condition
, gender
, generation
, FactsWeekSegment
, trupricesegment
, shopstylesegment
, MarketDescription
, DistrictName
, CASE WHEN marketdescription = 'All Other Locations' THEN 'rural'
ELSE 'urban' END as area
FROM MARKETING.LOY_AOBasketStretcher a
JOIN POS_MART.CustomerDimension cd ON cd.CustomerUUID = a.CustomerUUID
JOIN POS_MART.LocationDimension l ON l.StoreNumber = cd.FrequentStore
where customer_type = 'AO'")


dbGetQuery(con, 
           "
IF OBJECT_ID('tempdb..#target') IS NOT NULL
drop table #target
select cd.ClubCardID
into #target
FROM MARKETING.LOY_AOBasketStretcher a
JOIN POS_MART.CustomerDimension cd ON cd.CustomerUUID = a.CustomerUUID
where customer_type = 'AO'
")

AO_history <- dbGetQuery(con, "
DECLARE @StartDate date = (SELECT DATEADD(WEEK, -520, GETDATE()))

IF OBJECT_ID('tempdb..#AO') is not null
DROP TABLE #AO
SELECT distinct t.ClubCardID
, ActualDate
, BasketKey
, SaleAmount
FROM POS_MART.SalesBasketFact sbf
JOIN POS_MART.CustomerDimension cd ON cd.CustomerDimensionID = sbf.CustomerDimensionID
JOIN #target t ON t.ClubCardID = cd.ClubCardID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionID = sbf.DateDimensionID
WHERE ActualDate BETWEEN @StartDate and cast('2021-10-20' as date)
AND CheckoutTypeDimensionID = 3   ")

AO_history$BasketKey <- as.character(AO_history$BasketKey)

library(lubridate)

AO_summary <- AO_history %>% 
  group_by(ClubCardID) %>% 
  summarise(baskets = n_distinct(BasketKey),
            sales = sum(SaleAmount),
            basket_size = sales/baskets,
            basket_iqr = IQR(SaleAmount),
            min_basket = min(SaleAmount),
            max_basket = max(SaleAmount),
            first_basket = min(ActualDate),
            last_basket = max(ActualDate))

AO_summary <-  AO_summary %>% 
  mutate(elapsed.time = first_basket %--% last_basket,
            time_between = elapsed.time / ddays(1),
            elapsed.time2 = last_basket %--% '2021-10-20',
            time_since_last = elapsed.time2 / ddays(1),
         elapsed.time3 = first_basket %--% '2021-10-20',
         tenure = elapsed.time3 / ddays(1),
         frequency = baskets/time_between) %>% 
  select(-elapsed.time, -elapsed.time2, -elapsed.time3, -time_between)

AO_all <- inner_join(customers, AO_summary, by = "ClubCardID") 
AO_all$ClubCardID <- as.numeric(AO_all$ClubCardID)

final_df <- inner_join(AO_all, qualified %>% select(ClubCardID, pre_basket, tier, promotion, condition2,
                                                    AO_flag, promo_flag, qualified, AO_sales1), 
                       by = "ClubCardID")


# split into train/test
smp_size <- floor(0.70 * nrow(final_df))

set.seed(123)
train_ind <- sample(seq_len(nrow(final_df)), size = smp_size)

train <- final_df[train_ind, ]
test <- final_df[-train_ind, ]

library(gtools)
train_100 <- train %>% 
  filter(tier == '100') %>% 
  # mutate(basket_group = quantcut(baskets)) %>% 
  group_by(condition2) %>% 
  summarise(customers = n(),
            AO_pct = mean(AO_flag),
            promo_pct = mean(promo_flag),
            qual_pct = mean(qualified))

library(rstatix)
t_test(final_df %>% 
         filter(tier == '100') %>% 
         mutate(basket_group = quantcut(baskets)), 
       qualified ~ condition2,
       detailed = T)

promo <- promo %>% 
  mutate(tier2 = case_when(promo == 'SPOOKY1' ~ '100',
                          (promo == 'SPOOKY2' | promo == 'SPOOKY3') ~ '150',
                          (promo == 'SPOOKY4' | promo == 'SPOOKY5') ~ '200'),
         promotion2 = if_else(promo == 'SPOOKY3' | promo == 'SPOOKY5', 0.05, 0.1)) %>% 
  dplyr::select(ClubCardID, promo_sales, promo, tier2, promotion2)

target_check <- inner_join(target, promo, by = "ClubCardID")


mismatch <- target_check %>% 
  filter(tier != tier2 | promotion != promotion2)
mismatch$tier <- as.numeric(mismatch$tier)
mismatch$tier2 <- as.numeric(mismatch$tier2)
mismatch$promotion <- as.numeric(mismatch$promotion)
mismatch$promotion2 <- as.numeric(mismatch$promotion2)

upgrade <- mismatch %>% 
  filter(promotion2 > promotion)

downgrade <- mismatch %>% 
  filter(promotion2 < promotion)

no_stretch <- mismatch %>%
  filter(tier > tier2)

more_stretch <- mismatch %>% 
  filter(tier < tier2)
