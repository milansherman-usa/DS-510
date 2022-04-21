library(dplyr)
library(tidyverse)
library(RODBC)
library(readxl)
library(odbc)
library(janitor)
library(scales)
library(formattable)

theme_ben <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      text=element_text(size=base_size,  family="serif"),
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black"),
      # La légende
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.position="top",
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}

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


con <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "hy-vee-dw-sql-server-p.database.windows.net",
                 Database = "hy-vee-dw-db",
                 UID = "MiSherman@hy-vee.net",
                 Authentication = "ActiveDirectoryInteractive")

dbGetQuery(con, 
"
IF OBJECT_ID('tempdb..#basket_stretcher') IS NOT NULL
drop table #basket_stretcher 
select ClubCardID
, basket_size AS pre_basket
, tier
, channel
, promotion
, condition
, customer_type
INTO #basket_stretcher
FROM MARKETING.LOY_AOBasketStretcher")

dbGetQuery(con, "IF OBJECT_ID('tempdb..#sales') IS NOT NULL
drop table #sales 
SELECT ClubCardID
, SUM(SaleAmount) AS AO_sales
INTO #sales
FROM  POS_MART.SalesBasketFact sbf
JOIN POS_MART.CustomerDimension cd ON cd.CustomerDimensionID = sbf.CustomerDimensionID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionID = sbf.DateDimensionID
WHERE ActualDate BETWEEN '2021-10-21' and '2021-10-24'
AND CheckoutTypeDimensionID = 3
AND cd.CustomerDimensionID <> 1
GROUP BY ClubCardID")


target <-  dbGetQuery(con, 
"SELECT 
b.ClubCardID
, pre_basket
, tier
, CASE WHEN (channel = 'Push' OR channel = 'Both') THEN 1
ELSE 0 END AS push
, CASE WHEN (channel = 'Email' OR channel = 'Both') THEN 1
ELSE 0 END AS email
, CASE WHEN promotion = '0.1' THEN 1
ELSE 0 END AS ten_percent
, CASE WHEN promotion = '0.05' THEN 1
ELSE 0 END AS five_percent
, condition
, customer_type
, AO_sales
FROM #basket_stretcher b
LEFT JOIN #sales s ON s.ClubCardID = b.ClubCardID ")

target2 <-  dbGetQuery(con, 
                      "SELECT 
b.ClubCardID
, pre_basket
, tier
, channel 
, promotion
, condition
, customer_type
, AO_sales
FROM #basket_stretcher b
LEFT JOIN #sales s ON s.ClubCardID = b.ClubCardID ")

# post hoc data

dbGetQuery(con, 
           "
IF OBJECT_ID('tempdb..#basket_stretcher') IS NOT NULL
drop table #basket_stretcher 
select CustomerUUID
, basket_size AS pre_basket
, tier
, condition
INTO #basket_stretcher
FROM MARKETING.LOY_AOBasketStretcher ")

dbGetQuery(con, "IF OBJECT_ID('tempdb..#pre') IS NOT NULL
drop table #pre
SELECT DISTINCT b.CustomerUUID
, SUM(SaleAmount) AS sales
, COUNT(DISTINCT BasketKey) AS baskets
, COUNT(UPC) AS items
, 'pre' as [period]
INTO #pre
FROM  POS_MART.SalesLineItemFact slf
JOIN POS_MART.CustomerDimension cd ON cd.CustomerDimensionID = slf.CustomerDimensionID
JOIN #basket_stretcher b ON b.CustomerUUID = cd.CustomerUUID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionID = slf.DateDimensionID
WHERE ActualDate BETWEEN DATEADD(DAY, -60, '2021-10-20') and '2021-10-20'
AND CheckoutTypeDimensionID = 3
AND cd.CustomerDimensionID <> 1
GROUP BY b.CustomerUUID")


dbGetQuery(con, 
                      "IF OBJECT_ID('tempdb..#sales') IS NOT NULL
drop table #sales 
SELECT DISTINCT b.CustomerUUID
, SUM(SaleAmount) AS sales
, COUNT(DISTINCT BasketKey) AS baskets
, COUNT(UPC) AS items
, 'during' AS [period]
INTO #sales
FROM  POS_MART.SalesLineItemFact slf
JOIN POS_MART.CustomerDimension cd ON cd.CustomerDimensionID = slf.CustomerDimensionID
JOIN #basket_stretcher b ON b.CustomerUUID = cd.CustomerUUID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionID = slf.DateDimensionID
WHERE ActualDate BETWEEN '2021-10-21' and '2021-10-24'
AND CheckoutTypeDimensionID = 3
AND cd.CustomerDimensionID <> 1
GROUP BY b.CustomerUUID ")

dbGetQuery(con, 
"IF OBJECT_ID('tempdb..#pre_trips') IS NOT NULL
drop table #pre_trips
SELECT tier
, condition
, [period]
, COUNT(b.CustomerUUID) as customers
, SUM(sales) AS sales
, sum(baskets) AS baskets
, sum(items) AS items
INTO #pre_trips
FROM #pre p
JOIN #basket_stretcher b ON b.CustomerUUID = p.CustomerUUID
GROUP BY tier, condition, [period]")

dbGetQuery(con,
"IF OBJECT_ID('tempdb..#during_trips') IS NOT NULL
drop table #during_trips
SELECT tier
, condition
, [period]
, COUNT(b.CustomerUUID) as customers
, SUM(sales) AS sales
, sum(baskets) AS baskets
, sum(items) AS items
INTO #during_trips
FROM #sales s
JOIN #basket_stretcher b ON b.CustomerUUID = s.CustomerUUID
GROUP BY tier, condition, [period]")

summary <- dbGetQuery(con, "
SELECT tier
, condition
, [period]
, customers
, baskets/cast(customers as numeric) AS trips
, items/cast(baskets as numeric) AS items_per_trip
, sales/items AS avg_price
, sales/baskets AS basket_size
FROM #pre_trips 
UNION
SELECT tier
, condition
, [period]
, customers
, baskets/cast(customers as numeric) AS trips
, items/cast(baskets as numeric) AS items_per_trip
, sales/items AS avg_price
, sales/baskets AS basket_size
FROM #during_trips 
order by tier, [period], condition")

# setwd("Q:/Business Analytics/MARKETING - OTHERS KEEP OUT/AO Analysis/AO Basket Stretcher October")
# write.csv(summary, "post_hoc_data.csv")
# write.csv(target2, "target_data.csv")


target_promo <- left_join(target, promo %>% select(ClubCardID, promo_sales), by = "ClubCardID") %>% 
   mutate(AO_flag = if_else(is.na(AO_sales), 0, 1),
          promo_flag = if_else(is.na(promo_sales),0, 1),
          AO_sales1 = if_else(is.na(AO_sales), 0, AO_sales),
          qualified = if_else((tier == '100' & AO_sales1 >= 100) |
                                (tier == '150' & AO_sales1 >= 150) |
                                (tier == '200' & AO_sales1 >= 200), 1, 0))
            

# write.csv(target_promo, "target_promo.csv")

AO_conversion <- target_promo %>% 
  tabyl(condition, AO_flag) %>% 
  adorn_percentages()

promo_conversion <- target_promo %>% 
  tabyl(condition, promo_flag) %>% 
  adorn_percentages()

# check the effect of channel on propensity to buy for each tier

# tier = 100
glm1 <- glm(AO_flag ~ push + email + push:email, 
            data = target_promo %>% filter(tier == 100 & customer_type == 'AO'), 
            family = "binomial")
summary(glm1)

newdata <- expand.grid(push = c(0,1),
                       email = c(0,1))

preds <- predict(glm1, newdata = newdata, type = "link", se.fit = TRUE)
summary(lm1)
critval <- 1.96

upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2 <- glm1$family$linkinv(fit)
upr2 <- glm1$family$linkinv(upr)
lwr2 <- glm1$family$linkinv(lwr)

# tier = 150
glm3 <- glm(AO_flag ~ push + email + ten_percent + push:ten_percent + email:ten_percent +
              push:email + push:email:ten_percent, 
            data = target_promo %>% filter(tier == 150 & customer_type == 'AO'), 
            family = "binomial")
summary(glm3)

# tier = 200
glm5 <- glm(AO_flag ~ push*email*ten_percent, 
            data = target_promo %>% filter(tier == 200 & customer_type == 'AO'), 
            family = "binomial")
summary(glm5)

#check the effect of channel on basket size among those who purchased
lm4 <- lm(AO_sales ~ push + email + push:email,
          data = target_promo %>% filter(tier == 100 & customer_type == 'AO' & AO_flag == 1))
summary(lm4)

lm5 <- lm(AO_sales ~ push + email + ten_percent + push:ten_percent + email:ten_percent +
            push:email + push:email:ten_percent, 
          data = target_promo %>% filter(tier == 150 & customer_type == 'AO' & AO_flag == 1))
summary(lm5)

lm7 <- lm(AO_sales ~ push*email*ten_percent,
          data = target_promo %>% filter(tier == 200 & customer_type == 'AO' & AO_flag == 1))
summary(lm7)





# check the same for No AO customers

glm2 <- glm(AO_flag ~ push + email + push:email, 
            data = target_promo %>% filter(tier == 100 & customer_type != 'AO'), 
            family = "binomial")
summary(glm2)

lm5 <- lm(AO_sales ~ push + email + push:email,
          data = target_promo %>% filter(tier == 100 & customer_type != 'AO' & AO_flag == 1))
summary(lm5)


glm4 <- glm(AO_flag ~ push + email + push:email, 
            data = target_promo %>% filter(tier == 150 & customer_type != 'AO'), 
            family = "binomial")
summary(glm4)

lm6 <- lm(AO_sales ~ push + email + push:email,
          data = target_promo %>% filter(tier == 150 & customer_type != 'AO' & AO_flag == 1))
summary(lm6)


glm6 <- glm(AO_flag ~ push*email*ten_percent, 
            data = target_promo %>% filter(tier == 200 & customer_type != 'AO'), 
            family = "binomial")
summary(glm6)

lm8 <- lm(AO_sales ~ push*email*ten_percent,
          data = target_promo %>% filter(tier == 200 & customer_type != 'AO' & AO_flag == 1))
summary(lm8)


overall <- target2 %>% 
  tabyl(tier, condition)

email <- target2 %>% 
  filter(channel == 'Email') %>%
  tabyl(tier, promotion) %>% 
  mutate(channel = 'email')

push <- target2 %>% 
  filter(channel == 'Push') %>% 
  tabyl(tier, promotion) %>% 
  mutate(channel = 'push')

both <- target2 %>% 
  filter(channel == 'Both') %>% 
  tabyl(tier, promotion) %>% 
  mutate(channel = 'both')

df <- bind_rows(email, push, both)

# aggregate model

# tier 100

# conversion rate
AO_100 <- target_promo %>% 
  filter(tier == '100') %>% 
  group_by(push, email, five_percent, ten_percent) %>% 
  summarise(conversion = mean(AO_flag),
            customers = n(),
            SE = sd(AO_flag)/sqrt(customers),
            degrees.freedom = customers - 1,
            t.score = qt(p=.025, df=degrees.freedom,lower.tail=F),
            margin.error = t.score * SE,
            lower.bound = conversion - margin.error, 
            upper.bound = conversion + margin.error) %>% 
  mutate(condition = case_when(push == 0 & email == 0 & five_percent == 0 & ten_percent == 0 ~ 'control',
                               push == 0 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'email',
                               push == 1 & email == 0 & five_percent == 0 & ten_percent == 1 ~ 'push',
                               push == 1 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'both')) %>% 
  ungroup() %>% 
  select(condition, conversion, lower.bound, upper.bound)




AO_100$condition <- factor(AO_100$condition, ordered = TRUE, levels = c('email', 'push', 'both', 'control'))

AO_100 <- AO_100%>%
  mutate(conversion_pct_rd = round(conversion, 2),
         lower_pct_rd = round(lower.bound, 2),
         upper_pct_rd = round(upper.bound, 2))

AO_100$conversion_pct_rd <- percent(AO_100$conversion_pct_rd, .1)
AO_100$lower_pct_rd <- percent(AO_100$lower_pct_rd, .1)
AO_100$upper_pct_rd <- percent(AO_100$upper_pct_rd, .1)

p <- ggplot(AO_100, aes(x = condition,
                   y = conversion_pct_rd))+
  geom_text(aes(label = conversion_pct_rd, hjust = 1.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.1,.25)) +
  geom_pointrange( aes(x=condition, ymin=lower_pct_rd, ymax=upper_pct_rd), colour="red", alpha=0.9, size=1.3) +
  labs(title = paste0("$100 Basket Tier Conversion Rate Confidence Intervals"),
        # subtitle = "for $100 tier",
       x = " ",
       y = "Percent of Customers with an AO Basket",
       shape = ""
  )

p + theme_ben() 


# basket size

AO_100_sales <- target_promo %>% 
  filter(tier == '100' & AO_flag == 1) %>% 
  group_by(push, email, five_percent, ten_percent) %>% 
  summarise(spend = mean(AO_sales),
            customers = n(),
            SE = sd(AO_sales)/sqrt(customers),
            degrees.freedom = customers - 1,
            t.score = qt(p=.025, df=degrees.freedom,lower.tail=F),
            margin.error = t.score * SE,
            lower.bound = spend - margin.error,
            upper.bound = spend + margin.error) %>%
  mutate(condition = case_when(push == 0 & email == 0 & five_percent == 0 & ten_percent == 0 ~ 'control',
                               push == 0 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'email',
                               push == 1 & email == 0 & five_percent == 0 & ten_percent == 1 ~ 'push',
                               push == 1 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'both')) %>%
  ungroup() %>% 
  select(condition, spend, lower.bound, upper.bound)


AO_100_sales$spend <- currency(AO_100_sales$spend)
AO_100_sales$lower.bound <- currency(AO_100_sales$lower.bound)
AO_100_sales$upper.bound <- currency(AO_100_sales$upper.bound)
AO_100_sales$condition <- factor(AO_100_sales$condition, ordered = TRUE, levels = c('email', 'push', 'both', 'control'))

q <- ggplot(AO_100_sales, aes(x = condition,
                   y = spend))+
  geom_text(aes(label = spend, hjust = 1.5)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1), limits = c(50,100)) +
  geom_pointrange( aes(x=condition, ymin=lower.bound, ymax=upper.bound), colour="red", alpha=0.9, size=1.3) +
  labs(title = paste0("$100 Basket Tier Basket Size Confidence Intervals"),
       x = "",
       y = "Average Basket Size",
  )

q + theme_ben() 

# tier 150

# conversion rate
AO_150 <- target_promo %>% 
  filter(tier == '150') %>% 
  group_by(push, email, five_percent, ten_percent) %>% 
  summarise(conversion = mean(AO_flag),
            customers = n(),
            SE = sd(AO_flag)/sqrt(customers),
            degrees.freedom = customers - 1,
            t.score = qt(p=.025, df=degrees.freedom,lower.tail=F),
            margin.error = t.score * SE,
            lower.bound = conversion - margin.error, 
            upper.bound = conversion + margin.error) %>% 
  mutate(condition = case_when(push == 0 & email == 0 & five_percent == 0 & ten_percent == 0 ~ 'control',
                               push == 0 & email == 1 & five_percent == 1 & ten_percent == 0 ~ 'email 5%',
                               push == 0 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'email 10%',
                               push == 1 & email == 0 & five_percent == 1 & ten_percent == 0 ~ 'push 5%',
                               push == 1 & email == 0 & five_percent == 0 & ten_percent == 1 ~ 'push 10%',
                               push == 1 & email == 1 & five_percent == 1 & ten_percent == 0 ~ 'both 5%',
                               push == 1 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'both 10%')) %>% 
  ungroup() %>% 
  select(condition, conversion, lower.bound, upper.bound)


AO_150$condition <- factor(AO_150$condition, ordered = TRUE, 
                             levels = c('email 5%', 'email 10%', 'push 5%', 'push 10%','both 5%', 'both 10%','control'))

AO_150 <- AO_150%>%
  mutate(conversion_pct_rd = round(conversion, 2),
         lower_pct_rd = round(lower.bound, 2),
         upper_pct_rd = round(upper.bound, 2))

AO_150$conversion_pct_rd <- percent(AO_150$conversion_pct_rd, .1)
AO_150$lower_pct_rd <- percent(AO_150$lower_pct_rd, .1)
AO_150$upper_pct_rd <- percent(AO_150$upper_pct_rd, .1)

r <- ggplot(AO_150, aes(x = condition,
                        y = conversion_pct_rd))+
  geom_text(aes(label = conversion_pct_rd, hjust = 1.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.1,.3)) +
  geom_pointrange( aes(x=condition, ymin=lower_pct_rd, ymax=upper_pct_rd), colour="red", alpha=0.9, size=1.3) +
  labs(title = paste0("$150 Basket Tier Conversion Rate Confidence Intervals"),
       x = " ",
       y = "Percent of Customers with an AO Basket",
       shape = ""
  )

r + theme_ben() 


# basket size

AO_150_sales <- target_promo %>% 
  filter(tier == '150' & AO_flag == 1) %>% 
  group_by(push, email, five_percent, ten_percent) %>% 
  summarise(spend = mean(AO_sales),
            customers = n(),
            SE = sd(AO_sales)/sqrt(customers),
            degrees.freedom = customers - 1,
            t.score = qt(p=.025, df=degrees.freedom,lower.tail=F),
            margin.error = t.score * SE,
            lower.bound = spend - margin.error,
            upper.bound = spend + margin.error) %>%
  mutate(condition = case_when(push == 0 & email == 0 & five_percent == 0 & ten_percent == 0 ~ 'control',
                               push == 0 & email == 1 & five_percent == 1 & ten_percent == 0 ~ 'email 5%',
                               push == 0 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'email 10%',
                               push == 1 & email == 0 & five_percent == 1 & ten_percent == 0 ~ 'push 5%',
                               push == 1 & email == 0 & five_percent == 0 & ten_percent == 1 ~ 'push 10%',
                               push == 1 & email == 1 & five_percent == 1 & ten_percent == 0 ~ 'both 5%',
                               push == 1 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'both 10%')) %>% 
  ungroup() %>% 
  select(condition, spend, lower.bound, upper.bound)


AO_150_sales$spend <- currency(AO_150_sales$spend)
AO_150_sales$lower.bound <- currency(AO_150_sales$lower.bound)
AO_150_sales$upper.bound <- currency(AO_150_sales$upper.bound)

AO_150_sales$condition <- factor(AO_150_sales$condition, ordered = TRUE, 
                           levels = c('email 5%', 'email 10%', 'push 5%', 'push 10%','both 5%', 'both 10%','control'))

s <- ggplot(AO_150_sales, aes(x = condition,
                              y = spend))+
  geom_text(aes(label = spend, hjust = 1.2)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 2), limits = c(80,120)) +
  geom_pointrange( aes(x=condition, ymin=lower.bound, ymax=upper.bound), colour="red", alpha=0.9, size=1.3) +
  labs(title = paste0("$150 Basket Tier Basket Size Confidence Intervals"),
       x = "",
       y = "Average Basket Size",
  )

s + theme_ben() 

# 200 tier

# conversion rate
AO_200 <- target_promo %>% 
  filter(tier == '200') %>% 
  group_by(push, email, five_percent, ten_percent) %>% 
  summarise(conversion = mean(AO_flag),
            customers = n(),
            SE = sd(AO_flag)/sqrt(customers),
            degrees.freedom = customers - 1,
            t.score = qt(p=.025, df=degrees.freedom,lower.tail=F),
            margin.error = t.score * SE,
            lower.bound = conversion - margin.error, 
            upper.bound = conversion + margin.error) %>% 
  mutate(condition = case_when(push == 0 & email == 0 & five_percent == 0 & ten_percent == 0 ~ 'control',
                               push == 0 & email == 1 & five_percent == 1 & ten_percent == 0 ~ 'email 5%',
                               push == 0 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'email 10%',
                               push == 1 & email == 0 & five_percent == 1 & ten_percent == 0 ~ 'push 5%',
                               push == 1 & email == 0 & five_percent == 0 & ten_percent == 1 ~ 'push 10%',
                               push == 1 & email == 1 & five_percent == 1 & ten_percent == 0 ~ 'both 5%',
                               push == 1 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'both 10%')) %>% 
  ungroup() %>% 
  select(condition, conversion, lower.bound, upper.bound)


AO_200$condition <- factor(AO_200$condition, ordered = TRUE, 
                           levels = c('email 5%', 'email 10%', 'push 5%', 'push 10%','both 5%', 'both 10%','control'))

AO_200 <- AO_200%>%
  mutate(conversion_pct_rd = round(conversion, 2),
         lower_pct_rd = round(lower.bound, 2),
         upper_pct_rd = round(upper.bound, 2))

AO_200$conversion_pct_rd <- percent(AO_200$conversion_pct_rd, .1)
AO_200$lower_pct_rd <- percent(AO_200$lower_pct_rd, .1)
AO_200$upper_pct_rd <- percent(AO_200$upper_pct_rd, .1)

t <- ggplot(AO_200, aes(x = condition,
                        y = conversion_pct_rd))+
  geom_text(aes(label = conversion_pct_rd, hjust = 1.2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.1,.3)) +
  geom_pointrange( aes(x=condition, ymin=lower_pct_rd, ymax=upper_pct_rd), colour="red", alpha=0.9, size=1.3) +
  labs(title = paste0("$200 Basket Tier Conversion Rate Confidence Intervals"),
       x = " ",
       y = "Percent of Customers with an AO Basket",
       shape = ""
  )

t + theme_ben() 


# basket size

AO_200_sales <- target_promo %>% 
  filter(tier == '200' & AO_flag == 1) %>% 
  group_by(push, email, five_percent, ten_percent) %>% 
  summarise(spend = mean(AO_sales),
            customers = n(),
            SE = sd(AO_sales)/sqrt(customers),
            degrees.freedom = customers - 1,
            t.score = qt(p=.025, df=degrees.freedom,lower.tail=F),
            margin.error = t.score * SE,
            lower.bound = spend - margin.error,
            upper.bound = spend + margin.error) %>%
  mutate(condition = case_when(push == 0 & email == 0 & five_percent == 0 & ten_percent == 0 ~ 'control',
                               push == 0 & email == 1 & five_percent == 1 & ten_percent == 0 ~ 'email 5%',
                               push == 0 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'email 10%',
                               push == 1 & email == 0 & five_percent == 1 & ten_percent == 0 ~ 'push 5%',
                               push == 1 & email == 0 & five_percent == 0 & ten_percent == 1 ~ 'push 10%',
                               push == 1 & email == 1 & five_percent == 1 & ten_percent == 0 ~ 'both 5%',
                               push == 1 & email == 1 & five_percent == 0 & ten_percent == 1 ~ 'both 10%')) %>% 
  ungroup() %>% 
  select(condition, spend, lower.bound, upper.bound)


AO_200_sales$spend <- currency(AO_200_sales$spend)
AO_200_sales$lower.bound <- currency(AO_200_sales$lower.bound)
AO_200_sales$upper.bound <- currency(AO_200_sales$upper.bound)

AO_200_sales$condition <- factor(AO_200_sales$condition, ordered = TRUE, 
                                 levels = c('email 5%', 'email 10%', 'push 5%', 'push 10%','both 5%', 'both 10%','control'))

u <- ggplot(AO_200_sales, aes(x = condition,
                              y = spend))+
  geom_text(aes(label = spend, hjust = 1.2)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 2), limits = c(120,180)) +
  geom_pointrange( aes(x=condition, ymin=lower.bound, ymax=upper.bound), colour="red", alpha=0.9, size=1.3) +
  labs(title = paste0("$200 Basket Tier Basket Size Confidence Intervals"),
       x = "",
       y = "Average Basket Size",
  )

u + theme_ben() 


#number shopped

shopped <- target_promo %>% 
  filter(AO_flag == 1) %>% 
  tabyl(tier, condition)






promo$promo_sales <- as.numeric(promo$promo_sales) 

promo_summary <- promo %>% 
  mutate(tier = case_when(promo == 'SPOOKY1' ~ '100',
                          promo == 'SPOOKY2' | promo == 'SPOOKY3' ~ '150',
                          promo == 'SPOOKY4' | promo == 'SPOOKY5' ~ '200'),
         discount = case_when(promo == 'SPOOKY1' | promo == 'SPOOKY2' | promo == 'SPOOKY4' ~ '10%',
                              promo == 'SPOOKY3' | promo == 'SPOOKY5' ~ '5%')) %>% 
  group_by(tier, discount) %>% 
  summarise(customers = n(),
            avg_sales = mean(promo_sales, na.rm = T))

promo$promo_sales <- as.numeric(promo$promo_sales)
cost <- promo %>% 
  inner_join(target, by = "ClubCardID") %>% 
  select(ClubCardID, promo, pre_basket, promo_sales) %>% 
  mutate(tier = case_when(promo == 'SPOOKY1' ~ '100',
                          promo == 'SPOOKY2' | promo == 'SPOOKY3' ~ '150',
                          promo == 'SPOOKY4' | promo == 'SPOOKY5' ~ '200'),
         discount = case_when(promo == 'SPOOKY1' | promo == 'SPOOKY2' | promo == 'SPOOKY4' ~ '10%',
                              promo == 'SPOOKY3' | promo == 'SPOOKY5' ~ '5%')) %>% 
  ungroup()

cost <- cost %>% 
  mutate(cost = if_else(discount == '10%', promo_sales*0.1, promo_sales*.05)) %>% 
    summarise(customers = n(),
              total_cost = sum(cost))

target_summary <- target %>% 
  tabyl(tier, condition)

target2 %>% 
  tabyl(tier, promotion, channel)
