rm(list = ls())
library(readxl)
library(ggplot2)
library(dplyr)
require(odbc)
require(lubridate)
require(reshape2)
library(zoo)
library(MatchIt)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(gridExtra)
require(broom)
library(grid)
library(gtable)
require(tidyr)
require(caret)
library(tidyr)

get_data <- function(){
  library(RODBC)
  conn <- odbcDriverConnect("Driver={ODBC Driver 17 for SQL Server};Server=hy-vee-dw-sql-server-p.database.windows.net;Database=hy-vee-dw-db;UID=misherman@hy-vee.net;Authentication=ActiveDirectoryInteractive;")
  
  prep_query <- "SET NOCOUNT ON;
  -------------------------------IDENTIFY PROMOS AND ITEMS-------------------------------------------
DECLARE @currbegdate DATE = (select MAX(ActualDate) from POS_MART.DateDimension where ActualDate < DATEADD(day, -1, GETDATE()) and DayOfWeekName = 'Wednesday')
DECLARE @adbegdate DATE = '2021-12-22'
DECLARE @quotientdate DATE = DATEADD(day, -1, @adbegdate) --Quotient sets their start dates a day before the ad week, while LU's will be on Wed
DECLARE @adenddate DATE = DATEADD(day, 6, @currbegdate) --End dates are the same for both types
--select @adbegdate, @quotientdate, @adenddate, @currbegdate, DATEDIFF(day, @currbegdate,@adenddate)
---This gets the UPC set for the LU promotions (see notes on filters)
---This is a complicated query that I got from Austin, but it essentially breaks out individual items as well as item sets depending on how the promo is set up
---The result is a list of UPC's that I can match on
IF OBJECT_ID('tempdb..#adweek') IS NOT NULL
    DROP TABLE #adweek;
select distinct 
	ActualDate as AdWeek
into #adweek
from 
	POS_MART.DateDimension 
where 
	DayOfWeekName = 'Wednesday' 
	and ActualDate between DATEADD(week, -4, CAST(@adbegdate as DATE)) and DATEADD(day, 13 ,CAST(@adbegdate as DATE))
IF OBJECT_ID('tempdb..#SuperHotDealsItems') IS NOT NULL
    DROP TABLE #SuperHotDealsItems;
select distinct
ph.PromotionHeaderId,
ph.PromotionHeaderDescription,
ph.PromotionHeaderStartDate,
ph.PromotionHeaderEndDate,
g.itemid as indv_itemid,
g9.itemid,
(CASE WHEN LEN(g9.ItemId) = 12 THEN ''
WHEN LEN(g9.ItemId) = 11 THEN '0' ELSE '00' END) +
CASE WHEN LEN(g9.ItemId) = 9 THEN '0' + g9.ItemId
WHEN LEN(g9.ItemId) = 8 THEN '00' + g9.ItemId
WHEN LEN(g9.ItemId) = 7 THEN '000' + g9.ItemId
WHEN LEN(g9.ItemId) = 6 THEN '0000' + g9.ItemId
WHEN LEN(g9.ItemId) = 5 THEN '00000' + g9.ItemId
WHEN LEN(g9.ItemId) = 4 THEN '000000' + g9.ItemId
ELSE g9.ItemId END + '0' [UPC_Group],
(CASE WHEN LEN(g.ItemId) = 12 THEN ''
WHEN LEN(g.ItemId) = 11 THEN '0' ELSE '00' END) +
CASE WHEN LEN(g.ItemId) = 9 THEN '0' + g.ItemId
WHEN LEN(g.ItemId) = 8 THEN '00' + g.ItemId
WHEN LEN(g.ItemId) = 7 THEN '000' + g.ItemId
WHEN LEN(g.ItemId) = 6 THEN '0000' + g.ItemId
WHEN LEN(g.ItemId) = 5 THEN '00000' + g.ItemId
WHEN LEN(g.ItemId) = 4 THEN '000000' + g.ItemId
ELSE g.ItemId END + '0' [UPC_Indv]
into #SuperHotDealsItems
FROM DIM_ODS.PromotionBucketEntity pbe
join DIM_ODS.PromotionHeader ph on ph.PromotionHeaderId = pbe.PromotionHeaderId
JOIN DIM_ODS.ItemInfo e ON e.ItemInternalKey = pbe.EntityInternalKey
LEFT JOIN DIM_ODS.ItemHierarchy h ON (h.CategoryKey = e.MainCategoryKey AND e.MainItemID LIKE '9999%')
LEFT JOIN DIM_ODS.ItemInfo f ON f.ItemInternalKey = h.ItemInternalKey
LEFT JOIN DIM_ODS.ItemCode g9 ON g9.ItemInternalKey = f.ItemInternalKey
LEFT JOIN DIM_ODS.ItemCode g ON (g.ItemInternalKey = e.ItemInternalKey AND e.MainItemID NOT LIKE '9999%')
where
(PromotionHeaderDescription LIKE 'SuperHot%' --THIS WILL BE THE NEW FILTER GOING FORWARD STARTING TOMORROW
OR PromotionGroupID = 27759) --THIS IDENTIFIES THE FIRST SET, USE THIS TO TEST
and CAST(PromotionHeaderStartDate as DATE) >= @adbegdate --only look at the promotions we want to see for this week
and CAST(PromotionHeaderEndDate as DATE) <= @adenddate
and DATEDIFF(day, PromotionHeaderStartDate, PromotionHeaderEndDate) = 6
AND (CASE WHEN g9.itemid IS NULL THEN g.itemid
ELSE g9.itemid END NOT LIKE '99999%')
---This gets the UPC set for the Quotient promotions (see notes on filters)
---Same query as above while identifying the quotient promo group and filtering to promotions that start and end on the expected days
IF OBJECT_ID('tempdb..#QuotientPool') IS NOT NULL
    DROP TABLE #QuotientPool;
select distinct
ph.PromotionHeaderId,
ph.PromotionHeaderDescription,
ph.PromotionHeaderStartDate,
ph.PromotionHeaderEndDate,
g.itemid as indv_itemid,
g9.itemid,
(CASE WHEN LEN(g9.ItemId) = 12 THEN ''
WHEN LEN(g9.ItemId) = 11 THEN '0' ELSE '00' END) +
CASE WHEN LEN(g9.ItemId) = 9 THEN '0' + g9.ItemId
WHEN LEN(g9.ItemId) = 8 THEN '00' + g9.ItemId
WHEN LEN(g9.ItemId) = 7 THEN '000' + g9.ItemId
WHEN LEN(g9.ItemId) = 6 THEN '0000' + g9.ItemId
WHEN LEN(g9.ItemId) = 5 THEN '00000' + g9.ItemId
WHEN LEN(g9.ItemId) = 4 THEN '000000' + g9.ItemId
ELSE g9.ItemId END + '0' [UPC_Group],
(CASE WHEN LEN(g.ItemId) = 12 THEN ''
WHEN LEN(g.ItemId) = 11 THEN '0' ELSE '00' END) +
CASE WHEN LEN(g.ItemId) = 9 THEN '0' + g.ItemId
WHEN LEN(g.ItemId) = 8 THEN '00' + g.ItemId
WHEN LEN(g.ItemId) = 7 THEN '000' + g.ItemId
WHEN LEN(g.ItemId) = 6 THEN '0000' + g.ItemId
WHEN LEN(g.ItemId) = 5 THEN '00000' + g.ItemId
WHEN LEN(g.ItemId) = 4 THEN '000000' + g.ItemId
ELSE g.ItemId END + '0' [UPC_Indv]
into #QuotientPool
FROM DIM_ODS.PromotionBucketEntity pbe
join DIM_ODS.PromotionHeader ph on ph.PromotionHeaderId = pbe.PromotionHeaderId
JOIN DIM_ODS.ItemInfo e ON e.ItemInternalKey = pbe.EntityInternalKey
LEFT JOIN DIM_ODS.ItemHierarchy h ON (h.CategoryKey = e.MainCategoryKey AND e.MainItemID LIKE '9999%')
LEFT JOIN DIM_ODS.ItemInfo f ON f.ItemInternalKey = h.ItemInternalKey
LEFT JOIN DIM_ODS.ItemCode g9 ON g9.ItemInternalKey = f.ItemInternalKey
LEFT JOIN DIM_ODS.ItemCode g ON (g.ItemInternalKey = e.ItemInternalKey AND e.MainItemID NOT LIKE '9999%')
where
PromotionGroupID = 26392 --quotient group
and CAST(PromotionHeaderStartDate as DATE) >= @quotientdate --look for promos that start and end on the expected days
and CAST(PromotionHeaderEndDate as DATE) <= @adenddate
and DATEDIFF(day, PromotionHeaderStartDate, PromotionHeaderEndDate) = 7
AND (CASE WHEN g9.itemid IS NULL THEN g.itemid
ELSE g9.itemid END NOT LIKE '99999%')
IF OBJECT_ID('tempdb..#LU_UPCs') IS NOT NULL
    DROP TABLE #LU_UPCs;
select distinct
	PromotionHeaderId,
	PromotionHeaderDescription,
	PromotionHeaderStartDate,
	PromotionHeaderEndDate,
	COALESCE(UPC_Group, UPC_Indv) as UPC
into #LU_UPCs
from
	#SuperHotDealsItems
IF OBJECT_ID('tempdb..#Qu_UPCs') IS NOT NULL
    DROP TABLE #Qu_UPCs;
select distinct
	PromotionHeaderId,
	PromotionHeaderDescription,
	PromotionHeaderStartDate,
	PromotionHeaderEndDate,
	COALESCE(UPC_Group, UPC_Indv) as UPC --see above
into #Qu_UPCs
from
	#QuotientPool qp
IF OBJECT_ID('tempdb..#promos') IS NOT NULL
    DROP TABLE #promos;
select distinct
	PromotionHeaderStartDate as AdWeek,
	SUBSTRING(PromotionHeaderDescription, CHARINDEX(':', PromotionHeaderDescription)+7, 50) as PromotionHeaderDescription,
	PromotionHeaderId,
	'LU' as RedeemType
into #promos
from
	#LU_UPCs
UNION
select distinct
	lu.PromotionHeaderStartDate as AdWeek,
	SUBSTRING(lu.PromotionHeaderDescription, CHARINDEX(':', lu.PromotionHeaderDescription)+7, 50) as PromotionHeaderDescription,
	q.PromotionHeaderId,
	'Digital Clip' as RedeemType
from
	#LU_UPCs lu
	join #Qu_UPCs q on lu.PromotionHeaderStartDate = DATEADD(day, 1, q.PromotionHeaderStartDate)
		AND (lu.UPC = q.UPC
		OR q.UPC LIKE CONCAT('%',RIGHT(Left(lu.UPC,11),10),'%')
		OR lu.PromotionHeaderDescription LIKE CONCAT('%',SUBSTRING(q.PromotionHeaderDescription, CHARINDEX('-', q.PromotionHeaderDescription)+1, 50),'%'))
IF OBJECT_ID('tempdb..#promoitems') IS NOT NULL
    DROP TABLE #promoitems;
select distinct
	p.AdWeek,
	p.PromotionHeaderDescription,
	p.PromotionHeaderId,
	p.RedeemType,
	lu.UPC
into #promoitems
from
	#promos p
	join #LU_UPCs lu on p.AdWeek = lu.PromotionHeaderStartDate
		and p.PromotionHeaderDescription = SUBSTRING(lu.PromotionHeaderDescription, CHARINDEX(':', lu.PromotionHeaderDescription)+7, 50)
where
	p.AdWeek = @adbegdate
-----------------------------------GET SALES/REDEMPTION INFO-------------------------------------------------
--get redemption information by week, promo, and customer
if object_id('tempdb..#redemptions') is not null 
drop table #redemptions
select distinct
	cd.ClubCardID,
	1 as Redemptions
into #redemptions
from
	POS_MART.SalesStorePromotionFact ssp
	join POS_MART.CustomerDimension cd on cd.CustomerDimensionID = ssp.CustomerDimensionID
	join POS_MART.POSPromotionDimension ppd on ppd.POSPromotionDimensionID = ssp.POSPromotionDimensionID
	join POS_MART.DateDimension dd on dd.DateDimensionID = ssp.DateDimensionID
	join DIM_ODS.PromotionHeader ph on ph.PromotionHeaderId = ppd.POSPromotionNumber
	join #promoitems p on p.PromotionHeaderId = ph.PromotionHeaderId
where
	ActualDate between CAST(@adbegdate as DATE) and DATEADD(day, 6 ,CAST(@adbegdate as DATE))
	and PromotionCount <> -1
	and cd.CustomerDimensionID <> 1
--get any sales from the period for each customer by day
IF OBJECT_ID('tempdb..#allsales') IS NOT NULL
    DROP TABLE #allsales;
select distinct
	ClubCardID,
	ad.AdWeek,
	SUM(SaleAmount) as TotalSales,
	COUNT(DISTINCT BasketKey) as TotalBaskets,
	SUM(Quantity) as TotalItems,
	SUM(CASE WHEN p.UPC is not null THEN SaleAmount ELSE 0 END) as TargetSales,
	SUM(CASE WHEN p.UPC is not null THEN Quantity ELSE 0 END) as TargetItems,
	COUNT(DISTINCT CASE WHEN p.UPC is not null THEN BasketKey ELSE NULL END) as TargetBaskets
into #allsales
from
	POS_MART.SalesLineItemFact sbf
	join POS_MART.StoreItemDimension sd on sd.StoreItemDimensionID = sbf.StoreItemDimensionID
	join POS_MART.CustomerDimension cd on cd.CustomerDimensionID = sbf.CustomerDimensionID
	join POS_MART.LocationDimension ld on cd.FrequentStore = ld.StoreNumber
	join POS_MART.DateDimension dd on dd.DateDimensionID = sbf.DateDimensionID
	left join #promoitems p on p.UPC = sd.UPC
	join #adweek ad on dd.ActualDate between ad.AdWeek and DATEADD(day, 6, ad.AdWeek)
where
	dd.ActualDate between DATEADD(week, -4, CAST(@adbegdate as DATE)) and DATEADD(day, 13 ,CAST(@adbegdate as DATE))
	and cd.CustomerDimensionID <> 1
	and SaleAmount > 0
group by
	ClubCardID,
	ad.AdWeek
--pull in all the active customers from the temp table #allsales and cross join with distinct ad weeks and distinct promotions
IF OBJECT_ID('tempdb..#cross') IS NOT NULL
    DROP TABLE #cross;
select distinct
	ClubCardID,
	AdWeek
into #cross
from
	(select distinct ClubCardID from #allsales where AdWeek = @adbegdate) c
	cross join (select distinct AdWeek from #adweek) w
IF OBJECT_ID('tempdb..#custinfo') IS NOT NULL
    DROP TABLE #custinfo;
select distinct
	h.ClubCardID,
	FactsWeekSegment,
	ShopStyleSegment,
	TruPriceSegment,
	Generation,
	Gender,
	CASE WHEN MarketDescription LIKE '%All other locations%' THEN 'Rural' ELSE 'Urban' END as Geo,
	FrequentStore
into #custinfo
from
	#allsales h
	join POS_MART.CustomerDimension cd on cd.ClubCardID = h.ClubCardID
	join POS_MART.LocationDimension ld on ld.StoreNumber = cd.FrequentStore
where
	AdWeek = @adbegdate
  "
  
  final_query <- "select
	c.ClubCardID,
	ci.FactsWeekSegment,
	ci.ShopStyleSegment,
	ci.TruPriceSegment,
	ci.Gender,
	ci.Generation,
	ci.Geo,
	ci.FrequentStore,
	c.AdWeek,
	ISNULL(Redemptions,0) as Redemptions,
	ISNULL(TargetSales,0) as TargetSales,
	ISNULL(TargetBaskets,0) as TargetBaskets,
	ISNULL(TargetItems,0) as TargetItems,
	ISNULL(TotalSales,0) as TotalSales,
	ISNULL(TotalBaskets,0) as TotalBaskets,
	ISNULL(TotalItems,0) as TotalItems
from
	#cross c
	join #custinfo ci on ci.ClubCardID = c.ClubCardID
	left join #allsales a on a.ClubCardID = c.ClubCardID
		and a.AdWeek = c.AdWeek
	left join #redemptions r on r.ClubCardID = c.ClubCardID"
  
  sqlQuery(channel = conn, query = prep_query)
  data  <- sqlQuery(channel = conn, query = final_query, stringsAsFactors = FALSE, as.is = TRUE)
  
  return(data)
}

psm_data <- get_data()

psm_data$AdWeek <- as.Date(psm_data$AdWeek, format = "%Y-%m-%d")
psm_data$TargetSales <- as.numeric(psm_data$TargetSales)
psm_data$TargetItems <- as.numeric(psm_data$TargetItems)
psm_data$TotalSales <- as.numeric(psm_data$TotalSales)
psm_data$TotalItems <- as.numeric(psm_data$TotalItems)

StartDate <- as.Date(sort(unique(psm_data$AdWeek))[5], format = "%Y-%m-%d")
PostDate <- as.Date(sort(unique(psm_data$AdWeek))[6], format = "%Y-%m-%d")

store_list <- unique(psm_data[psm_data$Redemptions == 1,8])


##########################################################
dta_m <- NULL #DONT FORGET TO RUN THIS BEFORE RUNNING LOOP

for(i in 1:length(store_list)){
  print(i)
  curr_psm <- psm_data[psm_data$FrequentStore == store_list[i],]
  
  curr_psm <- curr_psm %>%
    dplyr::mutate(TargetSpendPerItem = ifelse(TargetItems > 0 & TargetSales > 0, TargetSales/TargetItems, 0.0),
                  TargetItemsPerBasket = ifelse(TargetBaskets > 0, TargetItems/TargetBaskets, 0.0),
                  TotalSpendPerItem = ifelse(TotalItems > 0 & TotalSales > 0, TotalSales/TotalItems, 0.0),
                  TotalItemsPerBasket = ifelse(TotalBaskets > 0, TotalItems/TotalBaskets, 0.0))
  
  psm_data_wide <- curr_psm %>%
    dplyr::ungroup() %>%
    dplyr::filter(AdWeek < StartDate ) %>%
    dplyr::select(ClubCardID,
                  AdWeek,
                  TargetBaskets,
                  TargetItems,
                  TargetSales,
                  TotalBaskets,
                  TotalItems,
                  TotalSales) %>%
    dplyr::distinct() %>%
    pivot_wider(names_from = AdWeek, 
                values_from = c(TargetBaskets,
                                TargetItems,
                                TargetSales,
                                TotalBaskets,
                                TotalItems,
                                TotalSales),
                values_fill = 0)
  
  redemptions <- curr_psm %>%
    dplyr::filter(AdWeek == StartDate) %>%
    dplyr::group_by(ClubCardID) %>%
    dplyr::summarise(Redemptions = Redemptions)
  
  demographics <- curr_psm %>%
    dplyr::select(ClubCardID,
                  FactsWeekSegment,
                  ShopStyleSegment,
                  TruPriceSegment,
                  Generation,
                  Gender) %>%
    dplyr::distinct()
  
  dummies <- dummyVars(ClubCardID ~ ., data = demographics)
  demo_dummies <- as.data.frame(predict(dummies, newdata = demographics))
  
  demo_dummies$ClubCardID <- demographics$ClubCardID
  
  final <- psm_data_wide %>%
    dplyr::left_join(redemptions) %>%
    dplyr::left_join(demo_dummies)
  
  rm(demo_dummies)
  rm(redemptions)
  rm(demographics)
  rm(psm_data_wide)
  rm(dummies)
  
  ## Name clean up for matchit code & formula creation
  names(final) <- gsub(" ","",names(final))
  names(final) <- gsub("[[:punct:]]","",names(final))
  idvar <- "ClubCardID"
  
  Target <- "Redemptions"
  rmnames <- c(Target ,idvar)
  
  IndVars <- names(final)[!names(final) %in% rmnames]
  IndVars <- paste(IndVars, collapse="+ ")
  formstring <-as.formula( paste0(Target," ~ ",IndVars))
  
  ## Use propensity score matching via nearest neighbors to identify
  ## statistical twins of households that activate over prior 12 months
  mod_match <- matchit(formstring,
                       method = "nearest", 
                       distance = "glm",
                       data = final)
  
  tmp <- match.data(mod_match)
  print(dim(dta_m))
  print(dim(tmp))
  dta_m <- bind_rows(dta_m, tmp)
  print(dim(dta_m))
  rm(tmp)
  rm(mod_match)
}


### This section will create a dataset for analytical purposes from the 
### the matched data
Redemptions <- psm_data %>%
  dplyr::filter(AdWeek == StartDate) %>%
  dplyr::group_by(ClubCardID) %>%
  dplyr::summarise(Redemptions = Redemptions)

compare_df <- psm_data %>%
  dplyr::filter(ClubCardID %in% unique(dta_m$ClubCardID)) %>%
  dplyr::select(-Redemptions) %>%
  dplyr::distinct() %>%
  dplyr::left_join(Redemptions) %>%
  dplyr::mutate(pre = ifelse(AdWeek < StartDate, 1, 0),
                post = ifelse(AdWeek == StartDate, 1, 0),
                after = ifelse(AdWeek == PostDate, 1, 0),
                NonTargetSales = TotalSales - TargetSales,
                NonTargetItems = TotalItems - TargetItems,
                NonTargetBaskets = TotalBaskets - TargetBaskets,
                ItemsPerBasket = TotalItems/TotalBaskets,
                SpendPerItem = ifelse(TotalItems == 0, 0.0, TotalSales/TotalItems))

compare_df[is.na(compare_df)] <- 0

#During Promotion Lift

tb <- t.test(compare_df %>% filter(Redemptions == 1 & post == 1) %>% dplyr::select(TotalBaskets),
             compare_df %>% filter(Redemptions == 0 & post == 1) %>% dplyr::select(TotalBaskets)) 

ipb <- t.test(compare_df %>% filter(Redemptions == 1 & post == 1) %>% dplyr::select(ItemsPerBasket),
              compare_df %>% filter(Redemptions == 0 & post == 1) %>% dplyr::select(ItemsPerBasket)) 

spi <- t.test(compare_df %>% filter(Redemptions == 1 & post == 1) %>% dplyr::select(SpendPerItem),
              compare_df %>% filter(Redemptions == 0 & post == 1) %>% dplyr::select(SpendPerItem)) 

ts <- t.test(compare_df %>% filter(Redemptions == 1 & post == 1) %>% dplyr::select(TotalSales),
             compare_df %>% filter(Redemptions == 0 & post == 1) %>% dplyr::select(TotalSales)) 

tas <- t.test(compare_df %>% filter(Redemptions == 1 & post == 1) %>% dplyr::select(TargetSales),
              compare_df %>% filter(Redemptions == 0 & post == 1) %>% dplyr::select(TargetSales)) 

nts <- t.test(compare_df %>% filter(Redemptions == 1 & post == 1) %>% dplyr::select(NonTargetSales),
              compare_df %>% filter(Redemptions == 0 & post == 1) %>% dplyr::select(NonTargetSales)) 

print(paste("Total Baskets: PV - ", tb$p.value, "; treat - ", tb$estimate[[1]], "; ctrl - ", tb$estimate[[2]], "; diff - ", tb$estimate[[1]] - tb$estimate[[2]],";"))
print(paste("Items Per Basket: PV - ", ipb$p.value, "; treat - ", ipb$estimate[[1]], "; ctrl - ", ipb$estimate[[2]], "; diff - ", ipb$estimate[[1]] - ipb$estimate[[2]],";"))
print(paste("Spend Per Item: PV - ", spi$p.value, "; treat - ", spi$estimate[[1]], "; ctrl - ", spi$estimate[[2]], "; diff - ", spi$estimate[[1]] - spi$estimate[[2]],";"))
print(paste("Total Sales: PV - ", ts$p.value, "; treat - ", ts$estimate[[1]], "; ctrl - ", ts$estimate[[2]], "; diff - ", ts$estimate[[1]] - ts$estimate[[2]],";"))
print(paste("Target Sales: PV - ", tas$p.value, "; treat - ", tas$estimate[[1]], "; ctrl - ", tas$estimate[[2]], "; diff - ", tas$estimate[[1]] - tas$estimate[[2]],";"))
print(paste("Non Target Sales: PV - ", nts$p.value, "; treat - ", nts$estimate[[1]], "; ctrl - ", nts$estimate[[2]], "; diff - ", nts$estimate[[1]] - nts$estimate[[2]],";"))

#Week After Promotion Lift

post_tb <- t.test(compare_df %>% filter(Redemptions == 1 & after == 1) %>% dplyr::select(TotalBaskets),
                  compare_df %>% filter(Redemptions == 0 & after == 1) %>% dplyr::select(TotalBaskets)) 

post_ipb <- t.test(compare_df %>% filter(Redemptions == 1 & after == 1) %>% dplyr::select(ItemsPerBasket),
                   compare_df %>% filter(Redemptions == 0 & after == 1) %>% dplyr::select(ItemsPerBasket)) 

post_spi <- t.test(compare_df %>% filter(Redemptions == 1 & after == 1) %>% dplyr::select(SpendPerItem),
                   compare_df %>% filter(Redemptions == 0 & after == 1) %>% dplyr::select(SpendPerItem)) 

post_ts <- t.test(compare_df %>% filter(Redemptions == 1 & after == 1) %>% dplyr::select(TotalSales),
                  compare_df %>% filter(Redemptions == 0 & after == 1) %>% dplyr::select(TotalSales)) 

post_tas <- t.test(compare_df %>% filter(Redemptions == 1 & after == 1) %>% dplyr::select(TargetSales),
                   compare_df %>% filter(Redemptions == 0 & after == 1) %>% dplyr::select(TargetSales)) 

post_nts <- t.test(compare_df %>% filter(Redemptions == 1 & after == 1) %>% dplyr::select(NonTargetSales),
                   compare_df %>% filter(Redemptions == 0 & after == 1) %>% dplyr::select(NonTargetSales)) 

print(paste("Total Baskets: PV - ", post_tb$p.value, "; treat - ", post_tb$estimate[[1]], "; ctrl - ", post_tb$estimate[[2]], "; diff - ", post_tb$estimate[[1]] - post_tb$estimate[[2]],";"))
print(paste("Items Per Basket: PV - ", post_ipb$p.value, "; treat - ", post_ipb$estimate[[1]], "; ctrl - ", post_ipb$estimate[[2]], "; diff - ", post_ipb$estimate[[1]] - post_ipb$estimate[[2]],";"))
print(paste("Spend Per Item: PV - ", post_spi$p.value, "; treat - ", post_spi$estimate[[1]], "; ctrl - ", post_spi$estimate[[2]], "; diff - ", post_spi$estimate[[1]] - post_spi$estimate[[2]],";"))
print(paste("Total Sales: PV - ", post_ts$p.value, "; treat - ", post_ts$estimate[[1]], "; ctrl - ", post_ts$estimate[[2]], "; diff - ", post_ts$estimate[[1]] - post_ts$estimate[[2]],";"))
print(paste("Target Sales: PV - ", post_tas$p.value, "; treat - ", post_tas$estimate[[1]], "; ctrl - ", post_tas$estimate[[2]], "; diff - ", post_tas$estimate[[1]] - post_tas$estimate[[2]],";"))
print(paste("Non Target Sales: PV - ", post_nts$p.value, "; treat - ", post_nts$estimate[[1]], "; ctrl - ", post_nts$estimate[[2]], "; diff - ", post_nts$estimate[[1]] - post_nts$estimate[[2]],";"))

#Pre Period

pre_tb <- t.test(compare_df %>% filter(Redemptions == 1 & pre == 1) %>% dplyr::select(TotalBaskets),
                 compare_df %>% filter(Redemptions == 0 & pre == 1) %>% dplyr::select(TotalBaskets)) 

pre_ipb <- t.test(compare_df %>% filter(Redemptions == 1 & pre == 1) %>% dplyr::select(ItemsPerBasket),
                  compare_df %>% filter(Redemptions == 0 & pre == 1) %>% dplyr::select(ItemsPerBasket)) 

pre_spi <- t.test(compare_df %>% filter(Redemptions == 1 & pre == 1) %>% dplyr::select(SpendPerItem),
                  compare_df %>% filter(Redemptions == 0 & pre == 1) %>% dplyr::select(SpendPerItem)) 

pre_ts <- t.test(compare_df %>% filter(Redemptions == 1 & pre == 1) %>% dplyr::select(TotalSales),
                 compare_df %>% filter(Redemptions == 0 & pre == 1) %>% dplyr::select(TotalSales)) 

pre_tas <- t.test(compare_df %>% filter(Redemptions == 1 & pre == 1) %>% dplyr::select(TargetSales),
                  compare_df %>% filter(Redemptions == 0 & pre == 1) %>% dplyr::select(TargetSales)) 

pre_nts <- t.test(compare_df %>% filter(Redemptions == 1 & pre == 1) %>% dplyr::select(NonTargetSales),
                  compare_df %>% filter(Redemptions == 0 & pre == 1) %>% dplyr::select(NonTargetSales)) 

print(paste("Total Baskets: PV - ", pre_tb$p.value, "; treat - ", pre_tb$estimate[[1]], "; ctrl - ", pre_tb$estimate[[2]], "; diff - ", pre_tb$estimate[[1]] - pre_tb$estimate[[2]],";"))
print(paste("Items Per Basket: PV - ", pre_ipb$p.value, "; treat - ", pre_ipb$estimate[[1]], "; ctrl - ", pre_ipb$estimate[[2]], "; diff - ", pre_ipb$estimate[[1]] - pre_ipb$estimate[[2]],";"))
print(paste("Spend Per Item: PV - ", pre_spi$p.value, "; treat - ", pre_spi$estimate[[1]], "; ctrl - ", pre_spi$estimate[[2]], "; diff - ", pre_spi$estimate[[1]] - pre_spi$estimate[[2]],";"))
print(paste("Total Sales: PV - ", pre_ts$p.value, "; treat - ", pre_ts$estimate[[1]], "; ctrl - ", pre_ts$estimate[[2]], "; diff - ", pre_ts$estimate[[1]] - pre_ts$estimate[[2]],";"))
print(paste("Target Sales: PV - ", pre_tas$p.value, "; treat - ", pre_tas$estimate[[1]], "; ctrl - ", pre_tas$estimate[[2]], "; diff - ", pre_tas$estimate[[1]] - pre_tas$estimate[[2]],";"))
print(paste("Non Target Sales: PV - ", pre_nts$p.value, "; treat - ", pre_nts$estimate[[1]], "; ctrl - ", pre_nts$estimate[[2]], "; diff - ", pre_nts$estimate[[1]] - pre_nts$estimate[[2]],";"))

