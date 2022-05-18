DECLARE @CYEndDate DATE = (SELECT MAX(FYWeekEndingDate)
							FROM POS_MART.DateDimension
							WHERE FYWeekEndingDate < GETDATE())

DECLARE @CYStartDate DATE = DATEADD(WEEK, -51, @CYEndDate)

IF OBJECT_ID('tempdb..#baskets') IS NOT NULL
DROP TABLE #baskets
SELECT CustomerDimensionID,
COUNT(DISTINCT(BasketKey)) AS Basket_count
INTO #baskets
FROM POS_MART.SalesLineItemFact sli
JOIN POS_MART.StoreItemDimension sid
ON sid.StoreItemDimensionID = sli.StoreItemDimensionID
WHERE DateDimensionID BETWEEN CONVERT(INT, CONVERT(CHAR(8), @CYStartDate, 112)) AND CONVERT(INT, CONVERT(CHAR(8), @CYEndDate, 112))
AND sli.CustomerDimensionID <> 1
AND sid.NielsenCategory IN ('Bird Accessories', 'Bird Food', 'Fish Care',
						'Pet Food Other Animals', 'Aquatic Supplies',
						'Dog Control', 'Other Pet Accessories',
						'Pet Toys', 'Cat Food', 'Dog Litter',
						'Pet Food', 'Pet Grooming', 'Dog Food',
						'Fish Food', 'Flea and Tick Pet Product',
						'Litter Supplies', 'Pet Brush', 'Pet Medicine')
GROUP BY CustomerDimensionID
--ORDER BY sli.CustomerDimensionID

IF OBJECT_ID('tempdb..#cust') IS NOT NULL
DROP TABLE #cust
SELECT CustomerDimensionID,
Basket_count,
CASE WHEN Basket_count > 100 THEN 'heavy'
	 WHEN Basket_count > 20 THEN 'medium'
	 WHEN Basket_count > 1 THEN 'light'
	 ELSE 'one_time' END AS pet_shopper
INTO #cust
FROM #baskets
GROUP BY CustomerDimensionID, Basket_count

SELECT pet_shopper,
COUNT(pet_shopper)
FROM #cust
GROUP BY pet_shopper