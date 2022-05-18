DECLARE @CYEndDate DATE = (SELECT MAX(FYWeekEndingDate)
							FROM POS_MART.DateDimension
							WHERE FYWeekEndingDate < GETDATE())

DECLARE @CYStartDate DATE = DATEADD(WEEK, -51, @CYEndDate)

IF OBJECT_ID('tempdb..#baskets') IS NOT NULL
DROP TABLE #baskets
SELECT CustomerDimensionID,
COUNT(DISTINCT(BasketKey)) AS baskets
INTO #baskets
FROM POS_MART.SalesLineItemFact sli
JOIN POS_MART.StoreItemDimension sid
ON sid.StoreItemDimensionID = sli.StoreItemDimensionID
WHERE DateDimensionID BETWEEN CONVERT(INT, CONVERT(CHAR(8), @CYStartDate, 112)) AND CONVERT(INT, CONVERT(CHAR(8), @CYEndDate, 112))
AND sli.CustomerDimensionID <> 1
AND sid.SubCategory IN ('Alternative Protein Meat Substitutes (Frozen)', 'Alternative Protein Meat Substitutes (Refrigerated', 'Alternative Protein Burgers & Brats')
GROUP BY CustomerDimensionID


IF OBJECT_ID('tempdb..#sales') IS NOT NULL
DROP TABLE #sales
SELECT CustomerDimensionID,
SUM(SaleAmount) AS sales
INTO #sales
FROM POS_MART.SalesLineItemFact sli
JOIN POS_MART.StoreItemDimension sid
ON sid.StoreItemDimensionID = sli.StoreItemDimensionID
WHERE sid.SubCategory IN ('Alternative Protein Meat Substitutes (Frozen)', 'Alternative Protein Meat Substitutes (Refrigerated', 'Alternative Protein Burgers & Brats')
AND DateDimensionID BETWEEN CONVERT(INT, CONVERT(CHAR(8), @CYStartDate, 112)) AND CONVERT(INT, CONVERT(CHAR(8), @CYEndDate, 112))
AND sli.CustomerDimensionID <> 1
GROUP BY CustomerDimensionID

IF OBJECT_ID('tempdb..#meat') IS NOT NULL
DROP TABLE #meat
SELECT CustomerDimensionID,
COUNT(Basket



SELECT b.CustomerDimensionID,
baskets,
sales 
FROM #baskets b
JOIN #sales s
ON s.CustomerDimensionID = b.CustomerDimensionID
ORDER BY baskets DESC, sales DESC


