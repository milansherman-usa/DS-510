DECLARE @LastWeekEnd DATE = (SELECT MAX(FYWeekEndingDate)
							FROM POS_MART.DateDimension
							WHERE FYWeekEndingDate < GETDATE())

IF OBJECT_ID('tempdb..#CY') IS NOT NULL
DROP TABLE #CY
SELECT NielsenCategory, 
NielsenCategory_Key,
SUM(SaleAmount) AS 'Sales'
INTO #CY
FROM POS_MART.SalesDailyItemFact sdf
JOIN POS_MART.StoreItemDimension sid
ON sid.StoreItemDimensionID = sdf.StoreItemDimensionID
WHERE NielsenCategory_Key IN (396, 405, 56, 403, 401, 194, 399, 155, 402, 397, 55, 398, 400, 154, 404, 367, 78, 77, 217)
AND sdf.DateDimensionID BETWEEN CONVERT(INT, CONVERT(CHAR(8), DATEADD(WEEK, -51, @LastWeekEnd), 112)) AND CONVERT(INT, CONVERT(CHAR(8), @LastWeekEnd, 112))
GROUP BY NielsenCategory, NielsenCategory_Key

IF OBJECT_ID('tempdb..#PY') IS NOT NULL
DROP TABLE #PY
SELECT NielsenCategory, 
NielsenCategory_Key,
SUM(SaleAmount) AS 'Sales'
INTO #PY
FROM POS_MART.SalesDailyItemFact sdf
JOIN POS_MART.StoreItemDimension sid
ON sid.StoreItemDimensionID = sdf.StoreItemDimensionID
WHERE NielsenCategory_Key IN (396, 405, 56, 403, 401, 194, 399, 155, 402, 397, 55, 398, 400, 154, 404, 367, 78, 77, 217)
AND sdf.DateDimensionID BETWEEN CONVERT(INT, CONVERT(CHAR(8), DATEADD(WEEK, -103, @LastWeekEnd), 112)) AND CONVERT(INT, CONVERT(CHAR(8), DATEADD(WEEK, -51, @LastWeekEnd), 112))
GROUP BY NielsenCategory, NielsenCategory_Key

SELECT c.NielsenCategory,
c.NielsenCategory_Key,
c.Sales AS 'Sales',
p.Sales AS 'PYSales',
(c.Sales - p.Sales)/NULLIF(p.Sales,0) AS 'PYchg'
FROM #CY c
LEFT JOIN #PY p
ON p.NielsenCategory = c.NielsenCategory