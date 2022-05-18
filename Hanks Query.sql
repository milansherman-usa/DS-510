DECLARE @startdate INT = 20220101
DECLARE @enddate INT = 20220228

;WITH ao_customers AS (
SELECT DISTINCT dd.CYMonthNumber, ClubCardID
FROM POS_MART.SalesBasketFact sbf
JOIN POS_MART.CustomerDimension cd ON cd.CustomerDimensionID = sbf.CustomerDimensionID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionID = sbf.DateDimensionID
WHERE sbf.DateDimensionID BETWEEN @startdate AND @enddate
AND CheckoutTypeDimensionID = 3
),
in_store AS (
SELECT DISTINCT dd.CYMonthNumber, ClubCardID
FROM POS_MART.SalesBasketFact sbf
JOIN POS_MART.CustomerDimension cd ON cd.CustomerDimensionID = sbf.CustomerDimensionID
JOIN POS_MART.DateDimension dd ON dd.DateDimensionID = sbf.DateDimensionID
WHERE sbf.DateDimensionID BETWEEN @startdate AND @enddate
AND CheckoutTypeDimensionID <> 3
),
lapsed AS (
SELECT online.CYMonthNumber, online.ClubCardId,
CASE WHEN online2.ClubCardId IS NULL AND store.ClubCardId IS NOT NULL THEN 'in_store'
WHEN online2.ClubCardId IS NULL AND store.ClubCardId IS NULL THEN 'lapsed' 
ELSE 'returned_online' END AS status
FROM ao_customers online
LEFT JOIN in_store store ON store.CYMonthNumber = online.CYMonthNumber + 1 AND store.ClubCardId = online.ClubCardId
LEFT JOIN ao_customers online2 ON online2.CYMonthNumber = online.CYMonthNumber + 1 AND online2.ClubCardId = online.ClubCardId
)

SELECT lapsed.CYMonthNumber
, SUM(CASE WHEN status = 'in_store' THEN 1 ELSE 0 END) AS InStoreTotals
, SUM(CASE WHEN status = 'lapsed' THEN 1 ELSE 0 END) AS LapsedTotals
, SUM(CASE WHEN status = 'returned_online' THEN 1 ELSE 0 END) AS ReturnedOnlineTotals
FROM lapsed
GROUP BY lapsed.CYMonthNumber