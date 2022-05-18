declare @enddate date = (select MAX(ActualDate) from POS_MART.DateDimension where FYMonthEndingDate < GETDATE())
declare @startdate date = DATEADD(day, -1095, @enddate)

if object_id('tempdb..#finaltable') is not null 
	drop table #finaltable
create table #finaltable (
		Iteration int,
		StartPeriod varchar(10),
		EndPeriod varchar(10),
		HouseholdID int,
		FirstKnownTransaction date, 
		Sales decimal(15,3),
		Baskets int,
		DaysSince int,
		Active int
)

if object_id('tempdb..#dates') is not null 
	drop table #dates

select
	CYMonthName,
	RANK() OVER (Order by MIN(ActualDate)) as rn,
	MIN(ActualDate) as EndPeriod,
	DATEADD(week, -52 ,MIN(ActualDate)) as StartPeriod,
	DATEADD(day, -180, DATEADD(week, -52 ,MIN(ActualDate))) as LapsedDateSoY,
	DATEADD(day, -180, MIN(ActualDate)) as LapsedDateEoY
into #dates
from
	POS_MART.DateDimension
where
	ActualDate between @startdate and @enddate
group by
	CYMonthName

declare @part_count int = (select count(*) from #dates) 
--declare @part_count int = 5
declare @i int = 1
while @i <= @part_count 

BEGIN
	declare @StartofYear varchar(10) = convert(varchar(10),(select StartPeriod from #dates where rn = @i), 112)
	declare @LapsedDateSoY varchar(10) = convert(varchar(10),(select LapsedDateSoY from #dates where rn = @i), 112)
	declare @EndofYear varchar(10) = convert(varchar(10),(select EndPeriod from #dates where rn = @i), 112)
	declare @LapsedDateEoY varchar(10) = convert(varchar(10),(select LapsedDateEoY from #dates where rn = @i), 112)

	IF OBJECT_ID('tempdb..#beginningcohort') IS NOT NULL
	drop TABLE	#beginningcohort

	select t2.HouseholdID, min(t3.cad_FirstLoyaltyTransaction) as FirstKnownTransaction
	into #beginningcohort
	from POS_MART.SalesBasketFact t1
	join POS_MART.CustomerDimension t2 on t2.CustomerDimensionID = t1.CustomerDimensionID
	join dim_ods.Loy_CustomerAddress t3 on t2.ClubCardId = cast(t3.cad_ClubCardId as varchar(24))
	join POS_MART.DateDimension t4 on t4.DateDimensionID = t1.DateDimensionID
	where t3.cad_FirstLoyaltyTransaction is not null
	and t3.cad_FirstLoyaltyTransaction <= convert(varchar(10),dateadd(yy,-1,convert(date, convert(char(8),@StartofYear), 112)), 112) 
	and t2.FirstName <> 'Hy-Vee'
	and t1.DateDimensionID between @LapsedDateSoY and @StartofYear
	group by t2.HouseholdID

	IF OBJECT_ID('tempdb..#RFM') IS NOT NULL
		drop TABLE	#RFM
	SELECT t3.HouseholdID
		 , t3.FirstKnownTransaction
		 , sum(t1.NonTaxAmount) as Sales
		 , count(t1.BasketKey) as Baskets
		 , datediff(dd,max(ActualDate), convert(date, convert(char(8),@StartofYear), 112)) as DaysSince
		 --, PERCENT_RANK() over (order by sum(t1.NonTaxAmount) desc) as Monetary
		 --, PERCENT_RANK() over (order by  count(t1.BasketKey) desc) as Frequency
		 --, PERCENT_RANK() over (order by datediff(dd,max(ActualDate), convert(date, convert(char(8),@StartofYear), 112))) as Recency
	into #RFM
	from POS_MART.SalesBasketFact t1
	join POS_MART.CustomerDimension t2 on t2.CustomerDimensionID = t1.CustomerDimensionID
	join #beginningcohort t3 on t3.HouseholdID = t2.HouseholdID
	join pos_mart.datedimension t4 on t1.datedimensionid = t4.datedimensionid
	where t1.DateDimensionID between convert(varchar(10),dateadd(dd,-364,convert(date, convert(char(8),@StartofYear), 112)), 112) and @startofyear
	group by t3.HouseholdID
		 , t3.FirstKnownTransaction
	HAVING sum(t1.NonTaxAmount) > 0

	IF OBJECT_ID('tempdb..#Active') IS NOT NULL
		drop TABLE	#Active
	select t2.HouseholdID
	into #active
	from POS_MART.SalesBasketFact t1
	join POS_MART.CustomerDimension t2 on t2.CustomerDimensionID = t1.CustomerDimensionID
	join #beginningcohort t3 on t3.HouseholdID = t2.HouseholdID
	where t1.DateDimensionID between @LapsedDateEoY and @EndofYear
	group by t2.HouseholdID

	insert into #finaltable (Iteration, StartPeriod, EndPeriod, HouseholdID, FirstKnownTransaction, Sales, Baskets, DaysSince,  Active)
	select @i
	, @StartofYear
	, @EndofYear
	, t1.HouseholdID
	, FirstKnownTransaction
	, Sales
	, Baskets
	, DaysSince
	, case when t2.HouseholdID is not null then 1 else 0 end as Active
	from #rfm t1
	left join #active t3 on t3.HouseholdID = t1.householdid

SET @i += 1
END


--create table MARKETING.LOY_ChurnRates3 (
--		Iteration int,
--		StartPeriod varchar(10),
--		EndPeriod varchar(10),
--		HouseholdID int,
--		FirstKnownTransaction date, 
--		Sales decimal(15,3),
--		Baskets int,
--		DaysSince int,
--		Active int
--)

/*If table exists, but you need to truncate or delete the rows,without deleting the table*/
--truncate table MARKETING.LOY_ChurnRates
/*If table exists and you need to add rows*/
insert into MARKETING.LOY_ChurnRates
SELECT * FROM #finaltable


--select top 100000*
--FROM MARKETING.LOY_ChurnRates2