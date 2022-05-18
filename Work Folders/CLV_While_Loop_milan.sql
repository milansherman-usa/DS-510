/*SQL while loop construction example*/
/*Step 1: We are going to throw our results into a temporary logging table
first see if that table exists - if so drop
then create the table -- make sure to assign all the columns/variables and their data types here*/
if object_id('tempdb..#allCampaigns_gap') is not null 
	drop table #allCampaigns_gap
create table #allCampaigns_gap (
		mailerID int,
		FactSegmentKey int,
		cc int,
		FST_AW_GAP decimal(10,3), 
		FST_Lift decimal(10,3)
)
/*Step 2: Create a table that you want to loop over.
This could be specific dates, campaigns, products, or customers depending on the task at hand
Make sure to have a row_count variable in this table, to allow for you to iterate over*/
if OBJECT_ID('tempdb..#dates') IS NOT NULL 
	DROP TABLE #dates

declare @StartofYear varchar(10) = DATEADD(Year, -3, GETDATE())
declare @LapsedDateSoY varchar(10) = convert(varchar(10),dateadd(dd,-180,convert(date, convert(char(8),@StartofYear), 112)), 112)
declare @EndofYear varchar(10) = convert(varchar(10),dateadd(dd,364,convert(date, convert(char(8),@StartofYear), 112)),112)
declare @LapsedDateEoY varchar(10) = convert(varchar(10),dateadd(dd,-180,dateadd(dd,364,convert(date, convert(char(8),@StartofYear), 112))),112)

SELECT 


--into #dates

/*Step 3: Construct the while loop statement
Declare variables for the iterator @i and how long to iterate for, @part_count*/
declare @part_count int = (select count(*) from #campaigns) 
declare @i int = 1
while @i <= @part_count 
begin
/*Step 4: Run While loop
First declare the variables that will change with each iteration - this is usually from the iterator table you created above
Then run the main query
Next store the results from said query into the temporary table you created above*/
    declare @mailerID int = (select mailerID from #campaigns where mailerID = @i)
	declare @begdate date = (select BeginDate from #campaigns where mailerID = @i)
	declare @enddate date = (select EndDate from #campaigns where mailerID = @i)
	/*[[[[[[[[ INSERT QUERY TO BE RUN EACH TIME FOR BUSINESS PURPOSE 
	         USES THE DEFINED VARIABLES ABOVE.
	]]]]]]]]]]*/
	insert into #allCampaigns_gap (mailerID,
FactSegmentKey,
cc,
FST_AW_GAP, 
FST_Lift)
	select @mailerID as mailerID, a.FactSegmentKey, a.cc
,a.FST_AW_GAP 
, case when (a.FST_AW_GAP - b.FST_Base_GAP)*a.cc < 0 then 0 else (a.FST_AW_GAP - b.FST_Base_GAP)*a.cc end as FST_Lift
FROM #adweekcompare a
join #basecompare b on b.FactSegmentKey = a.FactSegmentKey --and b.shopstylesegment_key = a.shopstylesegment_key and b.trupriceSegment_Key = a.trupriceSegment_Key
/*This increases the iterator and then finally ends once complete*/
SET @i += 1
end

/*OPTIONAL STEP: if storing in a physical table, you can do the following to store the results from the temp logging table above
FIRST CREATE THE PHYSICAL TABLE WITH A CREATE STATEMENT
DEPENDING ON WHEN/WHERE YOU WILL HAVE THESE OPTIONS AVAILABLE FOR YOUR NEEDS*/
/*If storing for the FIRST time and table does not exist*/
create table MARKETING.LOY_NielsenSegmentation_DeptSales
with (clustered index (customerdimensionid), distribution = replicate)
as
SELECT * FROM #depttemp
/*If table exists, but you need to truncate or delete the rows,without deleting the table*/
truncate table MARKETING.LOY_NielsenSegmentation_DeptSales
/*If table exists and you need to add rows*/
insert into MARKETING.LOY_NielsenSegmentation_DeptSales 
SELECT * FROM #depttemp

---

select
	CYMonthName,
	MIN(ActualDate) as EndPeriod,
	DATEADD(week, -52 ,MIN(ActualDate)) as StartPeriod
from
	POS_MART.DateDimension
where
	ActualDate between DATEADD(week, -156 , CAST(GETDATE() as Date)) and CAST(GETDATE() as DATE)
group by
	CYMonthName
order by
	MIN(ActualDate)
