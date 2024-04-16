SELECT UAoA_Compiled.Area_ID AS TransectID, First(UAoA_Compiled.Site_ID) AS SiteID, First(UAoA_Compiled.Date) AS [Date], Val(Left(UAoA_Compiled.Area_ID,(InStr(1,UAoA_Compiled.Area_ID,'.')-1))) AS TrSiteID, First(UAoA_Compiled.Site_Area) AS TArea, First(UAoA_Compiled.Number_Sightings) AS Number_Sightings, IIf(Max(UAoA_Compiled.Number_Observers) Is Null,1,Max(UAoA_Compiled.Number_Observers)) AS Number_Observers
FROM UAoA_Compiled
WHERE UAoA_Compiled.Method='UAoA'
GROUP BY UAoA_Compiled.Area_ID;
