SELECT UAoA_Compiled.Area_ID AS TransectID, First(UAoA_Compiled.Site_ID) AS SiteID, First(UAoA_Compiled.Date) AS [Date], Val(Left(UAoA_Compiled.Area_ID,(InStr(1,UAoA_Compiled.Area_ID,'.')-1))) AS TrSiteID, First(UAoA_Compiled.Site_Area) AS TArea, IIf(Count(UAoA_Compiled.Area_ID)>1,Count(UAoA_Compiled.Area_ID),IIf(Count(UAoA_Compiled.Area_ID)=1 And Max(UAoA_Compiled.Number_Sightings)>0,1,0)) AS Number_Sightings, IIf(Max(UAoA_Compiled.NumObservers) Is Null,1,Max(UAoA_Compiled.NumObservers)) AS Number_Observers
FROM UAoA_Compiled
WHERE UAoA_Compiled.Method='UAoA'
GROUP BY UAoA_Compiled.Area_ID;
