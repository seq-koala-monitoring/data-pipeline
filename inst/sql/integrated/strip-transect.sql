SELECT ST_Compiled.Transect_ID AS TransectID, First(ST_Compiled.Site_ID) AS SiteID, First(ST_Compiled.Date) AS [Date], Val(Left([Transect_ID],(InStr(1,[Transect_ID],'.')-1))) AS TrSiteID, First(ST_Compiled.T_Area) AS TArea, First(ST_Compiled.Number_Sightings) AS Number_Sightings, IIf(Max(ST_Compiled.Number_Observers) Is Null,1,Max(ST_Compiled.Number_Observers)) AS Number_Observers
FROM ST_Compiled
WHERE (((ST_Compiled.Method)='ST') AND Fatal_Problem=FALSE)
GROUP BY ST_Compiled.Transect_ID;
