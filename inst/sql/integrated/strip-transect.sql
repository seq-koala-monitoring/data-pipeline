SELECT ST_Compiled.Transect_ID AS TransectID, First(ST_Compiled.Site_ID) AS SiteID, First(ST_Compiled.Date) AS [Date], Val(Left([Transect_ID],(InStr(1,[Transect_ID],'.')-1))) AS TrSiteID, First(ST_Compiled.T_Area) AS TArea, IIf(Count([Transect_ID])>1,Count([Transect_ID]),IIf(Count([Transect_ID])=1 And Max(ST_Compiled.Number_Sightings)>0,1,0)) AS Number_Sightings, IIf(Max(ST_Compiled.Number_Observers) Is Null,1,Max(ST_Compiled.Number_Observers)) AS Number_Observers
FROM ST_Compiled
WHERE (((ST_Compiled.Method)='ST') AND Fatal_Problem=FALSE)
GROUP BY ST_Compiled.Transect_ID;
