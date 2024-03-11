SELECT SOL_Compiled.Transect_ID AS TransectID, First(SOL_Compiled.Site_ID) AS SiteID, First(SOL_Compiled.Date) AS [Date], Val(Left([Transect_ID],(InStr(1,[Transect_ID],'.')-1))) AS TrSiteID, First(SOL_Compiled.T_Length) AS Tlength, First(SOL_Compiled.Number_Sightings) AS Number_Sightings, IIf(Max(SOL_Compiled.Number_Observers) Is Null,1,Max(SOL_Compiled.Number_Observers)) AS Number_Observers, First(SOL_Compiled.Start_Eastings) AS Start_Eastings, First(SOL_Compiled.Start_Northings) AS Start_Northings, First(SOL_Compiled.End_Eastings) AS End_Eastings, First(SOL_Compiled.End_Northings) AS End_Northings
FROM SOL_Compiled
WHERE SOL_Compiled.Method='SOL' OR SOL_Compiled.Method='DOL'
GROUP BY SOL_Compiled.Transect_ID;
