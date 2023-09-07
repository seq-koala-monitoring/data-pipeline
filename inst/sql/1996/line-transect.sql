SELECT FIRST(combined_table.UniqueID) AS TransectID, FIRST(combined_table.SiteID) AS SiteID, FIRST(combined_table.Date) AS [Date], FIRST(combined_table.Tlength) AS Tlength, IIf(Count([UniqueID])>1,Count([UniqueID]),IIf(Count([UniqueID])=1 And Max([Sighting_Number])>0,1,0)) AS Number_Sightings
FROM ( SELECT tblTransects.[Site Number] & '.' & tblTransects.[Sub-Survey Number] & '_' & tblTransects.[Transect Number] & '_' & Switch(tblTransects.SurveyMethod='Strip', 'ST', tblTransects.SurveyMethod='Line', 'SOL', tblTransects.SurveyMethod='Urban', 'UAoA') & '.' & Format(tblTransects.Date, 'yyyymmdd') AS UniqueID, tblTransects.[Site Number] AS SiteID, tblTransects.Date, tblTransects.Length AS Tlength, tblSightings.[Sighting Number] AS Sighting_Number, tblTransects.[Number of Searchers] AS Number_Observers
    FROM tblSightings RIGHT OUTER JOIN tblTransects ON (tblSightings.[Transect Number] = tblTransects.[Transect Number]) AND (tblSightings.[Site Number] = tblTransects.[Site Number]) AND (tblSightings.[Sub-Survey Number] = tblTransects.[Sub-Survey Number]) AND (tblSightings.[Survey Number] = tblTransects.[Survey Number])
    WHERE tblTransects.SurveyMethod='Line'
) AS combined_table
GROUP BY combined_table.UniqueID;