SELECT TransectID & '.' & tblSightings.[Sighting Number] AS SightingID, tblTransects.[Site Number] & '_' & tblSightings.[Sub-Survey Number] & '_' & tblSightings.[Transect Number] & '_' & Switch(tblTransects.SurveyMethod='Strip', 'ST', tblTransects.SurveyMethod='Line', 'SOL', tblTransects.SurveyMethod='Urban', 'UAoA') & '_' & Format(tblTransects.Date, 'yyyymmdd') AS TransectID, tblSightings.[Sighting Number] AS SightingNo
FROM tblSightings LEFT JOIN tblTransects ON (tblSightings.[Transect Number] = tblTransects.[Transect Number]) AND (tblSightings.[Site Number] = tblTransects.[Site Number]) AND (tblSightings.[Sub-Survey Number] = tblTransects.[Sub-Survey Number]) AND (tblSightings.[Survey Number] = tblTransects.[Survey Number]);