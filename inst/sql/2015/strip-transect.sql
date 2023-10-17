SELECT TransectID, SiteID, Date, XID AS TrSiteID, SiteName, TArea_effective AS TArea, NumSightings AS Number_Sightings, NumObservers AS Number_Observers, Eastings, Northings
FROM ST_compiled
WHERE Fatal_problem = 0 AND Method = 'ST'
