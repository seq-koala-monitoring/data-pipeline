SELECT TransectID, SiteID, Date, Val(Left([TransectID],(InStr(1,[TransectID],'.')-1))) AS TrSiteID, Tlength, Sighting_Number, Number_Observers, Start_Eastings, Start_Northings, End_Eastings, End_Northings
FROM tblKoalaSurveyData2020_cur
WHERE Method='SOL' and Sighting_Number<=1
