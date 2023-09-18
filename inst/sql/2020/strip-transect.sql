SELECT tblKoalaSurveyData2020_cur.TransectID, First(tblKoalaSurveyData2020_cur.SiteID) AS SiteID, First(tblKoalaSurveyData2020_cur.Date) AS [Date], Val(Left([TransectID],(InStr(1,[TransectID],'.')-1))) AS TrSiteID, First(tblKoalaSurveyData2020_cur.Tarea) AS TArea, IIf(Count([TransectID])>1,Count([TransectID]),IIf(Count([TransectID])=1 And Max([Sighting_Number])>0,1,0)) AS Number_Sightings, IIf(Max(tblKoalaSurveyData2020_cur.Number_Observers) Is Null,1,Max(tblKoalaSurveyData2020_cur.Number_Observers)) AS Number_Observers
FROM tblKoalaSurveyData2020_cur
WHERE (((tblKoalaSurveyData2020_cur.Method)='ST'))
GROUP BY tblKoalaSurveyData2020_cur.TransectID;
