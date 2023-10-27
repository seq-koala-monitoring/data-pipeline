SELECT tblKoalaSurveyData2020_cur.TransectID, tblKoalaSurveyData2020_cur.SightingID, IIf([tblKoalaSurveyData2020_cur]![Perp_Dist] Is Null,([tblKoalaSurveyData2020_cur]![Distance_bt_Observer_Koala]*Sin([tblKoalaSurveyData2020_cur]![Angle_Koala]*3.14159/180)/1),[tblKoalaSurveyData2020_cur]![Perp_Dist]) AS Perp_Dist, Date
FROM tblKoalaSurveyData2020_cur
WHERE (((tblKoalaSurveyData2020_cur.Sighting_Number)>0) AND ((tblKoalaSurveyData2020_cur.Method)='SOL'));
