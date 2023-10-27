SELECT TransectID, SightingID, IIF([Sightings_compiled]![Perp_Dist] IS NULL, [Sightings_compiled]![Distance_bt_Obs]*Sin([Sightings_compiled]![Angle_Koala]*3.14159/180)/1, [Sightings_compiled]![Perp_Dist]) AS Perp_Dist, Date
FROM Sightings_compiled
