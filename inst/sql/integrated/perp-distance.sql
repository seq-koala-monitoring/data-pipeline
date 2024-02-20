SELECT SOL_Compiled.Transect_ID AS TransectID, SOL_Compiled.Sighting_ID AS SightingID, IIf([SOL_Compiled]![Perp_Dist] Is Null,([SOL_Compiled]![Distance_Observer_Koala]*Sin([SOL_Compiled]![Angle_Koala]*3.14159/180)/1),[SOL_Compiled]![Perp_Dist]) AS Perp_Dist, Date
FROM SOL_Compiled
WHERE (SOL_Compiled.Sighting_Number>0) AND (SOL_Compiled.Method='SOL' OR SOL_Compiled.Method='DOL');
