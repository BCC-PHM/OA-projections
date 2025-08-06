/*
Link diagnosis codes to admission dates and total over last 3 years for each older adult 5-year age band

Definition: Emergency admissions for falls injuries classified by primary diagnosis code (ICD10 code S00 to T98) 
 and external cause (ICD10 code W00 to W19) and an emergency admission code (episode order number equals 1, admission 
 method starts with 2).

*/
WITH Admissions AS (SELECT
	DISTINCT d1.[EpisodeId],
	CASE
		WHEN AgeOnAdmission BETWEEN 0 AND 49 THEN '0-49'
		WHEN AgeOnAdmission BETWEEN 50 AND 54 THEN '50-54'
		WHEN AgeOnAdmission BETWEEN 55 AND 59 THEN '55-59'
		WHEN AgeOnAdmission BETWEEN 60 AND 64 THEN '60-64'
		WHEN AgeOnAdmission BETWEEN 65 AND 69 THEN '65-69'
		WHEN AgeOnAdmission BETWEEN 70 AND 74 THEN '70-74'
		WHEN AgeOnAdmission BETWEEN 75 AND 79 THEN '75-79'
		WHEN AgeOnAdmission BETWEEN 80 AND 84 THEN '80-84'
		WHEN AgeOnAdmission BETWEEN 85 AND 89 THEN '85-89'
		WHEN AgeOnAdmission >= 90 THEN '90+'
		ELSE 'Unknown'
	END AS AgeBand,
	GenderDescription AS Sex
	FROM EAT_Reporting_BSOL.SUS.VwInpatientEpisodesDiagnosisRelational d1
	JOIN EAT_Reporting_BSOL.SUS.VwInpatientEpisodesDiagnosisRelational d2
		ON d1.[EpisodeId] = d2.[EpisodeId]
	LEFT JOIN EAT_Reporting_BSOL.SUS.VwInpatientEpisodesPatientGeography AS B
		ON d1.[EpisodeId] = B.[EpisodeId]
	WHERE d1.DiagnosisOrder = 1
		AND LEFT(d1.DiagnosisCode, 3) BETWEEN 'S00' AND 'T98'
		AND LEFT(B.AdmissionMethodCode, 1) = '2' 
		AND LEFT(d2.DiagnosisCode, 3) BETWEEN 'W00' AND 'W19' AND
		YEAR(AdmissionDate) IN (2022, 2023, 2024) AND
		OSLAUA = 'E08000025' AND
		NOT GenderDescription = 'Not known'
	)

SELECT 
	AgeBand, 
	Sex,
	COUNT(*) AS Num_Admissions
FROM Admissions
GROUP BY AgeBand,Sex
ORDER BY AgeBand


SELECT TOP 10 *
FROM EAT_Reporting_BSOL.SUS.VwInpatientEpisodesPatientGeography