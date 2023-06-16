paths <- list(
  db_path_1996=r"{M:\Users\uqfcho\Documents\seq-koala-monitoring\working_data\SEQkoalaData.accdb}",
  db_path_2020=r"{M:\Users\uqfcho\Documents\seq-koala-monitoring\working_data\KoalaSurveyData2020_cur.accdb}",
  seq_koala_survey_database=r"{M:\Users\uqfcho\Documents\seq-koala-monitoring\our_data\seq_koala_survey_database_ver2_0\koala_survey_data_ver2_0.gdb}",
  koala_survey_data=r"{M:\Users\uqfcho\Documents\seq-koala-monitoring\working_data\KoalaSurveyData.gdb}"
)

# Check whether the files exist
for (i in names(paths)) {
  filepath <- paths[[i]]
  if (!file.exists(filepath)) {
    stop(sprintf("File %s does not exist in the directory. Make sure you update `parameters.R` to the directory of the database(s) in your system.", i))
  }
}