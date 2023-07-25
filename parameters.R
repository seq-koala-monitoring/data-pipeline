## Parameters file ------------
# Specifies the file location

# 1. Databases -----------
# a) 1996 - 2015 database (accdb)
db_path_1996 = "M:\Users\uqfcho\Documents\seq-koala-monitoring\working_data\SEQkoalaData.accdb"

# b) 

# c) 2020-current monitoring database (accdb)
db_path_2020=r"{M:\Users\uqfcho\Documents\seq-koala-monitoring\working_data\KoalaSurveyData2020_cur.accdb}"

# 3. 
seq_koala_survey_database=r"{M:\Users\uqfcho\Documents\seq-koala-monitoring\our_data\seq_koala_survey_database_ver2_0\koala_survey_data_ver2_0.gdb}"

koala_survey_data=r"{M:\Users\uqfcho\Documents\seq-koala-monitoring\working_data\KoalaSurveyData.gdb}"

paths <- list(
  db_path_1996,
  db_path_2020,
  seq_koala_survey_database,
  koala_survey_data
)

# Check whether the files exist
for (i in names(paths)) {
  filepath <- paths[[i]]
  if (!file.exists(filepath)) {
    stop(sprintf("File %s does not exist in the directory. Make sure you update `parameters.R` to the directory of the database(s) in your system.", i))
  }
}