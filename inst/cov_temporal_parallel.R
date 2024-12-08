# Run data pipeline for inputs to the statistical model
#rm(list=ls())
#gc()

library(SEQKoalaDataPipeline)
library(rstudioapi)

## 2. Set global variables -------------------------------------------------
source('inst/set_params.R')

dates <- fcn_get_date_intervals()

cov_constant_array <- fcn_cov_array('constant', write_path = out_dir)

for (i in 1:length(dates)) {
  d <- dates[[i]]
  id <- d$id
  d <- list(d)
  #source('inst/run_cov_date.R')
  if (exists("use_parallel") & use_parallel){
    rstudioapi::jobRunScript(
    'inst/run_cov_date.R',
      name = id,
      importEnv = TRUE,
      workingDir = getwd()
    )
    if (i %% 8 == 0) Sys.sleep(180)
    Sys.sleep(10)
  } else {
    source('inst/run_cov_date.R')
  }

}
if (exists("use_parallel") & use_parallel) Sys.sleep(200)
