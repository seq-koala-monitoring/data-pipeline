library(SEQKoalaDataPipeline)

source('inst/set_params.R')

fcn_cov_array('temporal', d, write_path = paste0(out_dir, "\\cov_raster"))
