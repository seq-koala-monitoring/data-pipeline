# Stand-alone to write the list of RDS objects into a three-dimensional array on the QUT linux HPC

out_dir <- "output/20240301"

fcn_temporal_covariate_rds_array <- function(out_dir = NULL) {
  out_dir_files <- list.files(out_dir, pattern = "cov_temporal_\\d{6}_\\d{6}.rds")
  pattern <- "(\\d{6}_\\d{6})"
  dates <- regmatches(out_dir_files, regexpr(pattern, out_dir_files))

  for (i in 1:length(dates)) {
    date_id <- dates[i]
    print(paste("Processing covariate date: ", date_id))
    file <- paste0("cov_temporal_", date_id, ".rds")
    if (!(file %in% out_dir_files)) {
      stop(paste("Date", date_id, "does not exist in out_dir."))
    }
    cov <- readRDS(file.path(out_dir, file))
    if (i==1) {
      out <- cov
    } else {
      out <- abind::abind(out, cov, along = 3)
    }
    rm(cov)
    gc()
  }
  return(out)
}


output <- fcn_temporal_covariate_rds_array(paste0(out_dir,'/cov_raster'))
saveRDS(output, file.path(out_dir, "cov_temporal_array.rds"))
