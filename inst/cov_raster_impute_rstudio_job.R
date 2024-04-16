library(rstudioapi)
rm(list=ls())

setwd("H:/seq-koala-monitor/")
cov_layer_df <- read.csv("working_data/covariates_impute/cov_layer_df.csv")

for (cov in unique(cov_layer_df$name)){
  id <- which(cov_layer_df$name == cov)
  jobRunScript("data-pipeline/inst/cov_raster_impute.R",
               name = paste0("cov_impute:", cov),
               workingDir = "H:/seq-koala-monitor/",
               importEnv = TRUE)
}
