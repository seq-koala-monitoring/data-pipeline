library(knitr)
library(filesstrings)

# Precompile vignette
# 1. Scans and deletes .orig files if the Rmd file exists
# 2.

vignette_list <- c('v01-setup', 'v02-get-data', 'v03-linear-referencing', 'v04-covariates')

lapply(vignette_list, function(v) {
  v_source_path <- sprintf('vignettes/source/%s.Rmd', v)
  v_orig_path <- sprintf('vignettes/%s.Rmd.orig', v)
  v_knit_path <- sprintf('vignettes/%s.Rmd', v)
  if (file.exists(v_source_path)) {
    file.copy(v_source_path, v_knit_path, overwrite = T)
    if (file.exists(v_orig_path)) file.remove(v_orig_path)
    file.rename(v_knit_path, v_orig_path)
    knitr::knit(v_orig_path, v_knit_path)
  }
  return()
})

#knitr::knit('vignettes/v01-setup.Rmd.orig','vignettes/v01-setup.Rmd' )
#knitr::knit('vignettes/v02-get-data.Rmd.orig','vignettes/v02-get-data.Rmd')
#knitr::knit('vignettes/v03-linear-referencing.Rmd.orig','vignettes/v03-linear-referencing.Rmd' )
#knitr::knit('vignettes/v04-covariates.Rmd.orig','vignettes/v04-covariates.Rmd' )

fig_files <- grep("fig-", list.files(), value = TRUE)
filesstrings::move_files(fig_files, 'vignettes/', overwrite = T)
