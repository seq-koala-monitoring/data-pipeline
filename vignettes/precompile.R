library(knitr)
library(filesstrings)

knitr::knit('vignettes/v01-setup.Rmd.orig','vignettes/v01-setup.Rmd' )
knitr::knit('vignettes/v02-get-data.Rmd.orig','vignettes/v02-get-data.Rmd' )
knitr::knit('vignettes/v03-linear-referencing.Rmd.orig','vignettes/v03-linear-referencing.Rmd' )
knitr::knit('vignettes/v04-covariates.Rmd.orig','vignettes/v04-covariates.Rmd' )

fig_files <- grep("fig-", list.files(), value = TRUE)
filesstrings::move_files(fig_files, 'vignettes/', overwrite = T)
