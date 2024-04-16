## Study area buffer
fcn_set_home_dir(r"(H:\seq-koala-monitor\working_data)")
fcn_set_raster_path(list(covariates = 'covariates/output'))
study_area <- fcn_get_study_area()

cov_layer_df <- fcn_covariate_layer_df()

readr::write_csv(cov_layer_df, '../working_data/covariates_impute/cov_layer_df.csv')
