library(httr2)
library(sf)
library(SEQKoalaDataPipeline)
library(future)
library(future.apply)

# Time dimensions
dates <- fcn_date_intervals() %>%
  lapply(\(x) x$middle_date)

# Study grid
grid_size = 20000

seq_study_extent <- st_read(r'{C:\Users\uqfcho\Documents\seq-koala-monitoring\working_data\basedata.gdb}', layer = 'seqrp_study_area_2017_mga56')
seq_study_extent_buffer <- seq_study_extent %>%
  st_buffer(3000)
seq_grid <- seq_study_extent_buffer %>%
  st_make_grid(grid_size) %>%
  st_transform(4283)
seq_grid_intersect <- seq_grid %>%
  st_intersects(st_transform(seq_study_extent_buffer, 4283), sparse = F)
seq_grid <- seq_grid[seq_grid_intersect]
st_crs(seq_grid) <- 4283
seq_grid_bbox <- lapply(seq_grid, \(x) st_bbox(x))
print(paste("Number of downloads per time slice:", length(seq_grid)))

# Download script

fcn_extract_api <- function(bbox,
                            date,
                            grid_size = 20000,
                            path_to_write = "C:/Users/uqfcho/Documents/seq-koala-monitoring/time_cadastre",
                            lotplan_only = TRUE) {
  req_code <- sprintf("date_%s_size_%s", date, grid_size)
  request <- request('https://uat-spatial-gis.information.qld.gov.au/arcgis/rest/services/PlanningCadastre/LandParcelPropertyFrameworkFeature_TimeAware/MapServer/0/query') %>%
    req_url_query(
      where = sprintf("(date_st IS NULL OR date_st <= timestamp '%s 23:59:59') AND (date_end > timestamp '%s 23:59:59' OR date_end IS NULL)", date, date),
      text= "",
      objectIds= "",
      time= "",
      timeRelation= "esriTimeRelationOverlapsStartWithinEnd",
      geometry = sprintf("{xmin: %s, ymin: %s, xmax: %s, ymax: %s}", bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax),
      geometryType = "esriGeometryEnvelope",
      inSR = 4283,
      spatialRel= "esriSpatialRelIntersects",
      distance = "",
      units = "esriSRUnit_Meter",
      relationParam = "",
      outFields = "",
      returnGeometry = TRUE,
      returnTrueCurves = FALSE,
      maxAllowableOffset = "",
      geometryPrecision = "",
      outSR = "",
      havingClause = "",
      returnIdsOnly = FALSE,
      returnCountOnly = FALSE,
      orderByFields = "",
      groupByFieldsForStatistics = "",
      outStatistics = "",
      returnZ = FALSE,
      returnM = FALSE,
      gdbVersion = "",
      historicMoment = "",
      returnDistinctValues = FALSE,
      resultOffset = "",
      resultRecordCount = "",
      returnExtentOnly = FALSE,
      sqlFormat = "none",
      datumTransformation = "",
      parameterValues = "",
      rangeValues = "",
      quantizationParameters = "",
      featureEncoding = "esriDefault",
      f = "geojson"
    )

  res <- request |> req_perform(verbosity = 0)

  # Error handling
  if (res |> resp_status() != 200) {
    print(sprintf("Request id %s failed with error message: %s", req_code, res |> resp_status_desc()))
    return()
  }

  res_geojson <- res |> resp_body_string()
  sf_obj <- st_read(res_geojson)
  sf_obj_lotplan <- sf_obj %>%
    dplyr::filter(!is.na(lotplan))
  if (lotplan_only) {
    st_write(sf_obj_lotplan, dsn = paste0(path_to_write, '/cadastre_', req_code, '_lotplan.shp'), update = T)
    return (sf_obj_lotplan)
  } else {
    st_write(sf_obj, dsn = paste0(path_to_write, '/cadastre_', req_code, '_full.shp'), update = T)
    return(sf_obj)
  }
}

# Execute in parallel
plan(multisession)
lapply(dates, function (d) {
  future.apply::future_lapply(seq_grid_bbox, function(b) fcn_extract_api(b, d))
})
