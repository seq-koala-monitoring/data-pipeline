library(dotenv)
library(RODBC)
library(dplyr)
library(readr)
library(sf)
library(mapview)

# Source dependency function files
source("code/line-transect.R")
source("parameters.R")

# Data file
year <- 1996
DB_PATH <- paths[paste0('db_path_', year)]
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
PATH <- paste0(DRIVERINFO, "DBQ=", DB_PATH)
channel <- odbcDriverConnect(PATH)

## Get line transect table and get sf object for the line transects (after 2015)
query <- read_file(paste0("queries/", year, "/line-transect.sql"))
lineTransect <- sqlQuery(channel, query, errors = T)

if (sum(duplicated(lineTransect$SiteID)) > 0) {
  stop(sprintf("%i duplicate SiteID", sum(duplicated(lineTransect$SiteID))))
}

lineTransectSf <- fcn_line_transect_sf(lineTransect)
mapview(lineTransectSf)

## Get perpendicular distances
query <- read_file(paste0("queries/", year, "/perpendicular-distances.sql"))
solObservations <- sqlQuery(channel, query)

if (length(unique(solObservations$SightingID)) != nrow(solObservations)) {
  error("Sighting ID does not uniquely identify koala observations")
}


## Strip transect table


