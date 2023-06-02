library(dotenv)
library(RODBC)
library(dplyr)
library(readr)
library(sf)
library(mapview)

# Source dependency function files
source("code/line-transect.R")

# Get the path of the database from the .env file
DB_PATH <- Sys.getenv("DB_PATH")

DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
PATH <- paste0(DRIVERINFO, "DBQ=", DB_PATH)
channel <- odbcDriverConnect(PATH)

## Get line transect table and get sf object for the line transects
query <- read_file("queries/line-transect.sql")
lineTransect <- sqlQuery(channel, query)
lineTransectSf <- fcn_line_transect_sf(lineTransect)
mapview(lineTransectSf)

 ## Get single observer line koala observations
query <- read_file("queries/sol-observations.sql")
solObservations <- sqlQuery(channel, query)

if (length(unique(solObservations$SightingID)) != nrow(solObservations)) {
  error("Sighting ID does not uniquely identify koala observations")
}
