# Data Processing Pipeline for SEQ Koala Monitoring Data Analysis project

An R package with functionality that consumes ACCESS databases of koala monitoring data and outputs tables that are direct inputs to a Bayesian state-space model.

This vignette describes the usage of the toolbox provided by the custom
R package `SEQKoalaDataPipeline` that reads from ACCESS databases of
koala monitoring data, spatial files of monitoring transects, and
rasterised data of covariates, and produces outputs for the spatial data
model.

The functions in the toolbox perform several functions useful for
producing data outputs for the statistical models and other
functionalities. Features include but are not limited to the following:

1.  Establish connections to ACCESS through ODBC

2.  Generate grids "fishnets" at any given spatial resolution around the
    study area and generates unique IDs for each grid cell
    
3.  Produce spatial feature classes (sf) of line transects based on
    start and end coordinate columns in dataframes
    
4.  Linear referencing: calculate the proportion of, and distance of
    that segment from the origin, of a linear transect lying within a
    grid cell
    
5.  Linear referencing: calculate the proportion of, and distance of
    segments from the origin, of a linear transect from raster covariate
    layers
    
6.  Dynamic segmentation: return the coordinates of koala sightings
    based on distance from transect origin (work-in-progress)
    
7.  Dynamic segmentation: return the covariate information of each koala
    sighting based on linear referencing route tables (work-in-progress)
    

# Installation

If `devtools` is installed on the local machine, the package can be installed using Github.

```
devtools::install_github("seq-koala-monitoring/data-pipeline")
```

# Data Structure

This package produces table inputs for the statistical model, with the possibility to export auxiliary columns of the processed survey data on demand. The data structure for the inputs are as follows.

## Line Transects (2 tables)

Table of the surveys (each line is a separate transect surveyed) with the following fields:
* Record unique ID (primary key)
* Site number (with this being a unique ID relating to a spatial representation of the site surveyed– for pre 2015 data)
* Transect number (with this being a unique ID relating to a spatial representation of the transect surveyed – for post 2015 data)
* Date of survey
* Transect length
* Number of koalas observed
* Number of observers (although this should always be 1 for line transects)
* Time and location specific covariates (still to be discussed what these will be)

Table of the perpendicular distances of observations (each line relates to a specific koala observation):
* Record unique ID (primary key)
* Unique ID from line transect table (this relates to the survey that the koala observation occurred on from the surveys table)
* Perpendicular distance (noting sometimes this has to be calculated from the sighting distance and angle and sometimes the perpendicular distance is measured directly – to discuss)

## Strip Transects (1 table)

Table of the surveys (each line is a separate transect surveyed) with the following fields:
* Record unique ID (primary key)
* Site number (with this being a unique ID relating to a spatial representation of the site surveyed– for pre 2015 data)
* Transect number (with this being a unique ID relating to a spatial representation of the transect surveyed – for post 2015 data)
* Date of survey
* Transect area
* Number of koalas observed
* Number of observers
* Time and location specific covariates (still to be discussed what these will be)

## All of Area Searches (1 table)

Table of the surveys (each line is a separate area search) with the following fields:
* Record unique ID (primary key)
* Site number (with this being a unique ID relating to a spatial representation of the site surveyed– for pre 2015 data)
* Area number (with this being a unique ID relating to a spatial representation of the area surveyed – for post 2015 data)
* Date of survey
* Area searches
* Number of koalas observed
* Number of observers
* Time and location specific covariates (still to be discussed what these will be)
