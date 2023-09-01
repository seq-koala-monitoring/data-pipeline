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
