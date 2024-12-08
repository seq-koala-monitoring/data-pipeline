% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_referencing.R
\name{fcn_route_table_raster}
\alias{fcn_route_table_raster}
\title{Get Route Table (linear referencing) for a series of linear transects}
\usage{
fcn_route_table_raster(input_raster, line_transect, n = 50)
}
\arguments{
\item{input_raster}{Input raster (SpatRaster)}

\item{line_transect}{a \code{sf} object with fields TransectID (unique id), Tlength (length of transect) -- generated by \code{fcn_line_transect_sf}}

\item{n}{number of sample points (including start and finish) for each transect line (precise for (100/n)\% of the length -- e.g. n=50 means precise for 2\% of the length)}
}
\description{
Get Route Table (linear referencing) for a series of linear transects
}
