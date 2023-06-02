# Data Processing Pipeline for SEQ Koala Monitoring Data Analysis project

Reads an ACCESS database directly with R script, and then creates the following tables as inputs to a Bayesian state-space model. The user will need to specify the parameters in the .env file.

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
