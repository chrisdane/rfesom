##########################
## runscript for rfesom ##
##########################
# hints:
# R counts from 1, not zero
# index syntax in R is [], not ()
# T = TRUE, F = FALSE
# 'not equal' condition is !=

## clear work space and close possibly open plot devices
rm(list=ls()); graphics.off()

## Load default options
source("namelists/namelist.config.r") 

## This is the demo runscript
runid <- "demo" # in filenames of fesom data
cpl_tag <- F # demodata is from ocean-only experiment
meshpath <- "example_data/mesh" # *.out files
meshid <- "demomesh" # name of the mesh; basename(meshpath) if not given
rotate_mesh <- T # demomesh needs to get rotated back to geograhic coords
cycl <- F # demomesh is not global
datainpath <- "example_data/data" # fesom data
varname <- "ssh" # see namelist.var.r
area <- "lsea" # see namelist.area.r
regular_transient_out <- T
out_mode <- "area"

## Load plot options
source("namelists/namelist.plot.r") 

## Load variable options
source("namelists/namelist.var.r") 

## Load area and projection options
source("namelists/namelist.area.r") 

## Run rfesom
source("lib/main_rfesom.r")
