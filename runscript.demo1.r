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

###################### User input start ######################

## Define mesh & experiment
# Define paths either relative to this runscript or absolute.
# Any variable already defined in namelist.config.r can be overwritten here.
# If you change nothing and run this runscript in R, by e.g. source("rfesom.run.r"), 
# you'll see demo1.

runid <- "demo1" # in filenames of fesom data
meshid <- "demomesh" # 'name' of the mesh; basename(meshpath) if not given
meshpath <- "example_data/mesh/demomesh" # *.out files
rotate_mesh <- T # demomesh needs to get rotated back to geograhic coords
cycl <- F # demomesh is not global
datainpath <- "example_data/data" # fesom data
cpl_tag <- F # demodata is from ocean-only experiment
postpath <- "example_data/post" # where to save posprocessing output
plotpath <- "example_data/plot" # where to save plots

###################### User input end ######################

## Load variable options
source("namelists/namelist.var.r") 

## Load area and projection options
source("namelists/namelist.area.r") 

## Load plot options
source("namelists/namelist.plot.r") 

## Run rfesom
source("lib/main_rfesom.r")
