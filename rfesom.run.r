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
if (T) { # demo1
    runid <- "demo1" # in filenames of fesom data
    meshid <- "demomesh" # 'name' of the mesh; basename(meshpath) if not given
    meshpath <- "example_data/mesh/demomesh" # *.out files
    rotate_mesh <- T # demomesh needs to get rotated back to geograhic coords
    cycl <- F # demomesh is not global
    datainpath <- "example_data/data" # fesom data
    cpl_tag <- F # demodata is from ocean-only experiment
    postpath <- "example_data/post" # where to save posprocessing output
    plotpath <- "example_data/plot" # where to save plots

} else if (F) { # martin
    meshpath <- "/work/ab0995/a270046/meshes_default/core"
    fnames_user <- "tos_PI_CTRL_fesom.nc" 
    runid <- "PI_CTRL_mw"
    interppath <- "/pf/a/a270106/snow_depth_PI_CTRL/interp"
    postpath <- "/pf/a/a270106/snow_depth_PI_CTRL"
    plotpath <- "/pf/a/a270106/snow_depth_PI_CTRL"
    varname <- "tso"
    area <- "global"

} else if (F) { # hp5km08 --> historic with coupled Greenland ice sheet
    meshpath <- "/work/ab0995/a270046/meshes_default/core"
    runid <- "hp5km08"
    datainpath <- "/work/ollie/lackerma/awicm_pism_tests/hp5km08/awicm/outdata/fesom"
    varname <- "virtual_salt"
    area <- "global"
    fnames_user <- "/work/ollie/lackerma/awicm_pism_tests/hp5km08/awicm/outdata/fesom/zos_fesom_20050101_monmean.nc"

} else if (F) { # me
    source("myrunids.r")
}

###################### User input end ######################

## Load variable options
source("namelists/namelist.var.r") 

## Load area and projection options
source("namelists/namelist.area.r") 

## Load plot options
source("namelists/namelist.plot.r") 

## Run rfesom
source("lib/main_rfesom.r")
