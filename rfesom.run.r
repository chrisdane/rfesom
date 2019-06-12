##########################
## runscript for rfesom ##
##########################
# hints:
# R counts from 1, not zero
# index syntax in R is [], not ()
# T = TRUE, F = FALSE
# not equal condition is !=

## clear work space and close possibly open plot devices
rm(list=ls()); graphics.off()

## Default Mesh Options
rotate_mesh <- T # rotate back to geographic coordinates around Euler angles
                 # If you dont know whether the coordinates in your *.out files are
                 # rotated or not, check 'rotated_grid' in namelist.config of 
                 # the model run (there, .true. means they are rotated so that the
                 # north pole is NOT at 90° N) of the model run or ask the person 
                 # who performed the run.
Ealpha <- 50 # Euler angles (from namelist.config)
Ebeta <- 15
Egamma <- -90
cycl <- T # treat cyclic elements; set true for global mesh
cpl_tag <- F # F: ocean-only, T: coupled
             # needed for filename convention:
             # if cpl_tag == T
             #      <varname_fesom>_fesom_YYYY0101.nc (older esm version)
             #      <runid>_<varname_fesom>_fesom_YYYY0101.nc (newer esm version)
             # if cpl_tag == F
             #      <runid>.YYYY.oce.mean.nc

## Define mesh & experiment
# define paths either relative to this runscript or absolute
if (T) { # demo1
    meshid <- "demomesh"
    meshpath <- "example_data/mesh/demomesh" # *.out files
    cycl <- F # demomesh is not global
    datainpath <- "example_data/data" # fesom data
    runid <- "demo1" # in filenames of fesom data
    postpath <- "example_data/post" # where to save posprocessing output
    plotpath <- "example_data/plot" # where to save plots

} else if (F) { # martin
    meshpath <- "/work/ab0995/a270046/meshes_default/core"
    rotate_mesh <- F
    fnames_user <- "tos_PI_CTRL_fesom.nc" 
    runid <- "PI_CTRL_mw"
    interppath <- "/pf/a/a270106/snow_depth_PI_CTRL/interp"
    postpath <- "/pf/a/a270106/snow_depth_PI_CTRL/"
    plotpath <- "/pf/a/a270106/snow_depth_PI_CTRL/"

} else if (F) { # me
    source("~/scripts/r/myrunids.r")
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948-2009.phi_rotated_grid_false.nc"
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948-2009.phi_rotated_grid_true.nc"
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948.phi_sum_u_times_dx.nc"
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948.phi_sum_udx.nc
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948-2009.dxphi_dyphi.nc"
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/LSea5.1948-2009.dxphi.nc"
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/LSea5.1948-2009.dyphi.nc"
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948-2009.dxphi.nc"
    #fnames_user <- "/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948-2009.dyphi.nc"
    #rotate_mesh <- F
    #cycl <- F
}

## Load postprocessing options
source("namelists/namelist.config.r") 

## Load variable Options
source("namelists/namelist.var.r") 

## Load area and projection Options
source("namelists/namelist.area.r") 

## Load plot Options
source("namelists/namelist.plot.r") 

## Run rfesom
source("lib/main_rfesom.r")

