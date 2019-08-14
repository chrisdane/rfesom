#########################################
## runscript for rfesom                ##
## https://github.com/chrisdane/rfesom ##
#########################################

# hints:
# R counts from 1, not zero
# index syntax in R is [], not ()
# T = TRUE, F = FALSE
# 'not equal' condition is !=

## clear work space and close possibly open plot devices
rm(list=ls()); graphics.off()
rfesompath <- system("git rev-parse --show-toplevel", intern=T) # default; change here if necessary

## Load default options
source(paste0(rfesompath, "/namelists/namelist.config.r")) 

###################### User input start ######################

# Define paths either relative to this runscript or absolute.
# Any variable already defined in namelist.config.r can be overwritten here.
if (F) { # martin
    meshpath <- "/work/ab0995/a270046/meshes_default/core"
    #fnames_user <- "tos_PI_CTRL_fesom.nc" 
    fnames_user <- "/pf/a/a270106/snow_depth_PI_CTRL/tos_fesom_DJF1.nc" 
    runid <- "PI_CTRL_mw"
    interppath <- "/pf/a/a270106/snow_depth_PI_CTRL/interp"
    #interppath <- "/work/ba0941/a270073/mesh/core/interp"
    postpath <- "/pf/a/a270106/snow_depth_PI_CTRL"
    plotpath <- "/pf/a/a270106/snow_depth_PI_CTRL"
    derivpath <- "/work/ba0941/a270073/mesh/core/derivatives"
    varname <- "resolutionkm"
    area <- "global"

} else if (F) { # christian
    runid <- "MPmes"
    meshid <- "p_mesh_MP"
    cpl_tag <- F
    if (T) {
        fesom_version <- "1.4"
        setting <- "fesom1"
        meshpath <- "/pf/a/a270061/prep_bound/PlioMIP2/fesom/p_mesh_MP/mesh"
        datainpath <- "/mnt/lustre01/work/ab0246/a270061/out/partMesh/s1"
        varname <- "temp"
    } else if (T) {
        fesom_version <- "2.0"
        setting <- "fesom2"
        meshpath <- "/pf/a/a270061/prep_bound/PlioMIP2/fesom2/p_mesh_MP/mesh"
        datainpath <- "/mnt/lustre01/work/ab0246/a270061/out/partMesh/s1_fesom2"
        varname <- "temp" #"sst"
        recs <- 1
    }
    years <- 1948#:1955

} else if (F) { # hp5km08 --> historic with coupled Greenland ice sheet
    meshpath <- "/work/ollie/pool/FESOM/meshes_default/core"
    runid <- "hp5km08"
    datainpath <- "/work/ollie/lackerma/awicm_pism_tests/hp5km08/awicm/outdata/fesom"
    varname <- "virtual_salt"
    area <- "global"
    years <- 1850:2005
    regular_ltm_out <- T
    transient_out <- F
    out_mode <- "area"

} else { # chris
    source("~/scripts/r/myrunids.r")
}

###################### User input end ######################

## Load plot options
source(paste0(rfesompath, "/namelists/namelist.plot.r")) 

## Load variable options
source(paste0(rfesompath, "/namelists/namelist.var.r"))

## Load area and projection options
source(paste0(rfesompath, "/namelists/namelist.area.r"))

## Run rfesom
source(paste0(rfesompath, "/lib/main_rfesom.r"))
