#########################################
## runscript for rfesom                ##
## https://github.com/chrisdane/rfesom ##
#########################################

this_runscript_filename <- "rfesom.run.r" # just basename if saved in directory 
# rfesom/runscripts or absolute path if saved somehwhere else
#rfesompath  <- normalizePath("../") # assume this runscript was not moved 
rfesompath <- system("git rev-parse --show-toplevel", intern=T)

# any variable already defined in rfesom/namelists/namelist.config.r can be overwritten here
if (T) {
    datainpath <- "/work/ab0246/a270124/esm-experiments/awicm_pism/LIG01/outdata/fesom"
    fpattern <- "<runid>_fesom_<varname_nc>_<YYYY>0101.nc"
    runid <- "LIG01"
    meshpath <- "/work/ab0246/a270064/meshes/CORE2_final"
    rotate_mesh <- T 
    #varname <- "resolutionkm"
    varname <- "tos"
    years <- 1935
    recs <- 1:2
    regular_ltm_out <- T
}

## do not change below this line ##
# run rfesom
source(paste0(rfesompath, "/lib/main_rfesom.r")) 

