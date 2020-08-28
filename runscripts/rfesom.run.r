#########################################
## runscript for rfesom                ##
## https://github.com/chrisdane/rfesom ##
#########################################

rfesompath <- "~/scripts/r/rfesom" 

# any variable already defined in rfesom/namelists/namelist.config.r can be overwritten here
if (F) {
    datainpaths <- "/work/ab0246/a270124/esm-experiments/awicm_pism/LIG01/outdata/fesom"
    fpatterns <- "<runid>_fesom_<varname_nc>_<YYYY>0101.nc"
    runid <- "LIG01"
    meshpath <- "/work/ab0246/a270064/meshes/CORE2_final"
    rotate_mesh <- T 
    #varname <- "resolutionkm"
    varname <- "tos"
    years <- 1935
    recs <- 1:2
    regular_ltm_out <- T
} else if (T) { # esgf
    if (F) {
        datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/Omon/tob/gn/v20200212"
        fpatterns <- "<varname>_Omon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
        meshpath <- "/work/ab0995/a270046/meshes_default/core"
        rotate_mesh <- F
        postprefix <- "awi-esm-1-1-lr-piControl"
        YYYY_from <- 1855
        YYYY_to <- 1954
        #years <- 1855
        years <- 1855:1954
    } else if (T) {
        datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/PMIP/AWI/AWI-ESM-1-1-LR/lgm/r1i1p1f1/Omon/tob/gn/v20200212"
        fpatterns <- "<varname>_Omon_AWI-ESM-1-1-LR_lgm_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
        meshpath <- "/work/ab0246/a270064/meshes/CORE2_lgmf"
        rotate_mesh <- T
        postprefix <- "awi-esm-1-1-lr-lgm"
        YYYY_from <- 3901
        YYYY_to <- 4000
        years <- 3901
        #years <- 3901:4000
    }
    varname <- "tob"
    postpath <- "/work/ab0246/a270073/post/fesom"
    plotpath <- "/work/ab0246/a270073/plots/fesom"
    #transient_out <- T
    #out_mode <- "mean"
    #regular_ltm_out <- F
}

# do not change below this line
source(paste0(rfesompath, "/lib/main_rfesom.r")) 
