#############################
## User input for rfesom.r ##
#############################
# define with '<<-' instead of '<-'
# T = TRUE; F = FALSE

## Default rfesom options
rfesom_path <<- getwd() # = pwd

## Default Experiment options
cpl_tag <<- F ## F: ocean-only, T: coupled
              ## this concerns the fesom filename convention, e.g.
              ## <runid>.YYYY.oce.mean.nc if cpl_tag = F or
              ## <varname_fesom>_fesom_YYYY0101.nc if cpl_tag = T and older esm version or
              ## <runid>_<varname_fesom>_fesom_YYYY0101.nc if cpl_tag = T and newer esm version
              ## (see <varname_fesom> definition in namelist.var.r)

## Default Mesh Options
rotate_mesh <<- T # back from rotated to geographic coordinates 
Ealpha      <<- 50 # Euler angles (from FESOMs namelist.config)
Ebeta       <<- 15
Egamma      <<- -90
cycl        <<- T # treat cyclic element

## User experiment and mesh options (overwrite defaults here)
# demo
if (F) { 
    runid <<- "demoid"
    meshid <<- "demomeshid"
    cycl <<- F
    meshpath <<- paste0(rfesom_path, "/example_data/mesh/demomeshid/")
    imatpath <<- paste0(rfesom_path, "/example_data/mesh/demomeshid/interp/")
    datainpath <<- paste0(rfesom_path, "/example_data/data/demoid/")
    postpath <<- paste0(rfesom_path, "./post/")
    plotpath <<- paste0(rfesom_path, "./plot/")

# for martin 
} else if (F) {
    runid <<- "PI_CTRL_mw"
    meshid <<- "core"
    cpl_tag <<- T
    rotate_mesh <<- F
    meshpath <<- "/work/ab0995/a270046/meshes_default/core/"
    imatpath <<- "/pf/a/a270106/snow_depth_PI_CTRL/interp/"
    datainpath <<- "/pf/a/a270106/snow_depth_PI_CTRL/"
    fnames_user <<- "tos_PI_CTRL_fesom.nc" 
    postpath <<- datainpath
    plotpath <<- datainpath

# for testing
} else {
    source("~/scripts/r/myrunids.r")
}

## General Options
verbose <<- 3 # give some information while the script is running
              # 0 = no info
              # 1 = what is happening
              # 2 = what is happening more detailed (default)
              # 3 = min/max of data in between
ssh_aviso_correct <<- F ## special

## Global constants
Rearth    <<- 6367.5 * 1e3 # [m] earth radius
g         <<- 9.81 # [m s-2] acceleration due to gravity
omega     <<- 2*pi/86400 # [s-1] angular frequency of earth (1 day here = 86400 sec)
cp        <<- 3985 # [m2 s-2 K-1] specific heat capacity of water
rho0      <<- 1027 # [kg m-3] average density
sea_water <<- "TEOS10" # "EOS80": sw_* (obsolete) or 
                      # "TEOS10": gsw_* (Gibbs Sea Water; http://www.teos-10.org/) 
pres_ref  <<- 0 # [dbar] reference pressure for potential density
                # 0, 1000, 2000, 3000 or 4000
mv        <<- NA # missing value for netcdf output
prec      <<- "double" # precision of nc output
base      <<- 10 # base for multiplication factors
sd_method <<- "default" # how to calculate sd of a vector variable?
                        # "default" for sd of speed of vector only
                        # "ackermann83" for sd of both the speed and direction of a vector

## Variable Options
if (runid == "demoid") {
    varname <<- "ssh" # see namelist.var.r for available variables
} else if (runid == "PI_CTRL_mw") {
    varname <<- "tos"
} else {
    varname <<- "thetao"
}

## Depth options [m]
## 0 : sea surface
## c(23, 27.5): average between 
## c(42, "max"): average between
## c("bottom"): only bottom layer
if (runid == "demoid") {
    depths <<- 0
    integrate_depth <<- F
} else if (runid == "PI_CTRL_mw") {
    depths <<- 0
    integrate_depth <<- F
} else {
    depths <<- 0
    #depths <<- c(0, 150)
    #depths <<- c(0, "max")
    #depths <<- c(0, "MLD")
    integrate_depth <<- F
}

## Area and Projection Options
if (runid == "demoid") {
    area <<- "lsea" # see namelist.area.r for corresponding coordinates
} else if (runid == "PI_CTRL_mw") {
    area <<- "global"
} else {
    area <<- "lsea"
    #area <<- "lseawNAtilt"
    #area <<- "lseawNA"
    #area <<- "LS30l"
}

## Time Options (if user provide 'fnames_user', then 'years' and 'output' are ignored)
years         <<- 2880:2881 # annual FESOM output files, woa13 overlap: 65-04
recs          <<- 32:90 #c(1, 2, 12) # records (ntime) per FESOM file (e.g. months, days, hours)
                        # e.g. c(1,2,12) for DJF if output=="monthly"
output        <<- "daily" # Output timestep of FESOM; ("monthly", "5day" for weekly, "daily")
                           # according to 'output_length_unit' in namelist.config
snapshot      <<- F # true for snapshot if available or false for mean data (.mean.nc) or
                   # if snapshot not available
all_recs      <<- T # read all records of one fesom output file if possible 
                   # set to F if memory of computer is not big enough
consider_leap <<- T # if 'output' == 'daily' and 365 is included in 'recs'

## Output Options
ltm_out          <<- F # irregular time-mean .nc output (no time dimension)
regular_ltm_out  <<- T # regular time-mean .nc output (no time dimension) 
uv_out           <<- T # save u- and v- components of vector variable

if (runid == "demoid") {
    transient_out         <<- T
    regular_transient_out <<- T
    transient_mode        <<- "area" # what kind of netcdf output (see table below)
    sd_out                <<- T
    regular_dx            <<- 1/4 # 2 1 1/4 1/3 [deg]
} else if (runid == "PI_CTRL_mw") {
    transient_out         <<- F
    regular_transient_out <<- T
    transient_mode        <<- "area"
    sd_out                <<- F
    regular_dx            <<- 1
} else {
    transient_out         <<- T
    regular_transient_out <<- F
    transient_mode        <<- "mean"
    sd_out                <<- F
    regular_dx            <<- 0.1
    #regular_dx          <<- 0.355
    #regular_dx          <<- 0.25
    #regular_dx          <<- 0.099
    #regular_dy          <<- 0.185
    #regular_dy          <<- 0.052
}

if (!exists("regular_dy")) {
    regular_dy          <<- regular_dx # regular_dx # [deg]
}
output_type         <<- "nodes" # for spatial netcdf output on _irregular_ grid:
                               # "nodes": output is vector of length nod2d_n
                               # "elems": output is matrix of dimensions (3 x elem2d_n)
moc_ltm_out      <<- F # save moc ltm if transient_mode == "moc_depth"
csec_ltm_out     <<- F # save cross section ltm if transient_mode == "csec_mean" or "csec_depth"
force_v4         <<- T # T for saving as netcdf-4 (requieres R package ncdf4) or 
                       # F for saving as netcdf-3 aka "classic"
horiz_deriv_node3d <<- T # method for calculating horizontal derivative of 3D variable
horiz_deriv_elem2d <<- F # elem2d for testing

    ###############################################################################################
    # Output table                  what is saved               netcdf output dimensions
    #                               x=lons of 'view',           [number of dimensions: length(s) of 
    #                               y=lat of 'view',             the respective dimensions]
    #                               z='depths'
    #                               t=time
    # =============================================================================================
    # ltm output:
    # ---------------------------------------------------------------------------------------------
    #       ltm_out==T           t_mean( z_*mean/int*(X) )   2: nnodes2D 
    #       regular_ltm_out==T   t_mean( z_*mean/int*(X) )   2: nlon x nlat (regular)
    #
    # transient output (transient_out==T):
    # ---------------------------------------------------------------------------------------------
    #   transient_mode 
    #       "mean"                  xy_mean( z_*mean/int*(X) )  1: ntime
    #       "meanint"               xy_int( z_*mean/int*(X) )   1: ntime
    #       "sum"                   xy_sum( z_*mean/int*(X) )   1: ntime
    #       "max"                   xy_max( z_*mean/int*(X) )   1: ntime
    #       "max3D"                 xyz_max(X)                  1: ntime
    #       "min"                   xy_min( z_*mean/int*(X) )   1: ntime
    #       "depth"                 xy_mean(X)                  2: ndepth x ntime
    #       "depthint"              xy_int(X)                   2: ndepth x ntime
    #       "depthmax"              xy_max(X)                   2: ndepth x ntime
    #       "area"                  z_*mean/int*(X)             3: 3 x nelem2D x ntime (transient_out==T && output_type == "elems")
    #       "area"                  z_*mean/int*(X)             3: nnodes2D x ntime (transient_out==T && output_type == "nodes")
    #       "area"                  z_*mean/int*(X)             3: nlon x nlat x ntime (regular_transient_out==T)
    #       "areadepth"             X                           4: nlon x nlat x ndepth x ntime (regular_transient_out==T)
    #       "csec_mean"             crossection_mean(X)         1: ntime
    #       "csec_depth"            crossection(X)              3: npoint_crossection x ndepth x ntime
    #       "moc_depth"             moc as in fpost1.4          3: nlat x ndepth x ntime
    # =============================================================================================
    #
    # Note: z_*mean/int* means that either the vertical *mean* or *integral* is calculated
    #       depending on how "integrate_depth" is defined (FALSE for depth mean, TRUE for depth integral).
    #
    ###############################################################################################

## Load Area and Projection Options
source(paste0(rfesom_path, "/preconfigured_namelists/namelist.area.r")) # change to your namelist here

## Load Plot Options
source(paste0(rfesom_path, "/preconfigured_namelists/namelist.plot.r")) # change to your namelist here

## Load Variable Options
source(paste0(rfesom_path, "/preconfigured_namelists/namelist.var.r")) # change to your namelist here


