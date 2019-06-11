###########################
## User input for rfesom ##
###########################
# note: T = TRUE, F = FALSE

## clear work space and close open plopt devices
rm(list=ls()); graphics.off()

## Global constants
verbose         <- 2 #2 # [0,1,2,3] give some information while the script is running; 0 = silent
mesh_dist_unit  <- "m" # for mesh settings (not variables)
Rearth          <- 6367.5*1e3 # earth radius in 'mesh_dist_unit'
g               <- 9.81 # [m s-2] acceleration due to gravity
omega           <- 2*pi/86400 # [s-1] angular frequency of earth 
cp              <- 3991.86795711963 # [m2 s-2 K-1] specific heat capacity of water 
                                    # 1) number, e.g. 3991.86795711963 for standard ocean with 
                                    #    reference salinity 35.16504 g kg-1 = 35 u_PS 
                                    #    (standard ocean practical salinity) and
                                    #    potential temperature θ = 25 °C from eq. 3.3.3 of 
                                    #    http://www.teos-10.org/pubs/TEOS-10_Manual.pdf 
                                    # or
                                    # 2) "gsw_cp_t_exact" from
                                    #    http://www.teos-10.org/pubs/gsw/html/gsw_cp_t_exact.html
rho0            <- 1027 # [kg m-3] constant reference density
sea_water       <- "TEOS10" # "EOS80": sw_* (deprecated) or 
                            # "TEOS10": gsw_* (Gibbs Sea Water; http://www.teos-10.org/) 
p_ref           <- "in-situ" # 1) "in-situ" for obtaining pressure from the
                             #    depth of each 3d-node using gsw::gsw_p_from_z().
                             #    rho = gsw_rho(SA,CT,p) --> in-situ density
                             #    or
                             # 2) Positive number such as 0, 42, 1338, 4000 [dbar]
                             #    as reference pressure for potential density.
                             #    rho = gsw_rho(SA,CT,p) --> potential density
                             # You can overwrite this default value in namelist.var.r
                             # in the respective variable specifications.
mv              <- NA        # missing value for netcdf output
prec            <- "double"  # precision of nc output
force_v4        <- T # T for saving as netcdf-4 (requieres R package ncdf4) or 
                     # F for saving as netcdf-3 aka "classic"
sd_method       <- "default" # how to calculate standard deviation of a vector variable?
                             # "default" for sd of speed of vector only
                             # "ackermann83" for sd of both speed and direction of vector
horiz_deriv_node3d <- T      # method for calculating horizontal derivative of 3D variable
horiz_deriv_elem2d <- F      # special
regular_norm_correction <- F # special
ssh_aviso_correct <- F       # special

## Default Experiment options
cpl_tag <- F ## F: ocean-only, T: coupled
             ## this concerns the fesom filename convention, e.g.
             ## <runid>.YYYY.oce.mean.nc if cpl_tag = F or
             ## <varname_fesom>_fesom_YYYY0101.nc if cpl_tag = T and older esm version or
             ## <runid>_<varname_fesom>_fesom_YYYY0101.nc if cpl_tag = T and newer esm version
             ## (see <varname_fesom> definition in namelist.var.r)

## Default Mesh Options
rotate_mesh <- T # rotate back to geographic coordinates around Euler angles
Ealpha      <- 50 # Euler angles (from FESOMs namelist.config)
Ebeta       <- 15
Egamma      <- -90
cycl        <- T # treat cyclic elements; True for global mesh

## Overwrite default experiment and mesh options
if (F) { # demo1
    runid <- "demo1"
    meshid <- "demomesh"
    cycl <- F
    meshpath <- paste0("example_data/mesh/", meshid, "/")
    interppath <- paste0("example_data/mesh/", meshid, "/interp/")
    datainpath <- "example_data/data/"
    postpath <- "example_data/post/"
    plotpath <- "example_data/plot/"

} else if (F) { # for martin
    runid <- "PI_CTRL_mw"
    meshid <- "core"
    cpl_tag <- T
    rotate_mesh <- F
    meshpath <- "/work/ab0995/a270046/meshes_default/core/"
    interppath <- "/pf/a/a270106/snow_depth_PI_CTRL/interp/"
    datainpath <- "/pf/a/a270106/snow_depth_PI_CTRL/"
    fnames_user <- "tos_PI_CTRL_fesom.nc" 
    postpath <- datainpath
    plotpath <- datainpath

} else { # for me
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


## Variable Options (see namelist.var.r for available variables)
if (runid == "demo1") {
    varname <- "ssh" 
} else if (runid == "PI_CTRL_mw") {
    varname <- "tos"
} else {
    #varname <- "ssh"    
    #varname <- "tos"
    #varname <- "temp"
    #varname <- "sic"
    #varname <- "foverh"
    #varname <- "hvel"
    #varname <- "vertvel"
    #varname <- "hvel_geo"
    #varname <- "wind"
    #varname <- "curltau"
    #varname <- "RossbyNo"
    #varname <- "divuvt"
    #varname <- "divuvteddy"
    #varname <- "divuvsgsttot"
    #varname <- "divuvsgst"
    #varname <- "divuvsgsteddy"
    #varname <- "divuvsgsttot"
    #varname <- "divuvs"
    #varname <- "divuvseddy"
    #varname <- "vertvel"
    #varname <- "mixlay"
    #varname <- "qnet"
    #varname <- "Nsquared"
    #varname <- "c_barotrop"
    #varname <- "c_barocline"
    #varname <- "wkb_hvel_mode"
    #varname <- "wkb_vertvel_mode"
    #varname <- "c_long_rossby"
    #varname <- "bathy"
    #varname <- "qnet"
    #varname <- "wind"
    #varname <- "intz_uvteddy_div"
    #varname <- "dxphi"
    #varname <- "dyphi"
    #varname <- "gradphi"
    #varname <- "gradbathy"
    #varname <- "hvel_dot_gradbathy"
    #varname <- "Frhobudget"
    #varname <- "Frho"
    #varname <- "Frho2"
    #varname <- "MOCw"
    #varname <- "MOCv"
    #varname <- "HRS"
    #varname <- "VRS"
    varname <- "wbeddy"
    #varname <- "FeKe"
    #varname <- "gradB"
}

## Depth options [m]
# e.g.  depths = 0              -> sea surface
#       depths = c(23, 27.5)    -> average between 
#       depths = c(42, "max")   -> average between
#       depths = c(0, "MLD")    -> average between
#       depths = "bottom"       -> deepest layer
if (runid == "demo1") {
    depths <- 0
    integrate_depth <- F
} else if (runid == "PI_CTRL_mw") {
    depths <- 0
    integrate_depth <- F
} else {
    #depths <- 0
    #depths <- c(0, 150)
    #depths <- c(0, "max")
    depths <- c(0, "MLD")
    #depths <- "bottom"
    #integrate_depth <- F
    integrate_depth <- T
}

## Area and Projection Options (see namelist.area.r)
if (runid == "demo1") {
    area <- "lsea" 
} else if (runid == "PI_CTRL_mw") {
    area <- "global"
} else {
    #area <- "global"
    #area <- "atlantic3"
    #area <- "bigna"
    #area <- "lsea"
    #area <- "lseawNAtilt"
    #area <- "lseawNA"
    #area <- "lsea3"
    area <- "LS30l"
    #area <- "LS30l2"
    #area <- "nadja1"
    #area <- "Cstg"
    #area <- "N20"
    #area <- "N25"
    #area <- "Cstg2"
    #area <- "N35"
    #area <- "N40"
    #area <- "N45"
    #area <- "N50"
    #area <- "wunsch97"
    #area <- "NA_sst1"
    #area <- "moc_NA"
}

## Time Options (if user provide 'fnames_user', then 'years' and 'output' are ignored)
years         <- 1948:2009 # annual FESOM output files, woa13 overlap 65-04
recs          <- 1:12 #c(1,2,3) # records per FESOM file, eg months, days, hours
                      # e.g. c(1,2,12) for DJF if output=="monthly"
output        <- "monthly" # Output timestep of FESOM; ("monthly", "5day" for weekly, "daily")
                           # according to 'output_length_unit' in namelist.config
snapshot      <- F # true for snapshot if available or false for mean data (.mean.nc) or
                   # if snapshot not available
all_recs      <- T # read all records of one fesom output file if possible 
                   # set to F if memory of computer is not big enough
consider_leap <- T # if 'output' == 'daily' and data includes doy 366
if (runid == "demo1") {
    years <- 2009
    recs <- 1:12
    output <- "monthly"
    snapshot <- F
    all_recs <- T
}

## Default output Options (see table below for availabe output types)
ltm_out               <- F # time-average output on irregular mesh 
regular_ltm_out       <- T # time-average output on regular mesh
transient_out         <- T # transient output
regular_transient_out <- F # transient output on regular mesh 
regular_dx            <- 1 # degree
uv_out                <- T # save u- and v- components of vector variable
sd_out                <- F # calc and save standard deviation

# overwrite output options
if (runid == "demo1") {
    ltm_out                 <- F
    regular_treansient_out  <- T
    transient_out           <- F 
    regular_transient_out   <- T 
    out_mode                <- "area" 
    sd_out                  <- T
    regular_dx              <- 1/4 # 2 1 1/4 1/3 [deg]
} else if (runid == "PI_CTRL_mw") {
    transient_out           <- F
    regular_transient_out   <- T
    out_mode                <- "area"
    regular_dx              <- 1
} else {
    regular_ltm_out         <- F
    transient_out           <- T
    regular_transient_out   <- F
    out_mode                <- "mean" # "mean" "area" "areadepth" "meanint" "depth" "depthint" "moc_depth"
    #sd_out                  <- T
    regular_dx               <- 0.1
    #regular_dx              <- 0.099
    #regular_dy              <- 0.052
    #regular_dx              <- 0.355
    #regular_dy              <- 0.185
    #regular_dx              <- 0.25
    #regular_dx              <- 0.355
    #regular_dx              <- 0.5
}

if (!exists("regular_dy")) {
    regular_dy          <- regular_dx # regular_dx # [deg]
}
output_type         <- "nodes" # for spatial netcdf output on irregular grid:
                               # "nodes": output is vector of length nod2d_n
                               # "elems": output is matrix of dimensions (3 x elem2d_n)
moc_ltm_out      <- F # save moc ltm if out_mode == "moc_depth"
csec_ltm_out     <- F # save cross section ltm if out_mode == "csec_mean" or "csec_depth"

    ###############################################################################################
    # Output table                  what is saved               netcdf output dimensions
    #                               x=lons of 'view',           [number of dimensions: length(s) of 
    #                               y=lat of 'view',             the dimension(s)]
    #                               z='depths'
    #                               t=time
    # =============================================================================================
    # ltm output if:
    #       ltm_out         = T     t_mean( z_*mean/int*(X) )   2: nnodes2D 
    #       regular_ltm_out = T     t_mean( z_*mean/int*(X) )   2: nlon x nlat (regular)
    #
    # transient output if:
    #       transient_out   = T
    #
    # output modes:
    #   out_mode = 
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
source(paste0(rfesom_path, "namelists/namelist.area.r")) # change to your own namelist here

## Load Plot Options
source(paste0(rfesom_path, "namelists/namelist.plot.r")) # change to your own namelist here

## Load Variable Options
source(paste0(rfesom_path, "namelists/namelist.var.r")) # change to your own namelist here

## Run rfesom
source(paste0(rfesom_path, "lib/main_rfesom.r"))


