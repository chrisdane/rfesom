###########################
## User input for rfesom ##
###########################

## Global settings
verbose <- 2 # [0,1,2,3] give some information while the script is running; 0 = silent
sea_water <- "TEOS10" # "EOS80": sw_* (deprecated) or 
                      # "TEOS10": gsw_* (Gibbs Sea Water; http://www.teos-10.org/) 
p_ref <- "in-situ" # 1) "in-situ" for obtaining pressure from the
                   #    depth of each 3d-node using gsw::gsw_p_from_z().
                   #    rho = gsw_rho(SA,CT,p) --> in-situ density
                   #    or
                   # 2) Positive number such as 0, 42, 1338, 4000 [dbar]
                   #    as reference pressure for potential density calculation.
                   #    rho = gsw_rho(SA,CT,p=p_ref) --> potential density
                   # You can overwrite this default value also in namelist.var.r
                   # in the respective variable specifications.
cp <- 3991.86795711963 # [m2 s-2 K-1] specific heat capacity of water 
                       # 1) finite numeric, e.g. 3991.86795711963 for standard ocean with 
                       #    reference salinity 35.16504 g kg-1 = 35 u_PS 
                       #    (standard ocean practical salinity) and
                       #    potential temperature θ = 25 °C from eq. 3.3.3 of 
                       #    http://www.teos-10.org/pubs/TEOS-10_Manual.pdf 
                       # or
                       # 2) "gsw_cp_t_exact" from
                       #    http://www.teos-10.org/pubs/gsw/html/gsw_cp_t_exact.html
rho0 <- 1027 # [kg m-3] a mean density
mesh_dist_unit <- "m" # for mesh settings, let "m" to obtain SI units
Rearth <- 6367.5*1e3 # earth radius; must have same unit as 'mesh_dist_unit'
g <- 9.81 # [m s-2] acceleration due to gravity
omega <- 2*pi/86400 # [s-1] angular frequency of earth 
mv <- NA # missing value for netcdf output
prec <- "double" # precision of nc output
force_v4 <- T # T for saving as netcdf-4 (requieres R package ncdf4) or 
              # F for saving as netcdf-3 aka "classic"
sd_method <- "default" # how to calculate standard deviation of a vector variable?
                       # "default" for sd of speed of vector only
                       # "ackermann83" for sd of both speed and direction of vector
output_type <- "nodes" # for irregular netcdf output:
                       # "nodes": output is vector of length nod2d_n
                       # "elems": output is matrix of dimensions (3 x elem2d_n)
horiz_deriv_node3d <- T # method for calculating horizontal derivative of 3D variable
horiz_deriv_elem2d <- F # special
ssh_aviso_correct <- F # special


## Default mesh options
rotate_mesh <- F # rotate back to geographic coordinates around Euler angles
                 # If you dont know whether the coordinates in your *.out files are
                 # rotated or not, check 'rotated_grid' in namelist.config of 
                 # the model run (there, .true. means they are rotated so that the
                 # north pole is NOT at 90° N) of the model run or ask the person 
                 # who performed the run.
Ealpha <- 50 # Euler angles (from namelist.config)
Ebeta <- 15
Egamma <- -90
cycl <- T # treat cyclic elements; set true for global mesh

## Default experiment options
cpl_tag <- T # F: ocean-only, T: coupled
             # needed for filename convention:
             # if cpl_tag == T
             #      <varname_fesom>_fesom_YYYY0101.nc (older esm version)
             #      <runid>_<varname_fesom>_fesom_YYYY0101.nc (newer esm version)
             # if cpl_tag == F
             #      <runid>.YYYY.oce.mean.nc

## Default variable (see namelist.var.r)
varname <- "tso"

## At what depth? [m]
depths <- 0
# e.g. depths = 0            # sea surface
#      depths = c(13.37, 42) # interpolate from model levels to 13.37 and 42 m depths
#      depths = c(0, "max")  # as above but down to maximum depth, i.e. bottom
#      depths = c(0, "MLD")  # as above but down to mixed layer depth (MLD)
#      depths = "bottom"     # bottom layer
integrate_depth <- F # if F, average between depths[1] and depths[2]
                     # if T, integrate from depths[1] to depths[2]

## Where? (see namelist.area.r)
area <- "global"

## When? 
# If user provides own fesom1.4 file on irregular grid via 
# 'fnames_user' (in the runscript), 'years' and 'output' are ignored in the following. 
# Instead, the 'recs' of the time dimension of 'fnames_user' will be used.
years         <- 2009 # annual FESOM output files, woa13 overlap 65-04
recs          <- 1:12 #c(1,2,3) # records of FESOM file time dimension (e.g. months, days, hours)
                      # e.g. c(1,2,12) for DJF if output=="monthly"
output        <- "monthly" # Output timestep of FESOM; ("monthly", "5day" for weekly, "daily")
                           # according to 'output_length_unit' in namelist.config
consider_leap <- T # if 'output' == 'daily' and data includes doy 366
snapshot      <- F # true for snapshot if available or false for mean data (.mean.nc) or
                   # if snapshot not available
all_recs      <- T # read all records of one fesom output file if possible 
                   # set to F if memory of computer is not big enough

## Which postprocessing output? (see table below for availabe output types)
# ltm (long term mean) -> output has no time dimension)
# transient -> output has time dimension
ltm_out               <- F # irregular time-average output
regular_ltm_out       <- T # regular (i.e. interpolated) time-average output
transient_out         <- F # transient output defined via 'out_mode' (see table at the bottom)
regular_transient_out <- F # regular (i.e. interpolated) transient output (see table at the bottom)
out_mode              <- "area" # what kind of ouptut (see table at the bottom) 
regular_dx            <- regular_dy <- 1/4 # resolution of interpolated data on regular grid [degree]
uv_out                <- T # save horizontal components of vector variable
sd_out                <- F # calc and save standard deviation
moc_ltm_out           <- F # save moc ltm if out_mode == "moc_depth"
csec_ltm_out          <- F # save cross section ltm if out_mode == "csec_mean" or "csec_depth"


###############################################################################################
# Output table                  what is saved               netcdf output dimensions
#                               x=lons of 'area',           [number of dimension(s): length(s) 
#                               y=lats of 'area',            of the dimension(s)]
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
#       "area"                  z_*mean/int*(X)             3: nlon x nlat x ntime (regular_transient_out==T)
#       "area"                  z_*mean/int*(X)             3: nnodes2D x ntime (transient_out==T && output_type == "nodes")
#       "area"                  z_*mean/int*(X)             3: 3 x nelem2D x ntime (transient_out==T && output_type == "elems")
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

