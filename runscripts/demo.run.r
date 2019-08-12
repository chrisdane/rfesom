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

## This is the demo configuration
## Set the blocks below to T or F to run the different demos
runid       <- "demo" # in filenames of fesom data
cpl_tag     <- F # demodata is from ocean-only experiment
meshpath    <- paste0(rfesompath, "/example_data/mesh") # *.out files
meshid      <- "demomesh" # name of the mesh; used for saving mesh-related things like interp matrix
rotate_mesh <- T # demomesh needs to get rotated back to geograhic coords
cycl        <- F # demomesh is not global and cyclic elements are not present
datainpath  <- paste0(rfesompath, "/example_data/data") # fesom data

if (T) {
    ## demo1
    # Saves fesoms mesh resolution in km on regular coordinates as netcdf and saves a spatial plot.
    # See post/demo/regular_grid/ltm/area/lsea/resolutionkm
    # See plot/resolutionkm
    # If in active R session:
    #image.plot(xi, yi, drop(datamat_reg_ltm), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " ", longname, " [", units_plot, "]"))
    regular_ltm_out <- T # see namelist.config.r
    varname         <- "resolutionkm" # see namelist.var.r
    area            <- "lsea" # see namelist.area.r

} else if (F) {
    ## demo2
    # Saves 1) 1948 mean, 2) standard deviation (sd) and 3) 12 (monthly) records of 
    # ssh on regular coordinates as netcdf and saves a spatial plot of the temporal 
    # mean and sd fields.
    # See post/demo/regular_grid/area/lsea/ssh
    # See post/demo/regular_grid/ltm/area/lsea/ssh
    # See plot/ssh
    # If in active R session:
    #image.plot(xi, yi, drop(datamat_reg_ltm["ssh",,,,]), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " ", longname, " [", units_plot, "]"))
    #image.plot(xi, yi, drop(datamat_reg_ltm["ssh_sd",,,,]), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " sd(", varname, ") [", units_plot, "]"))
    varname               <- "ssh"
    area                  <- "lsea"
    regular_ltm_out       <- T
    regular_transient_out <- T 
    out_mode              <- "area"
    sd_out                <- T
    years                 <- 1948
    recs                  <- 1:12

} else if (F) {
    ## demo3 
    # Saves potential temperature field mean in area "lsea" averaged between 0 and 100
    # meters depth as netcdf and saves a spatial plot of the temporal and depth mean.
    # See post/demo/mean/lsea/temp
    # See plot/temp
    # If in active R session:
    #plot(time, data_funi, t="o", main=paste0(area, " ", longname, " [", units_plot, "]"), ylab=varname, las=1)
    varname       <- "temp"
    depths        <- c(0, 100)
    area          <- "lsea"
    transient_out <- T
    out_mode      <- "mean"
    
} else if (F) {
    ## demo4
    # Saves potential temperature field mean in area "lsea" as a function of depth as
    # netcdf and saves a spatial plot of the temporal and depth mean.
    # See post/demo/depth/lsea/temp
    # See plot/temp
    # If in active R session:
    #image.plot(time, interpolate_depths, drop(data_funi[1,,]), ylim=rev(range(interpolate_depths)), ylab="Depth [m]", las=1, main=paste0(area, " ", longname, " [", units_plot, "]"))
    varname       <- "temp"
    depths        <- c(0, "max")
    area          <- "lsea"
    transient_out <- T
    out_mode      <- "depth"
}

## Load plot options
source(paste0(rfesompath, "/namelists/namelist.plot.r")) 

## Load variable options
source(paste0(rfesompath, "/namelists/namelist.var.r"))

## Load area and projection options
source(paste0(rfesompath, "/namelists/namelist.area.r")) 

## Run rfesom
source(paste0(rfesompath, "/lib/main_rfesom.r"))
