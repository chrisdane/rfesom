#########################################
## runscript for rfesom                ##
## https://github.com/chrisdane/rfesom ##
#########################################

this_runscript_filename <- "demo.run.r" # just basename if saved in directory 
# rfesom/runscripts or absolute path if saved somehwhere else
rfesompath  <- normalizePath("../") # assume this demo runscript was not moved 

## Set the blocks below to T or F to run the different demos
datainpath  <- paste0(rfesompath, "/example_data/data/demo") # fesom data
fpattern    <- "demo.<YYYY>.<fsuffix>"
cpl_tag     <- F # demodata is from ocean-only experiment
meshpath    <- paste0(rfesompath, "/example_data/meshes/demo") # *.out files
rotate_mesh <- T # demomesh needs to get rotated back to geograhic coords
cycl        <- F # demomesh is not global and cyclic elements are not present
if (F) {
    ## demo1
    # Saves fesoms mesh resolution in km on regular coordinates as netcdf and saves a spatial plot.
    # See post/demo/regular_grid/ltm/area/lsea/resolutionkm
    # See plot/resolutionkm
    # If in active R session:
    #image.plot(xi, yi, drop(datamat_reg_ltm), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " ", longname, " [", units_plot, "]"))
    runid           <- "demo1"
    regular_ltm_out <- T # see namelist.config.r
    varname         <- "resolutionkm" # see namelist.var.r
    area            <- "lsea" # see namelist.area.r

} else if (F) {
    ## demo2
    # Saves 1) year 1948 mean, 2) standard deviation (sd) and 3) 12 (monthly) records of 
    # ssh on regular coordinates as netcdf and saves a spatial plot of the temporal 
    # mean and sd fields.
    # See post/demo/regular_grid/area/lsea/ssh
    # See post/demo/regular_grid/ltm/area/lsea/ssh
    # See plot/ssh
    # Take a look in the result in an active R session:
    #image.plot(xi, yi, drop(datamat_reg_ltm["ssh",,,,]), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " ", longname, " [", units_plot, "]"))
    #image.plot(xi, yi, drop(datamat_reg_ltm["ssh_sd",,,,]), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " sd(", varname, ") [", units_plot, "]"))
    runid                 <- "demo2"
    varname               <- "ssh"
    area                  <- "lsea"
    regular_ltm_out       <- T
    regular_transient_out <- T 
    out_mode              <- "area"
    sd_out                <- T
    years                 <- 1948
    #season                <- "DJF"

} else if (F) {
    ## demo3 
    # Saves potential temperature field mean in area "lsea" averaged between 0 and 100
    # meters depth as netcdf and saves a spatial plot of the temporal and depth mean.
    # See post/demo/mean/lsea/temp and plot/temp
    # Take a look in the result in an active R session:
    #plot(time, drop(data_funi), t="o", main=paste0(area, " ", longname, " [", units_plot, "]"), ylab=varname, las=1)
    runid         <- "demo3"
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

} else if (T) {
    ## demo5
    datainpath  <- paste0(rfesompath, "/example_data/data/PI-CTRL")
    runid <- "PI-CTRL"
    meshpath <- paste0(rfesompath, "/example_data/meshes/core")
    rotate_mesh <- F
    #varname <- "ssh"
    #varname <- "thetao"
    varname <- "MOCw"
    area <- "NA"
    #depths <- c(0, 133.7)
    depths <- c(0, "max")
    transient_out <- T
    #out_mode <- "mean"
    out_mode <- "moc_depth"
    fpattern <- "<runid>_fesom_<varname_nc>_<YYYY>0101.nc"
    years <- 1948
    #years <- 1948:1949
    #recs <- 47:67
    #season <- "JFM"

}


## do not change below this line ##
# run rfesom
source(paste0(rfesompath, "/lib/main_rfesom.r")) 

