#########################################
## demo runscript for rfesom           ##
## https://github.com/chrisdane/rfesom ##
#########################################

# R hints:
# 1) booleans `TRUE` and `FALSE` can be abbreviated with `T` and `F`
# 2) index syntax is `arrayname[]`, not `arrayname()`
# 3) R counts from 1, not 0
# 4) 'not equal' condition is `!=`

rfesompath  <- normalizePath("../") # assumes this demo runscript is not moved somewhere else
datainpaths <- paste0(rfesompath, "/example_data/data") # fesom data
fpatterns   <- "demo.<YYYY>.oce.mean.nc.ncpdq_L_5" # demo.1948.oce.mean.nc.ncpdq_L_5
meshpath    <- paste0(rfesompath, "/example_data/meshes/demo") # *.out files
rotate_mesh <- T # demomesh needs to get rotated back to geograhic coords
global_mesh <- F # demomesh is not global to save space 
plotpath    <- paste0(rfesompath, "/example_data/plots")
postpath    <- paste0(rfesompath, "/example_data/post")

# Set the blocks below to T or F to run the different demos
if (F) { # demo1
    # Saves fesoms mesh resolution in km on regular coordinates as netcdf and saves a lon,lat plot of it.
    # If in active R session:
    postprefix      <- "demo1"
    regular_ltm_out <- T # see namelist.config.r for other entries
    varname         <- "resolutionkm" # see namelist.var.r
    area            <- "lsea" # see namelist.area.r
    #image.plot(xi, yi, drop(datamat_reg_ltm), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " ", longname, " [", units_plot, "]"))

} else if (T) { # demo2
    # Saves 1) year 1948 mean, 2) standard deviation (sd) and 3) 12 (monthly) records of 
    # ssh on regular coordinates as netcdf and saves a lon,lat plot of the temporal 
    # mean and sd fields.
    postprefix            <- "demo2"
    varname               <- "ssh"
    area                  <- "lsea"
    regular_ltm_out       <- T
    regular_transient_out <- T 
    out_mode              <- "area"
    sd_out                <- T
    years                 <- 1948
    #season                <- "DJF"
    # If in active R session:
    #image.plot(xi, yi, drop(datamat_reg_ltm["ssh",,,,]), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " ", longname, " [", units_plot, "]"))
    #image.plot(xi, yi, drop(datamat_reg_ltm["ssh_sd",,,,]), xlab="Longitude [°]", ylab="Latitude [°]", las=1, main=paste0(area, " sd(", varname, ") [", units_plot, "]"))

} else if (F) { # demo3 
    # Saves potential temperature field mean in area "lsea" averaged between 0 and 100
    # meters depth as netcdf and saves a spatial plot of the temporal and depth mean.
    postprefix    <- "demo3"
    varname       <- "temp"
    depths        <- c(0, 100)
    area          <- "lsea"
    transient_out <- T
    out_mode      <- "fldmean"
    # If in active R session:
    #plot(time, drop(data_funi), t="o", main=paste0(area, " ", longname, " [", units_plot, "]"), ylab=varname, las=1)
    
} else if (F) { # demo4
    # Saves potential temperature field mean in area "lsea" as a function of depth as
    # netcdf and saves a spatial plot of the temporal and depth mean.
    postprefix    <- "demo4"
    varname       <- "temp"
    depths        <- c(0, "max")
    area          <- "lsea"
    transient_out <- T
    out_mode      <- "depth"
    # If in active R session:
    #image.plot(time, interpolate_depths, drop(data_funi[1,,]), ylim=rev(range(interpolate_depths)), ylab="Depth [m]", las=1, main=paste0(area, " ", longname, " [", units_plot, "]"))

} else if (F) { # demo5
    postprefix <- "demo5"
    datainpaths  <- paste0(rfesompath, "/example_data/data/PI-CTRL")
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
    #out_mode <- "fldmean"
    out_mode <- "moc_depth"
    fpattern <- "<runid>_fesom_<varname_nc>_<YYYY>0101.nc"
    years <- 1948
    #years <- 1948:1949
    #recs <- 47:67
    #season <- "JFM"

} # which demo

# do not change below this line
source(paste0(rfesompath, "/lib/main_rfesom.r"))
