##########################
## runscript for rfesom ##
##########################
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

## This is the demo runscript
runid                 <- "demo" # in filenames of fesom data
cpl_tag               <- F # demodata is from ocean-only experiment
meshpath              <- paste0(rfesompath, "/example_data/mesh") # *.out files
meshid                <- "demomesh" # name of the mesh; used for saving mesh-related things like interp matrix
rotate_mesh           <- T # demomesh needs to get rotated back to geograhic coords
cycl                  <- F # demomesh is not global and cyclic elements are not present
datainpath            <- paste0(rfesompath, "/example_data/data") # fesom data
varname               <- "ssh" # see namelist.var.r
area                  <- "lsea" # see namelist.area.r
regular_transient_out <- T # see namelist.config.r
out_mode              <- "area" # see namelist.config.r

## Load plot options
source(paste0(rfesompath, "/namelists/namelist.plot.r")) 

## Load variable options
source(paste0(rfesompath, "/namelists/namelist.var.r"))

## Load area and projection options
source(paste0(rfesompath, "/namelists/namelist.area.r")) 

## Run rfesom
source(paste0(rfesompath, "/lib/main_rfesom.r"))
