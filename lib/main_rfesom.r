#############################################################################
## R-script for reading, plotting and saving FESOM output                   #
##                                                                          #
## Necessary R-packages:                                                    #
##  Package     Function        Purpose                                     #
##  ------------------------------------------------------                  #
##  ncdf4       ncdf4::*        read/manipulate/save netcdf version 4 data  #
##                                                                          #
## Optional R-packages (depending on user options):                         #
##  Package     Function        Purpose/when needed                         #
##  ----------------------------------------------------------------------  #
##  data.table  fread()         faster than base::scan()                    #
##  ncdf.tools  readNcdf()      faster than ncdf4::ncvar_get()              #
##  fields      image.plot()    colorbar if plot_map==T                     #
##  akima       interp()        if plot_type == "interp"                    #
##  maps        map()           if plot_type == "interp"                    #
##  splancs     inpip()         if *regular* == T  or  'area' is not        #
##                              a box but an irregular polygon              #
##  mapproj     mapproject()    if projection != "rectangular"              #
##  phonR       fillTriangle()  if plot_type == "interp2" # special         #
##  pracma      mldivide()      if out_mode == "csec_mean" or "csec_depth"  #
##  abind       abind()         concatinating multi-dim arrays              #
##  gsw         gsw_*()         Gibbs Sea Water functions                   #
##                                                                          #
## R-subroutines (.r files):                                                #
##  Filename                        Purpose                                 #
##  ------------------------------------------------------------------      #
##  rfesom.run.r                    load user settings                      #
##  namelist.var.r                  load user variable properties           # 
##  namelist.area.r                 load user area properties               #
##  namelist.plot.r                 load user plot properties               #
##  sub_*.r                         variable specific calculations          #
##  grid_rotate_r2g.r               rotate grid                             #
##  vec_rotate_r2g.r                rotate vector variables                 #
##  leap_function.r                 check if year is leap year              #
##  sub_calc_load_regular_IMAT.r    calc and save regular                   #
##                                  interpolation matrix                    #
##  sub_calc_regular_2d_interp.r    apply regular interpolation matrix      #
##                                  to irregular data                       #
##  deriv_2d.r                      calc and save 2d horizontal derivative  #
##  deriv_3d.r                      calc and save 3d horizontal derivative  #
##                                                                          #
## Coded by C. Danek (cdanek@awi.de)                                        # 
## Version 0.9, 30 Nov 2018                                                 #
#############################################################################

## show line number in case of errors
#options(show.error.locations=T)
#options(error=recover)
#options(warn=0) # default
#options(warn=2)

## vector/array element-selection as in matlab
fctbackup <- `[`; `[` <- function(...) { fctbackup(..., drop=F) }
# use drop() to reduce dimensions

message("********************************************************************")
message("* Run rfesom for reading, postprocessing and plotting FESOM output *")
message("* https://github.com/chrisdane/rfesom                              *")
message("********************************************************************")

helppage <- "https://github.com/chrisdane/rfesom#help"

## check user input
if (!exists("meshpath")) stop("No 'meshpath' provided.")
if (file.access(meshpath, mode=0) == -1) { # does not exist
    stop("meshpath = ", meshpath, " does not exist.")
}
meshpath <- suppressWarnings(normalizePath(meshpath))
if (!exists("meshid")) {
    stop("'meshid' not given. You can set a meshid in the runscript with e.g.\n",
         "   meshid <- \"core\"")
}
if (!exists("subroutinepath")) {
    subroutinepath <- paste0(getwd(), "/lib") # path where subroutines are saved
}
subroutinepath <- suppressWarnings(normalizePath(subroutinepath))
if (file.access(subroutinepath, mode=0) == -1) {
    stop("subroutinepath = ", subroutinepath, " does not exist.")
}
if (!exists("setting")) setting <- ""
if (!exists("regular_dy")) {
    message("'regular_dy' is not given. Use regular_dx ...")
    regular_dy <- regular_dx # regular_dx # [deg]
}
colors_script <- paste0(subroutinepath, "/functions/colors/color_function.r")

## add more directories to where to look for packages to load
if (exists("rpackagepaths")) {
    if (file.access(rpackagepaths, mode=0) == -1) { # mode=0: existing, -1: no success
        if (verbose > 0) {
            message(paste0("Note: your 'rpackagepaths' = ", rpackagepaths, " does not exist ..."))
        }
        rm(rpackagepaths)
    }
}
if (exists("rpackagepaths")) {
    if (file.access(rpackagepaths, mode=4) == -1) { # mode=4: reading, -1: no success
        stop(paste0("You have no reading rights in your provided 'rpackagepaths' = ", rpackagepaths, " ..."))
    }
    rm(rpackagepaths)
}
# if exist and readable, add to R search path for this session
if (exists("rpackagepaths")) {
    .libPaths(rpackagepaths) 
}

## load rfesom subroutines
for (i in c("vec_rotate_r2g.r", "grid_rotate_g2r.r", "grid_rotate_r2g.r",
            "sub_calc.r", "sub_e2xde_to_n2xde.r", "sub_n2xde_to_n3.r",
            "sub_n3_to_n2xde.r", "sub_prepare1.r", "sub_prepare2.r",
            "sub_vertical_average.r", 
            "sub_vertical_integral.r", "sub_vertical_integral_keepz.r")) {
    source(paste0(subroutinepath, "/", i))
}

## load misc subroutines
for (i in c("leap_function.r", "load_package.r", "mytxtProgressBar.r",
            "image.plot.pre.r", "gcd.r", "myls.r")) {
    source(paste0(subroutinepath, "/functions/", i))
}

## load ncdf4 package which is almost always needed
success <- load_package("ncdf4")
if (!success) stop(helppage)

# TODO: check all needed packages at the beginning!!! e.g. gsw

## load akima package and functions needed if
if (plot_map && plot_type == "interp") {
    success <- load_package("akima")
    if (!success) stop(helppage)
    source(paste0(subroutinepath, "/m2lon.r"))
    source(paste0(subroutinepath, "/m2lat.r"))
}

## check stuff
if (is.null(varname_fesom)) {
    nfiles <- 0
} else {
    nfiles <- length(varname_fesom)
}
if (exists("fnames_user")) {
    fuser_tag <- T
    if (nfiles != 0) { # variable like resolution in case of user provided file
        nfiles <- length(fnames_user)
    }
} else {
    fuser_tag <- F
}
if (fuser_tag) {
    if (!exists("runid")) {
        runid <- "runid"
        message("'runid' not given. Use default: runid\n",
                "You can set a runid somewhere in the runscript or a namelist with e.g.\n",
                "   runid <- \"myexp\"")
        runid <- "runid"
    }
    if (!exists("datainpath")) {
        datainpath <- dirname(fnames_user[1])
    }
} else if (!fuser_tag) {
    if (!exists("runid")) stop("Set 'runid'.")
    if (!exists("datainpath")) {
        stop("No 'datainpath' provided.")
    } else {
        datainpath <- suppressWarnings(normalizePath(datainpath))
    }
}
if (nfiles == 0) {
    transient_out <- F
    regular_transient_out <- F
    rms_out <- F
    sd_out <- F
}
if (out_mode == "area" && regular_transient_out && transient_out) {
    stop("error: for out_mode='area', set either 'transient_out' OR 'regular_transient_out' to TRUE")
}
if (out_mode == "areadepth" && transient_out) {
    regular_transient_out <- T
    transient_out <- F
}
if (regular_transient_out &&
    !any(out_mode == c("area", "areadepth"))) {
    stop("If 'regular_transient_out'=T, 'out_mode' must equal 'area' or 'areadepth'.")
}
# check if spatial interpolation to regular is even necessary 
# depending on the user choice area
if (exists("map_geogr_lim_lon") && length(map_geogr_lim_lon) == 1) { # a single point
    if (regular_transient_out) {
        transient_out <- T
        if (out_mode == "area") {
            out_mode <- "mean"
        } else if (out_mode == "areadepth") {
            out_mode <- "depth"
        }
    }
    regular_transient_out <- F
    regular_ltm_out <- F
}   
if (!vec) uv_out <- F
if (!uv_out && sd_method == "ackermann83") {
    message(paste0("warning: you set 'sd_method'=ackermann83 but 'varname'=",
                 varname,
                 " is not a vector variable. continue with 'sd_method'=default ..."))
    sd_method <- "default"
}
if (uv_out || rms_out || sd_out || horiz_deriv_tag != F) {
    success <- load_package("abind")
    if (!success) stop(helppage)
}
if ((out_mode == "csec_mean" || out_mode == "csec_depth") &&
    varname != "transport") {
    out_mode <- "mean"
}
if (transient_out && integrate_depth && dim_tag == "3D" &&
    (out_mode != "mean" && out_mode != "meanint")) {
    message(paste0("Output should be integrated over depth ('integrate_depth'=T) but out_mode=", out_mode, "."))
    message(paste0("Switch variable 'out_mode' to 'mean' or 'mean_int' or do something else ..."))
    stop()
}
if (transient_out && any(out_mode == c("csec_mean", "csec_depth"))) {
    regular_transient_out <- F
    regular_ltm_out <- F
    if (out_mode == "csec_mean") {
        csec_conds_n <- length(csec_conds) # apply conditions before averaging over section
    } else if (out_mode == "csec_depth") {
        csec_conds_n <- 0 # do not apply averaging when saving data of the complete section
    }
} else {
    csec_conds_n <- 0
}
if (transient_out && any(out_mode == c("moc_mean", "moc_depth"))) {
    regular_transient_out <- F
    regular_ltm_out <- F
}
if (!exists("average_depth")) { # potentially set by user in namelist.var
    if (dim_tag == "2D" || 
        integrate_depth || 
        #horiz_deriv_tag || 
        (transient_out && 
         any(out_mode == c("depth", "depthint", "depthmax", "max3D",
                                 "csec_mean", "csec_depth"))) ||
        (regular_transient_out &&
         any(out_mode == c("areadepth"))) ||
        any(varname == c("c_long_rossby")) ||
        regexpr("MOC", varname) != -1) {
        average_depth <- F
    } else {
        average_depth <- T
    }
}
if (regexpr("MOC", varname) != -1) plot_map <- F 
if (moc_ltm_out && regexpr("MOC", varname) == -1) {
    moc_ltm_out <- F # calc MOC only when varname is MOCx
}
if (csec_ltm_out && varname != "transport") {
    csec_ltm_out <- F # calc csec_ltm only if varname is transport
}
## If there are data loaded from diag file, then snapshot is not possible
if (cpl_tag ||
    (!cpl_tag && nfiles > 0 && any(diagsuffix == "diag."))) {
    snapshot <- F
}
if (dim_tag == "2D" || varname == "rossbyrad") {
    integrate_depth <- F
}
if (dim_tag == "3D" && integrate_depth && length(depths) != 2) {
    message(paste0("Cannot integrate over 'depths'=", paste0(depths, collapse=","), ". Set integrate_depth to FALSE ..."))
    integrate_depth <- F
}
if (integrate_depth && any(depths == "MLD")) {
    success <- load_package("abind")
    if (!success) stop(helppage)
}
if (dim_tag == "2D" && transient_out &&
    (!any(out_mode == c("mean", "meanint", "min", "max")))) {
    stop(message(paste0("Choose a proper 'out_mode' for the 2D variable '", varname, " ...")))
}
if (any(out_mode == c("csec_mean", "csec_depth")) &&
    varname != "transport") {
    stop(paste0("For 'out_mode'=", out_mode, " 'varname' must be 'transport'"))
}
if (transient_out && any(out_mode == c("depth", "depthint", "depthmax",
                                             "areadepth"))) {
    if (length(depths) == 1) {
        stop("You specificed 'out_mode'=", out_mode, 
                " but 'depths'=", depths, ". change depths to e.g. c(0, 100) or c(0, \"max\")")
    }
}
if (regular_ltm_out && !any(out_mode == c("area", "areadepth"))) {
    stop("You want regular ltm output ('regular_ltm_out'=T) but then 'out_mode' must be one of 'area', 'areadepth'")
}
if (regular_ltm_out && out_mode == "areadepth") {
    average_depth <- F
}


## check paths
if (any(ltm_out, regular_ltm_out, transient_out, regular_transient_out, 
        moc_ltm_out, csec_ltm_out)) {

    if (!exists("postpath")) {
        postpath <- paste0(getwd(), "/post")
        message("No 'postpath' is given for saving postprocessing results.\n",
                "   Use default: ", postpath, " (= getwd()'/post').\n",
                "   You can set postpath <- \"/path/with/writing/rights\" in the runscript.")
    } else {
        postpath <- suppressWarnings(normalizePath(postpath))
    }
    if (file.access(postpath, mode=0) == -1) { # mode=0: existing, -1: no success
        #message(paste0("'postpath' = ", postpath, " does not exist ... "))
        message(paste0("Try to create 'postpath' = ", postpath, " ... "), appendLF=F)
        dir.create(postpath, recursive=T, showWarnings=F)
        if (file.access(postpath, mode=0) == -1) {
            message("")
            stop("Could not create 'postpath' = ", postpath)
        } else {
            message("done")
        }
    } else if (file.access(postpath, mode=2) == -1) { # mode=2: writing, -1: no success
        message("You have no writing rights in 'postpath' = ", postpath, " ...")
        postpath <- paste0(getwd(), "/post")
        message("   Use default: ", postpath, " (= getwd()'/post').\n",
                "   You can set postpath <- \"/path/with/writing/rights\" in the runscript.")
    }

    # make dirs in postpath. at this point, writing rights were already checked
    if (transient_out) {
        if (!exists("transientpath")) {
            transientpath <- paste0(postpath, "/", runid, "/", setting, "/",
                                    out_mode, "/", area, "/", varname)
            dir.create(transientpath, recursive=T, showWarnings=F)
            transientpath <- suppressWarnings(normalizePath(transientpath))
        }
    }

    if (any(ltm_out, moc_ltm_out, csec_ltm_out)) {
        if (!exists("ltmpath")) {
            ltmpath <- paste0(postpath, "/", runid, "/", setting, "/",
                              "ltm/", area, "/", varname)
            dir.create(ltmpath, recursive=T, showWarnings=F)
            ltmpath <- suppressWarnings(normalizePath(ltmpath))
        }
    }

    if (any(regular_transient_out, regular_ltm_out)) {
        
        if (regular_transient_out) {
            if (!exists("reg_transient_outpath")) {
                reg_transient_outpath <- paste0(postpath, "/", runid, "/", setting, 
                                                "/regular_grid/", out_mode, "/", area, 
                                                "/", varname)
                dir.create(reg_transient_outpath, recursive=T, showWarnings=F)
                reg_transient_outpath <- suppressWarnings(normalizePath(reg_transient_outpath))
            }
        }

        if (regular_ltm_out) {
            if (!exists("reg_ltm_outpath")) {
                reg_ltm_outpath <- paste0(postpath, "/", runid, "/", setting,
                                          "/regular_grid/ltm/", out_mode, "/", area, 
                                          "/", varname)
                dir.create(reg_ltm_outpath, recursive=T, showWarnings=F)
                reg_ltm_outpath <- suppressWarnings(normalizePath(reg_ltm_outpath))
            }
        }

        if (!exists("interppath")) {
            # use default
            interppath <- paste0(getwd(), "/mesh/", meshid, "/interp")
            message("No 'interppath' is given for saving/reading regular interpolation matrix.\n",
                    "   Use default: ", interppath, " (=getwd()'/mesh/'meshid'/interp')\n",
                    "   You can set interppath <- \"/path/with/writing/rights\" in the runscript.")
        } else {
            interppath <- suppressWarnings(normalizePath(interppath))
        }
        if (file.access(interppath, mode=0) == -1) { # mode=0: existing, -1: no success
            message(paste0("Try to create 'interppath' = ", interppath, " ... "), appendLF=F)
            dir.create(interppath, recursive=T, showWarnings=T)
            if (file.access(interppath, mode=0) == -1) {
                message("")
                stop("Could not create 'interppath' = ", interppath)
            } else {
                message("done.")
            }
        }

    } # if regular_transient_out regular_ltm_out

} # check post paths if output wanted

if (plot_map || plot_csec) {
    
    if (!exists("plotpath")) {
        # use default
        plotpath <- paste0(getwd(), "/plot/", varname)
        message("No 'plotpath' is given for saving plots.\n",
                "   Use default: ", plotpath, " (= getwd()'/plot/'varname)\n",
                "   You can set plotpath <- \"/path/with/writing/rights\" in the runscript.")
    } else {
        plotpath <- suppressWarnings(normalizePath(plotpath))
    }
    if (file.access(plotpath, mode=0) == -1) { # mode=0: existing, -1: no success
        #message(paste0("'plotpath' = ", plotpath, " does not exist ..."))
        message(paste0("Try to create 'plotpath' = ", plotpath, " ... "), appendLF=F)
        dir.create(plotpath, recursive=T, showWarnings=F)
        if (file.access(plotpath, mode=0) == -1) {
            message("")
            stop(" Could not create 'plotpath' = ", plotpath)
        } else {
            message("done.")
        }   
    } else if (file.access(plotpath, mode=2) == -1) { # mode=2: writing, -1: no success
        message("You have no writing rights in 'plotpath' = ", plotpath, " ...")
        plotpath <- paste0(getwd(), "/plot/", varname)
        message("   Use default: ", plotpath, " (= getwd()'/plot/'varname)\n",
                "   You can set plotpath <- \"/path/with/writing/rights\" in the runscript.")
    }
    success <- load_package("fields")
    if (!success) stop(helppage)
} # check paths if plot_mat || plot_csec


## set defaults; do not change
restart <- F # for rfesom
var_coords <- "geo" # "rot" or "geo" # only geo for rfesom
out_coords <- "geo" # "geo" or "rot", only "geo" implemented
#nisobaths <- length(isobaths)
nisobaths <- 0
zave_method <- 1 # default = 1
    # 1 = for i all depths: data[inds_2d] <- data[inds_2d] + data_global_vert[inds_2d]*deltaz[i]
    # 2 = sum(data[inds_3d]*cluster_vol_3d[inds_3d])
keep_gsw <- T
pb_char <- "#"
pb_width <- 30
pb_style <- 3

## Special SSH aviso correction !! special
if (varname == "ssh" && ssh_aviso_correct) {
    ssh_aviso_correct_fname <- "_aviso_correct"
    ssh_aviso_correct_file <- "/work/ba0941/a270073/data/AVISO/madt/h/aviso_h_Jan-Dec_1993-2009_transient_global_mean.txt"
    ssh_aviso_correct_data <- read.table(ssh_aviso_correct_file, header=T)
} else {
    ssh_aviso_correct_fname <- ""
}


## special
if (F) { # not yet
    if (horiz_deriv_tag != F) { # horiz_deriv_tag is needed

        if (horiz_deriv_tag != F && horiz_deriv_tag != T) { # horiz_deriv_tag is specified by user
            if (horiz_deriv_tag != var_coords) {
                message("error:")
                message(paste0("   by setting 'horiz_deriv_tag'=", horiz_deriv_tag, " in the 'sub_variable_lookup.r',"))
                message(paste0("   you say that the horiz_deriv_tag necessary for calculating ", varname))
                message(paste0("   *must* be carried out in ", horiz_deriv_tag, "-coordinates."))
                message(paste0("   but the output of this fesom experiment '", runid, "' is in "))
                message(paste0("   ", var_coord, "-coordinates. if it is possible to just rotate the data to "))
                message(paste0("   ", horiz_deriv_tag, "-coordinates prior to taking the horiz_deriv_tag, "))
                message(paste0("   then set 'horiz_deriv_tag'=T in the 'sub_variable_lookup.r'."))
                message(paste0("   if this is not possible, you cannot calculate ", varname, ", since e.g."))
                stop(paste0("   (uv)_rot * d (u_geo)/dx is not allowed."))
            }

        } else { # horiz_deriv_tag in rot or geo coordinates may be used

            message("note: ")
            message(paste0("   by setting 'horiz_deriv_tag'=", horiz_deriv_tag, " in the 'sub_variable_lookup.r',"))
            message(paste0("   you say that the horiz_deriv_tag necessary for calculating ", varname))
            message(paste0("   *can* be carried out in either geo- or rot-coordinates."))
            message(paste0("   by setting 'out_coords'=", out_coords, " you want the output/plot of this script"))
            message(paste0("   in ", out_coords, "-coordinates."))


        }
    } # if (horiz_deriv_tag != F)
} # not yet

################################## check done ################################################

## Create Time vectors step 1: check for inconsistency
if (exists("recs")) {
    if (length(recs) > 1) {
        if (any(diff(recs) < 1)) {
            stop("Time records of fesom file need to be in increasing order.")
        }
        if (any(diff(recs) != 1)) { # if recs per year are irregular, i.e. DJF
            all_recs <- F
        }
    }
    if (all_recs) { # user choice
        rec_tag <- T
        #if (length(recs) == 1 || fuser_tag) { # dont read complete nc if fuser_tag
        if (length(recs) == 1) {
            rec_tag <- F 
        }
    } else if (!all_recs) {
        rec_tag <- F
    }
} else if (!exists("recs")) {
    stop("Provide 'recs' in time options of namelist.config.")
}

## so far only 1 user file is allowed
if (fuser_tag) {
    if (length(fnames_user) > 1) {
        stop("not defined yet since ntime needs to be determined first!")
    }
}

## start
months_plot <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
months <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

## Create Time vectors step 2: check if leap years exist and are wanted
leap_tag <- F # default
if (fuser_tag) {
    #nyears <- length(fnames_user)
    nyears <- 1 # just used for ltm
    #timeunit <- ""
    #npy

} else if (!fuser_tag) {
    
    nyears <- length(years)

    if (output == "daily") {
        timeunit <- "days"
        npy <- 365
        
        ## check for leap years if wanted
        if (consider_leap) {
        
            if (any(is.leap(years))) {

                leap_tag <- T
                npy_leap <- 366
                days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
                d2m_inds <- cbind(c(1, cumsum(days[1:11]) + 1), cumsum(days))
                days_leap <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
                d2m_inds_leap <- cbind(c(1, cumsum(days_leap[1:11]) + 1), cumsum(days_leap))

                ## check if user wants monthly e.g. only Jan-Feb output
                if ((any(length(recs) == d2m_inds[,2]) || # length of recs indicates months
                     any(length(recs) == d2m_inds_leap[,2])) &&
                    (any(recs[1] == d2m_inds[,1]) || # start points indicate months
                     any(recs[1] == d2m_inds_leap[,1])) &&
                    (any(recs[length(recs)] == d2m_inds[,2]) || # end points indicate months
                     any(recs[length(recs)] == d2m_inds_recs[,2]))) {

                    if (verbose > 0) {
                        message("   =================================================")
                        message("   note: 'consider_leap' is set to true (T) AND")
                        message("         your 'output' is daily             AND")
                        message("         your 'years' contain leap years    AND")
                        message("         your 'recs' start and end at months: recs[1]=", 
                                recs[1], ", recs[length(recs)]=", recs[length(recs)], ".")
                        message("         So it is assumed you want to include leap days")
                        message("         of leap years, e.g. day 366 of year ", 
                                years[which(is.leap(years)[1])], ".")
                        message("         If you do not want that, set 'consider_leap'")
                        message("         to false (F) and rerun the script.")
                        message("   =================================================")
                    }

                    if (length(recs != 1)) {
                        inds <- c(which(recs[1] == d2m_inds[,1]),
                                  which(recs[length(recs)] == d2m_inds[,2]))
                        if (length(inds) == 2) { # user provided start and end of non-leap months
                            recs_leap <- d2m_inds_leap[inds[1],1]:d2m_inds_leap[inds[2],2]
                        }

                        inds <- c(which(recs[1] == d2m_inds_leap[,1]),
                                  which(recs[length(recs)] == d2m_inds_leap[,2]))
                        if (length(inds) == 2) { # user provided start and end of leap months
                            recs_leap <- recs
                            recs <- d2m_inds[inds[1],1]:d2m_inds[inds[2],2]
                        }

                    } else if (length(recs) == 1 && recs == 366) {
                        recs_leap <- c(365, 366)
                    }

                    if (!exists("recs_leap")) stop("recs_leap: this should not happen")

                } else {
                    leap_tag <-F

                } # if user indicates monthly output on daily fesom data
            } # if there are leap years
        } # if consider_leap == T

    } else if (output == "monthly") {
        timeunit <- "months"
        npy <- 12
    } else  if (output == "5day") {
        timeunit <- "weeks"
        npy <- 73
    } else {
        stop(paste0("output '", output, "' not defined"))
    }
} # if fuser_tag or not

## Create Time vectors step 3: yearvec
nrecspf <- length(recs)
if (fuser_tag) {
    # yearvec

} else if (!fuser_tag) {
    if (leap_tag) {
        nrecspf_leap <- length(recs_leap)
        # loop through all years (how to improve that?!)
        yearvec <- c()
        for (i in 1:length(years)) {
            if (is.leap(years[i])) {
                yearvec <- c(yearvec, rep(years[i], e=nrecspf_leap))
            } else {
                yearvec <- c(yearvec, rep(years[i], e=nrecspf))
            }
        }
    } else {
        yearvec <- rep(years, e=nrecspf)
    }
} # if (exists("fnames_user"))

## Create Time vectors step 4: monthly output and DJF is wanted:
djf <- F # default
if (!fuser_tag) {
    if (output == "monthly" && 
        length(recs) == 3 && 
        all(recs[1:3] == c(1, 2, 12))) {
        djf <- T
        ## check if user defined time fits DJF season
        ## e.g.: DJF 1948 = december 47, jan 48, feb 48
        # find first possible year of fesom data
        if (any(regexpr(years[1]-1, list.files(datainpath)) != -1)) {
            include_zeroth_december <- T
            yearvec <- c(years[1]-1, yearvec[1:(length(yearvec)-1)])
        } else {
            include_zeroth_december <- F
            yearvec <- yearvec[1:(length(yearvec)-1)]
        }
    } # if DJF
} # if (!fuser_tag)

## Create Time vectors step 5: finish time
if (fuser_tag) {
    # ntime, dt, time, recvec, recvec_plot, timechar

} else if (!fuser_tag) {

    ntime <- length(yearvec)
    dt <- 1/npy
    if (leap_tag) dt_leap <- 1/npy_leap
    if (djf) { # assumes monthly data
        if (include_zeroth_december) {
            if (length(yearvec) == 3) { # only 1 year
                time <- c(yearvec[1] + 1, yearvec[2:3] + (1:2)*dt) - dt
                recvec <- c(12, 1, 2)
            } else {
                time <- c(yearvec[1] + 1, # first year dec
                          yearvec[2:(ntime-2)] + recs*dt,
                          yearvec[(ntime-1):ntime] + (1:2)*dt) - dt # last year jan, feb
                recvec <- c(12, rep(recs, t=nyears-1), 1:2)
            }
        } else {
            if (length(yearvec) == 3) { # only 1 year
                time <- yearvec + (1:2)*dt - dt
                recs <- 1:2
                recvec <- recs
            } else {
                time <- c(yearvec[1:(ntime-2)] + dt*(1:npy)[recs] - dt, 
                          yearvec[(ntime-1):ntime] + dt*(1:npy)[1:2] - dt) # last year only jan, feb
                recvec <- c(rep(recs, t=nyears-1), 1:2)
            }
        }

    } else if (leap_tag) {
        # loop through all years (how to improve that?!)
        time <- c()
        recvec <- time
        for (i in 1:nyears) {
            if (is.leap(years[i])) {
                time <- c(time, years[i] + dt_leap*(0:(npy_leap - 1))[recs_leap])
                recvec <- c(recvec, recs_leap)
            } else {
                time <- c(time, years[i] + dt*(0:(npy - 1))[recs])
                recvec <- c(recvec, recs)
            }
        }

    } else {
        time <- yearvec + dt*(1:npy)[recs] - dt
        recvec <- rep(recs, t=nyears)
    }
    #recvec_plot <- sprintf(paste0("%.", max(nchar(recvec)), "i"), recvec)
    recvec_plot <- sprintf(paste0("%.", nchar(npy), "i"), recvec) # 1 --> 01 or 001
    timechar <- as.numeric(paste0(yearvec, recvec_plot))
    # if monthly: 200912 (npy = 12)
    # if 5day: 200969 (npy = 73)
    # if daily: 2009335 (npy = 365) 

} # if fuser_tag

## Create Time vectors step 6: Make timespan vector
if (fuser_tag) {
    # timespan, snapshotsuffix
    timespan <- ""

} else if (!fuser_tag) {

    if (nfiles > 0) {

        if (output == "monthly") {
            if (nyears == 1 && nrecspf == 1) {
                # only 1 month and 1 year chosen
                timespan <- paste0(months_plot[recs], "_", years)
            } else if (nyears != 1 && nrecspf == 1) {
                # only 1 month but several years chosen
                timespan <- paste0(months_plot[recs], "_", years[1], "-", years[nyears])
            } else if (nyears == 1 && nrecspf != 1) {
                # only 1 year but sveral months chosen
                if (nrecspf <= 4) {
                    if (djf) {
                        timespan <- paste0("DJF_", years)
                    } else {
                        timespan <- paste0(paste0(months[recs], collapse=""), "_", years)
                    }
                } else {
                    timespan <- paste0(months_plot[recs[1]], "-", months_plot[recs[nrecspf]],
                                       "_", years)
                }
            } else {
                #several months and several years chosen
                if (nrecspf <= 4) {
                    if (djf) {
                        timespan <- paste0("DJF_", years[1], "-", years[nyears])
                    } else {
                        timespan <- paste0(paste0(months[recs], collapse=""), "_", years[1],
                                           "-", years[nyears])
                    }
                } else {
                    timespan <- paste0(months_plot[recs[1]], "-", months_plot[recs[nrecspf]],
                                       "_", years[1], "-", years[nyears])
                }
            }

        } else if (output == "5day") {
            if (nyears == 1 && nrecspf == 1) {
                # only 1 month and 1 year chosen
                timespan <- paste0("week", recs, "_", years)
            } else if (nyears != 1 && nrecspf == 1) {
                # only 1 month but several years chosen
                timespan <- paste0("week", recs, "_", years[1], "-", years[nyears])
            } else if (nyears == 1 && nrecspf != 1) {
                # only 1 year but several weeks chosen
                timespan <- paste0("week", recs[1], "-", recs[nrecspf], "_", years)
            } else {
                #several months and several years chosen
                timespan <- paste0("week", recs[1], "-", recs[nrecspf],
                                   "_", years[1], "-", years[nyears])
            }

        } else if (output == "daily") {
            if (nyears == 1 && nrecspf == 1) {
                # only 1 day and 1 year chosen
                if (leap_tag) {
                    timespan <- paste0("day", recs_leap[2], "_", years)
                } else {
                    timespan <- paste0("day", recs, "_", years)
                }
            } else if (nyears != 1 && nrecspf == 1) {
                # only 1 day but several years chosen
                if (leap_tag) {
                    timespan <- paste0("day", recs_leap[2], "_", years[1], "-", years[nyears])
                } else {
                    timespan <- paste0("day", recs, "_", years[1], "-", years[nyears])
                }
            } else if (nyears == 1 && nrecspf != 1) {
                # only 1 year but several days chosen
                if (leap_tag) {
                    timespan <- paste0("day", recs[1], "-", recs_leap[nrecspf_leap], "_", years)
                } else {
                    timespan <- paste0("day", recs[1], "-", recs[nrecspf], "_", years)
                }
            } else {
                #several days and several years chosen
                if (leap_tag) {
                    timespan <- paste0("day", recs[1], "-", recs_leap[nrecspf_leap],
                                       "_", years[1], "-", years[nyears])
                } else {
                    timespan <- paste0("day", recs[1], "-", recs[nrecspf],
                                       "_", years[1], "-", years[nyears])
                }
            }

        } # if output == "monthly" ...

        ## Snapshot only allowed if (1) wanted and (2) none of the fesom data 
        ## is saved in 'diag' file
        snapshotsuffix <- rep("NA", nfiles)
        if (snapshot) {
            snapshotsuffix[] <- ""
            timespan <- paste0(timespan, "_snapshot")
        } else if (!snapshot) {
            for (file in 1:nfiles) {
                if (diagsuffix[file] == "diag.") {
                    snapshotsuffix[file] <- ""
                } else if (diagsuffix[file] != "diag") {
                    snapshotsuffix[file] <- "mean."
                }
            }
            timespan <- paste0(timespan, "_mean")
        }

    } else { # nfiles = 0
        timespan <- ""
    }
} # if fuser_tag

if (timespan == "") {
    timespan_fname <- ""
} else {
    timespan_fname <- paste0("_", timespan)
}

if (verbose > 0) {
    message("==============================================")
    message("verbose: ", verbose, " (change this in the runscript for more/less info)")
    message(paste0("fesom_version: ", fesom_version))
    message(paste0("runid: ", runid))
    message(paste0("setting: ", setting))
    message(paste0("mesh: ", meshid))
    message(paste0("   rotate mesh back to geographic coordinates: ", rotate_mesh))
    message(paste0("   treat cyclic elements: ", cycl))
    message(paste0("meshpath: ", meshpath))
    message(paste0("datainpath: ", datainpath))
    if (exists("fnames_user")) {
        message("fnames_user: ", fnames_user)
    }
    message(paste0("varname: ", varname))
    message(paste0("longname: ", longname))
    if (nfiles > 0) {
        if (dim_tag == "3D" && !integrate_depth) {
            message(paste0("depths: ", 
                         ifelse(length(depths) == 2, 
                                paste0(depths[1], "-", depths[2]), depths[1]), 
                         " m"))
        } else if (dim_tag == "3D" && integrate_depth) {
                message(paste0("Integrate over depths ", depths[1], "-", depths[2]))
        }
        message(paste0("snapshot: ", snapshot))
        if (exists("nyears")) {
            message(paste0("years: ", ifelse(nyears == 1, years, 
                               paste0(years[1], "-", years[nyears]))))
        }
        if (all(diff(recs) == 1) && length(recs) > 100) {
            message(paste0("recs: ", 
                         paste0(recs[1:10], collapse=","), ",...,",
                         paste0(recs[(length(recs) - 10):length(recs)], collapse=",")))
        } else {
            message(paste0("recs: ", paste0(recs, collapse=", ")))
        }
    }
    message(paste0("area: ", area))
    if (plot_map) {
        message(paste0("projection: ", projection))
        if (exists("map_geogr_lim_lon")) {
            message(paste0("plot map from longitude: ", round(range(map_geogr_lim_lon)[1], 2), 
                         " to ", round(range(map_geogr_lim_lon)[2], 2)))
            message(paste0("plot map from latitude: ", round(range(map_geogr_lim_lat)[1], 2),
                         " to ", round(range(map_geogr_lim_lat)[2], 2)))
            message(paste0("draw polygons from longitude: ", round(range(poly_geogr_lim_lon)[1], 2), 
                      " to ", round(range(poly_geogr_lim_lon)[2], 2)))
            message(paste0("draw polygons from latitude: ", round(range(poly_geogr_lim_lat)[1], 2),  
                      " to ", round(range(poly_geogr_lim_lat)[2], 2)))
        }
        if (projection != "rectangular") {
            message(paste0("orientation: c(lat=", orient[1], ",lon=", orient[2], 
                ",rot=", orient[3], ")"))
        }
        if (vec && quiver_tag) {
            if (quiver_thr != 0) {
                message(paste0("plot u- and v- quivers above ", quiver_thr, " m s^(-1)"))
            } else if (quiver_thr == 0) {
                message("plot u- and v- quivers")
            }
        }
    }
    
    if (transient_out) {
        message(paste0("save transient ", out_mode, " data in ", area, " area to:"))
        message(paste0("   ", transientpath))
        message("   (you can change this by defining 'postpath' and/or 'transientpath' in the runscript)")
    }
    
    if (any(ltm_out, moc_ltm_out, csec_ltm_out)) {
        message(paste0("Save ltm data in ", area, " area to:"))
        message(paste0("   ", ltmpath))
        message("   (you can change this by defining 'postpath' and/or 'ltmpath' in the runscript)")
    }
    
    if (regular_transient_out) {
        message(paste0("Save transient ", out_mode, " data in area ", area, " on regular (lon,lat) grid to:"))
        message(paste0("   ", reg_transient_outpath))
        message("   (you can change this by defining 'postpath' and/or 'reg_transient_outpath' in the runscript)")
    }
    
    if (regular_ltm_out) {
        message(paste0("Save ltm data in ", area, " area on regular (lon,lat) grid to:")) 
        message(paste0("   ", reg_ltm_outpath))
        message("   (you can change this by defining 'postpath' and/or 'reg_ltm_outpath' in the runscript)")
    }

    message("==============================================")
    message("Start clock ...")
} # if (verbose > 0)

ptm <- proc.time()

if (verbose > 0) {
    message("==============================================")
}

indent <- "   "
## 1) Read mesh if ...
if (!restart || # ... not a restart run 
    (restart && dim_tag == "2D" && nod2d_check == F) || # ... or if restart and new variable
                                                    # is 3D and 3D mesh was not loaded yet
    (restart && dim_tag == "3D" && nod3d_check == F)) { # ... or if restart and new variable 
                                                    # is 2D and 2D mesh was not leaded yet

    nod2d_n <- as.integer(readLines(paste0(meshpath, "/nod2d.out"), n=1))
    pos <- 1:nod2d_n # old here
    surfnodes <- pos # old here

    if (verbose > 0) {
        message("1) Read '", meshid, "' mesh from ", meshpath, " ...\n",
                "   nod2d_n=", nod2d_n)
    }

    ## check whether R package "data.table" is loaded
    success <- load_package("data.table")
    if (!success) {
        fread_tag <- F
        message(paste0(indent, "   use base::scan() instead."))
        message(paste0(indent, "   this is much slower. you should install 'data.table' ..."))
    } else {
        fread_tag <- T
    }

    if (dim_tag == "2D") {
        if (verbose > 1) {
            message(paste0(indent, "read ", nod2d_n, 
                         " 2D nodes from nod2d.out ..."))
        }
        fid <- paste0(meshpath, "/nod2d.out")
        if (!fread_tag) {
            tmp <- scan(fid, skip=1, quiet=T)
            nod2d <- matrix(tmp, nrow=nod2d_n, byrow=T)
        } else {
            tmp <- fread(fid, skip=1, showProgress=ifelse(verbose > 1, T, F))
            nod2d <- as.matrix(tmp)
        }
        nod2d_x <- drop(nod2d[,2])
        nod2d_y <- drop(nod2d[,3])
        nod2d_ind <- drop(nod2d[,4])
        nod_x <- nod2d_x
        nod_y <- nod2d_y
        nod2d_check <- T
        #message(str(nod2d))
        rm(tmp, nod2d)
    }

    if (dim_tag == "3D") {
        fid <- paste0(meshpath, "/nod3d.out")
        nod3d_n <- as.numeric(readLines(fid, n=1))
        if (verbose > 1) {
            message(paste0(indent, "read ", nod3d_n, 
                         " 3D nodes from nod3d.out ..."))
        }
        if (!fread_tag) {
            tmp <- scan(fid, skip=1, quiet=T)
            nod3d <- matrix(tmp, nrow=nod3d_n, byrow=T)
        } else if (fread_tag) {
            tmp <- fread(fid, skip=1, showProgress=ifelse(verbose > 1, T, F))
            nod3d <- as.matrix(tmp)
        }
        nod3d_x <- drop(nod3d[,2])
        nod3d_y <- drop(nod3d[,3])
        nod3d_z <- drop(nod3d[,4])
        nod_x <- nod3d_x
        nod_y <- nod3d_y
        nod_z <- nod3d_z
        fesom_depths <- abs(unique(nod_z)) # these are the model depths in m (positive downwards) 
        nod3d_check <- T
        rm(tmp, nod3d)

        fid <- paste0(meshpath, "/aux3d.out")
        aux3d_n <- as.numeric(readLines(fid, n=1))
        if (verbose > 1) {
            message(paste0(indent, "read ", aux3d_n*nod2d_n, 
                         " 3D node indices from aux3d.out ..."))
        }
        if (!fread_tag) {
            tmp <- scan(fid, skip=1, nlines=aux3d_n*nod2d_n, quiet=T)
            aux3d <- matrix(tmp, nrow=aux3d_n, ncol=nod2d_n)
        } else if (fread_tag) {
            tmp <- fread(fid, skip=1, nrows=aux3d_n*nod2d_n, 
                         showProgress=ifelse(verbose > 1, T, F))
            aux3d <- matrix(tmp$V1, nrow=aux3d_n, ncol=nod2d_n)
        }
        rm(tmp)

        ## fesom depths levels and dz
        # correct way for dz?!
        ndepths_all <- length(fesom_depths)
        deltaz_all <- rep(0, t=ndepths_all - 1)
        deltaz_all[1] <- (fesom_depths[1] - fesom_depths[2])/2
        deltaz_all[ndepths_all] <- (fesom_depths[ndepths_all - 1]- fesom_depths[ndepths_all])/2
        for (n in 2:(ndepths_all - 1)) {
            deltaz_all[n] <- (fesom_depths[n-1] - fesom_depths[n])/2 + (fesom_depths[n] - fesom_depths[n+1])/2
        }

        # note: it is possible that ndepths_all != aux3d_n.
        #       this might be the case because no nodes exist in the 
        #       bottom layer of aux3d: all(aux3d[aux3d_n,] == -999) is true.
        #       this may happen if only a subset of a mesh is used.

    } # end if 3d

    fid <- paste0(meshpath, "/elem2d.out")
    elem2d_n <- as.numeric(readLines(fid, n=1))
    if (verbose > 1) {
        message(paste0(indent, "read ", elem2d_n, 
                     " 2D elements from elem2d.out ..."))
    }
    if (!fread_tag) {
        tmp <- scan(fid, skip=1, quiet=T)
        elem2d <- matrix(tmp, nrow=elem2d_n, byrow=T)
    } else if (fread_tag) {
        tmp <- fread(fid, skip=1, showProgress=ifelse(verbose > 1, T, F))
        elem2d <- as.matrix(tmp)
    }
    rm(tmp)

    if (verbose > 0) {
        message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
                     " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
        message("==============================================")
    }


    ## 2) Calculate Geographic Coordinates
    if (verbose > 0) {
        if (rotate_mesh && cycl) {
            message("2) Rotate mesh back to geographic coords and treat cyclic nodes ...")
        } else if (rotate_mesh && !cycl) {
            message("2) Rotate mesh back to geographic coords ...")
        } else if (!rotate_mesh && cycl) {
            message("2) Treat cyclic nodes ...")
        }
    } # verbose

    if (!rotate_mesh) {
        # ugly workaround: rotate around 0 -__-
        Ealpha <- 0 # 1st Euler angle (from FESOMs namelist.config)
        Ebeta  <- 0 # 2nd
        Egamma <- 0 # 3rd
        rotate_mesh <- T
    }

    ## Save elem2d before treating cyclic elements
    elem2d_orig <- elem2d
    elem2d_n_orig <- dim(elem2d_orig)[1]

    if (rotate_mesh) {
        if (cycl) {
            if (verbose > 2) {
                    message(paste0(indent, "Treat cyclic elements part 1 ..."))
            }
            inds <- which(nod_x > 180)
            nod_x[inds] <- nod_x[inds] - 360
            inds <- which(nod_x < -180)
            nod_x[inds] <- nod_x[inds] + 360
        }

        ## Rotate coordinates back from rotated to geographical
        ## coordinates using Euler angles from FESOM code:
        if (verbose > 1 &&
            !all(c(Ealpha, Ebeta, Egamma) == 0)) { # ugly workaround
            message(paste0(indent, "Rotate mesh around Ealpha=", Ealpha,
                         ", Ebeta=", Ebeta, ", Egamma=", Egamma, " ..."))
        }
        rotated_coords <- grid_rotate_r2g(Ealpha, Ebeta, Egamma, nod_x, nod_y)
        nod_x <- rotated_coords$glon
        nod_y <- rotated_coords$glat
        rot_mat <- rotated_coords$rot_mat
        rm(rotated_coords)

        if (cycl) {
            if (verbose > 2) {
                message(paste0(indent, "Treat cyclic elements part 2 ..."))
            }
            inds <- which(abs(nod_x[elem2d[,3]] - nod_x[elem2d[,2]]) > 170 |
                          abs(nod_x[elem2d[,2]] - nod_x[elem2d[,1]]) > 170 |
                          abs(nod_x[elem2d[,3]] - nod_x[elem2d[,1]]) > 170) 

            auxelem <- elem2d[inds,]
            elem2d <- elem2d[-inds,]

            auxxc1 <- array(NA, dim(auxelem))
            auxyc1 <- auxxc1
            auxxc2 <- auxxc1
            auxyc2 <- auxxc1

            for (i in 1:3) {
                auxxc1[,i] <- nod_x[auxelem[,i]]
                auxyc1[,i] <- nod_y[auxelem[,i]]
                auxxc2[,i] <- nod_x[auxelem[,i]]
                auxyc2[,i] <- nod_y[auxelem[,i]]
            }

            mid <- (max(nod_x) + min(nod_x))/2
            for (i in 1:length(inds)) {
                inds2 <- which(auxxc1[i,] > mid)
                auxxc1[i,inds2] <- auxxc1[i,inds2] - 360
                inds2 <- which(auxxc2[i,] < mid)
                auxxc2[i,inds2] <- auxxc2[i,inds2] + 360
            }
            auxxc1[auxxc1 < -180] <- -180
            auxxc2[auxxc2 > 180] <- 180

        } # end if (cycl)

    } else if (!rotate_mesh) {
        
        if (cycl) {
            
            stop("not implemented1")
            #ai=find(nodes(4,:)>=4000);
            #if cycl & ~rotate_grid, % augment all arrays with mirror cyclic nodes
            #  xcyc=xcoord(ai(1));
            #  bnodes=find(xcoord==xcyc);
            #  bnodsurf=ai;
            #  bnnum=length(bnodes);
            #  bnnumsurf=length(bnodsurf);
            #  % assign numbers to their mirror nodes:
            #  nnodes=[n3d+1:n3d+bnnum];
            #  pos=zeros(1,n3d);
            #  pos(bnodes)=nnodes;
            #  % Augment the mesh
            #  % Straightforward part
            #  if(cycl_left)
            #    xcoord=[xcoord,xcoord(bnodes)+360];
            #  else
            #    xcoord=[xcoord,xcoord(bnodes)-360];
            #  end
            #  ycoord=[ycoord,ycoord(bnodes)];
            #  zcoord=[zcoord,zcoord(bnodes)];
            #  nodind=[nodind,nodind(bnodes)];
            #  % Difficult part (i) nod3d_below_nod2d
            #  nod32add= nod32(:,bnodsurf);
            #  validnodes=find(nod32add>-999);
            #  vl=length(validnodes);
            #  nod32add(validnodes)=pos(nod32add(validnodes));
            #  nod32=[nod32,nod32add];
            #  % (ii) 2d elements:
            #  % List all triangles that touch the cyclic boundary
            #  n1=find(abs(xcoord(elem(1,:))-xcyc)<0.1);
            #  n2=find(abs(xcoord(elem(2,:))-xcyc)<0.1);
            #  n3=find(abs(xcoord(elem(3,:))-xcyc)<0.1);
#
#              tlist=elem(:,n1);
#              maxt=max((xcoord(tlist)));
#              mint=min((xcoord(tlist)));
#              mind=find(maxt-mint>10);
#              elem(1,n1(mind))=pos(elem(1,n1(mind)));
#
#              tlist=elem(:,n2);
#              maxt=max((xcoord(tlist)));
#              mint=min((xcoord(tlist)));
#              mind=find(maxt-mint>10);
#              elem(2,n2(mind))=pos(elem(2,n2(mind)));
#
#              tlist=elem(:,n3);
#              maxt=max((xcoord(tlist)));
#              mint=min((xcoord(tlist)));
#              mind=find(maxt-mint>10);
#              elem(3,n3(mind))=pos(elem(3,n3(mind)));
#              % -------------------------------------
#              n2da=n2d+bnnumsurf;                 % new (augmented) n2d
#              n3da=n3d+bnnum;                     % new (augmented) n3d
#              surfnodes=[1:n2d,pos(bnodsurf)];    % surface nodes and
#              % Redefine pos                 % their positions in 2D arrays:
#              pos=zeros(1,n3da);
#              pos(1:n2d)=1:n2d;
#              pos(surfnodes(n2d+1:n2da))=n2d+1:n2da;
#            end    % cyclic part

        } else if (!cycl) {
            #message("nothing to do here")
        }

    }    # end if (rotate_mesh)

    # ugly workaround: restore Euler Angles back to default
    if (rotate_mesh && Ealpha == 0 && Ebeta == 0 && Egamma == 0) {
        Ealpha <- 50 # 1st Euler angle (from FESOMs namelist.config)
        Ebeta  <- 15 # 2nd
        Egamma <- -90 # 3rd
        rotate_mesh <- F
    }

    ## Surface nodes
    xcsur <- nod_x[1:nod2d_n]
    ycsur <- nod_y[1:nod2d_n]

    if (!rotate_mesh && area == "global") {
        map_geogr_lim_lon <- range(xcsur)
        map_geogr_lim_lat <- range(ycsur)
        poly_geogr_lim_lon <- map_geogr_lim_lon
        poly_geogr_lim_lat <- map_geogr_lim_lat
    }

    ## Coordinate matrix
    xc <- array(NA, c(dim(elem2d)))
    yc <- xc
    for (i in 1:3) {
        xc[,i] <- nod_x[elem2d[,i]]
        yc[,i] <- nod_y[elem2d[,i]]
    }
    if (!cycl) {
        xc <- t(xc)
        yc <- t(yc)
        elem2d <- t(elem2d)
    } else if (cycl) {
        xc <- cbind(t(xc), t(auxxc1), t(auxxc2))
        yc <- cbind(t(yc), t(auxyc1), t(auxyc2))
        elem2d <- cbind(t(elem2d), t(auxelem), t(auxelem))
    }

    elem2d_n <- dim(elem2d)[2]

    ## Save global coordinate matrices
    xc_global <- xc
    yc_global <- yc

    ## Save for restart run
    if (F) {
        if (dim_tag == "2D") {
            nod_x_save_2d <- nod_x
            nod_y_save_2d <- nod_y
        } else if (dim_tag == "3D") {
            nod_x_save_3d <- nod_x
            nod_y_save_3d <- nod_y
            nod_z_save_3d <- nod_z
            aux3d_save_3d <- aux3d
            deltaz_all_save <- deltaz_all
        }
        xcsur_save <- xcsur
        ycsur_save <- ycsur
        xc_save <- xc
        yc_save <- yc
        xc_global_save <- xc_global
        yc_global_save <- yc_global
        elem2d_orig_save <- elem2d_orig
    }

# else: restart run: mesh reading not necessary
# this is work in progress
} else {
    if (verbose > 0) {
        message(paste0("This is a restart run."))
        message(paste0("1) Reload mesh and 2) get geographic coordinates for ", 
              dim_tag, " variable ", varname, " ..."))
    }

    ## Reload variables
    if (dim_tag == "2D") {
        nod_x <- nod_x_save_2d
        nod_y <- nod_y_save_2d
    } else if (dim_tag == "3D") {
        nod_x <- nod_x_save_3d
        nod_y <- nod_y_save_3d
        aux3d <- aux3d_save_3d
    }
    xcsur <- xcsur_save
    ycsur <- ycsur_save
    xc <- xc_save
    yc <- yc_save
    xc_global <- xc_global_save
    yc_global <- yc_global_save
    elem2d_orig <- elem2d_orig

} # end if (restart or not) 

# special: save elem2d as netcdf
if (F) {
    elem2d_fname <- paste0(meshpath, "/", meshid, "_elem2d",
                             ifelse(cycl, "_cycl", ""), ".nc")
   
    message("**********************")
    message("special: save xc_global, yx_global, and elem2d in")
    message(elem2d_fname)
    message("**********************")

    node_per_elem_dim <- ncdim_def("nodes_per_element", "", 
                                   1:3, create_dimvar=F)
    elem_dim <- ncdim_def("elem2d_n", "",
                          1:elem2d_n, create_dimvar=F)

    xc_global_var <- ncvar_def("xc_global", "degrees east",
                            list(node_per_elem_dim, elem_dim),
                            prec="double")
    yc_global_var <- ncvar_def("yc_global", "degrees north",
                            list(node_per_elem_dim, elem_dim),
                            prec="double")
    elem2d_var <- ncvar_def("elem2d", "", 
                            list(node_per_elem_dim, elem_dim),
                            prec="integer")

    nc <- nc_create(elem2d_fname, 
                    list(xc_global_var, yc_global_var, elem2d_var),
                    force_v4=force_v4)
    ncvar_put(nc, elem2d_var, elem2d)
    ncvar_put(nc, xc_global_var, xc_global)
    ncvar_put(nc, yc_global_var, yc_global)
    nc_close(nc)
} # if special: save elem2d as netcdf

if (verbose == 2 || verbose == 3) {
        message(paste0(indent, "Loaded ", dim(xc)[2], " elements ..."))
        message(paste0(indent, "All longitudinal mesh elements = ",
                     round(range(xc)[1], 3), " deg to ",
                     round(range(xc)[2], 3), " deg"))
        message(paste0(indent, "All latitudinal mesh elements = ",
                     round(range(yc)[1], 3), " deg to ",
                     round(range(yc)[2], 3), " deg"))
}


## Calc bafux_2d/bafuy_2d/custer_area_2d/resolution as in fesom1.4 *.F90 if needed
if (horiz_deriv_tag != F || 
    (any(c(transient_out, regular_transient_out)) && 
     any(out_mode == c("mean", "depth", "areadepth", "meanint", "depthint"))) ||  # <- cluster_area_2d is needed
    (plot_map && plot_type == "interp" && 
        (interp_dlon_plot == "auto" || interp_dlon_plot == "auto"))) {


    if (!exists("derivpath")) {
        # use default
        derivpath <- paste0(getwd(), "/mesh/", meshid, "/derivatives")
        message(indent, "No 'derivpath' is given for saving/reading horizontal derivative/custer area/resolution matrices.\n",
                indent, "   Use default: ", derivpath, " (=getwd()'/mesh/'meshid'/derivatives')\n",
                indent, "   You can set derivpath <- \"/path/with/writing/rights\" in the runscript.")
    } else {
        derivpath <- suppressWarnings(normalizePath(derivpath))
    }
    if (file.access(derivpath, mode=0) == -1) { # mode=0: existing, -1: no success
        message(paste0(indent, "Try to create 'derivpath' = ", derivpath, " ... "), appendLF=F)
        dir.create(derivpath, recursive=T, showWarnings=T)
        if (file.access(derivpath, mode=0) == -1) {
            message("")
            stop(indent, "Could not create 'derivpath' = ", derivpath)
        } else {
            message("done.")
        }
    } else if (file.access(derivpath, mode=2) == -1) { # mode=2: writing, -1: no success
        message(indent, "You have no writing rights in 'derivpath' = ", derivpath, " ...")
        derivpath <- paste0(getwd(), "/mesh/", meshid, "/derivatives")
        message(indent, "   Use default: ", derivpath, " (=getwd()'/mesh/'meshid'/derivatives')\n",
                indent, "   You can set derivpath <- \"/path/with/writing/rights\" in the runscript.")
    }

    deriv_2d_fname <-  paste0(derivpath, "/mesh_", meshid, "_deriv_2d_",
                              out_coords, ifelse(cycl, "_cycl", ""), ".nc")
    
    if (!file.exists(deriv_2d_fname)) {
        if (verbose > 0) {
            message(indent, "Calc horizontal derivative/custer area/resolution matrices for ", meshid, " mesh\n",
                    indent, "   and save result in\n",
                    indent, "   ", deriv_2d_fname, " (= deriv_2d_fname)")
            message(indent, "Run lib/deriv_2d.r ...")
        }
        source(paste0(subroutinepath, "/deriv_2d.r"))
        deriv_2d <- deriv_2d_function(elem2d=elem2d, xcsur=xcsur, ycsur=ycsur,
                                      meshid=meshid, mv=mv, 
                                      deriv_2d_fname=deriv_2d_fname)
    } # if deriv_2d_fname does not exist

    if (verbose > 0) {
        message(paste0(indent, "Load ", meshid,
                     " mesh bafuxy_2d/cluster_area_2d/resolution file"))
        message(paste0(indent, indent, "'deriv_2d_fname' = ", deriv_2d_fname, " ..."))
    }
    deriv_2d_nc <- nc_open(deriv_2d_fname)
    bafux_2d <- ncvar_get(deriv_2d_nc, "bafux_2d")
    bafuy_2d <- ncvar_get(deriv_2d_nc, "bafuy_2d")
    voltriangle <- as.vector(ncvar_get(deriv_2d_nc, "voltriangle"))
    cluster_area_2d <- as.vector(ncvar_get(deriv_2d_nc, "cluster_area_2d")) # dim=nod2d_n
    resolution <- as.vector(ncvar_get(deriv_2d_nc, "resolution"))
    resolution_unit <- ncatt_get(deriv_2d_nc, "resolution", "units")$value

} # if horiz_deriv_tag
 
if (zave_method == 2 &&
    (transient_out || regular_transient_out) &&
    out_mode == "mean") {
 
    if (F) { # old
        deriv_3d_fname <- paste0(derivpath, "/mesh_", meshid, "_deriv_3d_",
                                 ifelse(horiz_deriv_tag != F, horiz_deriv_tag, out_coords),
                                 ".nc")
    } else {
        deriv_3d_fname <- paste0(derivpath, "/mesh_", meshid, "_deriv_3d_",
                                 out_coords, ".nc")
    }

    if (!file.exists(deriv_3d_fname)) {
        if (verbose > 1) {
            message(paste0(indent, "Calc '", meshid,
                         "' mesh bafuxy_3d/custer_vol_3d as in fesom1.4 *.F90"))
            message(paste0(indent, indent, "using deriv_3.r and save result in"))
            message(paste0(indent, indent, "'deriv_3d_fname' = ", deriv_3d_fname, " ..."))
        }
        if (!exists("derivpath")) {
            # use default
            message(indent, "Need to calculate and save horizontal derivative/cluster area and resolution matrix.\n",
                    indent, "No 'derivpath' is given for saving the result. Use default ...")
            derivpath <- paste0(meshpath, "/derivatives")
        } else {
            derivpath <- suppressWarnings(normalizePath(derivpath))
        }
        if (file.access(derivpath, mode=0) == -1) { # mode=0: existing, -1: no success
            #message(paste0("'derivpath' = ", derivpath, " does not exist ..."))
            message(paste0(indent, "Try to create 'derivpath' = ", derivpath, " ... "), appendLF=F)
            dir.create(derivpath, recursive=T, showWarnings=F)
            if (file.access(derivpath, mode=0) == -1) {
                message("")
                stop("Could not create 'derivpath' = ", derivpath)
            } else {
                message("done.")
            }
        } else if (file.access(derivpath, mode=2) == -1) { # mode=2: writing, -1: no success
            message(indent, "You have no writing rights in 'derivpath' = ", derivpath, " ...")
            derivpath <- paste0(getwd(), "/mesh/", meshid, "/derivatives")
            message(indent, "   Use default: ", derivpath, " (=getwd()'/mesh/'meshid'/derivatives')\n",
                    indent, "   You can set derivpath <- \"/path/with/writing/rights\" in the runscript.")
        }

        # load elem3d
        fid <- paste0(meshpath, "/elem3d.out")
        elem3d_n <- as.numeric(readLines(fid, n=1))
        if (verbose == 2 || verbose == 3) {
            message(paste0(indent, "   read ", elem3d_n,
                         " 3D elements from elem3d.out ..."))
        }
        if (!fread_tag) {
            tmp <- scan(fid, skip=1, quiet=T)
            elem3d <- t(matrix(tmp, nrow=elem3d_n, byrow=T))
        } else if (fread_tag) {
            tmp <- fread(fid, skip=1, showProgress=ifelse(verbose > 0, T, F))
            elem3d <- t(as.matrix(tmp))
        }
        if (F) {
            elem3d_save <- elem3d
        }

        # Elementwise derivation:
        source(paste0(subroutinepath, "/deriv_3d.r"))
        deriv_3d <- deriv_3d_function(elem3d=elem3d, nod_x, nod_y, nod_z,
                                      meshid=meshid, mv=mv, 
                                      deriv_3d_fname=deriv_3d_fname)
    } # if deriv_3d_fname does not exist

    if (verbose > 1) {
        message(paste0(indent, "Load ", meshid,
                     " mesh bafuxy_3d/cluster_vol_3d file"))
        message(paste0(indent, "   ", deriv_3d_fname, " ..."))
    }
    deriv_3d_nc <- nc_open(deriv_3d_fname)
    bafux_3d <- ncvar_get(deriv_3d_nc, "bafux_3d")
    bafuy_3d <- ncvar_get(deriv_3d_nc, "bafuy_3d")
    bafuz_3d <- ncvar_get(deriv_3d_nc, "bafuz_3d") 
    voltetra <- ncvar_get(deriv_3d_nc, "voltetra")
    cluster_vol_3d <- ncvar_get(deriv_3d_nc, "cluster_vol_3d")

} # if zave_method == 2 && out_mode == "mean"


## Interpolate irregular mesh to regular
if (any(regular_transient_out, regular_ltm_out)) {

    source(paste0(subroutinepath, "/sub_calc_load_regular_IMAT.r"))
    source(paste0(subroutinepath, "/sub_calc_regular_2d_interp.r"))

    # if namelist.config@fesom: rotated_grid = .true.
    #    --> namelist@rfesom: rotate_mesh = T
    interpfname <- paste0(meshid,
                          "_dx", sprintf("%.3f", regular_dx),
                          "_dy", sprintf("%.3f", regular_dy),
                          "_imat", 
                          #ifelse(rotate_mesh, "_rotated_grid_true", "_rotated_grid_false"),
                          ifelse(cycl, "_cycl", ""), 
                          ".nc")

    # interpolation matrix already exists
    if (file.exists(paste0(interppath, "/", interpfname))) {
        if (verbose > 0) {
            message(paste0(indent, "Found and load regular interpolation mat (dx=",
                         sprintf("%.3f", regular_dx), " deg,dy=", sprintf("%.3f", regular_dy),
                         " deg) for ", meshid, " mesh from"))
            message(paste0(indent, indent, "'interppath'/'interpfname' = ", interppath, "/", interpfname, " ..."))
        }
    
    # calculate interpolation matrix 
    } else {
        if (verbose > 0) {
            message(paste0(indent, "Calc regular interpolation mat (dx=",
                         sprintf("%.3f", regular_dx), " deg,dy=", sprintf("%.3f", regular_dy),
                         " deg) for '", meshid, "' mesh using"))
            message(paste0(indent, indent, "sub_calc_load_regular_IMAT.r and save result in"))
            message(paste0(indent, indent, "'interppath'/'interpfname' = ", interppath, "/", interpfname, " ..."))
        }
        if (file.access(interppath, mode=2) == -1) { # mode=2: writing, -1: no success
            message("You have no writing rights in 'interppath' = ", interppath, " ...")
            interppath <- paste0(getwd(), "/mesh/", meshid, "/interp")
            message("   Use default: ", interppath, " (=getwd()'/mesh/'meshid'/interp')\n",
                    "   You can set interppath <- \"/path/with/writing/rights\" in the runscript.")
        }
        dir.create(interppath, recursive=T, showWarnings=F)
        sub_calc_load_regular_IMAT(regular_dx=regular_dx, regular_dy=regular_dy,
                                   xp=xc_global, yp=yc_global,
                                   interppath=interppath,
                                   interpfname=interpfname,
                                   mv=mv)
    }

    imatncin <- nc_open(paste0(interppath, "/", interpfname))
    xi <- ncvar_get(imatncin, "xi")
    yi <- ncvar_get(imatncin, "yi")
    XI <- ncvar_get(imatncin, "XI")
    YI <- ncvar_get(imatncin, "YI")
    IMAT <- ncvar_get(imatncin, "IMAT")

    ## Select data in defined area in regular x,y space
    xinds <- which(xi >= range(map_geogr_lim_lon)[1] & 
                   xi <= range(map_geogr_lim_lon)[2])
    yinds <- which(yi >= range(map_geogr_lim_lat)[1] & 
                   yi <= range(map_geogr_lim_lat)[2])
    if (length(xinds) == 0 || length(yi) == 0) {
        stop("Error: Cannot find '", area, "# coordinates in regular xi,yi!")
    }
    xi <- xi[xinds]
    yi <- yi[yinds]
    nxi <- length(xi)
    nyi <- length(yi)

} # if any(regular_transient_out, regular_ltm_out)

if (verbose > 0) {
    message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
             " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
    message("==============================================")
}

if (out_mode != "csec_mean" && out_mode != "csec_depth" &&
    out_mode != "moc_mean" && out_mode != "moc_depth") {
    if (verbose > 0) {
        message(paste0("3) Choose coordinates from plot area '", area, "' ..."))
    }

    ## Choose from geographic coordinates (gives projected map
    ## with data only on projected area, e.g. piinterpolate_depthsa piece for high
    ## latitudes in stereographic projection)
    if (proj_lims) {
        if (verbose > 1) {
            message(paste0(indent, "Find coordinates in plot area with projection plot lims in '", 
                     projection, "' projection ..."))
        }

        ## Find all projected coordinates within chosen plot area (variable "area")
        poly_inds_geogr <- which(xc > range(poly_geogr_lim_lon)[1] & 
                                 xc < range(poly_geogr_lim_lon)[2] &
                                 yc > range(poly_geogr_lim_lat)[1] & 
                                 yc < range(poly_geogr_lim_lat)[2], arr.ind=T)
        #poly_inds_geogr <- which(xc > poly_geogr_lim_lon[1] & xc < poly_geogr_lim_lon[2] &
        #                         yc > poly_geogr_lim_lat[1] & yc < poly_geogr_lim_lat[2], arr.ind=T)
        poly_inds_geogr <- unique(poly_inds_geogr[,2])

        if (length(poly_inds_geogr) > 0) {
            xc <- xc[,poly_inds_geogr]
            yc <- yc[,poly_inds_geogr]
        }

        ## Project coordinates from geographical to target projection (variable "projection")
        success <- load_package("mapproj")
        if (!success) stop(helppage)
 
        xp <- array(NA, dim(xc))
        yp <- xp
        for (i in 1:dim(xc)[1]) {
            tmp <- mapproject(xc[i,], yc[i,], projection=projection, orientation=orient, 
                              par=projection_par)
            xp[i,] <- tmp$x
            yp[i,] <- tmp$y
        }
        tmp <- mapproject(xcsur, ycsur,
                          projection=projection, orientation=orient,
                          par=projection_par)
        xpsur <- tmp$x
        ypsur <- tmp$y

    ## or choose data from projected coordinates (gives projected map
    ## with data everywhere in plot area)
    } else if (geogr_lims) {
        if (verbose > 1) {
            message(paste0(indent, "Find coordinates in plot area with geographic plot lims in '", 
                         projection, "' projection ..."))
        }
            
        if (projection != "rectangular") {
            ## Project coordinates from geographical to target projection (variable "projection")
            success <- load_package("mapproj")
            if (!success) stop(helppage)

            xp <- array(NA, dim(xc))
            yp <- xp
            for (i in 1:dim(xc)[1]) {
                tmp <- mapproject(xc[i,], yc[i,], projection=projection, orientation=orient,
                                  par=projection_par)
                xp[i,] <- tmp$x
                yp[i,] <- tmp$y
            }
            tmp <- mapproject(xcsur, ycsur, projection=projection, orientation=orient,
                              par=projection_par)
            nod_xp <- tmp$x
            nod_yp <- tmp$y

            if (projection == "stereographic") {
                ## Find all projected coordinates within chosen plot area (variable "area")
                success <- load_package("maps")
                if (!success) stop(helppage)
           
                map("world", t="n", proj=projection, 
                    orient=orient, par=projection_par,
                    xlim=range(map_geogr_lim_lon), 
                    ylim=range(map_geogr_lim_lat))
                extreme_coords <- par("usr")
                dev.off()
                #tmp <- mapproject(poly_geogr_lim_lon, poly_geogr_lim_lat, projection=projection, 
                #              orientation=orient, par=projection_par)
                #poly_proj_lim_lon <- tmp$x
                #poly_proj_lim_lat <- tmp$y
                poly_proj_lim_lon <- extreme_coords[1:2]
                poly_proj_lim_lat <- extreme_coords[3:4]
            
            } else if (projection == "orthographic") {
                # Find out range of coordinates which are not on the "dark" side of the 
                # earth as areaed from space.
                # Use "world" (not "world2") as dataset here because longitudes of FESOM 
                # are -180:180 and not 0:360.
                if (F) { # this was buggy
                    success <- load_package("maps")
                    if (!success) stop(helppage)

                    tmp <- map("world", t="n", proj=projection, orient=orient, 
                               par=projection_par, plot=F)
                    tmp <- na.omit(data.frame(do.call(cbind, tmp[c("x","y")])))
                    poly_proj_lim_lon <- range(tmp$x, na.rm=T)
                    poly_proj_lim_lat <- range(tmp$y, na.rm=T)
                } else {
                    poly_proj_lim_lon <- range(nod_xp, na.rm=T)
                    poly_proj_lim_lat <- range(nod_yp, na.rm=T)
                }
            }

            poly_inds_proj <- which(xp > poly_proj_lim_lon[1] & xp < poly_proj_lim_lon[2] &
                                    yp > poly_proj_lim_lat[1] & yp < poly_proj_lim_lat[2], 
                                    arr.ind=T)
            poly_inds_proj <- unique(poly_inds_proj[,2])
            
            poly_node_inds_proj <- nod_xp > poly_proj_lim_lon[1] &
                                        nod_xp < poly_proj_lim_lon[2] &
                                        nod_yp > poly_proj_lim_lat[1] &
                                        nod_yp < poly_proj_lim_lat[2]

            if (length(poly_inds_proj) > 0) {
                xp <- xp[,poly_inds_proj]
                yp <- yp[,poly_inds_proj]
            }

        } else if (projection == "rectangular") {

            ## Find area inds in element space
            # Consider both pos. and neg. longitudes
            if (F && any(map_geogr_lim_lon > 0) && any(map_geogr_lim_lon < 0)) {
            #if (map_geogr_lim_lon[1] < 0 && map_geogr_lim_lon[2] > 0) {
                cyclic_plot <- T # T
                poly_inds_geogr1 <- unique(which(xc > poly_geogr_lim_lon[1] &
                                                 yc > poly_geogr_lim_lat[1] &
                                                 yc < poly_geogr_lim_lat[2], arr.ind=T)[,2])

                poly_inds_geogr2 <- unique(which(xc < poly_geogr_lim_lon[2] &
                                                 yc > poly_geogr_lim_lat[1] &
                                                 yc < poly_geogr_lim_lat[2], arr.ind=T)[,2])
                poly_inds_geogr <- c(poly_inds_geogr1, poly_inds_geogr2)
            
            # all other cases
            } else {
                cyclic_plot <- F
                poly_inds_geogr <- which(xc > range(poly_geogr_lim_lon)[1] & 
                                         xc < range(poly_geogr_lim_lon)[2] &
                                         yc > range(poly_geogr_lim_lat)[1] & 
                                         yc < range(poly_geogr_lim_lat)[2], arr.ind=T)
                poly_inds_geogr <- unique(poly_inds_geogr[,2])

                # closed polygon of arbitrary shape
                if (length(poly_geogr_lim_lon) > 1 && # not a single point
                    poly_geogr_lim_lon[1] == poly_geogr_lim_lon[length(poly_geogr_lim_lon)]) {
                    success <- load_package("splancs")
                    if (!success) stop(helppage)

                    if (F) {
                        # even worse
                        test <- cbind(poly_geogr_lim_lon, poly_geogr_lim_lat)
                        test <- test[sort(test[,1], index.return=T)$ix,]
                    }

                    tmp <- vector("list", l=3)
                    for (i in 1:3) {
                        if (F) {
                            tmp[[i]] <- splancs::inpip(pts=cbind(xc[i,], yc[i,]),
                                                       poly=cbind(poly_geogr_lim_lon,
                                                                  poly_geogr_lim_lat),
                                                       #poly=test,
                                                       bound=T, quiet=F)
                        } else if (T) {
                            success <- load_package("sp")
                            if (!success) stop(helppage)
                            tmp[[i]] <- point.in.polygon(xc[i,], yc[i,], 
                                                         poly_geogr_lim_lon, poly_geogr_lim_lat, 
                                                         mode.checked=F) 
                                                         # = default FALSE, used internally to save time when all the
                                                         #   other argument are known to be of storage mode double

                        }
                    } # i:3
                    
                    if (T) {
                        tmp <- lapply(tmp, function(x) which(x == 1)) # 1 = interior; 2 = on edge
                    }
                    
                    # only elements whose all 3 nodes are within xlim/ylim
                    poly_inds_geogr <- Reduce(intersect, tmp)
                    rm(tmp)
               
                # 4-corner box
                } else if (length(map_geogr_lim_lon) == 2) {
                    poly_inds_geogr <- which(xc > range(poly_geogr_lim_lon)[1] &
                                             xc < range(poly_geogr_lim_lon)[2] &
                                             yc > range(poly_geogr_lim_lat)[1] &
                                             yc < range(poly_geogr_lim_lat)[2], arr.ind=T)
                    # all nodes of elements 
                    poly_inds_geogr <- unique(poly_inds_geogr[,2])
                
                # 1 single location
                } else if (length(map_geogr_lim_lon) == 1) {
                    
                    # find polygon in which the point is located
                    success <- load_package("sp")
                    if (!success) stop(helppage)
                    for (i in 1:elem2d_n) {
                        poly <- cbind(xc[,i], yc[,i])
                        poly <- rbind(poly, poly[1,])
                        ind <- point.in.polygon(map_geogr_lim_lon, map_geogr_lim_lat,
                                                poly[,1], poly[,2])
                        if (ind == 1) {
                            poly_inds_geogr <- i
                            break
                        }
                        if (i == elem2d_n) {
                            stop(paste0("The single-point '", area, "' location (x=", 
                                        map_geogr_lim_lon, ",y=", map_geogr_lim_lat, 
                                        ") is not contained in your mesh. choose another location."))
                        }
                    }
                    # check
                    if (F) {
                        plot(map_geogr_lim_lon, map_geogr_lim_lat, pch=4, 
                             xlim=range(xc[,poly_inds_geogr]), ylim=range(yc[,poly_inds_geogr]))
                        polygon(xc[,poly_inds_geogr], yc[,poly_inds_geogr]) 
                    }
                } # arbitrary
            }

            # Cut area in element space
            if (length(poly_inds_geogr) == 0) {
                stop(paste0(indent,
                            "Error: not any mesh elements within the chosen area. Choose another!"))
            }
            xp <- xc[,poly_inds_geogr] # these elements may include nodes 
            yp <- yc[,poly_inds_geogr] # outside of xlim and/or ylim

            xpsur <- xcsur
            ypsur <- ycsur
            
            ## Find area inds in node space
            # closed polygon of arbitrary shape
            if (length(poly_geogr_lim_lon) > 1 &&
                poly_geogr_lim_lon[1] == poly_geogr_lim_lon[length(poly_geogr_lim_lon)]) {
                success <- load_package("splancs")
                if (!success) stop(helppage)

                poly_node_inds_geogr <- splancs::inpip(cbind(xcsur, ycsur), 
                                                       cbind(poly_geogr_lim_lon, 
                                                             poly_geogr_lim_lat), 
                                                       bound=T)

            # 4-corner box 
            } else if (length(poly_geogr_lim_lon) == 2) {
                poly_node_inds_geogr <- which(xcsur > range(poly_geogr_lim_lon)[1] &
                                              xcsur < range(poly_geogr_lim_lon)[2] &
                                              ycsur > range(poly_geogr_lim_lat)[1] &
                                              ycsur < range(poly_geogr_lim_lat)[2])
            # single-point location
            } else if (length(poly_geogr_lim_lon) == 1) {

                # find polygon in which the point is located
                success <- load_package("sp")
                if (!success) stop(helppage)
                for (i in 1:elem2d_n) {
                    poly <- cbind(xc[,i], yc[,i])
                    poly <- rbind(poly, poly[1,])
                    ind <- point.in.polygon(map_geogr_lim_lon, map_geogr_lim_lat,
                                            poly[,1], poly[,2])
                    if (ind == 1) {
                        poly_node_inds_geogr <- i
                        break
                    }
                    if (i == elem2d_n) {
                        stop(paste0("The single-point '", area, "' location (x=",
                                    map_geogr_lim_lon, ",y=", map_geogr_lim_lat,
                                    ") is not contained in your mesh. choose another location."))
                    }
                }
                # from element to nodes
                poly_node_inds_geogr <- elem2d[,poly_node_inds_geogr]

                # check arbitrary polygon
                if (F) {
                    plot(map_geogr_lim_lon, map_geogr_lim_lat, pch=4,
                         xlim=range(xcsur[poly_node_inds_geogr]), ylim=range(ycsur[poly_node_inds_geogr]))
                    points(xcsur[poly_node_inds_geogr], ycsur[poly_node_inds_geogr], col=2)
                    polygon(xcsur[poly_node_inds_geogr], ycsur[poly_node_inds_geogr])
                }
            
            } # arbitrary polygon, box, or single-point location

            if (length(poly_inds_geogr) == 0) {
                stop(paste0(indent, 
                            "Error: not any nodes within the chosen area. Choose another!"))
            }

        } # if projection == "rectangular" or not
    } # end if proj_lims or geogr_lims

    ## Check projected coordinates for NA
    na_inds <- unique(which(is.na(xp), arr.ind=T)[,2])
    if (length(na_inds) > 0) {
        message(paste0(indent, "Remove ", length(na_inds),
                     " NAs in projected coordiantes ..."))
        xp <- xp[,-na_inds]
        yp <- yp[,-na_inds]
        # data values at the same na_inds are removed later (once the datamat exists)
    }

    ## Check coordinates so far
    if (verbose > 1) {
        message(paste0(indent, "Found ", dim(xp)[2], " elements in ", area, " area"))
        message(paste0(indent, "Projected (", projection,
                     ") longitudinal elements in ", area, " = ",
                     round(range(xp)[1], 3), " deg to ",
                     round(range(xp)[2], 3), " deg"))
        message(paste0(indent, "Projected (", projection,
                     ") latitudinal elements in ", area, " = ",
                     round(range(yp)[1], 3), " deg to ",
                     round(range(yp)[2], 3), " deg"))
    }

    if (verbose > 0) {
        message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
                     " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
        message("==============================================")
    }

} # out_mode != csec_mean csec_depth moc_mean moc_depth 


## 3) for Crossection
if (transient_out && 
    (out_mode == "csec_mean" || out_mode == "csec_depth")) {
    if (verbose > 0) {
        message(paste0("3) Find coordinates of cross section ", area, " ..."))
    }

    csec_n_vertices <- length(map_geogr_lim_lon) ## csection vertices defined by user
    csec_n_edges <- csec_n_vertices - 1
    csec_norm_edge <- rep(NA, t=csec_n_edges)
    csec_e_vec_edge <- array(NA, c(2, csec_n_edges))

    for (i in 2:length(map_geogr_lim_lon)) {
        # MAKE NORM OFF CROSSECTION SEGMENT
        csec_norm_edge[i-1] <- sqrt(sum(c(map_geogr_lim_lon[i]-map_geogr_lim_lon[i-1],
                                          map_geogr_lim_lat[i]-map_geogr_lim_lat[i-1])^2))

        # MAKE UNIT VECTOR OFF CROSSECTION SEGMENT
        csec_e_vec_edge[,i-1] <- c(map_geogr_lim_lon[i]-map_geogr_lim_lon[i-1],
                                   map_geogr_lim_lat[i]-map_geogr_lim_lat[i-1])/csec_norm_edge[i-1]
    }
    # MAKE NORMAL UNIT VECTOR
    csec_n_vec_edge <- rbind(csec_e_vec_edge[2,], -csec_e_vec_edge[1,])

    if (F) {
        # DS: 
        #xlim <- c(-40, -20)
        #ylim <- c(62, 68)
        # Iceland-Scotland:
        #xlim <- c(-16, 2)
        #ylim <- c(59, 66)
        # lsea
        xlim <- c(-70, -35)
        ylim <- c(50, 65)
        # NAC
        #xlim <- c(-55, -40)
        #ylim <- c(44, 52)
        #xlim <- range(nod_x[surfnodes])
        #ylim <- range(nod_y[surfnodes])
        #xlim <- c(-65, -41)
        #ylim <- c(50, 60)
        # lsea3
        #xlim <- c(-80, -10)
        #ylim <- c(45, 80)
        # Weddel
        #xlim <- c(-60, 60)
        #ylim <- c(-80, -55)
        project <- F
        if (project) {
            if (!any(search() == "package:mapproj")) library(mapproj)
            maps::map("world", xlim=xlim, ylim=ylim, interior=F, 
                      projection="stereographic", o=c(mean(ylim), mean(xlim), 0))
            points(mapproject(x=nod_x[surfnodes], y=nod_y[surfnodes]), cex=0.2)
            
            # FramStrait
            lines(mapproject(x=c(10.7, -12.83), y=c(78.99, 81.48)), col=1, lwd=2)
            
            # Denmark Strait
            # Jochumsen et al. 2012, check supp!
            # 3.4 Sv [sigma_theta > 27.8], sd = 1.4 Sv, variance = 2.0 Sv^2, 1996-2011, "no significant trend"
            # "no seasonal cycle which is in contrast to high resolution modeling studies"
            # seasonal variability accounts only to 5 % of the overall variability
            # interannual variability is on the order of 10 %
            # no relation to wind stress curl or NAO
            # daily variation: 1.4 (northward) - 8.7 (southward) Sv

            # Macrander et al. 2005
            # defintion of deep water not given -___-
            # 3.68 Sv, Oct-Dec 1999
            # 3.66 Sv, Aug 2000-Apr 2001
            # 3.16 Sv, Jul 2001-Jul 2002
            # 3.07 Sv, Oct 2002-Apr 2003
            # 20 % reduction of transport, warming of bottom at the same time
            lines(mapproject(x=c(-33.3, -25), y=c(67.2, 65.5)), col=2, lwd=2)

            # Ice-Scotland Ridge
            lines(mapproject(x=c(-12.9, -2.8), y=c(65, 60.5)), col=3, lwd=2)

            # Iceland-Faeroe_Ridge
            lines(mapproject(x=c(-7, -14), y=c(62.25, 65)), col=4, lwd=2)

            # Faeroe-Scotland_Ridge
            lines(mapproject(x=c(-2.5, -7), y=c(60, 62.25)), col=5, lwd=2)

            # Cape_Farewell
            # Dickson & Brown 1994, 13.3 Sv [sigma_theta >= 27.8]
            lines(mapproject(x=c(-43.92017, -43.92017), y=c(59.7705, 57)), col=6, lwd=2)
     
            # Dohrn_Bank 
            # Dickson & Brown 1994, 5.2 Sv [sigma_theta >= 27.8, v <= 0 (southward)], Mar 1990-Jul 1990
            lines(mapproject(x=c(-31, -30.5), y=c(65.25, 64.75)), col=7, lwd=2)

            # TTO
            # Dickson & Brown 1994, 5.1 Sv [sigma_theta >= 27.8, v <= 0 (southward)], Jul 1990-Jul 1991
            lines(mapproject(x=c(-32, -34.3), y=c(63.48, 65)), col=8, lwd=2)

            # Angmagssalik
            # Dickson & Brown 1994, 10.7 Sv [sigma_theta >= 27.8], Jun 1987-Aug 1990
            # Dickson et al 1990, 10.7 Sv, [sigma_theta >= 27.8, v <= 0], Jun 1987-Jul 1989
            lines(mapproject(x=c(-37, -33.7), y=c(63.65, 62.2)), col=9, lwd=2)

            # FBC 
            # Hansen et al. 2016
            # 2.2 +/- 0.2 Sv, Nov 1995-May 2015 ("stable with slight but not statistically significant upward trend")
            # Hansen et al. 2001
            # ~1.25 Sv (by eye), Jul 1995-Jun 2000, decreasing trend of 2-4 % per year
            lines(mapproject(x=c(-8.666667, -7.25), y=c(61.1667, 62.5)), col=10, lwd=2)

            # N53 
            # Fischer et al 2004: Aug 1997-Jul 1999 means
            #   11.4 Sv LSW [27.74 < sigma_theta < 27.8]   
            #   9.7 +/- 2.2 Sv GFZW [27.8 < sigma_theta < 27.88]
            #   4.5 +/- 1.3 Sv DSOW [sigma_theta > 27.88]
            #   14.1 +/- 3.3 Sv GFZW+DSOW [sigma_theta > 27.8]
            #   26.2 +/- 6.1 Sv [sigma_theta > 27.74]
            #   28.3 +/- 4.6 Sv [sigma_theta > 27.74] Jul 1997-Jun 1998
            #   24.2 +/- 4.7 Sv [sigma_theta > 27.74] Jul 1998-Jun 1999
            lines(mapproject(x=c(-51.75, -49), y=c(52.75, 53.8)), col=11, lwd=2)
        
        } else {
           
            if (!any(search() == "package:mapdata")) library(mapdata) # for "worldHires" dataset
            maps::map("worldHires", xlim=xlim, ylim=ylim, interior=F) 
            points(x=nod_x[surfnodes], y=nod_y[surfnodes], cex=0.2)
            axis(1)
            axis(2)

            # lswNA box
            if (area == "csec_lseawNA") {
                polygon(map_geogr_lim_lon, map_geogr_lim_lat, 
                        border="black")
            } else {

                # davis
                lines(x=c(-53.72, -61.75), y=c(66.59, 66.59), col=2, lwd=3) 

                # irm
                lines(x=c(-43, -33.4), y=c(60, 57.4), col=7, lwd=2)
            
                # ar7w
                lines(x=c(-55.45, -48.26), y=c(53.66, 60.54), col=1, lwd=2)

                # BP1215
                lines(c(-36.85739, -31.15646), c(52.50856, 47.66858), col="black", lwd=4)

                # flemish pass

                # FramStrait
                lines(x=c(10.7, -12.83), y=c(78.99, 81.48), col=1, lwd=2)
                
                # Denmark Strait
                lines(x=c(-33.3, -25), y=c(67.2, 65.5), col=2, lwd=2)

                # Eastern Denmark Strait from 29 deg West to Iceland
                lines(x=c(-29, -25), y=c(66.31783, 65.5), col="purple", lwd=2)

                # Western Denmark Strait from Greenland to 29 deg West
                lines(x=c(-33.3, -29), y=c(67.2, 66.31783), col="cyan", lwd=2)

                # Ice-Scotland Ridge "IceShet"
                lines(x=c(-1.2, -14.6), y=c(60.2, 64.5), col="black", lwd=2)

                # Iceland-Faeroe_Ridge "IFR"
                lines(x=c(-7.037389, -14.6), y=c(62.073192, 64.5), col="red", lwd=2)

                # Faeroe Shetland Channel "FSC"
                lines(x=c(-1.2, -7.037389), y=c(60.2, 62.073192), col="blue", lwd=2)

                # Cape_Farewell
                lines(x=c(-43.92017, -43.92017), y=c(59.7705, 57), col=6, lwd=2)
         
                # Dohrn_Bank 
                lines(x=c(-31, -30.5), y=c(65.25, 64.75), col=7, lwd=2)

                # TTO
                lines(x=c(-32, -34.3), y=c(63.48, 65), col=8, lwd=2)

                # Angmagssalik
                lines(x=c(-37, -33.7), y=c(63.65, 62.2), col=9, lwd=2)

                # FBC 
                lines(x=c(-6.9, -8.4), y=c(61.9, 61), col=10, lwd=2)

                # N53 
                lines(x=c(-52, -49), y=c(52.51, 54.16667), col=11, lwd=2)

            } # if area_csec_lseawNA

        } # if project
        
        #plot(nod_x[surfnodes], nod_y[surfnodes], xlim=c(-60, 0), ylim=c(60, 70))
        #segments(map_geogr_lim_lon[1], map_geogr_lim_lat[1], map_geogr_lim_lon[2], map_geogr_lim_lat[2])
    
        #stop("asd")
    
    } # if F show csection

    ## analyse cross-section: which trianglse are crossed by section...
    P <- rep(NA, t=6)
    csec_crossed_tri <- rep(0, t=elem2d_n)
    csec_crossed_nodes <- rep(0, t=nod2d_n)
    csec_edge_pointsx <- vector("list", l=csec_n_edges)
    csec_edge_pointsy <- csec_edge_pointsx
    min_norm <- rep(NA, t=csec_n_edges)
    max_norm <- min_norm
    eps <- .Machine$double.eps
    elem_area_inds <- rep(0, t=elem2d_n)
    offset <- 0.1

    ## calculate regional limited triangles for every csection edge:
    for (j in 2:csec_n_vertices) {
        tmp <- (apply(xc, 2, max) < min(map_geogr_lim_lon) - offset |
                apply(xc, 2, min) > max(map_geogr_lim_lon) + offset) |
               (apply(yc, 2, max) < min(map_geogr_lim_lat) - offset |
                apply(yc, 2, min) > max(map_geogr_lim_lat) + offset)
        elem_area_inds[!tmp] <- 1
    } # for 2 csec_n_vertices

    elem_area_inds <- which(elem_area_inds == 1)
    elem_area_inds_n <- length(elem_area_inds)

    # points(xc[,elem_area_inds], yc[,elem_area_inds], col="red")

    # create progress bar
    pb <<- mytxtProgressBar(min=0, max=elem_area_inds_n, style=pb_style,
                            char=pb_char, width=pb_width,
                            indent=paste0("     ", indent)) # 5 " " for default message()

    for (i in 1:elem_area_inds_n) { # check all 2d elems within area

        #progress_function(elem_area_inds_n, i, indent=paste0(indent, "   "))

        for (j in 2:csec_n_vertices) { # check all csection edges

            if (i == 1) {
                max_norm[j-1] <- 0
                min_norm[j-1] <- 1e6
            }

            A <- array(0, c(6, 6))

            #___TRIANGLE EDGE 1_________________________________________________
            norm_tri_edge1 <- sqrt((xc[2,elem_area_inds[i]] - xc[1,elem_area_inds[i]])^2 + 
                                   (yc[2,elem_area_inds[i]] - yc[1,elem_area_inds[i]])^2)
            A[1,1] <- (xc[2,elem_area_inds[i]] - xc[1,elem_area_inds[i]])/norm_tri_edge1
            A[2,1] <- (yc[2,elem_area_inds[i]] - yc[1,elem_area_inds[i]])/norm_tri_edge1
            A[1,2] <- -(map_geogr_lim_lon[j] - map_geogr_lim_lon[j-1])/csec_norm_edge[j-1]
            A[2,2] <- -(map_geogr_lim_lat[j] - map_geogr_lim_lat[j-1])/csec_norm_edge[j-1]
            
            #___TRIANGLE EDGE 2_________________________________________________
            norm_tri_edge2 <- sqrt((xc[3,elem_area_inds[i]] - xc[2,elem_area_inds[i]])^2 + 
                                   (yc[3,elem_area_inds[i]] - yc[2,elem_area_inds[i]])^2)
            A[3,3] <- (xc[3,elem_area_inds[i]] - xc[2,elem_area_inds[i]])/norm_tri_edge2
            A[4,3] <- (yc[3,elem_area_inds[i]] - yc[2,elem_area_inds[i]])/norm_tri_edge2
            A[3,4] <- -(map_geogr_lim_lon[j] - map_geogr_lim_lon[j-1])/csec_norm_edge[j-1]
            A[4,4] <- -(map_geogr_lim_lat[j] - map_geogr_lim_lat[j-1])/csec_norm_edge[j-1]
            
            #___TRIANGLE EDGE 3_________________________________________________
            norm_tri_edge3 <- sqrt((xc[1,elem_area_inds[i]] - xc[3,elem_area_inds[i]])^2 + 
                                   (yc[1,elem_area_inds[i]] - yc[3,elem_area_inds[i]])^2)
            A[5,5] <- (xc[1,elem_area_inds[i]] - xc[3,elem_area_inds[i]])/norm_tri_edge3
            A[6,5] <- (yc[1,elem_area_inds[i]] - yc[3,elem_area_inds[i]])/norm_tri_edge3
            A[5,6] <- -(map_geogr_lim_lon[j] - map_geogr_lim_lon[j-1])/csec_norm_edge[j-1]
            A[6,6] <- -(map_geogr_lim_lat[j] - map_geogr_lim_lat[j-1])/csec_norm_edge[j-1]
            
            #___________________________________________________________________
            P[1] <- map_geogr_lim_lon[j-1] - xc[1,elem_area_inds[i]]
            P[2] <- map_geogr_lim_lat[j-1] - yc[1,elem_area_inds[i]]
            P[3] <- map_geogr_lim_lon[j-1] - xc[2,elem_area_inds[i]]
            P[4] <- map_geogr_lim_lat[j-1] - yc[2,elem_area_inds[i]]
            P[5] <- map_geogr_lim_lon[j-1] - xc[3,elem_area_inds[i]]
            P[6] <- map_geogr_lim_lat[j-1] - yc[3,elem_area_inds[i]]

            #___SOLVE LINEAR EQUATION SYSTEM____________________________________
            success <- load_package("pracma")
            if (!success) stop(helppage)

            if (F) { # try if base::solve() is successful or pracma::mldivide() must be used
                if (i == 1) {
                    try <- tryCatch(solve(A, P), error=function(e) e, warning=function(w) w)
                    if (any(attributes(try)$names == "message")) {
                        message(paste0(indent, "Warning: 'stats::solve(A, P)' was not successful:"))
                        message(paste0(indent, indent, "         '", try$message, "'"))
                        message(paste0(indent, "         Use 'pracma::mldivide(A, P)' instead ..."))
                        mldivide_check <- T
                    } else {
                        mldivide_check <- F
                    }
                } # 1st element

                if (mldivide_check) {
                    X <- mldivide(A, P)
                } else {
                    X <- solve(A, P)
                }
            } else { # use pracma::mldivide() as default
                X <- mldivide(A, P)
            }
            
            # if cutted element
            if (((X[1] >= 0 && (X[1] - norm_tri_edge1) <= eps) && 
                 (X[2] >= 0 && (X[2] - csec_norm_edge[j-1]) <= eps)) ||
                ((X[3] >= 0 && (X[3] - norm_tri_edge2) <= eps) && 
                 (X[4] >= 0 && (X[4] - csec_norm_edge[j-1]) <= eps)) ||
                ((X[5] >= 0 && (X[5] - norm_tri_edge3) <= eps) && 
                 (X[6] >= 0 && (X[6] - csec_norm_edge[j-1]) <= eps))) {
                
                #message(paste0("elem ", elem_area_inds[i], " = (", 
                #             round(xcsur[elem2d[,elem_area_inds[i]]], 4), ",", 
                #             round(ycsur[elem2d[,elem_area_inds[i]]], 4), ")"))
                csec_crossed_tri[elem_area_inds[i]] <- 1
                csec_crossed_nodes[elem2d[,elem_area_inds[i]]] <- 1

                # CALC: coordinates of cross-section points of triangle
                # edge and crossection vector
                if ((X[1] >= 0 && (X[1] - norm_tri_edge1) <= eps) && 
                    (X[2] >= 0 && (X[2] - csec_norm_edge[j-1]) <= eps)) {

                    P_x_cut <- xc[1,elem_area_inds[i]] + 
                               X[1]*(xc[2,elem_area_inds[i]] - xc[1,elem_area_inds[i]])/norm_tri_edge1
                    P_y_cut <- yc[1,elem_area_inds[i]] + 
                               X[1]*(yc[2,elem_area_inds[i]] - yc[1,elem_area_inds[i]])/norm_tri_edge1
                 
                    #if isempty(find(  abs(obj(csi).crossed_edge_pts_x{segi-1}-P_x_cut)<=eps & abs(obj(csi).crossed_edge_pts_y{segi-1}-P_y_cut)<=eps,1));
                    #    obj(csi).crossed_edge_pts_x{segi-1} = [obj(csi).crossed_edge_pts_x{segi-1} P_x_cut];
                    #    obj(csi).crossed_edge_pts_y{segi-1} = [obj(csi).crossed_edge_pts_y{segi-1} P_y_cut];
                    #end

                    if (F) {
                        message(paste0(i, " (1,2): ", which(abs(csec_edge_pointsx[[j-1]] - P_x_cut) <= eps &
                                                         abs(csec_edge_pointsy[[j-1]] - P_y_cut) <= eps)[1]))
                    }

                    if (is.na(which(abs(csec_edge_pointsx[[j-1]] - P_x_cut) <= eps &
                                    abs(csec_edge_pointsy[[j-1]] - P_y_cut) <= eps)[1])) {
                        csec_edge_pointsx[[j-1]] <- c(csec_edge_pointsx[[j-1]], P_x_cut)
                        csec_edge_pointsy[[j-1]] <- c(csec_edge_pointsy[[j-1]], P_y_cut)
                    }
                }

                if ((X[3] >= 0 && (X[3] - norm_tri_edge2) <= eps) && 
                    (X[4] >= 0 && (X[4] - csec_norm_edge[j-1]) <= eps)) {

                    P_x_cut <- xc[2,elem_area_inds[i]] + 
                               X[3]*(xc[3,elem_area_inds[i]] - xc[2,elem_area_inds[i]])/norm_tri_edge2
                    P_y_cut <- yc[2,elem_area_inds[i]] + 
                               X[3]*(yc[3,elem_area_inds[i]] - yc[2,elem_area_inds[i]])/norm_tri_edge2
                   
                    if (F) {
                        message(paste0(i, " (3,4): ", which(abs(csec_edge_pointsx[[j-1]] - P_x_cut) <= eps &
                                                          abs(csec_edge_pointsy[[j-1]] - P_y_cut) <= eps)[1]))
                    }

                    if (is.na(which(abs(csec_edge_pointsx[[j-1]] - P_x_cut) <= eps &
                                    abs(csec_edge_pointsy[[j-1]] - P_y_cut) <= eps)[1])) {
                        csec_edge_pointsx[[j-1]] <- c(csec_edge_pointsx[[j-1]], P_x_cut)
                        csec_edge_pointsy[[j-1]] <- c(csec_edge_pointsy[[j-1]], P_y_cut)
                    }   
                }

                if ((X[5] >= 0 && (X[5] - norm_tri_edge3) <= eps) && 
                    (X[6] >= 0 && (X[6] - csec_norm_edge[j-1]) <= eps)) {

                    P_x_cut <- xc[3,elem_area_inds[i]] + 
                               X[5]*(xc[1,elem_area_inds[i]] - xc[3,elem_area_inds[i]])/norm_tri_edge3
                    P_y_cut <- yc[3,elem_area_inds[i]] +
                               X[5]*(yc[1,elem_area_inds[i]] - yc[3,elem_area_inds[i]])/norm_tri_edge3

                    if (F) {
                        message(paste0(i, " (5,6): ", which(abs(csec_edge_pointsx[[j-1]] - P_x_cut) <= eps &
                                                          abs(csec_edge_pointsy[[j-1]] - P_y_cut) <= eps)[1]))
                    }

                    if (is.na(which(abs(csec_edge_pointsx[[j-1]] - P_x_cut) <= eps &
                                    abs(csec_edge_pointsy[[j-1]] - P_y_cut) <= eps)[1])) {
                        csec_edge_pointsx[[j-1]] <- c(csec_edge_pointsx[[j-1]], P_x_cut)
                        csec_edge_pointsy[[j-1]] <- c(csec_edge_pointsy[[j-1]], P_y_cut)
                    }
                }

                # SEARCH both boundary points
                XXX <- rep(0, t=6)
                if ((X[1] >= 0 && (X[1] - norm_tri_edge1) <= eps) && 
                    (X[2] >= 0 && (X[2] - csec_norm_edge[j-1]) <= eps)) {
                    XXX[2] <- 1
                }
                if ((X[3] >= 0 && (X[3] - norm_tri_edge2) <= eps) && 
                    (X[4] >= 0 && (X[4] - csec_norm_edge[j-1]) <= eps)) {
                    XXX[4] <- 1
                }
                if ((X[5] >= 0 && (X[5] - norm_tri_edge3) <= eps) && 
                    (X[6] >= 0 && (X[6] - csec_norm_edge[j-1]) <= eps)) {
                    XXX[6] <- 1
                }
                max_norm[j-1] <- max(max_norm[j-1], X[which(XXX == 1)])
                min_norm[j-1] <- min(min_norm[j-1], X[which(XXX == 1)])

            } else { # if not cutted element
                csec_edge_pointsx[[j-1]] <- c(csec_edge_pointsx[[j-1]], numeric(0))
                csec_edge_pointsy[[j-1]] <- c(csec_edge_pointsy[[j-1]], numeric(0))
            
            }

        } # for j in length(map_geogr_lim_lon)

        # update progress bar
        setTxtProgressBar(pb, i)

    } # for i elem_area_inds_n

    # close progress bar
    close(pb)

    #stop("asd")

    ## through out csection edges without any interpolated points (e.g. because edge is too small)
    if (any(sapply(csec_edge_pointsx, function(x) length(x) == 0))) {
        message("throw out...")
        inds <- which(sapply(csec_edge_pointsx, function(x) length(x) == 0))
        
        # e.g. 5th edge has 0 points: throw out 6th csection vertex
        if (any(inds == csec_n_vertices)) { # last vertex of csection
            map_geogr_lim_lon <- map_geogr_lim_lon[-c(1, inds[-which(inds == csec_n_vertices)])]
            map_geogr_lim_lat <- map_geogr_lim_lat[-c(1, inds[-which(inds == csec_n_vertices)])]
        } else {
            map_geogr_lim_lon <- map_geogr_lim_lon[-(inds + 1)]
            map_geogr_lim_lat <- map_geogr_lim_lat[-(inds + 1)]
        }

        csec_n_vertices <- length(map_geogr_lim_lon)
        csec_n_edges <- csec_n_vertices - 1
        csec_norm_edge <- csec_norm_edge[-inds]
        csec_e_vec_edge <- csec_e_vec_edge[,-inds]
        csec_edge_pointsx <- csec_edge_pointsx[-inds]
        csec_edge_pointsy <- csec_edge_pointsy[-inds]
        max_norm <- max_norm[-inds]
        min_norm <- min_norm[-inds]
    }

    ## analyse cross-section: which trianglse are crossed by section...
    csec_crossed_nodes_n <- length(which(csec_crossed_nodes == 1))
    csec_crossed_tri_n <- length(which(csec_crossed_tri == 1))

    csec_BND_point1 <- array(NA, c(2, csec_n_edges))
    csec_BND_point2 <- csec_BND_point1
    csec_support_points <- vector("list", l=csec_n_edges)
    csec_interp_points <- csec_support_points
    csec_DeltaR <- csec_support_points
    csec_interp_index <- csec_support_points 
    csec_interp_index2 <- csec_support_points
    csec_dist_list <- csec_support_points
    csec_interp_fac1 <- csec_support_points
    csec_interp_fac2 <- csec_support_points
    csec_interp_d <- csec_support_points
    
    wanted_step <- 0.05 #0.01 #0.05 #degree
    #fprintf(' --> Reduced Crossect. CS(x)\n');
    #fprintf(' --> wanted dR = %1.4f deg\n',wanted_step);
    #_______________________________________________________________________________
    
    for (i in 2:csec_n_vertices) {

        #%___CALC: SUPPORTING POINTS |---o---|---o---|...____________________

        # CROSS-SECTION CONSISTS OF TWO POINTS
        if (csec_n_vertices == 2) {
            csec_BND_point1[,i-1] <- c(map_geogr_lim_lon[i-1] + min_norm[i-1]*csec_e_vec_edge[1,i-1],
                                       map_geogr_lim_lat[i-1] + min_norm[i-1]*csec_e_vec_edge[2,i-1])
            csec_BND_point2[,i-1] <- c(map_geogr_lim_lon[i-1] + max_norm[i-1]*csec_e_vec_edge[1,i-1],
                                       map_geogr_lim_lat[i-1] + max_norm[i-1]*csec_e_vec_edge[2,i-1])

            auxnorm_edge <- sqrt((csec_BND_point2[1,i-1] - csec_BND_point1[1,i-1])^2 +
                                 (csec_BND_point2[2,i-1] - csec_BND_point1[2,i-1])^2)

            # MAKE SUPPORTING POINTS for INTERP POINTS  
            nr_support_points <- ceiling((max_norm[i-1] - min_norm[i-1])/wanted_step)
            step <- (max_norm[i-1] - min_norm[i-1])/nr_support_points
            
            csec_support_points[[i-1]] = rbind(csec_BND_point1[1,i-1] + 
                                               seq(0, max_norm[i-1] - min_norm[i-1], b=step)*
                                               csec_e_vec_edge[1,i-1],
                                               csec_BND_point1[2,i-1] + 
                                               seq(0, max_norm[i-1] - min_norm[i-1], b=step)*
                                               csec_e_vec_edge[2,i-1])
                                              
        # CROSS-SECTION CONSISTS OF MORE THAN TWO POINTS
        } else if (csec_n_edges != 2) { 
            
            if (i == 2) {
                csec_BND_point1[,i-1] <- c(map_geogr_lim_lon[i-1] + min_norm[i-1]*csec_e_vec_edge[1,i-1],
                                           map_geogr_lim_lat[i-1] + min_norm[i-1]*csec_e_vec_edge[2,i-1])
                csec_BND_point2[,i-1] <- c(map_geogr_lim_lon[i], map_geogr_lim_lat[i])

            } else if (i == length(map_geogr_lim_lon)) {
                csec_BND_point1[,i-1] <- c(map_geogr_lim_lon[i-1], map_geogr_lim_lat[i-1])
                csec_BND_point2[,i-1] <- c(map_geogr_lim_lon[i-1] + max_norm[i-1]*csec_e_vec_edge[1,i-1],
                                           map_geogr_lim_lat[i-1] + max_norm[i-1]*csec_e_vec_edge[2,i-1])

            } else {
                csec_BND_point1[,i-1] <- c(map_geogr_lim_lon[i-1], map_geogr_lim_lat[i-1])
                csec_BND_point2[,i-1] <- c(map_geogr_lim_lon[i], map_geogr_lim_lat[i])
            }

            auxnorm_edge <- sqrt((csec_BND_point2[1,i-1] - csec_BND_point1[1,i-1])^2 +
                                 (csec_BND_point2[2,i-1] - csec_BND_point1[2,i-1])^2)

            # MAKE SUPPORTING POINTS for INTERP POINTS
            nr_support_points <- ceiling((auxnorm_edge)/wanted_step)
            step <- (sqrt((csec_BND_point2[1,i-1] - csec_BND_point1[1,i-1])^2 +
                          (csec_BND_point2[2,i-1] - csec_BND_point1[2,i-1])^2))/nr_support_points

            csec_support_points[[i-1]] <- rbind(csec_BND_point1[1,i-1] + 
                                                seq(0, auxnorm_edge, b=step)*
                                                csec_e_vec_edge[1,i-1],
                                                csec_BND_point1[2,i-1] + 
                                                seq(0, auxnorm_edge, b=step)*
                                                csec_e_vec_edge[2,i-1])
        
        } # if csec_n_vertices == 2 or not

        #___CALC: DeltaR in m______________________________________________
        aux_x_spoints <- Rearth * cos(csec_support_points[[i-1]][1,]*pi/180)*
                                  cos(csec_support_points[[i-1]][2,]*pi/180)
        aux_y_spoints <- Rearth * sin(csec_support_points[[i-1]][1,]*pi/180)*
                                  cos(csec_support_points[[i-1]][2,]*pi/180)
        aux_z_spoints <- Rearth * sin(csec_support_points[[i-1]][2,]*pi/180)
        csec_DeltaR[[i-1]] = Rearth*acos((aux_x_spoints[2:length(aux_x_spoints)]*
                                          aux_x_spoints[1:(length(aux_x_spoints)-1)] +
                                          aux_y_spoints[2:length(aux_y_spoints)]*
                                          aux_y_spoints[1:(length(aux_y_spoints)-1)] +
                                          aux_z_spoints[2:length(aux_z_spoints)]*
                                          aux_z_spoints[1:(length(aux_z_spoints)-1)])/Rearth^2)
        #message(str(csec_DeltaR))

        #___CALC: INTERPOLATION POINTS !!!!_________________________________
        csec_interp_points[[i-1]] <- rbind(csec_support_points[[i-1]][1,(1:length(csec_support_points[[i-1]][1,])-1)] + 
                                           (csec_support_points[[i-1]][1,(2:length(csec_support_points[[i-1]][1,]))] -
                                            csec_support_points[[i-1]][1,(1:length(csec_support_points[[i-1]][1,])-1)])/2,
                                           csec_support_points[[i-1]][2,(1:length(csec_support_points[[i-1]][2,])-1)] + 
                                           (csec_support_points[[i-1]][2,(2:length(csec_support_points[[i-1]][2,]))] -
                                            csec_support_points[[i-1]][2,(1:length(csec_support_points[[i-1]][2,])-1)])/2)

        #___________________________________________________________________
        # SORTIERT cross_sec.cross_edge_pointsx/y der Reihe nach vom einem der
        # Startpunkte aus
        magnitude <- sqrt((csec_edge_pointsx[[i-1]] - map_geogr_lim_lon[i-1])^2 +
                          (csec_edge_pointsy[[i-1]] - map_geogr_lim_lat[i-1])^2)

        IND <- sort(magnitude, index.return=T)$ix
        # neu sortiert
        csec_edge_pointsx[[i-1]] <- csec_edge_pointsx[[i-1]][IND]
        csec_edge_pointsy[[i-1]] <- csec_edge_pointsy[[i-1]][IND]

        #___CALC: TRIANGULAR INDEX OF INTERPOLATION POINT !!!!______________
        #       --> FOR HORIZONTAL INTERPOALTION 
        csec_interp_index[[i-1]] <- rep(0, t=length(csec_interp_points[[i-1]][1,])) # interp_tri_i_ip
        csec_interp_index2[[i-1]] <- rep(0, t=length(csec_edge_pointsx[[i-1]])) # interp_tri_i_ep

        XC <- xc[,which(csec_crossed_tri == 1)]
        YC <- yc[,which(csec_crossed_tri == 1)]
        
        if (F) {
            message("here1")
            message("XC")
            message(XC[,1:4])
            message("YC")
            message(YC[,1:4])
        }

        for (j in 1:dim(XC)[2]) { # for every element on csection
            A <- rbind(XC[,j], YC[,j], c(1, 1, 1))
            P <- rbind(csec_interp_points[[i-1]],
                       rep(1, t=length(csec_interp_points[[i-1]][1,])))
            X <- solve(A, P)

            inds <- which(X[1,] >= 0 & X[1,] <= 1 & X[2,] >= 0 & X[2,] <= 1 & X[3,] >= 0 & X[3,] <= 1)
            csec_interp_index[[i-1]][inds] <- which(csec_crossed_tri == 1)[j] #elem_area_inds[j] 

            P <- rbind(csec_edge_pointsx[[i-1]],
                       csec_edge_pointsy[[i-1]],
                       rep(1, t=length(csec_edge_pointsx[[i-1]])))
            X <- solve(A, P)

            inds <- which(X[1,] >= 0 & X[1,] <= 1 & X[2,] >= 0 & X[2,] <= 1 & X[3,] >= 0 & X[3,] <=1)
            csec_interp_index2[[i-1]][inds] <- which(csec_crossed_tri == 1)[j] #elem_area_inds[j]

        } # for j dim(XC)[2] every element in csection
      
        # CHECK IF ANY INTERPOLATION  POINT IS NOT IN A TRIANGLE
        inds <- which(csec_interp_index[[i-1]] == 0)
        if (length(inds) > 0) {
            message(paste0(length(inds), " interpolation points not in a triangle!"))
            csec_interp_index[[i-1]] <- csec_interp_index[[i-1]][-inds]
            csec_interp_points[[i-1]] <- csec_interp_points[[i-1]][,-inds]
            message(str(csec_DeltaR))
            csec_DeltaR[[i-1]] <- csec_DeltaR[[i-1]][-inds]
            message(str(csec_DeltaR))
        }

        inds <- which(csec_interp_index2[[i-1]] == 0)
        if (length(inds) > 0) {
            csec_interp_index2[[i-1]] <- csec_interp_index2[[i-1]][-inds]
            csec_edge_pointsx[[i-1]] <- csec_edge_pointsx[[i-1]][-inds]
            csec_edge_pointsy[[i-1]] <- csec_edge_pointsy[[i-1]][-inds]
        }
  
        ## Flip x axis if both lon and lat are decreasing
        ## this is not a nice workaround
        if (all(diff(csec_interp_points[[i-1]][1,]) < 0) &&
            all(diff(csec_interp_points[[i-1]][2,]) < 0)) {
            message(paste0(indent, "Flip x-axis of cross section because both lon and lat are decreasing ..."))
            csec_interp_index[[i-1]] <- rev(csec_interp_index[[i-1]])
            csec_interp_points[[i-1]] <- csec_interp_points[[i-1]][,length(csec_interp_points[[i-1]][1,]):1]
            csec_DeltaR[[i-1]] <- rev(csec_DeltaR[[i-1]])
            message(str(csec_DeltaR))
            csec_interp_index2[[i-1]] <- rev(csec_interp_index2[[i-1]])
            csec_edge_pointsx[[i-1]] <- rev(csec_edge_pointsx[[i-1]])
            csec_edge_pointsy[[i-1]] <- rev(csec_edge_pointsy[[i-1]])
        }

        # calculate factors for horizontal interpolation of the interpolation 
        # points obj(csi).interp_point{segi}
        XC <- xc[,csec_interp_index[[i-1]]]
        YC <- yc[,csec_interp_index[[i-1]]]

        if (F) {
            message("here2")
            message("XC")
            message(XC[,1:4])
            message("YC")
            message(YC[,1:4])
        }

        X  <- csec_interp_points[[i-1]][1,] - XC[1,]
        Y  <- csec_interp_points[[i-1]][2,] - YC[1,]
        X1 <- XC[2,] - XC[1,]
        Y1 <- YC[2,] - YC[1,]
        X2 <- XC[3,] - XC[1,]
        Y2 <- YC[3,] - YC[1,]
        D  <- Y2*X1 - X2*Y1
        csec_interp_fac1[[i-1]] <- Y2*X - X2*Y
        csec_interp_fac2[[i-1]] <- Y1*X - X1*Y
        csec_interp_d[[i-1]] <- D

        if (F) {
            aux_dr1 <- c(0, csec_DeltaR[[i-1]][1:(length(csec_DeltaR[[i-1]])-1)]/2)
            aux_dr2 <- c(0, aux_dr1[1:(length(aux_dr1)-1)])
            csec_dist_list[[i-1]] <- cumsum(aux_dr1 + aux_dr2) # [m]
        } else if (T) {
            csec_dist_tot <- dim(csec_support_points[[i-1]])[2] * unique(csec_DeltaR[[i-1]])[1]
            csec_dist_list[[i-1]] <- seq(from=0, to=csec_dist_tot, l=dim(csec_interp_points[[i-1]])[2])
        }

    } # for i csec_n_vertices 

    #stop("asd")

    csec_node_inds <- which(csec_crossed_nodes == 1) # in global 2d node space
    csec_elem_inds <- which(csec_crossed_tri == 1) # in global 2d element space
    csec_crossed_nodes_n <- length(csec_node_inds)
    csec_crossed_tri_n <- length(csec_elem_inds)

    csec_inds <- rep(0, t=nod2d_n)
    csec_inds[csec_node_inds] <- 1:csec_crossed_nodes_n

    nod2d_n_save <- nod2d_n
    nod2d_n <- length(csec_node_inds)
    aux3d_save <- aux3d
    aux3d <- aux3d[,csec_node_inds]
    pos_nodes <- rep(NA, t=nod2d_n_save)
    pos_nodes[csec_node_inds] <- 1:length(csec_node_inds)
    xcsur_save <- xcsur
    xcsur <- xcsur[csec_node_inds]
    ycsur_save <- ycsur
    ycsur <- ycsur[csec_node_inds]

    #___CALC: HORIZONTAL INTERPOLATION COEFFITIENT______________________
    # align all edges of cross section along 1 dimension
    XC <- xc[,csec_elem_inds]
    YC <- yc[,csec_elem_inds]

    csec_interp_points_vec <- matrix(unlist(csec_interp_points), nrow=2)
    csec_interp_index_vec <- unlist(csec_interp_index) 
    csec_DeltaR_vec <- unlist(csec_DeltaR)
    csec_dist_vec <- unlist(csec_dist_list)
    csec_interp_fac1_vec <- unlist(csec_interp_fac1)
    csec_interp_fac2_vec <- unlist(csec_interp_fac2)
    csec_interp_d_vec <- unlist(csec_interp_d)

    tmpx <- c()
    tmpy <- tmpx
    for (i in 1:csec_n_edges) {
        tmpx <- c(tmpx, rep(csec_n_vec_edge[1,i], e=length(csec_interp_index[[i]])))
        tmpy <- c(tmpy, rep(csec_n_vec_edge[2,i], e=length(csec_interp_index[[i]])))
    }
    csec_n_vec_edge_vec <- rbind(tmpx, tmpy)
    rm(tmpx, tmpy)

    #stop("asd")

    # check csection
    if (plot_csec) {
        if (!dir.exists(paste0(plotpath, "/", varname))) {
            dir.create(paste0(plotpath, "/", varname), recursive=T, showWarnings=F)
        }
        plotname <- paste0(plotpath, "/", varname, "/",
                           runid, "_", setting, 
                           "_csec_location_", area,
                           ".", plot_file)
        if (!file.exists(plotname)) {

            if (verbose > 1) {
                message(paste0(indent, "Open cross section '", area, "' location plot device ..."))
            }
            if (plot_file == "png") {
                ng(plotname,
                    width=plot_size[1], height=plot_size[2],
                    res=dpi, bg=bg_col,
                    family=font_family)
            } else if (plot_file == "pdf") {
                pdf(plotname, width=plot_size[1]/dpi,
                    height=plot_size[2]/dpi, family=font_family)#,pointsize=14)
            }
            par(oma=c(2,2,2,2))
            if (projection == "rectangular") {
                par(mar=c(5,4,4,6)) # leave some space on right border for legend
            } else {
                par(mar=c(5,3,4,6))
            }

            bathyf <- paste0("/work/ba0941/a270073/post/", 
                             substr(runid, 1, 5), 
                             "/", setting, "/regular_grid/ltm/global/bathy/",
                             substr(runid, 1, 5), 
                             "_", setting, "_bathy_ltm_global_rectangular_regular_dx0.100_dy0.100.nc")
            if (file.exists(bathyf)) {
                bathync <- nc_open(bathyf)
                bathy <- ncvar_get(bathync, "bathy")
                bathy[bathy < 0] <- 0
                levels <- pretty(range(bathy, na.rm=T), n=10)
                cols <- colorRampPalette(c("#c5ebdc", "#a0dfda", "#7fd5e8", "#5ecbe6", "#49add9",
                                           "#3d82c9", "#3259af", "#26468b", "#212f5c", "#141c34"))(length(levels) - 1)
                xlim <- vector("list", l=length(csec_interp_points))
                ylim <- xlim
                for (i in 1:length(csec_interp_points)) {
                    xlim[[i]] <- c(mean(csec_interp_points[[i]][1,]) - 
                                   1*ceiling(max(abs(c(diff(range(csec_interp_points[[i]][1,])),
                                                       diff(range(csec_interp_points[[i]][2,])))))),
                                   mean(csec_interp_points[[i]][1,]) + 
                                   1*ceiling(max(abs(c(diff(range(csec_interp_points[[i]][1,])),
                                                       diff(range(csec_interp_points[[i]][2,])))))))
                    ylim[[i]] <- c(mean(csec_interp_points[[i]][2,]) - 
                                   1*ceiling(max(abs(c(diff(range(csec_interp_points[[i]][1,])),
                                                       diff(range(csec_interp_points[[i]][2,])))))),
                                   mean(csec_interp_points[[i]][2,]) + 
                                   1*ceiling(max(abs(c(diff(range(csec_interp_points[[i]][1,])),
                                                       diff(range(csec_interp_points[[i]][2,])))))))
                }
                xlim <- range(xlim)
                ylim <- range(ylim)
                image.plot(bathync$dim$nxi$vals, bathync$dim$nyi$vals, bathy,
                           xlim=xlim, ylim=ylim,
                           breaks=levels, col=cols)
                for (i in 1:length(csec_interp_points)) {

                    # draw interp points of csection
                    points(csec_interp_points[[i]][1,], csec_interp_points[[i]][2,], col=i, cex=0.5)
                    
                    if (T) {
                        arrows(x0=mean(csec_interp_points[[i]][1,]),
                               y0=mean(csec_interp_points[[i]][2,]),
                               x1=mean(csec_interp_points[[i]][1,]) + 
                                  csec_n_vec_edge[1,i]/(sqrt(csec_n_vec_edge[1,i]^2 + csec_n_vec_edge[2,i]^2))*2,
                               y1=mean(csec_interp_points[[i]][2,]) + 
                                  csec_n_vec_edge[2,i]/(sqrt(csec_n_vec_edge[1,i]^2 + csec_n_vec_edge[2,i]^2))*2,
                               col=i, lwd=3)
                    } else {
                        quiver(x=mean(csec_interp_points[[i]][1,]),
                               y=mean(csec_interp_points[[i]][2,]),
                               u=csec_n_vec_edge[1,i],
                               v=csec_n_vec_edge[2,i],
                               col=i, lwd=3)
                    }
                }
                if (F) {
                    contour(bathync$dim$nxi$vals, bathync$dim$nyi$vals, bathy, add=T,
                            levels=c(500, 600, 700))
                }
                box()

            ## bathy does not exist
            } else {

                project <- F
                if (project) {
                    stop("not")
                    xy0 = mapproject(x=mean(csec_interp_points[[1]][1,]),
                                     y=mean(csec_interp_points[[1]][2,]))
                    xy1 = mapproject(x=mean(csec_interp_points[[1]][1,]) + csec_n_vec_edge[1,1]/(sqrt(csec_n_vec_edge[1,1]^2 + csec_n_vec_edge[2,1]^2))*10,
                                     y=mean(csec_interp_points[[1]][2,]) + csec_n_vec_edge[2,1]/(sqrt(csec_n_vec_edge[1,1]^2 + csec_n_vec_edge[2,1]^2))*10)
                    arrows(x0=xy0$x, y0=xy0$y,
                           x1=xy1$x, y1=xy1$y,
                           col=2, lwd=3)
                } else {
                    success <- load_package("maps")
                    if (!success) stop(helppage)
                    map("world", 
                        xlim=c(range(csec_interp_points[[1]][1,])[1] - abs(0.5*range(csec_interp_points[[1]][1,])[1]),
                               range(csec_interp_points[[1]][1,])[2] + abs(0.5*range(csec_interp_points[[1]][1,])[2])),
                        ylim=c(range(csec_interp_points[[1]][2,])[1] - abs(0.5*range(csec_interp_points[[1]][2,])[1]),
                               range(csec_interp_points[[1]][2,])[2] + abs(0.5*range(csec_interp_points[[1]][2,])[2])))
                    points(csec_interp_points[[1]][1,], csec_interp_points[[1]][2,]) 
                    arrows(x0=mean(csec_interp_points[[1]][1,]),
                           y0=mean(csec_interp_points[[1]][2,]),
                           x1=mean(csec_interp_points[[1]][1,]) + csec_n_vec_edge[1,1]/(sqrt(csec_n_vec_edge[1,1]^2 + csec_n_vec_edge[2,1]^2))*10,
                           y1=mean(csec_interp_points[[1]][2,]) + csec_n_vec_edge[2,1]/(sqrt(csec_n_vec_edge[1,1]^2 + csec_n_vec_edge[2,1]^2))*10,
                           col=2, lwd=3)
                }
            }
       
            if (verbose > 1) {
                message(paste0(indent, "Save ", plotname, " ..."))
            }
            dev.off()

        } else {
            if (verbose > 1) {
                message(paste0(indent, "Cross section location plot: ", plotname))
            }
        } # if csec location plot already exists
 
    } # if plot_csec

    if (verbose > 0) {
        message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
                     " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
        message("==============================================")
    }

} # transient_out && out_mode == "csec"


## 3) for moc
if (out_mode == "moc_mean" || out_mode == "moc_depth") {
    if (verbose > 0) {
        message(paste0("3) Find coordinates of area '", area, "' for MOC calculation: ..."))
    }

    # regular lats for binning
    moc_reg_lat_global <- seq(-90+regular_dy_moc/2, 90-regular_dy_moc/2, b=regular_dy_moc)
    
    # mask
    # moc_mask must be 0 (outside) or 1 (inside). NOT T or F
    if (exists("moc_mask_file") && file.exists(moc_mask_file)) {
        message(paste0(indent, "use maskfile ", moc_mask_file, " ..."))
        moc_mask_inds <- fread(moc_mask_file)$V1
        moc_mask_inds <- moc_mask_inds[2:moc_mask_inds[1]] # remove first line of dimas mask file
        moc_mask <- rep(0, t=nod2d_n)
        moc_mask[moc_mask_inds] <- 1
    } else {
        if (area == "moc_global") { # global moc
            moc_mask <- rep(1, t=nod2d_n)
        } else {
            # make new moc mask 
            moc_mask_file <- paste0(meshpath, "/moc_mask_", area, "_", meshid, ".dat")
            message(indent, "MOC mask area is not 'moc_global' AND no moc mask file is given for meshid = '", meshid, 
                    "'. Determine moc mask now in an interactive session.")
            if (!interactive()) stop("run this script in an interactive session.")
            
            # select MOC area in interactive session
            if (!capabilities("X11")) {
                stop("capabilities(\"X11\") = ", capabilities("X11"), " --> cannot open plot device")
            }
            X11(width=14, height=14) # inch
            plot(xcsur, ycsur, xlab="Longitude [°]", ylab="Latitude [°]",
                 pch=".", xaxt="n", yaxt="n")
            axis(1, at=pretty(xcsur, n=20))
            axis(2, at=pretty(ycsur, n=20), las=2)
            abline(v=pretty(xcsur, n=20), lwd=0.5)
            abline(h=pretty(ycsur, n=20), lwd=0.5)
            title(paste0("Select MOC mask area '", area, "' for mesh '", meshid, "'"))
            source(paste0(subroutinepath, "/functions/mylocator.r"))
            message(indent, "The polygon is closed automatically in the end. If you want to change the coordinates for MOC area '", 
                    area, "', remove ", moc_mask_file, " and rerun the script.")
            moc_mask_coords <- mylocator()
            
            # check user coords
            if (length(moc_mask_coords) == 0 || 
                length(moc_mask_coords) > 0 && length(moc_mask_coords$x) < 4) {
                stop("Rerun script and provide at least 3 surface points for MOC mask for MOC area '", area, "'.")
            }
            moc_mask_coords <- cbind(moc_mask_coords$x, moc_mask_coords$y)
            
            # close polygon
            moc_mask_coords <- rbind(moc_mask_coords, moc_mask_coords[1,])

            # find surface nodes within polygon
            success <- load_package("sp")
            if (!success) stop(helppage)
            moc_mask_inds <- sp::point.in.polygon(point.x=xcsur, point.y=ycsur,
                                                  pol.x=moc_mask_coords[,1], pol.y=moc_mask_coords[,2])
            moc_mask_inds <- which(moc_mask_inds == 1 | moc_mask_inds == 2 | moc_mask_inds == 3) # outside or on edge or on vertex
            moc_mask <- rep(0, t=nod2d_n)
            moc_mask[moc_mask_inds] <- 1
            
            # show surface nodes if there is an open plot
            if (length(dev.list()) > 0) {
                points(xcsur[moc_mask == 1], ycsur[moc_mask == 1], col="blue", pch=".")
            }

            # save mask file in Dimas format
            if (verbose > 0) message("Save surface indices of MOC area '", area, "' for mesh '", 
                                     meshid, "' to ", moc_mask_file, " ...")
            write(c(length(moc_mask_inds), moc_mask_inds), file=moc_mask_file, ncolumns=1)
        
            # close selection plot
            dev.off()

        } # if mask file not given AND not global moc is wanted

    } # if exists("moc_mask_file")

    # save moc area if wanted
    if (plot_moc_mask) {
        message(indent, "Plot ", moc_mask_plotname, " ...")
        png(moc_mask_plotname, width=2666, height=2666, res=dpi)
        plot(xcsur, ycsur, xlab="Longitude [°]", ylab="Latitude [°]", 
             xaxt="n", yaxt="n", pch=".")
        axis(1, at=pretty(xcsur, n=20))
        axis(2, at=pretty(ycsur, n=20), las=2)
        abline(v=pretty(xcsur, n=20), lwd=0.5)
        abline(h=pretty(ycsur, n=20), lwd=0.5)
        points(xcsur[moc_mask == 1], ycsur[moc_mask == 1], col="blue", pch=".")
        title(paste0("MOC mask area '", area, "' for mesh '", meshid, "'"))
        dev.off()
    } # if plot_moc_mask

    map_geogr_lim_lon <- range(xcsur[which(moc_mask == 1)])
    map_geogr_lim_lat <- range(ycsur[which(moc_mask == 1)])
    poly_geogr_lim_lon <- map_geogr_lim_lon
    poly_geogr_lim_lat <- map_geogr_lim_lat
    if (verbose > 1) {
        message(indent, "   min/max longitude = ", min(map_geogr_lim_lon), "/", max(map_geogr_lim_lon))
        message(indent, "   min/max latitude = ", min(map_geogr_lim_lat), "/", max(map_geogr_lim_lat))
    }

    if (verbose > 0) {
        message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
                     " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
        message("==============================================")
    }

}


## 4) Vertical Interpolation
if (nfiles > 0) {

    if (dim_tag == "2D") {
        
        if (verbose > 0) {
            message(paste0("4) Vertical interpolation not necessary for ", 
                        dim_tag, " variable ", varname, " ..."))
        }
        
        depths_plot <- ""
        depths_fname <- ""
        interpolate_depths <- 0
        ndepths <- 1
    
    } else {

        ## In case of restart, reload vertical interpolation coefficients...
        if (restart 
            && dim_tag == "3D" 
            && dim_tag == dim_old 
            && depths == depth_old) {
            
            if (verbose > 0) {
                message(paste0("4) This is a restart run. Reload global vertical interpolation coefficients for ", 
                             depths[1], "-", depths[2], " m depths ..."))
            }

            indlower <- indlower_save
            indupper <- indupper_save
            indsurf <- indsurf_save
            indcoef <- indcoef_save
            indlevel <- indlevel_save
            deltaz <- deltaz_save

        ## Do vertical interpolation ...
        } else if ((restart 
                    && dim_tag == "3D" 
                    && (dim_tag != dim_old || depths != depth_old)) 
                   || (!restart && dim_tag == "3D")) {

            if (verbose > 0) {
                message(paste0("4) Calculate coefficients for vertical interpolation in ", 
                             ifelse(length(depths) == 2, paste0(depths[1], "-", depths[2]), depths[1]), 
                             " m depths globally ..."))
            }

            ## Here, only the case (cycl && rotate_mesh) is implemented.
            ## In the Matlab code, there is also the (cycl && !rotate_mesh) case.
            de <- aux3d # de contains depths (ndepths_all x nnodes_2d)
            rnd <- which(aux3d > -999, arr.ind=T)
            de[rnd] <- -nod3d_z[aux3d[rnd]]
            #fesom_depths <- unique(de[-which(de == -999)])
            # note: it is possible that length(fesom_depths) != aux3d_n.
            #       this might be the case because no nodes exist in the 
            #       bottom layer of aux3d (--> all(aux3d[aux3d_n,] == -999) is true)

            if (F) {
                png("depths.png", width=1000, height=1600, res=300)
                plot(1:length(fesom_depths), fesom_depths, t="o", col="blue", 
                     ylim=rev(range(fesom_depths)), 
                     xlab="nlevels", ylab="km", yaxt="n")
                axis(2, at=pretty(fesom_depths, n=10), labels=pretty(fesom_depths, n=10)/1000, las=2)
                legend("bottomleft", c("mesh '", meshid, "'"), 
                       col=c("blue"), lty=1, pch=1, lwd=1,
                       x.intersp=0.2, bty="n")
                box()
                dev.off()
            }

            # Find FESOM depths within user 'depths'
            if (length(depths) == 1) {

                if (depths != "max" && depths != "bottom") {
                    if (depths < min(fesom_depths) || depths > max(fesom_depths)) {
                        stop(paste0("Choose a depth level between ",
                                    min(fesom_depths), "-", max(fesom_depths), " m."))
                    }
                    depths_plot <- depths
                    interpolate_depths <- depths

                } else if (depths == "max") {
                    depths_plot <- fesom_depths[length(fesom_depths)]
                    interpolate_depths <- depths_plot
                
                } else if (depths == "bottom") {
                    depths_plot <- "bottom"
                    interpolate_depths <- NA

                } else {
                    stop(paste0("'depths='", depths, " not defined ..."))
                }

            } else if (length(depths) == 2) {

                if (depths[1] < min(fesom_depths) || depths[1] > max(fesom_depths)) {
                    stop(paste0("Choose depth levels between ",
                                min(fesom_depths), "-", max(fesom_depths), " m."))
                }

                if (!any(depths[2] == c("max", "MLD"))) {

                    if (depths[2] < min(fesom_depths) || depths[2] > max(fesom_depths)) {
                        stop(paste0("Choose depth levels between ",
                                    min(fesom_depths), "-", max(fesom_depths), " m."))
                    }

                    depths_plot <- paste0(depths[1], "-", depths[2])

                    # find model depths within user depths
                    interpolate_depths <- fesom_depths[fesom_depths >= depths[1] & 
                                                       fesom_depths <= depths[2]]
                    
                    # both user depths are in the same model depth interval (e.g. 2-8 m)
                    if (length(interpolate_depths) == 0) {
                        interpolate_depths <- depths

                    } else {
                       
                        # upper user depth is on in different fesom_depths intervals
                        if (interpolate_depths[1] != depths[1]) {
                            interpolate_depths <- c(depths[1], interpolate_depths)
                        }
                     
                        # lower depths are in different fesom_depths intervals
                        if (interpolate_depths[length(interpolate_depths)] != depths[2]) {
                            interpolate_depths <- c(interpolate_depths, depths[2])
                        }

                    }
                    
                } else if (any(depths[2] == c("max", "MLD"))) {
                    depths_plot <- paste0(depths[1], "-", fesom_depths[length(fesom_depths)])
                    interpolate_depths <- fesom_depths[fesom_depths >= as.numeric(depths[1]) &
                                                       fesom_depths <= fesom_depths[length(fesom_depths)]]
                   
                    # upper user depth is on in different fesom_depths intervals
                    if (interpolate_depths[1] != as.numeric(depths[1])) {
                        interpolate_depths <- c(as.numeric(depths[1]), interpolate_depths)
                    }
                   
                } # if depths[2] == "max" or not

            } # length(depths) == 1 or == 2

            ndepths <- length(interpolate_depths)
            if (length(depths) == 1 && depths == "bottom") {
                depths_fname <- paste0("_", depths_plot)
            } else {
                depths_fname <- paste0("_", depths_plot, "m")
            }

            if (integrate_depth) {
                if (length(depths) == 2 && depths[2] == "MLD") {
                    depths_plot <- paste0("int", depths[1], "-MLD")
                    depths_fname <- paste0("_", depths_plot)
                } else {
                    depths_plot <- paste0("int", depths_plot)
                    depths_fname <- paste0("_", depths_plot, "m")
                }
            }

            ## this deltaz is used for vertical average
            if (ndepths >= 3) {
                ndepths <- length(interpolate_depths)
                deltaz <- rep(0, t=ndepths - 1)
                deltaz[1] <- (interpolate_depths[1] - interpolate_depths[2])/2
                deltaz[ndepths] <- (interpolate_depths[ndepths - 1]- interpolate_depths[ndepths])/2
                for (n in 2:(ndepths - 1)) {
                    deltaz[n] <- (interpolate_depths[n-1] - interpolate_depths[n])/2 + (interpolate_depths[n] - interpolate_depths[n+1])/2
                }
                deltaz <- deltaz*-1 # to get positive dz values

                # test
                if (F) {
                    deltaz2 <- rep(0, t=ndepths-1)
                    middepths <- deltaz2
                    for (i in 1:(ndepths-1)) {
                        deltaz2[i] <- -interpolate_depths[i+1] - -interpolate_depths[i]
                        middepths[i] <- (-interpolate_depths[i+1] + -interpolate_depths[i])/2
                    }
                }

            } else if (ndepths == 2) {
                deltaz <- c(1, 1)
            } else if (ndepths == 1) {
                deltaz <- 1
            }

            ## find vertical interplation coefficients if necessary
            if (length(depths) == 1 && depths == "bottom") {
                
                indbottom <- array(NA, c(ndepths, nod2d_n))
                indsurf <- indbottom
                for (k in 1:nod2d_n) {
                    ## index of last 3d node above -999:
                    indbottom[1,k] <- aux3d[which(aux3d[,k] == -999)[1] - 1,k]
                    indsurf[1,k] <- aux3d[1,k] # = 1:nod2d_n if no subset mesh is used
                }

            } else {
                
                indupper <- array(NA, c(ndepths, nod2d_n)) 
                indcoef <- indupper
                indlower <- indupper
                indsurf <- indupper
                indlevel <- indupper

                for (l in 1:ndepths) {
                    
                    z <- interpolate_depths[l]

                    if (verbose > 1) {
                        if (ndepths > 1) {
                            if (l == 1) {
                                message(indent, "   ", appendLF=F)
                            } 
                            message(z, "m ", appendLF=F)
                            if (l == ndepths) {
                                message("") 
                            }
                        } else if (ndepths == 1) {
                            message(indent, "   ", z, "m")
                        }
                    }

                    # user depth is on model level
                    if (any(fesom_depths == z)) {
                        de_ind <- which(fesom_depths == z)
                        no_boundary_inds <- which(aux3d[de_ind,] != -999)
                        indsurf[l,no_boundary_inds] <- aux3d[1,no_boundary_inds]
                        indlevel[l,no_boundary_inds] <- aux3d[de_ind,no_boundary_inds]

                    # user level is not on model levels
                    } else {

                        ## rearrange fesom to vertical depth levels:
                        # 1 .
                        # 2 'u'pper layer
                        # 3 .
                        # 4 .
                        # 5 x <- user depth
                        # 6 .
                        # 7 'l'ower layer
                        # 8 .
                        # --> x = u + c(l - u)

                        # Upper and lower nodes:
                        for (k in 1:nod2d_n) {
                            rnd <- which(de[,k] >= z)
                            if (length(rnd) >= 1) {
                                indlower[l,k] <- aux3d[rnd[1],k]
                                if ((rnd[1] - 1) < 1) {
                                    indlower[l,k] <- NA
                                    next
                                }
                                indupper[l,k] <- aux3d[(rnd[1]-1),k]
                                indsurf[l,k] <- aux3d[1,k]
                                indcoef[l,k] <- (interpolate_depths[l] - de[(rnd[1]-1),k]) /
                                                (de[rnd[1],k] - de[(rnd[1]-1),k])
                            }
                        }

                    } # if any(fesom_depths == z) or not
                
                } # for l ndepths

                if (F) {
                    indlower_save <- indlower
                    indupper_save <- indupper
                    indsurf_save <- indsurf
                    indcoef_save <- indcoef
                    indlevel_save <- indlevel
                    deltaz_save <- deltaz
                }

                rm(rnd)
                rm(de)

            } # if depths == "bottom" or not

            #stop("asd")

        } # if restart or not for vertical interpolation coefficients 

    } # if dim_tag == 2D or varname == MOCw

} else if (nfiles == 0) {
     if (verbose > 0) {
        message(paste0("4) Vertical Interpolation not necessary for '", varname, "' (nfiles=0) ..."))
    }
    depths_plot <- ""
    depths_fname <- ""
}

if (verbose > 0) {
    message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
                 " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
    message("==============================================")
}


## 5) Read data through years and months
if (nfiles == 0) { # read data which are constant in time
    if (verbose > 0) {
        message(paste0("5) Reading FESOM files not necessary for '", 
                     varname, "' (nfiles=0) ..."))
        message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
                     " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
        message("==============================================")
    }

    total_rec <- 0 

} else if (nfiles > 0) { # read data which are not constant in time
    if (verbose > 0) {
        message(paste0("5) Read variable", ifelse(nfiles > 1, "s", ""), " '", 
                       paste0(varname_fesom, collapse="','"), "' (=varname_fesom)"))
        message(paste0("      for output variable '", varname, "' (=varname) aka '", longname, "' (=longname)"))
        if (transient_out || regular_transient_out) {
            message(paste0("      and save '", out_mode, "' (=out_mode) data"))
        }
        message(paste0("      in region '", area, "' (=area) ..."))
    }

    ## check whether R package "ncdf.tools" is loaded and can be used
    ## note: if the data is so large that rec_tag is set to FALSE,
    ##       than using ncdf.tools package doesnt work since it loads the
    ##       complete file.
    ## e.g. 5 GB fesom file (dim = 3668773 x 365)
    ##      all_recs = T : 2.17 mins (36 GB virtual mem)
    ##      all_recs = F : 3.5 mins
    if (rec_tag && !fuser_tag) {
        if (((length(recs) == npy && all(recs == 1:npy)) || # read all records of a file at once
             (leap_tag && length(recs_leap) == npy_leap && all(recs_leap == 1:npy_leap)))) {
            success <- load_package("ncdf.tools")
            if (!success) {
                ncdf.tools_tag <- F
                message(paste0(indent, "note: ncdf.tools::readNcdf() may be faster than ncdf4::ncvar_get() if\n",
                               indent, "      the whole netcdf file (i.e. all entries of all\n",
                               indent, "      dimensions) needs to get loaded ..."))
            } else if (success) {
                ncdf.tools_tag <- T
            }

        } else {
            ncdf.tools_tag <- F
        } # if ncdf.tools make sense

    # dont need to read the whole file
    } else {
        ncdf.tools_tag <- F
    }

    ## Data read loop preparation
    # Note: built-in functions such as mean() or apply()
    # are too slow for calculating the average because
    # the FESOM data might be too big.
    # Thats why a timestep (e.g. month, day, hour, etc.)
    # and year loop is chosen to read in the data and
    # average "by hand" (-> mean(x) = 1/n sum(x)).
    total_rec <- 0 # counter over all time steps
    year_cnt <- 0
    if (!fuser_tag) {
        years_loop <- unique(yearvec) # default annual files
    } else if (fuser_tag) {
        years_loop <- length(fnames_user)
    }
    fnames <- rep("NA", nfiles) # used for every year
    
    ## Initialize Mean Arrays (dimension: npoints x 1) for storing data output
    if (F) {
        host <- Sys.info()[4] #system("hostname", intern=T)
        pid <- Sys.getpid()
        byte <- 8 # 1 character = 4 byte, 1 numeric = 8 byte # 1 complex = 16 byte
        narrays <- ifelse(transient_out || regular_transient_out, 4, 3)
        if (F && (verbose == 2 || verbose == 3)) {
            message(paste0(indent, "Allocate data matrices ..."))
            source(paste0("~/scripts/r/functions/get_memory_of_workspace_gb.r"))
            source(paste0("~/scripts/r/functions/get_memory_of_session_gb.r"))
            source(paste0("~/scripts/r/functions/get_free_memory_of_hostname_gb.r"))
            message(paste0(indent, "   workspace memory = ", round(get_memory_of_workspace_gb(), 3), " Gb ..."))
            message(paste0(indent, "   session (pid=", pid, ") memory = ", round(get_memory_of_session_gb(pid), 3), " Gb ..."))
            message(paste0(indent, "   free memory (host=", host, ") = ", round(get_free_memory_of_hostname_gb(), 3), " Gb ..."))
        }
    }

    ## Check available memory on server
    if (rec_tag) {

        if (F) {
            if (dim_tag == "2D") {
                approx_size_gb <- narrays*nfiles*nod2d_n*nrecspf*byte/1024^3
            
            } else if (dim_tag == "3D") {
                
                if (!integrate_depth) {
                    approx_size_gb <- max(narrays*nfiles*nod3d_n*nrecspf*byte/1024^3,
                                          narrays*nfiles*nod2d_n*ndepths*nrecspf*byte/1024^3)
                } else if (integrate_depth) {
                    approx_size_gb <- max(narrays*nfiles*nod3d_n*nrecspf*byte/1024^3, # raw_data, data_node_ltm, data_node, data
                                          narrays*aux3d_n*nfiles*nod2d_n*nrecspf*byte/1024^3)
               }
            
            }
            
            if (F && (verbose == 2 || verbose == 3)) {
                message(paste0(indent, "   requiered workspace size = ", narrays, " x ",  
                             round(approx_size_gb/narrays, 2), " = ", 
                             round(approx_size_gb, 2), " Gb ..."))
            }
        }

        #if (workspace + approx_size_gb >= memfree_gb) {
        #    if (verbose == 2 || verbose == 3) {
        #        message(paste0(indent, "Cannot use several records per file, data is too big. Loop through records instead..."))
        #    }
        #    nrecloop <- nrecspf
        #} else {
            nrecloop <- 1
        #}
    } # if rec_tag

    ## Year loop
    for (year in years_loop) { 
        
        indent <- "   "
        if (verbose > 1) message(paste0(indent, "Year ", year))
        year_cnt <- year_cnt + 1

        ## construct fesom filenames
        for (file in 1:nfiles) { # nfiles=2 if e.g. temp and salt are needed per time (e.g. year)
           
            if (fuser_tag) {
                ## check nc fast: library(raster); b <- brick("akhil.nc", var="hur")
                #fnames[file] <- paste0(datainpath, fnames_user[year_cnt])
                fnames[file] <- fnames_user[year_cnt]

            } else if (!fuser_tag) {

                if (fesom_version == "1.4") {

                    if (cpl_tag) { # coupled fesom-echam
                        
                        if ((runid == "bold" && setting == "cpl_output_01") || # tido
                            (runid == "awicm-CMIP6_hu" && setting == "pi")) { # hu
                            fnames[file] <- paste0(datainpath, "/fesom.", year, ".",
                                                   typesuffix[file], diagsuffix[file],
                                                   snapshotsuffix[file], "nc")
                       
                        } else if (setting == "htrans02") {
                            fnames[file] <- paste0(datainpath, "/", varname_fesom[file], "_fesom_", 
                                                   year, "0101_monmean.nc")

                        # coupled default
                        } else { 
                            fnames[file] <- paste0(datainpath, "/", varname_fesom[file], "_fesom_", 
                                                   year, "0101.nc")
                            # newer esm version: exp name in fesom file names
                            if (!file.exists(fnames[file])) {
                                fnames[file] <- paste0(datainpath, "/", setting, "_", varname_fesom[file], 
                                                       "_fesom_", year, "0101.nc")
                            }
                        }

                    }  else if (!cpl_tag) { # ocean-only
                   
                        if (any(runid == c("demo1"))) {
                            fnames[file] <- paste0(datainpath, "/demo.", year, ".", 
                                                   typesuffix[file], diagsuffix[file], 
                                                   snapshotsuffix[file], "nc")

                        # xuezhu wangs 1st CORE2 spinups (w/out GIS)
                        } else if (runid == "CORE2" || runid == "CORE2_ctl") {
                            fnames[file] <- paste0(datainpath, "/CORE2.", year, ".", 
                                                   typesuffix[file], diagsuffix[file], 
                                                   snapshotsuffix[file], "nc")
                        
                        # claudis
                        } else if (any(runid == c("Arc22_daily", "Arc22_sub_daily",
                                                      "Arc22_sub", "Arc22_sub_small"))) {
                            if (runid == "Arc22_sub_daily" && dim_tag == "2D") {
                                fnames[file] <- paste0(datainpath, "/Arc22.", year, ".",
                                                       typesuffix[file], diagsuffix[file],
                                                       snapshotsuffix[file], "sub.n2d.nc")
                            } else {
                                fnames[file] <- paste0(datainpath, "/Arc22.", year, ".",
                                                       typesuffix[file], diagsuffix[file],
                                                       snapshotsuffix[file], "sub.nc")
                            }

                        # default 
                        } else {
                            fnames[file] <- paste0(datainpath, "/", runid, ".", year, ".", 
                                                   typesuffix[file], diagsuffix[file], 
                                                   snapshotsuffix[file], "nc")
                        }

                    } # if cpl_tag

                } else if (fesom_version == "2.0") {
                    
                    if (cpl_tag) { # coupled fesom-echam

                    }  else if (!cpl_tag) { # ocean-only
                        # ssh.MPmes.1955.nc
                        fnames[file] <- paste0(datainpath, "/", varname_fesom[file], 
                                               ".", runid, ".", year, ".nc") 
                    
                    } # if cpl_tag

                } else {
                    stop("fesom_version = ", fesom_version, " not defined.")
                
                } # which fesom_version

            } # if fuser_tag

            if (!file.exists(fnames[file])) {
                stop(paste0("file ", fnames[file], " does not exist."))
            }

        } # for file nfiles

        ## Open annual ncdf files
        # Do not load the same file more than once
        ncids <- vector("list", l=length(unique(fnames)))
        for (file in 1:length(unique(fnames))) {
            if (ncdf.tools_tag == F) {
                ncids[[file]] <- ncdf4::nc_open(unique(fnames)[file])
            } else if (ncdf.tools_tag == T) {
                ncids[[file]] <- unique(fnames)[file] 
            }
        }

        # try to obtain the time dimension of user file
        if (fuser_tag) {
            time_dim_names <- c("time", "T", "month", "months", "year", "years")
            dim_names <- names(ncids[[1]]$dim)
            # success
            if (any(match(time_dim_names, dim_names))) {
                time_dim <- which(!is.na(match(time_dim_names, dim_names)))
                time <- ncids[[1]]$dim[[time_dim]]$vals[recs]
                timeunit <- ncids[[1]]$dim[[time_dim]]$units
            # no success
            } else {
                message(indent, "your provided nc file\n", ncids[[1]]$filename, "\n",
                        " does not have a dimension named '", paste0(time_dim_names, collapse="','"), "'.")
                non_nod_dims <- which(regexpr("nod", dim_names) == -1)
                if (length(non_nod_dims) > 0) {
                    non_nod_dims <- non_nod_dims[1]
                    message(indent, "Use dimension no. ", non_nod_dims, ": \"", dim_names[non_nod_dims], "\" for time ...")
                    time <- ncids[[1]]$dim[[non_nod_dims]]$vals[recs] 
                    timeunit <- ncids[[1]]$dim[[non_nod_dims]]$units
                    if (timeunit == "") timeunit <- "timeunit"
                } else { # only spatial dim
                    time <- 1
                    timeunit <- "timeunit"
                }
            } # if time dim found in fuser
            ntime <- length(time)
            timechar <- timedate <- timesec <- time
            timesec_unit <- timeunit
            timespan <- time[1]
            if (ntime > 1) timespan <- paste0(timespan, "-", time[ntime])
        
        } else { # fuser_tag = F
            # time axis for nc output
            # there are two POSIXt types, POSIXct and POSIXlt
            # "ct" stands for calendar time
            # "lt" for local time, keeps the date as a list of time attributes (such as "hour" and "mon")
            # --> unclass(as.POSIXlt(x))$mon
            origin <- "1970-01-01" # needs to be this, dont know why
            if (output == "monthly") { # yyyymm01 -> day must be provided, also if format="%Y%m", dont know why 
                timedate <- as.POSIXlt(paste0(timechar, 01), format="%Y%m%d", origin=origin, tz="UTC")
            } else if (output == "daily") {
                timedata <- as.POSIXlt(timechar, format="%Y%j", origin=origin, tz="UTC") # yyyydoy
            } else if (outpt == "5day") {
                stop("how?")
            }
            timesec <- as.numeric(timedate) # date --> seconds
            timesec_unit <- paste0("seconds since ", origin) 
        
        }  # if fuser_tag

        if (length(ncids) == 1) {
            var_nc_inds <- rep(1, t=length(varname_fesom))
        
        } else if (length(ncids) > 1) {

            if (ncdf.tools_tag == F) {
                var_nc_names <- lapply(ncids, function(x) names(x$var))
                var_nc_inds <- sapply(var_nc_names, function(x) varname_fesom %in% x)
                var_nc_inds <- apply(var_nc_inds, 1, which)
                if (typeof(var_nc_inds) == "list") {
                    stop(paste0("could not find '", 
                                paste(varname_fesom[which(sapply(var_nc_inds, length) == 0)], collapse="','"),
                                "' in ", runid, " fesom data ..."))
                }
            } else if (ncdf.tools_tag == T) {

                var_nc_inds <- rep(NA, t=length(varname_fesom))
                for (nci in 1:length(ncids)) {
                    varsin <- ncdf.tools::infoNcdfVars(ncids[[nci]])
                    for (vari in 1:length(varname_fesom)) {
                        if (any(varsin[["name"]] == varname_fesom[vari])) {
                            var_nc_inds[vari] <- nci
                        }
                    }
                }
                if (any(is.na(var_nc_inds))) {
                    stop(paste0("could not find varname_fesom='",
                                paste(varname_fesom[which(is.na(var_nc_inds))], collapse="','"),
                                "' in ", runid, " fesom data ..."))
                }
            }
        }

        # load mld for integrate over MLD depth
        if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {

            if (cpl_tag) {
                mld_varname <- "mlotst"
            } else if (!cpl_tag) {
                mld_varname <- "mixlay"
            }
 
            if (ncdf.tools_tag == F) {

                if (cpl_tag) {

                    # if oce.diag already loaded
                    if (any(regexpr(mld_varname, sapply(ncids, "[[", "filename")) != -1)) {
                        #mld_fname <- fnames[which(regexpr("oce.diag", fnames) != -1)]
                        #mld_nc_ind <- which(regexpr("oce.diag", fnames) != -1)
                        mld_nc_ind <- which(regexpr(mld_varname, sapply(ncids, "[[", "filename")) != -1)
                    
                    # or oce.diag not loaded yet
                    } else { 
                        mld_fname <- paste0(datainpath, "/", mld_varname, "_fesom_",
                                            year, "0101.nc")
                        # newer esm version: exp name in fesom file names
                        if (!file.exists(mld_fname)) {
                            mld_fname <- paste0(datainpath, "/", setting, "_", mld_varname, "_fesom_",
                                                year, "0101.nc")
                        }
                        if (!file.exists(mld_fname)) {
                            message("Error: cannot find MLD file:\n")
                            stop(mld_fname)
                        }
                        ncids[[length(ncids) + 1]] <- ncdf4::nc_open(mld_fname)
                        mld_nc_ind <- length(ncids)
                    }

                } else if (!cpl_tag) {

                    # if oce.diag already loaded
                    if (any(regexpr("oce.diag", sapply(ncids, "[[", "filename")) != -1)) {
                        #mld_fname <- fnames[which(regexpr("oce.diag", fnames) != -1)]
                        #mld_nc_ind <- which(regexpr("oce.diag", fnames) != -1)
                        mld_nc_ind <- which(regexpr("oce.diag", sapply(ncids, "[[", "filename")) != -1)
                    
                    # or oce.diag not loaded yet
                    } else { 
                        mld_fname <- paste0(substr(fnames[1], 1, 
                                                   regexpr(year,fnames[1]) + 4), 
                                            "oce.diag.nc")
                        if (!file.exists(mld_fname)) {
                            message("Error: cannot find MLD file:\n")
                            stop(mld_fname)
                        }
                        ncids[[length(ncids) + 1]] <- ncdf4::nc_open(mld_fname)
                        mld_nc_ind <- length(ncids)
                    }
                
                } # if cpl_tag
                        
            } else if (ncdf.tools_tag == T) {

                if (cpl_tag) {
                    # if oce.diag already loaded
                    if (any(sapply(ncids, function(x) regexpr(mld_varname, x)) != -1)) {
                        mld_nc_ind <- which(sapply(ncids, function(x) regexpr(mld_varname, x)) != -1)

                    # or oce.diag not loaded yet
                    } else {
                        mld_fname <- paste0(datainpath, "/", mld_varname, "_fesom_",
                                            year, "0101.nc")
                        # newer esm version: exp name in fesom file names
                        if (!file.exists(mld_fname)) {
                            mld_fname <- paste0(datainpath, "/", setting, "_", mld_varname, "_fesom_",
                                                year, "0101.nc")
                        }
                        if (!file.exists(mld_fname)) {
                            message("Error: cannot find MLD file:\n")
                            stop(mld_fname)
                        }
                        ncids[[length(ncids) + 1]] <- mld_fname
                        mld_nc_ind <- length(ncids)
                    } # if mixlay file was already loaded due to varname_fesom

                } else if (!cpl_tag) {

                    # if oce.diag already loaded
                    if (any(sapply(ncids, function(x) regexpr("oce.diag", x)) != -1)) {
                        mld_nc_ind <- which(sapply(ncids, function(x) regexpr("oce.diag", x)) != -1)

                    # or oce.diag not loaded yet
                    } else {
                        mld_fname <- paste0(substr(fnames[1], 1,
                                                   regexpr(year,fnames[1]) + 4),
                                            "oce.diag.nc")
                        if (!file.exists(mld_fname)) {
                            message("Error: cannot find MLD file:\n")
                            stop(mld_fname)
                        }
                        ncids[[length(ncids) + 1]] <- mld_fname
                        mld_nc_ind <- length(ncids)

                    } # if oce.diag was already loaded due to varname_fesom

                } # if cpl_tag
            } # if ncdf.tools_tag
        } # if load mld for integrate over MLD depth
       
        ## Timestep loop
        if (rec_tag) {
            recsi <- 1 # load all recs of a year in one array
        } else {
            if (fuser_tag) {
                recsi <- recs   
            } else if (!fuser_tag) {
                recsi <- recvec[which(yearvec == year)]
            }
        }

        ## start/count indices
        if (rec_tag) { 
            istart <- c(1, recs[1]) # (nodes, time)
            if (dim_tag == "2D") {
                icount <- c(nod2d_n, nrecspf) # (nodes, time)
                if (leap_tag) {
                    icount_leap <- c(nod2d_n, nrecspf_leap)
                }
            } else if (dim_tag == "3D") {
                icount <- c(nod3d_n, nrecspf)
                if (leap_tag) {
                    icount_leap <- c(nod3d_n, nrecspf_leap)
                }
            }
        } else if (!rec_tag) {
            if (dim_tag == "2D") {
                icount <- c(nod2d_n, 1) # # set istart in time loop
            } else if (dim_tag == "3D") {
                icount <- c(nod3d_n, 1) # set istart in time loop
            }
        } # if rec_tag
        icounts <- array(NA, c(nfiles, 2))
        
        #recsloop_systime <- system.time({
        for (rec in 1:length(recsi)) {
            indent <- "      "
            if (rec_tag) {
                if (verbose > 1) {
                    if (leap_tag && is.leap(year)) {
                        message(paste0(indent, "Recs ", paste0(recs_leap, collapse=","),
                                     " (total ", total_rec+nrecspf_leap, "/", ntime, ")"))
                    } else {
                        message(paste0(indent, "Recs ", paste0(recs, collapse=","), 
                                     " (total ", total_rec+nrecspf, "/", ntime, ")"))
                    }
                }

            } else if (!rec_tag) {
                istart <- c(1, recsi[rec]) # c(node, time)
                if (verbose > 1) {
                    if (fuser_tag) {
                        message(paste0(indent, "Rec ", istart[2], " of ", length(recs), 
                                     " in file (total ", total_rec+1, ")"))
                    } else if (!fuser_tag) {
                        message(paste0(indent, "Rec ", istart[2], 
                                     " (total ", total_rec+1, "/", ntime, ")"))
                    }
                }
            } # if rec_tag or not

            ## benchmark
            if (F) { # load all times vs single times
          
                # 5 GB file nnod x ntime = 3668773 x 365
                f <- "/work/ab0246/a270073/awicm-CMIP6/PI-CTRL/outdata/fesom/so_fesom_28150101.nc"
                library(ncdf4)
                library(ncdf.tools)
                
                time_all_nc4 <- system.time({
                    nc <- nc_open(f)
                    test <- ncdf4::ncvar_get(nc, "so")
                }) # 32.821 sec
                
                time_all_nctools <- system.time({
                    test <- ncdf.tools::readNcdf(f, "so")
                }) # 32.094
                
                time_single_nc4 <- system.time({
                    ncid <- nc_open(f)
                    nnod <- ncid$dim$nodes_3d$len
                    ntime <- ncid$dim$time$len
                    for (i in 1:ntime) {
                        test <- ncvar_get(nc, "so", start=c(1, i), count=c(nnod, 1))
                    }
                }) # 32.418 
            
            } # benchmark


            ## Transient time variable
            if (rec_tag) {
                if (leap_tag && is.leap(year)) {
                    timei <- timechar[(total_rec+1):(total_rec+nrecspf_leap)]
                } else {
                    timei <- timechar[(total_rec+1):(total_rec+nrecspf)]
                }
            } else if (!rec_tag) {
                if (fuser_tag) {
                    timei <- time[recsi[rec]]
                } else if (!fuser_tag) {
                    timei <- recsi + (year_cnt - 1)*length(recsi)
                    timei <- timechar[timei[rec]]
                }
            }
            if (!fuser_tag) {
                if (snapshot) {
                    timei <- paste0(timei, "_snapshot")
                } else {
                    timei <- paste0(timei, "_mean")
                }
            }

            # need to take care of leap year: clean time of last year
            # because it could include day 366 of leap year
            #dimnames(data_node)[[4]] <- rep(NA, t=dim(data_node)[4])
            #dimnames(data_node)[[4]][1:length(timei)] <- timei

            # declare matrix necessary for every year
            declare_time <- system.time({
                if (rec_tag) {
                    if (dim_tag == "2D") {
                        if (leap_tag && is.leap(year)) {
                            data_node <- array(0,
                                               dim=c(nfiles, nod2d_n, 1, nrecspf_leap),
                                               dimnames=list(var=varname_fesom,
                                                             node=NULL,
                                                             depth=depths_plot,
                                                             rec=timei))
                            icount_leap <- c(nod2d_n, nrecspf_leap)
                        } else {
                            data_node <- array(0,
                                               dim=c(nfiles, nod2d_n, 1, nrecspf),
                                               dimnames=list(var=varname_fesom,
                                                             node=NULL,
                                                             depth=depths_plot,
                                                             rec=timei))
                        }

                    } else if (dim_tag == "3D") {
                        if (leap_tag && is.leap(year)) {
                            data_node <- array(0,
                                               dim=c(nfiles, nod3d_n, 1, nrecspf_leap),
                                               dimnames=list(var=varname_fesom,
                                                             node=NULL,
                                                             depth=depths_plot,
                                                             rec=timei))
                            icount_leap <- c(nod3d_n, nrecspf_leap)
                        } else {
                            data_node <- array(0,
                                               dim=c(nfiles, nod3d_n, 1, nrecspf),
                                               dimnames=list(var=varname_fesom,
                                                             node=NULL,
                                                             depth=depths_plot,
                                                             rec=timei))
                        }
                    } # if dim_tag == "2D" or "3D"

                } else if (!rec_tag) {
                    if (dim_tag == "2D") {
                        data_node <- array(0,
                                           dim=c(nfiles, nod2d_n, 1, 1),
                                           dimnames=list(var=varname_fesom,
                                                         node=NULL,
                                                         depth=depths_plot,
                                                         rec=1))
                    } else if (dim_tag == "3D") {
                        data_node <- array(0,
                                           dim=c(nfiles, nod3d_n, 1, 1),
                                           dimnames=list(var=varname_fesom,
                                                         node=NULL,
                                                         depth=depths_plot,
                                                         rec=1))
                    }
                } # if rec_tag or not

                if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {

                    if (rec_tag) {
                        if (leap_tag && is.leap(year)) {
                            mld_node <- array(0,
                                               dim=c(1, nod2d_n, 1, nrecspf_leap),
                                               dimnames=list(var="mld",
                                                             node=NULL,
                                                             depth=NULL,
                                                             rec=timei))
                        } else {
                            mld_node <- array(0,
                                               dim=c(1, nod2d_n, 1, nrecspf),
                                               dimnames=list(var="mld",
                                                             node=NULL,
                                                             depth=NULL,
                                                             rec=timei))
                        }
                    } else if (!rec_tag) {
                        mld_node <- array(0,
                                           dim=c(1, nod2d_n, 1, 1),
                                           dimnames=list(var="mld",
                                                         node=NULL,
                                                         depth=NULL,
                                                         rec=1))
                    } # if rec_tag or not

                } # if mld

            }) # declare_time

            indent <- "         "
            ## Read raw FESOM output
            for (file in 1:nfiles) {
                
                # find available variables in user provided file
                if (fuser_tag) {
                    if (ncdf.tools_tag == F) {
                        varname_fuser <- names(ncids[[var_nc_inds[file]]]$var)
                        if (length(varname_fuser) == 0) {
                            stop("Your file\n", "   ", ncids[[var_nc_inds[file]]]$filename, "\n",
                                 "does not contain a single variable")
                        }
                        
                        # check if one of the variables in the user file equals varname_fesom
                        if (any(varname_fuser %in% varname_fesom)) { # use the varname options of the runscript
                            varname_fesom <- varname_fuser[which(varname_fuser %in% varname_fesom)]
                        } else { # use defaults options from namelist.var.r
                            varname_fesom <- varname_fuser
                        }

                    } else { # if ncdf.tools_tag == T
                        stop("asdadasdasdas")
                    } # if ncdf.tools_tag
                }

                if (verbose > 1) {
                    message(paste0(indent, "Get '", varname_fesom[file],
                                 "' from ", fnames[file]))
                }
                ## note: using start and count in ncvar_get() is ~4 times faster for not too big files
                ##       compared to reading the whole nc file
                ##       39 GB file: loop (n=15):   11.375 sec
                ##                   all:           42.036 sec
                ##      458 GB file: loop (n=188): 159.694 sec
                ##                   all:          155.094 sec
                
                ## workaround if both 2d and 3d data will be read
                if (ncdf.tools_tag == F) {
                    dimcheck <- eval(parse(text=paste0("ncids[[var_nc_inds[file]]]$var$", 
                                                       varname_fesom[file], "$size")))
                
                } else if (ncdf.tools_tag == T) {
                    dimsin <- ncdf.tools::infoNcdfDims(ncids[[var_nc_inds[file]]])
                    varsin <- ncdf.tools::infoNcdfVars(ncids[[var_nc_inds[file]]])
                    varsin <- unlist(varsin[which(varsin[,2] == varname_fesom[file]),])
                    if (length(varsin) == 0) {
                        stop("Cannot find needed variable '", varname_fesom[file], "'.")
                    }
                    if (F) { # old
                        dimcheck <- c(dimsin["length"][which(dimsin["id"] == as.numeric(varsin["dim.id.1"])),], # time dim
                                      dimsin["length"][which(dimsin["id"] == as.numeric(varsin["dim.id.2"])),]) # node dim
                    } else {
                        dimcheck <- c(dimsin$length[which(dimsin$id == as.numeric(varsin["dim.id.1"]))], # time dim
                                      dimsin$length[which(dimsin$id == as.numeric(varsin["dim.id.2"]))]) # node dim
                    }
                }

                # dimcheck = NULL --> varname_fesom not included in nc file!
                if (is.null(dimcheck)) {
                    stop("varname_fesom[", file, "] = '", varname_fesom[file], "' not included in file\n",
                         ncids[[var_nc_inds[file]]]$filename)
                }
                
                # order correct dims: 1:node, 2:time
                if (exists("nod2d_n") && exists("nod3d_n")) {
                    nodedim <- which(dimcheck == nod2d_n | dimcheck == nod3d_n)
                } else if (exists("nod2d_n") && !exists("nod3d_n")) {
                    nodedim <- which(dimcheck == nod2d_n)
                } else if (!exists("nod2d_n") && exists("nod3d_n")) {
                    nodedim <- which(dimcheck == nod3d_n)
                }
                if (length(nodedim) == 0) {
                    stop("The nc dimensions of file\n", "   ", ncids[[var_nc_inds[file]]]$filename, "\n",
                         "do not equal\n", 
                         ifelse(exists("nod2d_n"), paste0("nod2d_n = ", nod2d_n, "\n"), ""),
                         ifelse(exists("nod3d_n"), paste0("nod3d_n = ", nod3d_n, "\n"), ""))
                }
                dimcheck <- c(dimcheck[nodedim], dimcheck[-nodedim]) 

                # check number of nodes of variable
                if (dimcheck[1] != icount[1]) { # icount[1] always equals icount_leap[1]
                    if (verbose > 2) {
                        message(paste0(indent, "   Note: ", varname_fesom[file], " data saved on ",
                                     dimcheck[1], " nodes instead of ", icount[1]))
                    }
                    icount[1] <- dimcheck[1]
                }

                if (verbose > 1) {
                    if (rec_tag && leap_tag && is.leap(year)) {
                        message(paste0(indent, 
                                     "   istart=c(node=", istart[1], ",rec=", istart[2], 
                                     "), icount_leap=c(nnode=", icount_leap[1], ",nrec=", icount_leap[2], ")"))
                    } else {
                        message(paste0(indent, 
                                     "   istart=c(node=", istart[1], ",rec=", istart[2], 
                                     "), icount=c(nnode=", icount[1], ",nrec=", icount[2], ")"))
                    }
                }

                if (ncdf.tools_tag == F) {
                    aa <- system.time({
                        if (rec_tag && leap_tag && is.leap(year)) {
                            raw_data <- ncdf4::ncvar_get(ncids[[var_nc_inds[file]]], varname_fesom[file],
                                                         istart, icount_leap)
                        } else {
                            raw_data <- ncdf4::ncvar_get(ncids[[var_nc_inds[file]]], varname_fesom[file], 
                                                         istart, icount)
                        }
                    })
                    units_raw <- ncatt_get(ncids[[var_nc_inds[file]]], varname_fesom[file], "units")$value
                    if (units_raw == 0) units_raw <- "not given by nc file"

                } else if (ncdf.tools_tag == T) {
                    bb <- system.time({
                        raw_data <- ncdf.tools::readNcdf(ncids[[var_nc_inds[file]]], 
                                                         var.name=varname_fesom[file])
                        # just in case some original data are in other dimensons than given by user
                        # for example if accidentally more time steps were appended to the file
                        if (leap_tag && is.leap(year)) {
                            raw_data <- raw_data[istart[1]:icount[1],istart[2]:icount_leap[2]]
                        } else {
                            raw_data <- raw_data[istart[1]:icount[1],istart[2]:icount[2]]
                        }
                    })
                    units_raw <- varsin["unit"]
                }
                # At this point, raw_data's time dimension may not equal the length of the
                # time dimension of the original fesom file, e.g. if a monthly fesom file 
                # (ntime = 12) was read in but JJA is wanted (recs = c(6,7,8)), then the
                # length of the raw_data's time dimension equals 3.
                
                # test
                if (ssh_aviso_correct && ssh_aviso_correct_fname != "") {
                    stop("continue")
                    ind <- which(ssh_aviso_correct_data$time == time[total_rec + 1])
                    if (length(ind) == 1) {
                        if (verbose > 1) {
                            message(paste0(indent, "Special! Do SSH aviso correction: add ", 
                                         ssh_aviso_correct_data$h_mean[ind], "m to fesom data ..."))
                        }
                        raw_data <- raw_data + ssh_aviso_correct_data$h_mean[ind]
                    }
                }   

                if (verbose > 2) {
                    message(paste0(indent, "   min/max 'raw_data' = ",
                                 paste0(range(raw_data), collapse="/"), 
                                 " ", units_raw))
                }

                ## Save data in array (vars,nodes,time,depths)
                # need to use 1:icount[1] for indexing since both 2D and 3D variables may be used
                if (rec_tag) {
                    if (leap_tag && is.leap(year)) {
                        data_node[file,1:icount[1],,1:nrecspf_leap] <- raw_data
                    } else {
                        data_node[file,1:icount[1],,1:nrecspf] <- raw_data
                    }
                } else {
                    data_node[file,1:icount[1],,] <- raw_data
                }

                ## save variable dimension info (2D or 3D)
                icounts[file,] <- icount

                ## read MLD for integrating only over MLD depths
                if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                                           
                    if (file == nfiles) { # read MLD only at the end
                        if (verbose > 1) {
                            message(paste0(indent, "Get '", mld_varname,
                                         "' from ", ifelse(ncdf.tools_tag,
                                                           ncids[[mld_nc_ind]],
                                                           ncids[[mld_nc_ind]]$filename)))
                            if (rec_tag && leap_tag && is.leap(year)) {
                                message(paste0(indent,
                                             "   istart=c(node=", istart[1], ",rec=", istart[2],
                                             "), icount_leap=c(nnode=", nod2d_n, ",nrec=", icount_leap[2], ")"))
                            } else {
                                message(paste0(indent,
                                             "   istart=c(node=", istart[1], ",rec=", istart[2],
                                             "), icount=c(nnode=", nod2d_n, ",nrec=", icount[2], ")"))
                            }
                        }

                        if (ncdf.tools_tag == F) {
                            if (rec_tag && leap_tag && is.leap(year)) {
                                mld_raw_data <- ncdf4::ncvar_get(ncids[[mld_nc_ind]], mld_varname,
                                                                 start=istart, count=c(nod2d_n, icount_leap[2]))
                            } else {
                                mld_raw_data <- ncdf4::ncvar_get(ncids[[mld_nc_ind]], mld_varname,
                                                                 start=istart, count=c(nod2d_n, icount[2]))
                            }
                            mld_units_raw <- ncatt_get(ncids[[mld_nc_ind]], mld_varname, "units")$value
                            if (mld_units_raw == 0) mld_units_raw <- "not given by nc file"

                        } else if (ncdf.tools_tag == T) {
                            mld_raw_data <- ncdf.tools::readNcdf(ncids[[mld_nc_ind]],
                                                                 var.name=mld_varname)
                            # just in case some original data are in other dimensons than given by user
                            # for example if accidentally more time steps were appended to the file
                            if (leap_tag && is.leap(year)) {
                                mld_raw_data <- mld_raw_data[istart[1]:nod2d_n,istart[2]:icount_leap[2]]
                            } else {
                                mld_raw_data <- mld_raw_data[istart[1]:nod2d_n,istart[2]:icount[2]]
                            }
                            mld_varsin <- ncdf.tools::infoNcdfVars(ncids[[mld_nc_ind]])
                            mld_varsin <- unlist(mld_varsin[which(mld_varsin[,2] == mld_varname),])
                            mld_units_raw <- mld_varsin["unit"]
                        }

                        if (verbose > 2) {
                            message(paste0(indent, "   min/max 'mld_raw_data' = ",
                                         paste0(round(range(mld_raw_data), 3), collapse="/"),
                                         " ", mld_units_raw))
                        }

                        ## Save data in array (vars,nodes,time,depths)
                        # need to use 1:icount[1] for indexing since both 2D and 3D variables may be used
                        if (rec_tag) {
                            if (leap_tag && is.leap(year)) {
                                mld_node[1,1:nod2d_n,1,1:nrecspf_leap] <- mld_raw_data
                            } else {
                                mld_node[1,1:nod2d_n,1,1:nrecspf] <- mld_raw_data
                            }
                        } else {
                            mld_node[1,1:nod2d_n,,] <- mld_raw_data
                        }

                    } # if file == nfiles
                } # if MLD needed

            } # for file nfiles per time step
            #stop("asd")
            rm(raw_data)
            if (rec_tag) rm(ncids)
            if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                rm(mld_raw_data)
            }
           
            ## Calculate and save transient data for each timestep if wanted
            if (any(transient_out, regular_transient_out, rms_out, sd_out)) {

                indent <- "         "
                if (verbose > 1) {
                    message(paste0(indent, "Calc and save transient ", 
                                   out_mode, " (='out_mode') of area ", area, " (='area') ..."))
                }

                ## Rotate vector components
                if (rotate_mesh && all(!!rotate_inds)) { 
                    # rotate some entries of 'varname_fesom' back to geographic coords
                    for (i in 1:(length(rotate_inds)/2)) {
                        inds <- rotate_inds[c((i-1)*2+1,(i-1)*2+2)]
                        if (verbose > 1) {
                            message(paste0(indent, "Rotate global ", 
                                         varname_fesom[inds[1]], " and ", 
                                         varname_fesom[inds[2]], 
                                         " back to geographic coordinates ... "))
                        }
                        rotated_coords <- vec_rotate_r2g(Ealpha, Ebeta, Egamma, nod_x, nod_y, 
                                                         data_node[inds[1],,,], 
                                                         data_node[inds[2],,,], 1)
                        data_node[inds[1],,,] <- rotated_coords$u
                        data_node[inds[2],,,] <- rotated_coords$v
                        rm(rotated_coords)
                    }
                }

                ## Preparations1 before calculations
                if (verbose > 1) {
                    message(paste0(indent, "Run ", subroutinepath, "/sub_prepare1.r ..."))
                }
                indent_save <- indent; indent <- paste0(indent_save, "   ")
                if (exists("tmp")) rm(tmp)
                sub_prepare1(data_node) # produces tmp
                if (exists("tmp")) {
                    data_node <- tmp
                    rm(tmp)
                }
                indent <- indent_save; rm(indent_save)

                ## At this point,
                ## dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf) if dim_tag == "2D"
                ## dim(data_node) = c(nvars,nod3d_n,ndepths=1,nrecspf) if dim_tag == "3D" 

                ## Save memory by depth averaging data if possible
                if (average_depth) {
                    
                    if (zave_method == 1) { # level-wise dz                        
                        if (verbose > 1) { # rearrange first
                            message(paste0(indent, "Bring data_node from (nod3d_n=", nod3d_n, 
                                         ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
                            if (verbose > 2) {
                                message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                            }
                        }
                        #indent_save <- indent; indent <- paste0(indent_save, "   ")
                        sub_n3_to_n2xde(data_node) # produces tmp
                        #indent <- indent_save; rm(indent_save)
                        data_vert <- tmp # dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
                        rm(tmp)

                    } else if (zave_method == 2) { # which zave_method
                        data_vert <- data_node # dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
                    }

                    if (verbose > 1 && ndepths > 1) {
                        message(paste0(indent, "Average over ", depths_plot, " m depths ..."))
                        if (verbose > 2) {
                            message(paste0(indent, "   run ", subroutinepath, "/sub_vertical_average.r ..."))
                        }
                    }
                    indent_save <- indent; indent <- paste0(indent_save, "   ")
                    sub_vertical_average(data_vert) # produces tmp
                    indent <- indent_save; rm(indent_save)
                    data_node <- tmp # overwrite old data_node
                    # if (zave_method == 1): dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf)
                    # if (zave_method == 2): dim(data_node) = c(nvars,nod[23]d_n=1,ndepths=1,nrecspf) # special!
                    rm(tmp)

                } # if average_depth

                ## Preparations2 before calculations e.g. calc rho, f, ... if needed
                if (verbose > 1) {
                    message(paste0(indent, "Run ", subroutinepath, "/sub_prepare2.r ..."))
                }
                indent_save <- indent; indent <- paste0(indent_save, "   ")
                sub_prepare2(data_node) # overwrites data_node with the result of sub_prepare2()
                indent <- indent_save; rm(indent_save)

                if (transient_out && any(out_mode == c("csec_mean", "csec_depth"))) {
                    if (verbose > 1) { 
                        message(paste0(indent, "For Cross section bring data_node from (nod3d_n=", nod3d_n,
                                     ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
                        if (verbose > 2) {
                            message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                        }
                    }
                    indent_save <- indent; indent <- paste0(indent_save, "   ")
                    sub_n3_to_n2xde(data_node) # produces tmp
                    indent <- indent_save; rm(indent_save)
                    data_global_vert <- tmp # dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
                    rm(tmp)
                }


                ## At this point,
                ## dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf) if 
                ##  (dim_tag == "2D") or (dim_tag == "3D" && average_depth && zave_method == 1)
                ## dim(data_node) = c(nvars,nod3d_n,ndepths=1,nrecspf) if 
                ##  (dim_tag == "3D" && !average_depth)
                ## dim(data_node) = c(nvars,nod_n=1,ndepths=1,nrecspf) if 
                ##  (dim_tag == "3D" && average_depth && zave_method == 2) # special!

                ## variable specific calculations
                if (verbose > 1) {
                    message(paste0(indent, "Run ", subroutinepath, "/sub_calc.r ..."))
                }
                indent_save <- indent; indent <- paste0(indent_save, "   ")
                sub_calc(data_node) # overwrites data_node with the result of sub_calc()
                indent <- indent_save; rm(indent_save)
                if (exists("tmp")) rm(tmp)

                ## set first dimension name to varname if length = 1
                if (is.null(dimnames(data_node)[[1]]) ||
                    (dim(data_node)[1] == 1 && dimnames(data_node)[[1]] != varname)) {
                    dimnames(data_node)[1] <- list(var=varname)
                }

                if (F) {
                    ws <- ls2()
                    message(ws[1:15,])
                    stop("asd")
                }

                ## Check data so far
                if (verbose > 2) {
                    for (i in 1:dim(data_node)[1]) {
                        message(paste0(indent, "   min/max data_node[", i, ":", 
                                       dimnames(data_node)[[1]][i], ",,,] = ",
                                       paste0(range(data_node[i,,,], na.rm=T), collapse="/")))
                    }
                }
                
                ## Change to proper units if wanted
                if (multfac_out != 1) {
                    for (i in 1:dim(data_node)[1]) {
                        if (verbose > 0) {
                            message(paste0(indent, "Multiply data_node[", i, ":",
                                         dimnames(data_node)[[1]][i], ",,,] by multfac_out=",
                                         multfac_out, " ..."))
                        }
                        data_node[i,,,] <- data_node[i,,,]*multfac_out
                        if (verbose > 0) {
                            message(paste0(indent, "   min/max data_node[", i, ":", 
                                           dimnames(data_node)[[1]][i], ",,,] = ",
                                           paste0(range(data_node[i,,,], na.rm=T), collapse="/"), 
                                           " ", units_out))
                        }
                    }
                }

                if (F) {
                    message(paste0(indent, "   workspace memory = ", round(get_memory_of_workspace_gb(), 3), " Gb ..."))
                    message(paste0(indent, "   session (pid=", pid, ") memory = ", round(get_memory_of_session_gb(pid), 3), " Gb ..."))
                    message(paste0(indent, "   free memory (host=", host, ") = ", round(get_free_memory_of_hostname_gb(), 3), " Gb ..."))

                    ws <- sort(sapply(ls(), function(x) object.size(get(x))), decreasing=T)/1024^2 # Mb
                    message(paste0(indent, "   10 biggest objects in draw_oce() [Mb]:"))
                    message(round(ws[1:10], 3))
                }

                #stop("asd")

                if (!any(out_mode == c("csec_mean", "csec_depth", "moc_mean", "moc_depth"))) {

                    ## integrate vertically
                    if (integrate_depth) {
                        if (verbose > 1) {
                            message(paste0(indent, "Integrate between ", depths_plot, " m ..."))
                            if (verbose > 2) {
                                message(paste0(indent, "Run ", subroutinepath, "/sub_vertical_integrate.r ..."))
                            }
                        }
                        sub_vertical_integral(data_node) # produces tmp
                        data_node <- tmp # dim(data_nod) = c(nvars,nod2d_n,ndepths=1,nrecspf)
                        rm(tmp)
                    } # if integrate_depth

                    ## at this point
                    ## dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf) if average_depth/integrate_depth
                    ## dim(data_node) = c(nvars,nod2d_n,ndepths,nrecspf) else

                    ## Declare matrix to save to netcdf
                    if (total_rec == 0) {

                        if (transient_out && out_mode != "area") {
                            if (any(out_mode == c("mean", "meanint", "sum", "max", "max3D", "min"))) {
                                data_funi <- array(NA, dim=c(dim(data_node)[1], ntime, 1)) # dim(data_funi) = c(nvars,ntime,ndepths=1)
                                dimnames(data_funi) <- c(dimnames(data_node)[1],
                                                         list(time=timechar,
                                                              depth=paste0(depths_plot, "m_", out_mode)))
        
                                if (out_mode == "max3D") {
                                    data_funi_depths <- data_funi
                                }
                            
                            } else if (any(out_mode == c("depth", "depthint", "depthmax"))) {
                                data_funi <- array(NA, 
                                                   dim=c(dim(data_node)[1], ntime, ndepths),
                                                   dimnames=c(dimnames(data_node)[1],
                                                              list(time=timechar,
                                                                   depth=interpolate_depths)))
                                # dim(data_funi) = c(nvars,ntime,ndepths)
                            
                            }
                            
                        } # if transient_out && out_mode != "area"

                        if (regular_transient_out && out_mode != "area") {
                            data_reg_funi <- array(NA, 
                                                   dim=c(dim(data_node)[1], ntime, 1),
                                                   dimnames=c(dimnames(data_node)[1],
                                                              list(time=timechar,
                                                                   depth=paste0(depths_plot, "_", out_mode))))
                        } # regular_transient_out && out_mode != "area"
                    
                    } # total_rec == 0
                   
                    ## Arrange datavector level-wise
                    if ((transient_out && out_mode == "area") ||
                        (transient_out && out_mode == "areadepth") ||
                        (regular_transient_out && out_mode == "area") ||
                        (regular_transient_out && out_mode == "areadepth")) {

                        if (length(poly_inds_geogr) == 0) {
                            stop("poly_inds_geogr: this should not happen")
                        }

                        if (!average_depth && !integrate_depth) {
                            if (dim_tag == "3D" && dim(data_node)[2] != nod2d_n && ndepths > 1) {
                                if (verbose > 1) { # rearrange first
                                    message(paste0(indent, "For regular interpolation bring data_node from (nod3d_n=", nod3d_n,
                                                 ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
                                    if (verbose > 2) {
                                        message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                                    }
                                }
                                indent_save <- indent; indent <- paste0(indent_save, "   ")
                                sub_n3_to_n2xde(data_node) # produces tmp
                                indent <- indent_save; rm(indent_save)
                                data_vert <- tmp # dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
                                rm(tmp)
                            } else { # already in level space
                                data_vert <- data_node
                            
                            } # dim_tag == "3D"
                        
                        } else {
                            data_vert <- data_node

                        } # if !average_depth && !integrate_depth

                        if (verbose > 1) {
                            message(paste0(indent, "For regular interpolation rearrange data_vert from (nod2d_n=", nod2d_n, 
                                         " x ndepths=", dim(data_vert)[3], ") to (3 x elem2d_n=", 
                                         elem2d_n, " x ndepths=", dim(data_vert)[3], ") ...")) 
                        }

                        # save regular interpolated data in area
                        datamat <- array(NA, 
                                         dim=c(dim(data_vert)[1],           # nvars
                                               3,                           # 3 nodes per element
                                               length(poly_inds_geogr),     # 2d-elems in area
                                               dim(data_vert)[3:4]),        # ndepths,nrecspf
                                         dimnames=c(dimnames(data_vert)[1],
                                                    list(node=1:3,
                                                         elem=NULL),
                                                    dimnames(data_vert)[3:4]))

                        if (regular_transient_out) {
                            if (verbose > 1) {
                                message(paste0(indent, "Interpolate on regular grid ('regular_dx'=",
                                             sprintf("%.3f", regular_dx), " deg,'regular_dy'=",
                                             sprintf("%.3f", regular_dy),
                                             " deg) and "))
                                message(paste0(indent, "   select regular data in '", area, "' area: ",
                                             round(range(map_geogr_lim_lon)[1], 2), " to ",
                                             round(range(map_geogr_lim_lon)[2], 2) , " deg longitude and ",
                                             round(range(map_geogr_lim_lat)[1], 2), " to ",
                                             round(range(map_geogr_lim_lat)[2], 2), " deg latitude ..."))
                            }
                        }

                        # create progress bar
                        if (dim(data_vert)[3] > 1) { # more than 1 depth
                            pb <- mytxtProgressBar(min=0, max=dim(data_vert)[3], style=pb_style,
                                                   char=pb_char, width=pb_width,
                                                   indent=paste0("     ", indent)) # 5 " " for default message()
                        }

                        for (di in 1:dim(data_vert)[3]) { # ndepths

                            # old:
                            #datamat[1,,,] <- data[,pos[elem2d[1,]],,]
                            #datamat[2,,,] <- data[,pos[elem2d[2,]],,]
                            #datamat[3,,,] <- data[,pos[elem2d[3,]],,]
                            data_elem <- array(data_vert[,pos[elem2d],di,], 
                                               dim=c(dim(data_vert)[1],    # nvars
                                                     3,                    # 3 nodes per element
                                                     elem2d_n,             # elem2d_n
                                                     1,                    # 1 depth
                                                     dim(data_vert)[4]),   # nrecspf
                                               dimnames=c(dimnames(data_vert)[1],
                                                          list(node=1:3, 
                                                               elem=NULL,
                                                               depth=dimnames(data_vert)[[3]][di]),
                                                          dimnames(data_vert)[4]))

                            ## Check data so far
                            if (verbose > 2) {
                                for (i in 1:dim(data_elem)[1]) {
                                    message(paste0(indent, "   min/max data_elem[", i, ":", 
                                                 dimnames(data_elem)[[1]][i], ",,,,] = ",
                                                 paste0(range(data_elem[i,,,,], na.rm=T), collapse="/")))
                                }
                            } 

                            ## Interpolation of transient data on regular grid
                            if (regular_transient_out) {
                                if (total_rec == 0 && di == 1) { # initialize matrices
                                    datamat_reg <- array(NA, 
                                                         dim=c(dim(data_vert)[1],              # nvars
                                                               length(xinds), length(yinds),   # x, y of _area_
                                                               dim(data_vert)[3:4]),           # ndepths, nrecspf
                                                         dimnames=c(dimnames(data_vert)[1],
                                                                    list(xi=round(xi, 2), 
                                                                         yi=round(yi, 2)),
                                                                    dimnames(data_vert)[3:4]))
                                } # if total_rec == 0
                               
                                ## interpolate on regular grid
                                # progress bar of case: only one depth but more than 1 vars
                                if (dim(data_vert)[3] == 1 && dim(data_elem)[1] > 1) {

                                }
                                for (i in 1:dim(data_elem)[1]) { # nvars
                                    if (dim(data_elem)[1] > 1 && verbose > 2) {
                                        message(paste0(indent, "   var = ", 
                                                     dimnames(data_elem)[[1]][i], " ..."))
                                    }
                                
                                    # progress bar of case: only one depth and only 1 var but several nrecspf
                                    if (dim(data_vert)[3] == 1 && dim(data_elem)[1] == 1 && dim(data_elem)[5] > 1) {

                                    }
                                    for (j in 1:dim(data_elem)[5]) { # nrecspf
                                        if (dim(data_elem)[5] > 1 && verbose > 2) {
                                            message(paste0(indent, "      time = ",
                                                         dimnames(data_elem)[[5]][j], " ..."))
                                        }
                                  
                                        # set values outside "area" to NA before interp
                                        # this is possible due to patricks nice interp method
                                        tmp <- drop(data_elem[i,,,,j])
                                        nainds <- rep(T, t=elem2d_n)
                                        if (!exists("poly_inds_geogr")) {
                                            stop("need to update the elem index within area. still depends on used projection")
                                        }
                                        nainds[poly_inds_geogr] <- F
                                        tmp[,nainds] <- NA
                                        datamat_reg[i,,,di,j] <- t(sub_calc_regular_2d_interp(
                                                                  I_MAT=IMAT[yinds,xinds], 
                                                                  XI=XI[yinds,xinds], 
                                                                  YI=YI[yinds,xinds],
                                                                  xp=xc_global, yp=yc_global,
                                                                  #datamat=drop(data_elem[i,,,,j])
                                                                  datamat=drop(tmp)
                                                                  ))
                                        # todo: check differnet progress bars  

                                    } # for j nrecspf
                                } # for i nvars
                                rm(tmp)
                                
                                ## Check data so far
                                if (verbose > 2) {
                                    for (i in 1:dim(datamat_reg)[1]) {
                                        if (!all(is.na(datamat_reg[i,,,di,]))) {
                                            message(paste0(indent, "   min/max datamat_reg[", i, ":", 
                                                         dimnames(datamat_reg)[[1]][i], ",,,", di, ",] = ",
                                                         paste0(range(datamat_reg[i,,,di,], na.rm=T), collapse="/")))
                                        }
                                    }
                                }

                            } # if regular_transient_out

                            ## Pick data from plot area irregular part
                            if (projection != "orthographic") {
                                if (proj_lims) {
                                    if (length(poly_inds_geogr) > 0) {
                                        datamat[,,,di,] <- data_elem[,,poly_inds_geogr,,]
                                    }

                                } else if (geogr_lims) {
                                    if (projection != "rectangular") {
                                        if (length(poly_inds_proj) > 0) {
                                            datamat[,,,di,] <- data_elem[,,poly_inds_proj,,]
                                        }
                                    } else if (projection == "rectangular") {
                                        if (length(poly_inds_geogr) > 0) {
                                            datamat[,,,di,] <- data_elem[,,poly_inds_geogr,,]
                                        }
                                    }
                                }
                            } else if (projection == "orthographic") {
                                datamat[,,,di,] <- data_elem[,,poly_inds_proj,,]
                            }

                            ## Remove NA locations due to coordinate transformation
                            if (length(na_inds) > 0) datamat <- datamat[,,-na_inds,,]

                            ## Check data so far
                            if (verbose > 2) {
                                for (i in 1:dim(datamat)[1]) {
                                    if (!all(is.na(datamat[i,,,di,]))) {
                                        message(paste0(indent, "   min/max datamat[", i, ":", 
                                                     dimnames(datamat)[[1]][i], ",,,", di, ",] = ",
                                                     paste0(range(datamat[i,,,di,], na.rm=T), collapse="/")))
                                    }
                                }
                            }

                            # update progress bar
                            if (dim(data_vert)[3] > 1) {
                                setTxtProgressBar(pb, di)
                            }

                            #stop("asd")

                        } # di dim(data_node)[3] # ndepths

                        # close progress bar
                        if (dim(data_vert)[3] > 1) {
                            close(pb)
                        }

                        rm(data_elem)

                    } # if plot || out_mode == "area" 
                   
                    #stop("asd")

                    ## Calc transient mean
                    if (transient_out) { # the non-regular part
                        if (out_mode != "area") {
                            if (rec_tag) {
                                if (leap_tag && is.leap(year)) {
                                    time_inds <- (total_rec + 1):(total_rec + nrecspf_leap)
                                } else {
                                    time_inds <- (total_rec + 1):(total_rec + nrecspf)
                                }
                            } else {
                                time_inds <- total_rec + 1
                            }

                            if (verbose > 1) {
                                message(paste0(indent, "Select irregular data from region ", area, " (=area) ..."))
                            }
                            
                            ## choose data from area in node space
                            if (out_mode == "mean" && zave_method == 2) {
                                
                                if (verbose > 2) {
                                    message(paste0(indent, "   using zave_method=2: cluster_vol_3d ..."))
                                }
                                nod3d_z_inds <- which(abs(nod_z) >= interpolate_depths[1] &
                                                      abs(nod_z) <= interpolate_depths[ndepths])
                                datavec <- data_node[,nod3d_z_inds,,]

                            } else {

                                ## arrange to level-space for calculations
                                if (F) { # old
                                    if (dim_tag == "2D" || integrate_depth ||
                                        average_depth || (dim_tag == "3D" && ndepths == 1)) {

                                        datavec <- data_node
                                    
                                    } else {
                                        if (verbose > 1) {
                                            message(paste0(indent, "Bring data_node from (nod3d_n=", nod3d_n,
                                                         ") on (nod2d_n=", nod2d_n, " x ndepths=", dim(data_node)[3], ") ..."))
                                            if (verbose > 2) {
                                                message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                                            }
                                        }
                                        sub_n3_to_n2xde(data_node) # produces tmp
                                        datavec <- tmp
                                        rm(tmp)
                                    }
                                } else if (T) {
                                    if (dim(data_node)[2] != nod2d_n) { # nod2d_n always defined
                                        if (verbose > 1) {
                                            message(paste0(indent, "Bring data_node from (nod3d_n=", nod3d_n,
                                                         ") on (nod2d_n=", nod2d_n, " x ndepths=", 
                                                         #dim(data_node)[3], 
                                                         ndepths, 
                                                         ") ..."))
                                            if (verbose > 2) {
                                                message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                                            }
                                        }
                                        sub_n3_to_n2xde(data_node) # produces tmp
                                        datavec <- tmp
                                        rm(tmp)

                                    } else { # data is already on 2d nodes
                                        datavec <- data_node
                                    }

                                } # old new
                                datavec <- datavec[,poly_node_inds_geogr,,] # inds in nod2d-space
                            
                            } # if out_mode == "mean" && zave_method == 2

                            ## change depth and time dim here for better netcdf output
                            datavec <- aperm(datavec, c(1, 2, 4, 3)) # nvars,nodes,nrecspf,ndepths

                            if (F) {
                                for (i in 1:ndepths) {
                                    message(paste0(interpolate_depths[i], "m: ", 
                                                 length(which(is.na(datavec[1,,1,i]))), 
                                                 " NA values"))
                                }
                            }

                            ## Check data so far
                            if (verbose > 2) {
                                for (i in 1:dim(datavec)[1]) {
                                    message(paste0(indent, "   min/max datavec[", i, ":", 
                                                 dimnames(datavec)[[1]][i], ",,,] = ",
                                                 paste0(range(datavec[i,,,], na.rm=T), collapse="/")))
                                }
                            }

                            if (verbose > 1) {
                                message(indent, "Calculate transient ", out_mode, " (=out_mode)", appendLF=F)
                                if (dim_tag == "3D") {
                                    message(" at ", depths_plot, appendLF=F)
                                    if (!(depths == 1 && depths == "bottom")) {
                                        message(" m", appendLF=F)
                                    }
                                    message(" (=depths_plot)", appendLF=F)
                                }
                                message("")
                                if (verbose > 2) {
                                    message(paste0(indent, "and save at 'time_inds'=",
                                                 paste(time_inds, collapse=",")))
                                }
                            }
                            
                            ## which calculation mode: mean, max, etc ...
                            if (any(out_mode == c("mean", "depth", "meanint", "depthint"))) {
                              
                                if (rec_tag && leap_tag && is.leap(year)) {
                                    if (out_mode == "mean" && zave_method == 2) { # special
                                        if (!exists("patch_vol_leap")) {
                                            patch_vol_leap <- cluster_vol_3d[nod3d_z_inds]
                                            patch_vol_leap <- replicate(patch_vol_leap, n=dim(datavec)[3]) # nrecspf
                                            patch_vol_leap <- replicate(patch_vol_leap, n=dim(datavec)[4]) # ndepth = 1
                                            patch_vol_leap <- replicate(patch_vol_leap, n=dim(datavec)[1]) # nvars
                                            patch_vol_leap <- aperm(patch_vol_leap, c(4, 1, 2, 3))
                                        }
                                    } else {
                                        if (!exists("patch_area_leap")) {
                                            patch_area_leap <- cluster_area_2d[poly_node_inds_geogr]
                                            patch_area_leap <- replicate(patch_area_leap, n=dim(datavec)[3]) # nrecspf
                                            patch_area_leap <- replicate(patch_area_leap, n=dim(datavec)[4]) # ndepths
                                            patch_area_leap <- replicate(patch_area_leap, n=dim(datavec)[1]) # nvars
                                            patch_area_leap <- aperm(patch_area_leap, c(4, 1, 2, 3))
                                        }
                                    }
                                } else { # only 1 rec and/or no leap
                                    if (out_mode == "mean" && zave_method == 2) { # special
                                        if (!exists("patch_vol")) {
                                            patch_vol <- cluster_vol_3d[nod3d_z_inds]
                                            patch_vol <- replicate(patch_vol, n=dim(datavec)[3]) # nrecspf
                                            patch_vol <- replicate(patch_vol, n=dim(datavec)[4]) # ndepth = 1
                                            patch_vol <- replicate(patch_vol, n=dim(datavec)[1]) # nvars
                                            patch_vol <- aperm(patch_vol, c(4, 1, 2, 3))
                                        }
                                    } else {
                                        if (!exists("patch_area")) {
                                            patch_area <- cluster_area_2d[poly_node_inds_geogr]
                                            patch_area <- replicate(patch_area, n=dim(datavec)[3]) # nrecspf
                                            patch_area <- replicate(patch_area, n=dim(datavec)[4]) # ndepths
                                            patch_area <- replicate(patch_area, n=dim(datavec)[1]) # nvars
                                            patch_area <- aperm(patch_area, c(4, 1, 2, 3))
                                        }
                                    }
                                } # if leap years are present
                                
                                # Calculate min/max/mean/median/nominal resolution of area as scalars.
                                # Resolution is defined per 2d-element (check deriv_2d.r).
                                # In these modes, the deriv_2d_nc was already loaded anyway.
                                add_res_to_nc <- T
                                if (add_res_to_nc) {
                                    if (out_mode == "mean" && zave_method == 2) {
                                        stop("not yet") 
                                    } else {
                                        if (verbose > 2) {
                                            message(indent, "   Calc min/max/mean/median/nominal res of ", area , " area ...")
                                        }
                                        res_node <- replicate(resolution, n=1) # resolution in elem space; dim = elem2d_n
                                        res_node <- replicate(res_node, n=1)
                                        res_node <- replicate(res_node, n=1)
                                        res_node <- replicate(res_node, n=1)
                                        res_node <- aperm(res_node, c(2, 3, 1, 4, 5)) # dim = c(nvars, 1, nelem2d_n, ndepths, nrecspf)
                                        sub_e2xde_to_n2xde(res_node) # produces tmp
                                        res_node <- drop(tmp[,poly_node_inds_geogr,,]) # resolution in node space
                                        # this may result single NAns if sub-data set (by ncl script)
                                        rm(tmp)
                                        
                                        ## Force km
                                        if (resolution_unit != "km") { # for output, convert to km
                                            if (resolution_unit == "m") { # this should be the default; m --> km
                                                fac <- 1e-3
                                                res_node_unit <- "km"
                                            } else {
                                                stop("not defined")
                                            }
                                            res_node <- res_node*fac
                                        } else {
                                            res_node_unit <- resolution_unit
                                        } # if resolution_unit != "km"

                                        ## Calc different resolution properties 
                                        res_node_min <- min(res_node, na.rm=T)
                                        res_node_max <- max(res_node, na.rm=T)
                                        res_node_median_simple <- median(res_node, na.rm=T)
                                        res_node_mean_simple <- mean(res_node, na.rm=T)
                                        if (rec_tag && leap_tag && is.leap(year)) {
                                            res_node_int <- res_node[!is.na(res_node)]*drop(patch_area_leap[1,!is.na(res_node),1,1])
                                            tmp_area <- sum(patch_area_leap[1,!is.na(res_node),1,1]) 
                                        } else {
                                            res_node_int <- res_node[!is.na(res_node)]*drop(patch_area[1,!is.na(res_node),1,1])
                                            tmp_area <- sum(patch_area[1,!is.na(res_node),1,1])
                                        }
                                        res_node_int <- sum(res_node_int) # sum data over nodes in area
                                        res_node_mean_weighted <- res_node_int/tmp_area
                                        # Nominal resolution calculation from CMIP6_global_attributes_filenames_CVs_v6.2.6.pdf:
                                        # In general, the nominal resolution characterizes the resolution of the 
                                        # grid used to report model output fields, which may differ from the native 
                                        # grid on which the fields are calculated by the model.
                                        # 1.) For each grid cell, calculate the distance (in km) between each pair 
                                        # of cell vertices and select the maximum distance ("d_max"). For latxlon grid cells, 
                                        # for example, "d_max" would be the diagonal distance.
                                        # 2.) Calculate the mean over all cells of "d_max", weighting each by the grid-cell's 
                                        # area (A). This defines the "mean resolution" (d_max). The formula is:
                                        # bar(d_max) = sum(A_i)^-1 * sum(d_max_i*A_i) for all i surface nodes
                                        # Note: For unstructured grid, there is no "d_max"?
                                        #       Use area weighted mean instead. Is this correct?
                                        res_node_nominal <- res_node_mean_weighted 
                                        if (res_node_nominal < 0.72) {
                                            res_node_nominal <- 0.5
                                        } else if (res_node_nominal >= 0.72 && res_node_nominal < 1.6) {
                                            res_node_nominal <- 1
                                        } else if (res_node_nominal >= 1.6 && res_node_nominal < 3.6) {
                                            res_node_nominal <- 2.5
                                        } else if (res_node_nominal >= 3.6 && res_node_nominal < 7.2) {
                                            res_node_nominal <- 5
                                        } else if (res_node_nominal >= 7.2 && res_node_nominal < 16) {
                                            res_node_nominal <- 10
                                        } else if (res_node_nominal >= 16 && res_node_nominal < 36) {
                                            res_node_nominal <- 25
                                        } else if (res_node_nominal >= 36 && res_node_nominal < 72) {
                                            res_node_nominal <- 50
                                        } else if (res_node_nominal >= 72 && res_node_nominal < 160) {
                                            res_node_nominal <- 100
                                        } else if (res_node_nominal >= 160 && res_node_nominal < 360) {
                                            res_node_nominal <- 250
                                        } else if (res_node_nominal >= 360 && res_node_nominal < 720) {
                                            res_node_nominal <- 500
                                        } else if (res_node_nominal >= 720 && res_node_nominal < 1600) {
                                            res_node_nominal <- 1000
                                        } else if (res_node_nominal >= 1600 && res_node_nominal < 3600) {
                                            res_node_nominal <- 2500
                                        } else if (res_node_nominal >= 3600 && res_node_nominal < 7200) {
                                            res_node_nominal <- 5000
                                        } else if (res_node_nominal >= 7200) {
                                            res_node_nominal <- 10000
                                        }
                                    }
                                } # if add_res_to_nc

                                # multiplay data by cluster area (in [unit of 'Rearth' in runscript]^2)
                                if (out_mode == "mean" && zave_method == 2) {
                                    if (rec_tag && leap_tag && is.leap(year)) {
                                        area_int <- datavec*patch_vol_leap
                                    } else {
                                        area_int <- datavec*patch_vol
                                    }
                                } else {
                                    if (rec_tag && leap_tag && is.leap(year)) {
                                        area_int <- datavec*patch_area_leap
                                    } else {
                                        area_int <- datavec*patch_area # c(var, nodes, recs, depth)
                                    } 
                                }

                                if (F) {
                                    for (i in 1:ndepths) {
                                        message(paste0(interpolate_depths[i], "m: ",
                                                     length(which(is.na(area_int[1,,1,i]))),
                                                     " NA values"))
                                    }
                                }

                                # sum data over nodes in area
                                area_int <- apply(area_int, c(1, 3, 4), sum, na.rm=T) # c(var, recs, depth)
                                area_int[area_int == 0] <- NA # where there are no values at depth

                                if (out_mode == "mean" && zave_method == 2) {
                                    area_mean <- area_int/sum(cluster_vol_3d[nod3d_z_inds])
                                    data_funi[,time_inds,] <- area_mean[,1:length(time_inds),] 

                                } else if ((out_mode == "mean" && zave_method == 1) || 
                                           out_mode == "depth") {
                                  
                                    # divide by total cluster area in area and depth i
                                    area_mean <- area_int
                                    for (i in 1:dim(datavec)[4]) { # for all depths
                                        if (rec_tag && leap_tag && is.leap(year)) {
                                            tmp_area <- sum(patch_area_leap[1,which(!is.na(datavec[1,,1,i])),1,i]) # NA positions do not change in time
                                        } else {
                                            tmp_area <- sum(patch_area[1,which(!is.na(datavec[1,,1,i])),1,i])
                                        }
                                        if (verbose > 2) {
                                            message(paste0(indent, "         area in ", interpolate_depths[i], " m depth = ", tmp_area, " m^2"))
                                        }
                                        area_mean[,,i] <- area_mean[,,i]/tmp_area
                                    } # for i ndepths

                                    # note that if deep depths have NA here, that simply means that 
                                    # the max depth in 'area' is shallower than the max depth of
                                    # the global mesh 'meshid'. 
                                    data_funi[,time_inds,] <- area_mean
                                
                                } else if (any(out_mode == c("meanint", "depthint"))) {
                                    data_funi[,time_inds,] <- area_int
                                }

                            } else if (out_mode == "sum") {
                                # sum over nodes
                                data_funi[,time_inds,] <- apply(datavec, c(1, 3, 4), sum, na.rm=T)[,1:length(time_inds),]
                                # For Hu Yang: sum only where wind curl is negative
                                #datavec2 <- datavec
                                #message("Special for Hu: Sum over areas where curltau is negative")
                                #datavec2[datavec2 >= 0] = 0
                                #message("Special: sum over aras where curltau is positive")
                                #datavec2[datavec2 <= 0] = 0
                                #data_funi[time_inds,] <- apply(datavec2, c(3, 4), sum, na.rm=T)
                            
                            } else if (out_mode == "max" || out_mode == "max3D" || out_mode == "depthmax") {
                                tmp <- apply(datavec, c(1, 3, 4), max, na.rm=T) # var, time, depths
                                tmp[tmp == -Inf] <- NA
                                
                                if (out_mode == "max" || out_mode == "depthmax") {
                                    data_funi[,time_inds,] <- tmp[,1:length(time_inds),] # time vs 1 depth for "max" or time vs depths for "depthmax"
                                
                                } else if (out_mode == "max3D") {
                                    data_funi[,time_inds,1] <- apply(tmp, c(1, 2), max)[,1:length(time_inds)] # time vs 1 maximum depth
                                    data_funi_depths[,time_inds,1] <- interpolate_depths[apply(tmp, c(1, 2), which.max)]
                                }
                            
                            } else if (out_mode == "min") {
                                data_funi[,time_inds,] <- apply(datavec, c(1, 3, 4), min, na.rm=T)[,1:length(time_inds),]
                            }
                           
                            ## Check data so far
                            if (verbose > 2) {
                                for (i in 1:dim(data_funi)[1]) {
                                    message(paste0(indent, "   min/max data_funi[", i, ":", 
                                                   dimnames(data_funi)[[1]][i], ",,] = ",
                                                   paste0(range(data_funi[i,,], na.rm=T), collapse="/"), 
                                                   " ", units_out))
                                }
                            }

                        ## Save transient area output
                        } else if (out_mode == "area") { # on irregular mesh
                            
                            if (total_rec == 0) { # prepare output
                                if (rec_tag) {
                                    transient_count <- c(3, dim(datamat)[3], dim(datamat)[5]) # c(elements, nodes, time)
                                } else {
                                    transient_count <- c(3, dim(datamat)[3], 1)
                                }

                                outname <- paste0(transientpath, "/", runid, "_", setting, "_", output, "_",
                                                  varname, "_transient_", out_mode, "_", timespan, 
                                                  depths_fname, "_", area, "_", 
                                                  projection, 
                                                  ssh_aviso_correct_fname, 
                                                  p_ref_suffix, fname_suffix, ".nc")

                                ## remove already existing data to avoid ncdf error:
                                ## Error in R_nc4_create: Permission denied (creation mode was 4096)
                                if (T) {
                                    system(paste0("rm ", outname), ignore.stderr=T) # silent
                                }

                                time_dim <- ncdim_def(name="time",
                                                      units="",
                                                      vals=time,
                                                      create_dimvar=T)
                                node_dim <- ncdim_def(name="nodes_per_element", 
                                                      units="", 
                                                      vals=1:3, 
                                                      create_dimvar=F)
                                elem_dim <- ncdim_def(name="nelements", 
                                                      units="", 
                                                      vals=1:dim(datamat)[3],
                                                      create_dimvar=F)

                                time_var <- ncvar_def(name="timechar", 
                                                      units=timeunit, 
                                                      dim=time_dim,
                                                      missval=-9999, 
                                                      prec="integer")
                                xp_var <- ncvar_def(name="xp", 
                                                    units="degrees_east", 
                                                    dim=list(node_dim, elem_dim),
                                                    missval=mv, 
                                                    prec=prec)
                                yp_var <- ncvar_def(name="yp", 
                                                    units="degrees_north", 
                                                    dim=list(node_dim, elem_dim),
                                                    missval=mv, 
                                                    prec=prec)
                                data_var <- vector("list", l=dim(datamat)[1]) 
                                for (i in 1:length(data_var)) {
                                    name <- dimnames(datamat)[[1]][i]
                                    data_var[[i]] <- ncvar_def(name=varname, 
                                                               units=units_out, 
                                                               dim=list(node_dim, 
                                                                        elem_dim, 
                                                                        time_dim), 
                                                               missval=mv, 
                                                               prec=prec, 
                                                               longname=paste0(longname, 
                                                                               ifelse(subtitle == "", "", paste0(", ", subtitle))))
                                }
                                outnc <- nc_create(outname, 
                                                   vars=c(data_var, 
                                                          list(xp_var, yp_var,
                                                               time_var)), 
                                                   force_v4=force_v4)
                                
                                ncvar_put(outnc, xp_var, xp)
                                ncvar_put(outnc, yp_var, yp)
                                ncvar_put(outnc, time_var, timechar)

                                ncatt_put(outnc, 0, "runid", runid)
                                ncatt_put(outnc, 0, "setting", setting)
                                ncatt_put(outnc, 0, "meshid", meshid)
                                ncatt_put(outnc, 0, "area", area)
                                ncatt_put(outnc, 0, "projection", projection)
                                ncatt_put(outnc, 0, "longitude_lims_deg", range(poly_geogr_lim_lon), prec="double")
                                ncatt_put(outnc, 0, "latitude_lims_deg", range(poly_geogr_lim_lat), prec="double")
                                if (exists("res_node_min")) {
                                    ncatt_put(outnc, 0, paste0("resolution_min_", res_node_unit), res_node_min)
                                    ncatt_put(outnc, 0, paste0("resolution_max_", res_node_unit), res_node_max)
                                    ncatt_put(outnc, 0, paste0("resolution_median_", res_node_unit), res_node_median_simple)
                                    ncatt_put(outnc, 0, paste0("resolution_mean_simple_", res_node_unit), res_node_mean_simple)
                                    ncatt_put(outnc, 0, paste0("resolution_mean_weigthed_", res_node_unit), res_node_mean_weighted)
                                    ncatt_put(outnc, 0, paste0("resolution_nominal_", res_node_unit), res_node_nominal)
                                }
                                if (dim_tag == "3D") {
                                    ncatt_put(outnc, 0, "depths_m", depths_plot, prec="double")
                                }
                                if (p_ref_suffix != "") {
                                    ncatt_put(outnc, 0, paste0("p_ref", ifelse(p_ref != "in-situ", "_dbar", "")), p_ref)
                                }
                            } # end if total_rec == 0			    

                            if (verbose > 1) {
                                message(paste0(indent, "Put ", timei[1], " transient ", varname,
                                     " in ", depths_plot, " m depths in ", area,
                                     " area to nc file ..."))
                                if (length(timei) > 1) {
                                    for (i in 2:length(timei)) {
                                        message(paste0(indent, " .. ", timei[i], " .."))
                                    }
                                }
                            }

                            transient_start <- c(1, 1, total_rec+1)
                            for (i in 1:length(data_var)) { # for nvars
                                ncvar_put(outnc, data_var[[i]], drop(datamat[i,,,1,]), # 3 nodes, elems, time 
                                          start=transient_start, 
                                          count=transient_count)
                            }
                        } # end if out_mode == "area" or not
                    } # end if transient_out
                    
                    if (regular_transient_out) {
                        if (out_mode != "area" && out_mode != "areadepth") {
                            # nothing to do
                        
                        } else if (out_mode == "area" || out_mode == "areadepth") {

                            # if mode == "areadepth", check first whether calculated data actually has a depth dim
                            if (out_mode == "areadepth") {
                                if (dim(datamat_reg)[4] == 1) { # only 1 depth, e.g. varname "c_baroclinic"
                                    out_mode <- "area"
                                    # check again if new path exist
                                    reg_transient_outpath <- paste0(postpath, "/", runid, "/", setting,
                                                               "/regular_grid/",
                                                               out_mode, "/", area, "/", varname, "/")
                                    dir.create(reg_transient_outpath, recursive=T, showWarnings=F)
                                }
                            }

                            # Prepare regular output file
                            if (total_rec == 0) {
                               
                                if (rec_tag) {
                                    if (out_mode == "area") {
                                        transient_count_reg <- c(nxi, nyi,
                                                                 dim(datamat_reg)[5]) # nrecs
                                    } else if (out_mode == "areadepth") {
                                        transient_count_reg <- c(nxi, nyi,
                                                                 dim(datamat_reg)[4], # ndepths
                                                                 dim(datamat_reg)[5]) # nrecs
                                    } 
                                } else {
                                    if (out_mode == "area") {
                                        transient_count_reg <- c(nxi, nyi,
                                                                 1) # nrecs
                                    } else if (out_mode == "areadepth") {
                                        transient_count_reg <- c(nxi, nyi, 
                                                                 dim(datamat_reg)[4], # ndepths
                                                                 1) # nrecs
                                    }
                                }

                                outname_reg <- paste0(reg_transient_outpath, "/", runid, "_", 
                                                      setting, "_", output, "_",
                                                      varname, "_transient_", out_mode, "_", timespan,
                                                      depths_fname, "_", area, "_",
                                                      projection, "_regular_dx",
                                                      sprintf("%.3f", regular_dx), "_dy",
                                                      sprintf("%.3f", regular_dy), 
                                                      ssh_aviso_correct_fname, 
                                                      p_ref_suffix, fname_suffix, ".nc")
                                ## remove already existing data to avoid ncdf error:
                                ## Error in R_nc4_create: Permission denied (creation mode was 4096)
                                if (T) {
                                    system(paste0("rm ", outname_reg), ignore.stderr=T) # silent
                                }
             
                                time_dim <- ncdim_def(name="time", units=timesec_unit, vals=timesec)                          
                                lon_dim <- ncdim_def(name="lon", units="degree_east", vals=xi)
                                lat_dim <- ncdim_def(name="lat", units="degree_north", vals=yi)

                                time_var <- ncvar_def(name="timechar", units=timeunit,
                                                      dim=time_dim,
                                                      missval=-9999, prec="integer")
                                if (out_mode == "areadepth") {
                                    depth_dim <- ncdim_def(name="depth", units="",
                                                           vals=-interpolate_depths, 
                                                           create_dimvar=T)
                                    depth_var <- ncvar_def(name="depthvec", units="m",
                                                           dim=depth_dim,
                                                           missval=9999, prec="integer")
                                }


                                data_reg_var <- vector("list", l=dim(datamat_reg)[1]) # nvars
                                if (out_mode == "area") {

                                    for (i in 1:length(data_reg_var)) {
                                        name <- dimnames(datamat_reg)[[1]][i]
                                        data_reg_var[[i]] <- ncvar_def(name=name, units=units_out,
                                                                       dim=list(lon_dim, lat_dim,
                                                                                time_dim),
                                                                       missval=mv,
                                                                       longname=paste0(longname, 
                                                                                       ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                                       prec=prec)
                                    }
                                 
                                    outnc_reg <- nc_create(filename=outname_reg,
                                                           vars=c(list(time_var), data_reg_var),
                                                           force_v4=force_v4)
                                
                                } else if (out_mode == "areadepth") {

                                    for (i in 1:length(data_reg_var)) {
                                        name <- dimnames(datamat_reg)[[1]][i]
                                        data_reg_var[[i]] <- ncvar_def(name=name, units=units_out,
                                                                       dim=list(lon_dim, lat_dim, 
                                                                                depth_dim, time_dim),
                                                                       missval=mv,
                                                                       longname=paste0(longname, 
                                                                                       ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                                       prec=prec)
                                    }
                                    
                                    outnc_reg <- nc_create(filename=outname_reg, 
                                                           vars=c(list(time_var, depth_var), data_reg_var),
                                                           force_v4=force_v4)
                                } # area or areadepth

                                if (out_mode == "areadepth") {
                                    ncvar_put(outnc_reg, depth_var, -interpolate_depths)
                                }
                                ncvar_put(outnc_reg, time_var, timechar)
                                
                                ncatt_put(outnc_reg, 0, "runid", runid)
                                ncatt_put(outnc_reg, 0, "setting", setting)
                                ncatt_put(outnc_reg, 0, "meshid", meshid)
                                ncatt_put(outnc_reg, 0, "area", area)
                                ncatt_put(outnc_reg, 0, "projection", projection)
                                ncatt_put(outnc_reg, 0, "longitude_lims_deg", range(xi), prec=prec)
                                ncatt_put(outnc_reg, 0, "latitude_lims_deg", range(yi), prec=prec)
                                ncatt_put(outnc_reg, 0, "regular_dx", sprintf("%.3f", regular_dx))
                                ncatt_put(outnc_reg, 0, "regular_dy", sprintf("%.3f", regular_dy))
                                if (dim_tag == "3D") {
                                    ncatt_put(outnc_reg, 0, "depths_m", depths_plot, prec="double")
                                }
                                if (p_ref_suffix != "") {
                                    ncatt_put(outnc_reg, 0, paste0("p_ref", ifelse(p_ref != "in-situ", "_dbar", "")), p_ref)
                                }
                            } # if total_rec == 0
                            
                            if (out_mode == "area") {
                                transient_start_reg <- c(1, 1, total_rec+1) # x, y, depth, time
                            } else if (out_mode == "areadepth") {
                                transient_start_reg <- c(1, 1, 1, total_rec+1) # x, y, depth, time
                            }

                            if (verbose > 1) {
                                if (dim_tag == "2D") {
                                    message(paste0(indent, "Put regular transient ", varname,
                                                 " in ", area, " area to nc file ..."))
                                } else if (dim_tag == "3D") {
                                    message(paste0(indent, "Put regular transient ", varname,
                                                 " in ", depths_plot, " m depths in ", area,
                                                 " area to nc file ..."))
                                }
                                message(paste0(indent, "   istart=c(", 
                                             paste0(transient_start_reg, collapse=","), 
                                             "), icount=c(", 
                                             paste0(transient_count_reg, collapse=","), ")"))
                                message(indent, "   ", timei[1], appendLF=F)
                                if (ntime != 1) message(" to ", timei[ntime], appendLF=F)
                                message("")
                            } # verbose
                            
                            for (i in 1:length(data_reg_var)) {
                                if (out_mode == "area") {
                                    ncvar_put(nc=outnc_reg, varid=data_reg_var[[i]],
                                              vals=drop(datamat_reg[i,,,1,]),
                                              start=transient_start_reg,
                                              count=transient_count_reg)
                                } else if (out_mode == "areadepth") {
                                    ncvar_put(nc=outnc_reg, varid=data_reg_var[[i]], 
                                              vals=drop(datamat_reg[i,,,,]),
                                              start=transient_start_reg, 
                                              count=transient_count_reg)
                                }
                            }
                            
                        } # end if out_mode == "area" or not
                    } # end if regular_transient_out


                } else if (any(out_mode == c("csec_mean", "csec_depth"))) {
                    if (verbose > 1) {
                        message(paste0(indent, "Interpolate '", area, "' area '", out_mode,
                                     "' data from nodes to elements ..."))
                    }

                    ## change depth and time dimensions of data_global_vert
                    #data_global_vert <- aperm(data_global_vert, c(1, 2, 4, 3))

                    ## for every csection edge
                    for (i in 1:(length(map_geogr_lim_lon)-1)) {

                        csec_use_middle <- F
                        if (csec_use_middle) {
                            # average over upper and lower nodes of prism
                            # from patrick; probably because he lost 1 value by dz = diff(z)
                            data_global_vert <- (data_global_vert[,,2:ndepths,] +
                                                 data_global_vert[,,1:(ndepths-1),])/2
                            # inputarray.range.depth_vek{1}(1:end-1)+(inputarray.range.depth_vek{1}(2:end)-inputarray.range.depth_vek{1}(1:end-1))/2;
                            csec_middle_depths <- interpolate_depths[2:ndepths] - diff(interpolate_depths)/2
                            dimnames(data_global_vert)[3] <- list(csec_middle_depths=interpolate_depths)

                        } else if (!csec_use_middle) {
                            data_global_vert <- data_global_vert
                            csec_middle_depths <- interpolate_depths
                        }

                        # for correct matrix multiplication dimensions
                        horiz_interp_coef1 <- csec_interp_fac1_vec/csec_interp_d_vec
                        horiz_interp_coef1 <- replicate(horiz_interp_coef1, n=dim(data_global_vert)[1]) # nvars
                        horiz_interp_coef1 <- replicate(horiz_interp_coef1, n=dim(data_global_vert)[3]) # ndepths
                        horiz_interp_coef1 <- replicate(horiz_interp_coef1, n=dim(data_global_vert)[4]) # nrecspf
                        horiz_interp_coef1 <- aperm(horiz_interp_coef1, c(2, 1, 3, 4))
                        horiz_interp_coef2 <- csec_interp_fac2_vec/csec_interp_d_vec
                        horiz_interp_coef2 <- replicate(horiz_interp_coef2, n=dim(data_global_vert)[1]) # nvars
                        horiz_interp_coef2 <- replicate(horiz_interp_coef2, n=dim(data_global_vert)[3]) # ndepths
                        horiz_interp_coef2 <- replicate(horiz_interp_coef2, n=dim(data_global_vert)[4]) # nrecspf
                        horiz_interp_coef2 <- aperm(horiz_interp_coef2, c(2, 1, 3, 4))

                        # function [variable_out]=csection_analyse_interp_point_se(obj,variable_in,csi)
                        # pos_nodes[elem2d[1,csec_elem_inds]] = 80 
                        data_vert_csec <- data_global_vert[,pos_nodes[elem2d[1,csec_interp_index_vec]],,] + 
                                        ((data_global_vert[,pos_nodes[elem2d[2,csec_interp_index_vec]],,] - 
                                          data_global_vert[,pos_nodes[elem2d[1,csec_interp_index_vec]],,])*horiz_interp_coef1) - 
                                        ((data_global_vert[,pos_nodes[elem2d[3,csec_interp_index_vec]],,] -
                                          data_global_vert[,pos_nodes[elem2d[1,csec_interp_index_vec]],,])*horiz_interp_coef2)

                        ## repair own stupidness: put time at the end
                        #data_vert_csec <- aperm(data_vert_csec, c(1, 2, 4, 3))

                        ## Irregular dx and dz for section mean
                        if (total_rec == 0) {
                            
                            if (csec_use_middle) {
                                # diff(interpolate_depths) = 
                                # (inputarray.range.depth_vek{1}(kk)-inputarray.range.depth_vek{1}(kk-1)) =
                                # inputarray.range.depth_vek{1}(2:end)-inputarray.range.depth_vek{1}(1:end-1);
                                drdz <- outer(csec_DeltaR_vec, diff(interpolate_depths), "*")
                            
                            } else if (!csec_use_middle) {
                                
                                if (F) {
                                    interpolate_depths <- interpolate_depths
                                    ndepths = length(interpolate_depths)
                                    deltaz=rep(0, t=ndepths-1)
                                    deltaz[1]=(interpolate_depths[1]-interpolate_depths[2])/2
                                    deltaz[ndepths]=(interpolate_depths[ndepths-1]-interpolate_depths[ndepths])/2
                                    for (n in 2:(ndepths-1)) {
                                        deltaz[n]=(interpolate_depths[n-1]-interpolate_depths[n])/2 + 
                                                  (interpolate_depths[n]-interpolate_depths[n+1])/2
                                    }
                                    deltaz <- abs(deltaz)
                                } else {
                                    # like patrick:
                                    deltaz <- diff(fesom_depths)[1:ndepths]
                                    deltaz[length(deltaz)] <- deltaz[length(deltaz) - 1]
                                }
                                drdz <- outer(csec_DeltaR_vec, deltaz, "*")
                            }

                            drdz <- replicate(drdz, n=dim(data_vert_csec)[4]) # add 3rd dim: nrecspf
                            drdz <- replicate(drdz, n=1) # add 4th dim: nvars 
                            drdz <- aperm(drdz, c(4, 1, 2, 3)) # nvars,nodslot,ndepths,nrecspf
                        }

                        ## Cross section calculations here
                        if (varname == "transport") {

                            # Transport through section s from point A to point B = 
                            # int_{A}^{B} int_{bottom}^{z=0} (\vec{u}_h \cdot \vec{n}) dz dr
                            
                            if (F) { # old
                                transport <- (data_vert_csec[which(varname_fesom == "u"),,,]*csec_n_vec_edge_vec[1,i] +
                                              data_vert_csec[which(varname_fesom == "v"),,,]*csec_n_vec_edge_vec[2,i])*drdz
                                transport <- transport / 1e6 # m3 s-1 --> Sv
                            } else {
                                # velocity normal to cross section in m/s
                                transport <- data_vert_csec[which(varname_fesom == "u"),,,]*csec_n_vec_edge_vec[1,i] +
                                             data_vert_csec[which(varname_fesom == "v"),,,]*csec_n_vec_edge_vec[2,i]
                            }
                            dimnames(transport)[[1]] <- list(var=varname)
                            success <- load_package("abind")
                            if (!success) stop(helppage)
                            data_vert_csec <- abind(data_vert_csec, transport, along=1, use.dnns=T)

                        } else if (varname == "sitransport") {

                            stop("update")
                            dr <- outer(csec_DeltaR_vec, diff(interpolate_depths), "*")
                            dr <- replicate(drdz, n=dim(data_vert_csec)[4]) # 3: ntime
                            dr <- replicate(drdz, n=1) # 4: var 
                            dr <- aperm(drdz, c(4, 1, 2, 3))

                            # Sea ice volume transport through section s from point A to point B = 
                            # int_{A}^{B} (\vec{u}_h \cdot \vec{n} * sic * hice) dr
                            sitransport <- (data_vert_csec[which(varname_fesom == "u"),,,]*csec_n_vec_edge_vec[1,i] +
                                            data_vert_csec[which(varname_fesom == "v"),,,]*csec_n_vec_edge_vec[2,i]) *
                                           data_vert_csec[which(varname_fesom == "area"),,,] *
                                           data_vert_csec[which(varname_fesom == "hice"),,,] * dr
                            sitransport <- sitransport / 1e6 # Sv = 10^6 m^3 s^-1
                            dimnames(sitransport)[[1]] <- list(var=varname)
                            data_vert_csec <- abind(data_vert_csec, sitransport, along=1, use.dnns=T)

                        } # which cross section varname 

                        if (F) {
                            dev.new()
                            xx <- 1:length(csec_interp_points_vec[1,])
                            image.plot(xx, csec_middle_depths, data_vert_csec[3,,,1], ylim=rev(range(csec_middle_depths)))
                        }

                        ## Apply csec conditions
                        if (out_mode == "csec_mean" && csec_conds_n > 0) {

                            cond_inds <- vector("list", l=csec_conds_n)
                            for (j in 1:csec_conds_n) {

                                if (csec_conds[j] == "gt") cond <- ">"
                                if (csec_conds[j] == "ge") cond <- ">="
                                if (csec_conds[j] == "lt") cond <- "<"
                                if (csec_conds[j] == "le") cond <- "<="
                                message(paste0(indent, "Apply ", varname, " condition: ",
                                             csec_cond_vars[j], " ", cond, " ",
                                             csec_cond_vals[j], " ", 
                                             csec_cond_units[j], " ..."))

                                # condition variable
                                cond_var_ind <- which(dimnames(data_vert_csec)[[1]] == csec_cond_vars[j])
                                # condition inds
                                cond_inds[[j]] <- eval(parse(text=paste0("data_vert_csec[", cond_var_ind, 
                                                                         ",,,] ", cond, " ", 
                                                                         csec_cond_vals[j])))
                                #if (is.na(any(cond_inds[[i]]))) {
                                #    stop("Error: no elements ", out_mode, varname 
                                #}
                            } # for j csec_conds_n

                        } # if csec_mean && csec_conds_n > 0

                        if (total_rec == 0) {
                            if (out_mode == "csec_mean") {
                                data_funi <- array(NA, c(dim(data_vert_csec)[1], ntime),
                                                   dimnames=c(dimnames(data_vert_csec)[1], 
                                                              list(rec=timechar)))
                            } else if (out_mode == "csec_depth") {
                                data_funi <- array(NA, c(dim(data_vert_csec)[1:3], ntime),
                                                   dimnames=c(dimnames(data_vert_csec)[1:3], 
                                                              list(rec=timechar)))
                            }
                        }
                        if (rec_tag) {
                            if (leap_tag && is.leap(year)) {
                                time_inds <- (total_rec + 1):(total_rec + nrecspf_leap)
                            } else {
                                time_inds <- (total_rec + 1):(total_rec + nrecspf)
                            }
                        } else {
                            time_inds <- total_rec + 1
                        }

                        if (verbose > 1) {
                            message(paste0(indent, "Calc ", area, " area ", out_mode,
                                         " and save at time inds=",
                                         paste(time_inds, collapse=",")))
                        }

                        ## Save csection mean after applying csection conditions
                        if (out_mode == "csec_mean") {

                            for (j in 1:dim(data_funi)[1]) { # for all vars

                                tmp <- data_vert_csec[j,,,]
                                drdz_tmp <- drdz
                                drdz_tmp[is.na(tmp)] <- NA # set land to NA

                                # set all data to NA which do not fulfill csec_conds
                                if (csec_conds_n > 0) {
                                    for (k in 1:csec_conds_n) {
                                        tmp[!cond_inds[[k]]] <- NA
                                        drdz_tmp[!cond_inds[[k]]] <- NA
                                    }
                                }

                                if (F) {
                                    dev.new()
                                    xx <- 1:length(csec_interp_points_vec[1,]) # might not all decreasing in irregular index area
                                    image.plot(xx, csec_middle_depths, tmp[1,,,1], ylim=rev(range(csec_middle_depths)))
                                }

                                if (dimnames(data_funi)[[1]][j] == "transport") {
                                    
                                    if (F) { # old
                                        # drdz allready multiplied
                                        data_funi[j,time_inds] <- apply(tmp, 4, sum, na.rm=T) # dim4 = time
                                    } else {
                                        # 1: a = n_vec*u_vec*dr*dz --> m3/s
                                        # 2: sum(a) over cross section
                                        # 3: /1e6 --> Sv
                                        data_funi[j,time_inds] <- apply(tmp*drdz_tmp, 4, sum, na.rm=T)/1e6 # m3 s-1 --> Sv = 10^6 m^3 s^-1 
                                    }

                                } else if (dimnames(data_funi)[[1]][j] == "sitransport") {
                                    stop("asdasdas")

                                } else { # default
                                    # irregular dx and dz weighted mean
                                    tmp <- tmp*drdz_tmp
                                    LH <- apply(drdz_tmp, 4, sum, na.rm=T) # dim4 = time
                                    data_funi[j,time_inds] <- apply(tmp, 4, sum, na.rm=T)/LH

                                }

                            } # for j all vars of csection

                            #stop("asd")

                        ## save csec as it is
                        } else if (out_mode == "csec_depth") {
                            data_funi[,,,time_inds] <- data_vert_csec
                        
                        }

                    } # for i vertices of csection


                } else if (any(out_mode == c("moc_mean", "moc_depth"))) {

                    ## dim(data_node) = # c(nvars,nreg_lat,ndepths,ntime)

                    ## remove possible redundant latitudes and bottom depths with no values
                    if (total_rec == 0) {
                        moc_reg_lat <- moc_reg_lat_global
                        lat_na_inds <- which(apply(moc_topo, 1, function(x) all(x == 1)))
                        depth_na_inds <- which(apply(moc_topo, 2, function(x) all(x == 1)))
                        if (length(lat_na_inds) > 0) {
                            moc_reg_lat <- moc_reg_lat[-lat_na_inds]
                            moc_topo <- moc_topo[-lat_na_inds,]
                        }
                        if (length(depth_na_inds) > 0) {
                            moc_topo <- moc_topo[,-depth_na_inds]
                        }
                    } # if total_rec == 0
                    if (length(lat_na_inds) > 0) {
                        data_node <- data_node[,-lat_na_inds,,]
                    }
                    if (length(depth_na_inds) > 0) {
                        data_node <- data_node[,,-depth_na_inds,]
                    }
                    if (total_rec == 0) {
                        # improve this: only use depths where MOC has data
                        interpolate_depths <- interpolate_depths[1:dim(data_node)[3]]
                    }

                    if (total_rec == 0) {
                        if (out_mode == "moc_mean") {
                            stop("not yet")
                            data_funi <- array(NA, c(dim(data_node), ntime),
                                               dimnames=c(dimnames(data)[1],
                                                          list(rec=timechar)))
                        } else if (out_mode == "moc_depth") {
                            data_funi <- array(NA, c(dim(data_node)[1:3], ntime),
                                               dimnames=c(var=dimnames(data_node)[1],
                                                          list(lat=moc_reg_lat, 
                                                               depth=interpolate_depths,
                                                               rec=timechar)))
                        }
                    }
                    if (rec_tag) {
                        if (leap_tag && is.leap(year)) {
                            time_inds <- (total_rec + 1):(total_rec + nrecspf_leap)
                        } else {
                            time_inds <- (total_rec + 1):(total_rec + nrecspf)
                        }
                    } else {
                        time_inds <- total_rec + 1
                    }

                    if (verbose > 1) {
                        message(paste0(indent, "Calc ", area, " area ", out_mode,
                                     " and save at time inds=",
                                     paste(time_inds, collapse=",")))
                    }

                    if (out_mode == "moc_mean") {
                        stop("not yettt")

                    } else if (out_mode == "moc_depth") {
                        data_funi[,,,time_inds] <- data_node
                    
                    }

                } # if normal, csec, or moc output

				## prepare and sum transient data for ltm
                if (any(ltm_out, regular_ltm_out, moc_ltm_out, rms_out, sd_out, plot_map)) {

                    ## vertical average for ltm if not done before
                    if (F && !integrate_depth && !average_depth) {
                   
                        ## rearrange first if necessary
                        if (dim_tag == "3D" && dim(data_node)[2] != nod2d_n && ndepths > 1) {

                            if (zave_method == 1) { # level-wise dz                        
                                if (verbose > 1) { # rearrange first
                                    message(paste0(indent, "For ltm/plot bring data_node from (nod3d_n=", nod3d_n,
                                                   ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
                                    if (verbose > 2) {
                                        message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                                    }
                                }
                                sub_n3_to_n2xde(data_node) # produces tmp
                                data_vert <- tmp # dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
                                rm(tmp)

                            } else if (zave_method == 2) { # which zave_method
                                data_vert <- data_node # dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
                            }
                        } else {
                            data_vert <- data_node
                            
                        } # if data is not on 2d yet

                        ## Check data so far
                        if (verbose > 2) {
                            for (i in 1:dim(data_node)[1]) {
                                message(paste0(indent, "   min/max data_node[", i, ":",
                                             dimnames(data_node)[[1]][i], ",,,] = ",
                                             paste0(range(data_node[i,,,], na.rm=T), collapse="/")))
                                if (F) {
                                    for (j in 1:dim(data_node)[3]) {
                                        message(range(data_node[i,,j,], na.rm=T))
                                    }
                                }
                            }
                        }

                        # not here or?
                        if (F) {
                            # if data_vert has more than 1 depth
                            if (dim(data_vert)[3] > 1) { 
                                if (verbose > 1) {
                                    message(paste0(indent, "Average over ", depths_plot, " m depths for ltm or plot ..."))
                                    if (verbose > 2) {
                                        message(paste0(indent, "   run ", subroutinepath, "/sub_vertical_average.r ..."))
                                    }
                                }
                                sub_vertical_average(data_vert) # produces tmp
                                data_node <- tmp # overwrite data_node
                                # if (zave_method == 1): dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf)
                                # if (zave_method == 2): dim(data_node) = c(nvars,nod[23]d_n=1,ndepths=1,nrecspf) # special!
                                rm(tmp, data_vert)

                                ## Check data so far
                                if (verbose > 2) {
                                    for (i in 1:dim(data_node)[1]) {
                                        message(paste0(indent, "   min/max data_node[", i, ":",
                                                     dimnames(data_node)[[1]][i], ",,,] = ",
                                                     paste0(range(data_node[i,,,], na.rm=T), collapse="/")))
                                    }
                                }
                            } else {
                                
                                data_node <- data_vert
                            
                            } # if data_vert has more than 1 depth
                        } # F

                    } # if !integrate_depth && !average_depth

                    ## at this point
                    # TODO
                    ## dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf)

                    ## matrix for ltm output 
                    if (total_rec == 0) {
                        data_node_ltm <- array(0,
                                               dim=dim(data_node), # c(nvars,nod2d_n,ndepths=1,nrecspf)
                                               dimnames=dimnames(data_node))
                        if (rms_out || sd_out) {
                            data_node_sd <- data_node_ltm
                            if (sd_method == "ackermann83") {
                                uv_sd <- array(0, 
                                               dim=c(1, dim(data_node_sd)[2:4]),
                                               dimnames=c(list(var=paste0(dimnames(data_node)[[1]][1], "*",
                                                                          dimnames(data_node)[[1]][2])),
                                                          dimnames(data_node_sd)[2:4]))
                            }
                        } # if rms_out || sd_out

                        if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                            mld_node_ltm <- mld_node
                            mld_node_ltm[] <- 0
                        } # if mld needed

                    } # if total_rec == 0

                    if (verbose > 1) {
                            message(paste0(indent, "Sum transient ", 
                                          paste0(dimnames(data_node)[[1]], collapse=","), " for ltm/plot ..."))
                    }   
                    
                    ## Save data in array (vars,nodes,depths=1,nrecspf)
                    if (rec_tag) {
                        if (leap_tag && is.leap(year)) {
                            data_node_ltm[,,1,1:nrecspf_leap] <- data_node_ltm[,,1,1:nrecspf_leap] + data_node
                        } else {
                            data_node_ltm[,,1,1:nrecspf] <- data_node_ltm[,,1,1:nrecspf] + data_node
                        }
                    } else {
                        data_node_ltm <- data_node_ltm + data_node
                    }
                    #message(range(data_node_ltm, na.rm=T))

                    if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                        if (rec_tag) {
                            if (leap_tag && is.leap(year)) {
                                mld_node_ltm[,,1,1:nrecspf_leap] <- mld_node_ltm[,,1,1:nrecspf_leap] + mld_node
                            } else {
                                mld_node_ltm[,,1,1:nrecspf] <- mld_node_ltm[,,1,1:nrecspf] + mld_node
                            }
                        } else {
                            mld_node_ltm <- mld_node_ltm + mld_node
                        }
                    } # if mld needed

                    if (rms_out || sd_out) {

                        if (verbose > 1) {
                            message(paste0(indent, "Sum transient ",
                                         paste0(paste0(dimnames(data_node)[[1]], "^2"), collapse=","), " for rms/sd ..."))
                        }

                        if (rec_tag) {
                            if (leap_tag && is.leap(year)) {
                                data_node_sd[,,1,1:nrecspf_leap] <- data_node_sd[,,1,1:nrecspf_leap] + data_node^2
                            } else {
                                data_node_sd[,,1,1:nrecspf] <- data_node_sd[,,1,1:nrecspf] + data_node^2
                            }
                        } else {
                            data_node_sd <- data_node_sd + data_node^2
                        }

                        if (sd_method == "ackermann83") {
                            varinds <- c(1, 2)
                            if (verbose > 1) {
                                message(paste0("Save ", dimnames(data_node)[[1]][varinds[1]], " * ",
                                             dimnmes(data_node)[[1]][varinds[2]],
                                             " for sd of direction of ", varname, " ..."))
                            }
                            if (rec_tag) {
                                if (leap_tag && is.leap(year)) {
                                    uv_sd[,,1,1:nrecspf_leap] <- uv_sd[,,1,1:nrecspf_leap] + data_node[varinds[1],,,]*data_node[varinds[2],,,]
                                } else {
                                    uv_sd[,,1,1:nrecspf] <- uv_sd[,,1,1:nrecspf] + data_node[varinds[1],,,]*data_node[varinds[2],,,]
                                }
                            } else {
                                uv_sd <- uv_sd + data_node[varinds[1],,,]*data_node[varinds[2],,,]
                            }
                        } # if sd_method == "ackermann83"

                    } # if rms_out || sd_out

                } # if (any(ltm_out, regular_ltm_out, moc_ltm_out, rms_out, sd_out, plot_map))

            # else if not transient or sd out
            } else { 

				# here, no multfac_* is applied yet

                if (any(ltm_out, regular_ltm_out, moc_ltm_out, plot_map)) {
             
                    if (verbose > 1) {
                        message(paste0(indent, "Sum non-transient ", 
                                     paste0(dimnames(data_node)[[1]], collapse=","), " for ltm/plot ..."))
                    }

                    if (total_rec == 0) { 
                        data_node_ltm <- array(0, 
                                               dim=dim(data_node),
                                               dimnames=dimnames(data_node)) # c(nvar,nnod,ndepths=1,nrecspf)

                        if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                            mld_node_ltm <- mld_node
                            mld_node_ltm[] <- 0
                        }

                    } # if total_rec == 0

                    ## Save data in array (vars,nodes,time,depths)
                    # need to use 1:icount[1] for indexing since both 2D and 3D variables may be used
                    for (i in 1:dim(data_node)[1]) { # nvars
                        if (rec_tag) {
                            if (leap_tag && is.leap(year)) {
                                data_node_ltm[i,1:icounts[i,1],1,1:nrecspf_leap] <- data_node_ltm[i,1:icounts[i,1],1,1:nrecspf_leap] + data_node[i,1:icounts[i,1],,]
                            } else {
                                data_node_ltm[i,1:icounts[i,1],1,1:nrecspf] <- data_node_ltm[i,1:icounts[i,1],1,1:nrecspf] + data_node[i,1:icounts[i,1],,]
                            }
                        } else {
                            data_node_ltm[i,1:icounts[i,1],,] <- data_node_ltm[i,1:icounts[i,1],,] + data_node[i,1:icounts[i,1],,]
                        }
                    }

                    if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                        if (rec_tag) {
                            if (leap_tag && is.leap(year)) {
                                mld_node_ltm[,1:nod2d_n,1,1:nrecspf_leap] <- mld_node_ltm[,1:nod2d_n,1,1:nrecspf_leap] + mld_node
                            } else {
                                mld_node_ltm[,1:nod2d_n,1,1:nrecspf] <- mld_node_ltm[,1:nod2d_n,1,1:nrecspf] + mld_node
                            }
                        } else {
                            mld_node_ltm[,1:nod2d_n,,] <- mld_node_ltm[,1:nod2d_n,,] + mld_node
                        }
                    } # if mld needed

                    rm(data_node) # remove here otherwise sub_calc(data_node_ltm) will not work correctly 

                } else { # !any(ltm_out, regular_ltm_out, moc_ltm_out, plot_map)

                    if (verbose > 0) {
                        message(paste0("Nothing to do o_O"))
                    }

                } # if any(ltm_out, regular_ltm_out, moc_ltm_out, csec_ltm_out, plot_map)

            } # if (any(transient_out, regular_transient_out, rms_out, sd_out))  

            # set total counter
            if (rec_tag) {
                if (leap_tag && is.leap(year)) {
                    total_rec <- total_rec + nrecspf_leap
                } else {
                    total_rec <- total_rec + nrecspf
                }
            } else {
                total_rec <- total_rec + 1
            }

        } # for recsi timesteps (e.g. monthly, daily, hourly) per fesom file loop
        #}) # recsloop_systime 
        #stop("asd")
    
    } # end year loop
  
    indent <- "   "
    if (verbose > 1) {
        message(paste0(indent, "Year loop done"))
    }

    #stop("asd")

    ## continue with transient output
    if (transient_out) { # irregular

        ## check
        if (length(dimnames(data_node)[[1]]) !=
            length(unique(dimnames(data_node)[[1]]))) {
            message(paste0("Warning: dimnames(data_node)[[1]]='", 
                         paste0(dimnames(data_node)[[1]], collapse="','"),
                         "' have double entries. this may cause trouble."))
        }

        ## Save transient area output
        if (out_mode == "area") { # this is irregular
        #if (out_mode == "area" || out_mode == "areadepth") {
            if (verbose > 1) {
                message(paste0(indent, "Save transient ", out_mode, " (=out_mode) file (=outname):"))
                message(paste0(indent, indent, outname))
            }
            nc_close(outnc)

        ## Save transient
        } else if (all(out_mode != c("area", "areadepth"))) {
            
            if (out_mode == "csec_depth") {
                csec_cond_depth <- csec_cond_vars
                if (any(csec_cond_depth == "u")) {
                    csec_cond_depth <- csec_cond_depth[csec_cond_depth != "u"]
                }
                if (any(csec_cond_depth == "v")) {
                    csec_cond_depth <- csec_cond_depth[csec_cond_depth != "v"]
                }
            } # if out_mode == "csec_depth"

            # nc name
            if (any(varname == c("iceextent", "icevol"))) {
                if (is.null(sic_cond_fname)) {
                    outname <- paste0(transientpath, "/", runid, "_", setting, "_", output, "_",
                                      varname, "_sic_transient_", out_mode, "_", 
                                      timespan, "_", area, "_", projection, p_ref_suffix, fname_suffix,
                                      ".nc")
                } else {
                    outname <- paste0(transientpath, "/", runid, "_", setting, "_", output, "_",
                                      varname, "_sic.", sic_cond_fname, ".", sic_thr*100, 
                                      "_transient_", out_mode, "_",
                                      timespan, "_", area, "_", projection, p_ref_suffix, fname_suffix,
                                      ".nc")
                }
            } else {
                outname <- paste0(transientpath, "/", runid, "_", setting, "_", output, "_", 
                                  varname, "_transient_", out_mode, "_", timespan,
                                  depths_fname, "_", area, "_", 
                                  ifelse(out_mode == "csec_mean" && csec_conds_n > 0, 
                                         paste0("conds_", paste0(csec_cond_vars, ".", csec_conds, ".", 
                                                                 csec_cond_vals, csec_cond_units, collapse="_"), 
                                                "_"), ""),
                                  ifelse(out_mode == "csec_depth" && !is.null(csec_cond_depth),
                                         paste0("conds_", paste0(unique(csec_cond_depth), collapse="_"), "_"), ""),
                                  projection, ssh_aviso_correct_fname, p_ref_suffix, fname_suffix,
                                  ".nc")
            }
            
            ## remove already existing data to avoid ncdf error:
            ## Error in R_nc4_create: Permission denied (creation mode was 4096)
            if (T) {
                system(paste0("rm ", outname), ignore.stderr=T) # silent
            }

            if (verbose > 1) {
                message(paste0(indent, "Save transient ", out_mode, " (=out_mode) file (=outname):"))
                message(paste0(indent, indent, outname))
            }

            ## Set dimensions for transient nc file
            time_dim <- ncdim_def(name="time", 
                                  units="", 
                                  vals=time, 
                                  create_dimvar=T)
     
            if (any(out_mode == c("depth", "depthint", "depthmax"))) {
                depth_dim <- ncdim_def(name="depth", 
                                       units="", 
                                       vals=-interpolate_depths, 
                                       create_dimvar=T)
            }

            if (out_mode == "csec_depth") {
                depth_dim <- ncdim_def(name="depth", 
                                       units="", 
                                       vals=-csec_middle_depths, 
                                       create_dimvar=T)
                csec_lon_dim <- ncdim_def(name="csec_lon", 
                                          units="", 
                                          vals=csec_interp_points_vec[1,],
                                          create_dimvar=T)
                csec_lat_dim <- ncdim_def(name="csec_lat", 
                                          units="",
                                          vals=csec_interp_points_vec[2,],
                                          create_dimvar=T)
                csec_dist_dim <- ncdim_def(name="csec_dist", 
                                           units="",
                                           vals=csec_dist_vec,
                                           create_dimvar=T)
            }

            if (out_mode == "moc_depth") {
                depth_dim <- ncdim_def(name="depth",
                                       units="",
                                       vals=-interpolate_depths,
                                       create_dimvar=T)
                moc_reg_lat_dim <- ncdim_def(name="lat",
                                             units="",
                                             vals=moc_reg_lat,
                                             create_dimvar=T)
            }

            ## Set dimension variables for nc file
            time_var <- ncvar_def(name="timechar", 
                                  units=timeunit, 
                                  dim=time_dim,
                                  missval=-9999, 
                                  prec="integer")

            if (any(out_mode == c("depth", "depthint", "depthmax", 
                                        "csec_depth", "moc_depth"))) {
                depth_var <- ncvar_def(name="depthvec", 
                                       units="m", 
                                       dim=depth_dim,
                                       missval=9999, 
                                       prec="integer")
            }

            if (out_mode == "csec_depth") {
                csec_lon_var <- ncvar_def(name="csec_lon_vec", 
                                          units="degrees east", 
                                          dim=csec_lon_dim,
                                          missval=mv, 
                                          prec=prec)
                csec_lat_var <- ncvar_def(name="csec_lat_vec", 
                                          units="degrees north",
                                          dim=csec_lat_dim,
                                          missval=mv, 
                                          prec=prec)
                csec_dist_var <- ncvar_def(name="csec_dist_vec", 
                                           units="m",
                                           dim=csec_dist_dim,
                                           missval=mv, 
                                           prec=prec)
            }

            if (out_mode == "moc_depth") {
                moc_reg_lat_var <- ncvar_def(name="moc_reg_lat",
                                             units="degrees north",
                                             dim=moc_reg_lat_dim,
                                             missval=mv,
                                             prec=prec)
                moc_topo_var <- ncvar_def(name="moc_topo",
                                          units="#",
                                          dim=list(moc_reg_lat_dim, depth_dim),
                                          missval=mv,
                                          prec=prec)
            }

            ## Set data variables for nc file
            data_fun_var <- vector("list", l=dim(data_funi)[1]) # nvars
            if (out_mode == "max3D") {
                depth_var <- data_fun_var
            }

            if (any(out_mode == c("mean", "meanint", "sum", "max", "max3D", "min"))) {

                for (i in 1:length(data_fun_var)) {
                    name <- paste0(dimnames(data_funi)[[1]][i], "_", out_mode)
                    data_fun_var[[i]] <- ncvar_def(name=name,
                                                   units=units_out,
                                                   dim=time_dim,
                                                   missval=mv,
                                                   longname=paste0(longname, 
                                                                   ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                   prec=prec)
                }

                if (out_mode == "max3D") {
                    for (i in 1:length(depth_var)) {
                        name <- paste0(dimnames(data_funi)[[1]][i], "_", out_mode, "_depth")
                        depth_var[[i]] <- ncvar_def(name=name,
                                                    units="m",
                                                    dim=time_dim,
                                                    missval=-9999,
                                                    paste0(longname, ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                    prec="integer")
                    }
                }

            } # if mean sum max min etc

            if (any(out_mode == c("depth", "depthint", "depthmax"))) {
                for (i in 1:length(data_fun_var)) {
                    name <- paste0(dimnames(data_funi)[[1]][i], "_", out_mode)
                    data_fun_var[[i]] <- ncvar_def(name=name,
                                                   units=units_out,
                                                   dim=list(time_dim, depth_dim),
                                                   missval=mv,
                                                   longname=paste0(longname, 
                                                                   ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                   prec=prec)
                }
            } # depth depthmax

            if (any(out_mode == c("csec_mean", "csec_depth"))) {

                #data_fun_var <- vector("list", l=dim(data_funi)[1])
                for (i in 1:length(data_fun_var)) {
                    if (dimnames(data_funi)[[1]][i] == "u" ||
                        dimnames(data_funi)[[1]][i] == "v") {
                        unit <- "m s-1"
                    } else if (dimnames(data_funi)[[1]][i] == "temp") {
                        unit <- "degC"
                    } else if (dimnames(data_funi)[[1]][i] == "salt") {
                        unit <- "psu"
                    } else if (dimnames(data_funi)[[1]][i] == "potdens") {
                        unit <- "kg m-3"
                    } else if (dimnames(data_funi)[[1]][i] == "transport") {
                        if (out_mode== "csec_mean") { 
                            unit <- "Sv"
                        } else if (out_mode == "csec_depth") {
                            unit <- "m s-1"
                        }
                    } else {
                        stop(paste0("error: variable ", dimnames(data_funi)[[1]][i], 
                                    " not implemented yet ..."))
                    }
                
                    name <- paste0(dimnames(data_funi)[[1]][i], "_", out_mode)
                    if (out_mode == "csec_mean") {
                        data_fun_var[[i]] <- ncvar_def(name=name,
                                                       units=unit,
                                                       dim=time_dim, 
                                                       missval=mv,
                                                       longname=paste0(longname, 
                                                                       ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                       prec=prec)
                    } else if (out_mode == "csec_depth") {
                        data_fun_var[[i]] <- ncvar_def(name=name,
                                                       units=unit,
                                                       dim=list(csec_dist_dim, depth_dim, time_dim), 
                                                       missval=mv,
                                                       longname=paste0(longname, 
                                                                       ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                       prec=prec)
                    }
                    
                } # for i length(data_fun_var)

            } # csec_mean || csec_depth 
          
            if (out_mode == "moc_depth") {
                for (i in 1:length(data_fun_var)) {
                    name <- paste0(dimnames(data_funi)[[1]][i], "_", out_mode)
                    data_fun_var[[i]] <- ncvar_def(name=name,
                                                   units=units_out,
                                                   dim=list(moc_reg_lat_dim, depth_dim, time_dim),
                                                   missval=mv,
                                                   longname=paste0(longname, 
                                                                   ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                   prec=prec)
                }
            } # moc_depth

            ## Create nc file
            if (any(out_mode == c("mean", "meanint", "sum", "max", "min"))) {
                outnc <- nc_create(filename=outname,
                                   vars=c(data_fun_var, list(time_var)),
                                   force_v4=force_v4)
            }

            if (out_mode == "max3D") {
                outnc <- nc_create(filename=outname,
                                   vars=c(data_fun_var, depth_var, list(time_var)),
                                   force_v4=force_v4)
            }

            if (any(out_mode == c("depth", "depthint", "depthmax"))) {
                outnc <- nc_create(filename=outname, 
                                   vars=c(data_fun_var, 
                                          list(time_var, depth_var)),
                                   force_v4=force_v4)
            }

            if (out_mode == "csec_mean") {
                outnc <- nc_create(filename=outname, 
                                   vars=c(data_fun_var, list(time_var)),
                                   force_v4=force_v4)
            } # csec_mean

            if (out_mode == "csec_depth") {
                outnc <- nc_create(filename=outname, 
                                   vars=c(data_fun_var,
                                          list(time_var, depth_var, csec_lon_var,
                                               csec_lat_var, csec_dist_var)),
                                   force_v4=force_v4)
            } # csec_depth

            if (out_mode == "moc_depth") {
                outnc <- nc_create(filename=outname, 
                                   vars=c(data_fun_var, 
                                          list(time_var, depth_var, 
                                               moc_reg_lat_var, moc_topo_var)),
                                   force_v4=force_v4)
            } # moc_depth

            ## Put dimensions to transient nc file
            ncvar_put(outnc, time_var, timechar)

            if (any(out_mode == c("depth", "depthint", "depthmax"))) {
                ncvar_put(outnc, depth_var, -interpolate_depths)
            }

            if (out_mode == "max3D") {
                for (i in 1:length(depth_var)) {
                     ncvar_put(outnc, depth_var[[i]], -data_funi_depths[i,,1])
                }
            }

            if (out_mode == "csec_depth") {
                ncvar_put(outnc, depth_var, -csec_middle_depths)
                ncvar_put(outnc, csec_lon_var, csec_interp_points_vec[1,])
                ncvar_put(outnc, csec_lat_var, csec_interp_points_vec[2,])
                ncvar_put(outnc, csec_dist_var, csec_dist_vec)
            }

            if (out_mode == "moc_depth") {
                ncvar_put(outnc, depth_var, -interpolate_depths)
                ncvar_put(outnc, moc_reg_lat_var, moc_reg_lat)
                ncvar_put(outnc, moc_topo_var, moc_topo)
            }

            ## Put data to nc file
            for (i in 1:length(data_fun_var)) {
                
                if (out_mode == "csec_mean") {
                    ncvar_put(outnc, data_fun_var[[i]], data_funi[i,])
                
                } else if (any(out_mode == c("csec_depth", "moc_depth"))) {
                    ncvar_put(outnc, data_fun_var[[i]], data_funi[i,,,])
                
                } else { # mean sum max min etc
                    ncvar_put(outnc, data_fun_var[[i]], data_funi[i,,])
                
                }

            } # for i vars

            ## Put attributes to nc file
            ncatt_put(outnc, 0, "runid", runid)
            ncatt_put(outnc, 0, "setting", setting)
            ncatt_put(outnc, 0, "meshid", meshid)
            ncatt_put(outnc, 0, "time", timespan)
            ncatt_put(outnc, 0, "area", area)
            ncatt_put(outnc, 0, "projection", projection)
            ncatt_put(outnc, 0, "longitude_lims_deg", range(poly_geogr_lim_lon), prec="double")
            ncatt_put(outnc, 0, "latitude_lims_deg", range(poly_geogr_lim_lat), prec="double")
            if (dim_tag == "3D") {
                ncatt_put(outnc, 0, "depths_m", depths_plot, prec="double")
            }
            if (exists("res_node_min")) {
                ncatt_put(outnc, 0, paste0("resolution_min_", res_node_unit), res_node_min)
                ncatt_put(outnc, 0, paste0("resolution_max_", res_node_unit), res_node_max)
                ncatt_put(outnc, 0, paste0("resolution_median_simple_", res_node_unit), res_node_median_simple)
                ncatt_put(outnc, 0, paste0("resolution_mean_simple_", res_node_unit), res_node_mean_simple)
                ncatt_put(outnc, 0, paste0("resolution_mean_weigthed_", res_node_unit), res_node_mean_weighted)
                ncatt_put(outnc, 0, paste0("resolution_nominal_", res_node_unit), res_node_nominal)
            }
            if (regexpr("MOC", varname) != -1 && exists("moc_mask_file")) {
                ncatt_put(outnc, 0, "moc_mask", moc_mask_file)
            }
            if (p_ref_suffix != "") {
                ncatt_put(outnc, 0, paste0("p_ref", ifelse(p_ref != "in-situ", "_dbar", "")), p_ref)
            }
            if (any(varname == c("iceextent", "icevol"))) {
                if (!is.null(sic_cond_fname)) {
                    ncatt_put(outnc, 0, paste0("sic_thr_", sic_cond_fname, "_%"), sic_thr*100, prec="double")
                }
            }
            if (any(out_mode == c("csec_mean", "csec_depth"))) {
                ncatt_put(outnc, 0, "csec_n_vec_u", csec_n_vec_edge[1,], prec="double")
                ncatt_put(outnc, 0, "csec_n_vec_v", csec_n_vec_edge[2,], prec="double")
                
                if (out_mode == "csec_mean" && csec_conds_n > 0) {
                    for (i in 1:csec_conds_n) {
                        ncatt_put(outnc, 0, 
                                  paste0("csec_cond_", i, "_of_", csec_conds_n, "_", csec_cond_vars[i], "_", 
                                         csec_cond_units[i], ".", csec_conds[i], "."), 
                                  csec_cond_vals[i], prec="double")
                    }
                }
            }

            ## Close nc
            nc_close(outnc)

        } # end if out_mode == "mean" or != "mean"
    } # end if transient_out

    if (regular_transient_out) { # regular
        
        if (any(out_mode == c("area", "areadepth"))) {
            if (verbose > 1) {
                message(paste0(indent, "Save transient regular ", out_mode, " file: (=outname_reg)"))
                message(paste0(indent, indent, outname_reg))
            }
            nc_close(outnc_reg)
        
        } else if (!any(out_mode == c("area", "areadepth"))) {
            
            # nothing to do, this was checked in the beginning
        
        } # end if out_mode == area or not
    
    } # end if regular_transient_out

    if (transient_out || regular_transient_out) {
        #rm(data_node) 
    }

    if (verbose > 0) {
        message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
                                  " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
        message("==============================================")
    }

} # end if (nfiles > 0)

#stop("asd")

### Continue with ltm
if (any(plot_map, ltm_out, regular_ltm_out, moc_ltm_out, csec_ltm_out)) {
    
    if (verbose > 0) {
        if (nfiles > 0) {
            message(paste0("6) Calculate ", varname, " ltm over ", timespan, " ..."))
        } else {
            message(paste0("6) Calculate ", varname, " ..."))
        }
    }
   
    ## continue with already calculated data
    if (any(transient_out, regular_transient_out, rms_out, sd_out) && nfiles > 0) { # nfiles > 0 for not bathy, resolution, etc.

        # append sum(u*v) to sd if needed to only have one sd matrix
        if (sd_method == "ackermann83") {
            data_node_sd <- abind(data_node_sd, uv_sd, along=1, use.dnns=T)
            rm(uv_sd)
        }

    } # if any(transient_out, regular_transient_out, rms_out, sd_out)

    ## Calculate Mean for 'timespan' of already calculated data
    if (total_rec > 1) {

        ## ltm average starts here
        if (!rec_tag) {
            if (verbose > 1) {
                message(paste0(indent, "Divide by total_rec=", total_rec, " ..."))
            }
            data_node_ltm <- data_node_ltm/total_rec
            if (rms_out || sd_out) {
                data_node_sd <- data_node_sd/total_rec
            }
            if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                mld_node_ltm <- mld_node_ltm/total_rec
            }

        } else if (rec_tag) {

            ## 1) annual average
            if (nyears > 1) {

                if (leap_tag && any(is.leap(years))) {
                    nyears_leap <- length(which(is.leap(years)))
                    if (verbose > 1) {
                        message(paste0(indent, "Divide rec ", nrecspf_leap, " by nyears_leap=", nyears_leap, " ..."))
                    }
                    data_node_ltm[,,,nrecspf_leap] <- data_node_ltm[,,,nrecspf_leap]/nyears_leap # (var,node,time,depth)
                    if (rms_out || sd_out) {
                        data_node_sd[,,,nrecspf_leap] <- data_node_sd[,,,nrecspf_leap]/nyears_leap
                    }
                    if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                        mld_node_ltm[,,,nrecspf_leap] <- mld_node_ltm[,,,nrecspf_leap]/nyears_leap
                    }
                }

                if (verbose > 1) {
                    message(paste0(indent, "Divide recs (1,...,", nrecspf, ") by nyears=", nyears, " ..."))
                }
                data_node_ltm[,,,1:nrecspf] <- data_node_ltm[,,,1:nrecspf]/nyears
                if (rms_out || sd_out) {
                    data_node_sd[,,,1:nrecspf] <- data_node_sd[,,,1:nrecspf]/nyears
                }
                if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                    mld_node_ltm[,,,1:nrecspf] <- mld_node_ltm[,,,1:nrecspf]/nyears
                }

            } # if nyears > 1


            ## 2) average over all times of a year (e.g. months)
            tmp <- array(0,
                         dim=c(dim(data_node_ltm)[1:2], 1, 1),
                         dimnames=c(dimnames(data_node_ltm)[1:2],
                                    list(depth=depths_plot,
                                         time=timespan)))
            if (rms_out || sd_out) {
                tmp_sd <- array(0,
                                dim=c(dim(data_node_sd)[1:2], 1, 1),
                                dimnames=c(dimnames(data_node_sd)[1:2],
                                           list(depth=depths_plot,
                                                time=timespan)))
            }
            if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                    tmp_mld <- array(0, c(dim(mld_node_ltm)[1:2], 1, 1),
                                     dimnames=c(dimnames(mld_node_ltm)[1:2],
                                                list(depth=NULL,
                                                     time=timespan)))
            }

            if (verbose > 1) {
                message(paste0(indent, "Divide by nrecspf=", nrecspf, " ..."))
            }

            # sum over all times of a year (e.g. months)
            #message("data_node_ltm")
            for (i in 1:dim(data_node_ltm)[4]) { # nrecspf
                tmp[,,1,1] <- tmp[,,,1] + data_node_ltm[,,,i]
                #message(range(data_node_ltm[,,,i], na.rm=T))
                if (rms_out || sd_out) {
                     tmp_sd[,,,1] <- tmp_sd[,,,1] + data_node_sd[,,,i]
                }
                if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                    tmp_mld[,,,1] <- tmp_mld[,,,1] + mld_node_ltm[,,,i]
                }
            }

            # divide though the number of times of a year (e.g. months)
            # note: leap years were already taken into account before
            data_node_ltm <- tmp/nrecspf
            rm(tmp)
            if (rms_out || sd_out) {
                data_node_sd <- tmp_sd/nrecspf
                rm(tmp_sd)
            }
            if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
                mld_node_ltm <- tmp_mld/nrecspf
                rm(tmp_mld)
            }

        } # if rec_tag or not

        if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
            mld_node <- mld_node_ltm # for sub_vertical_integrate() function
        }

        ## Check data so far
        if (verbose > 2) {
            for (i in 1:dim(data_node_ltm)[1]) {
                message(indent, "   min/max data_node_ltm[", i, ":",
                        dimnames(data_node_ltm)[[1]][i], ",,,] = ",
                        paste0(range(data_node_ltm[i,,,], na.rm=T), collapse="/"),
                        " ", units_out)
            }
        }

    } # if total_rec > 1

    ## calc varname with ltm data if not calculated before (=not transient)
    if (!any(transient_out, regular_transient_out, rms_out, sd_out) || nfiles == 0) { # nfiles == 0 for bathy, resolution, etc.  

        if (integrate_depth && length(depths) == 2 && depths[2] == "MLD") {
            mld_node <- mld_node_ltm # for sub_vertical_integrate() function
        }

        ## Rotate vector components
        if (rotate_mesh && all(!!rotate_inds)) { # some of 'varname_fesom' needs to be rotated
            for (i in 1:(length(rotate_inds)/2)) {
                inds <- rotate_inds[c((i-1)*2+1,(i-1)*2+2)]
                if (verbose > 1) {
                    message(paste0(indent, "Rotate global ",
                                 varname_fesom[inds[1]], " and ",
                                 varname_fesom[inds[2]],
                                 " back to geographic coordinates ... "))
                }
                rotated_coords <- vec_rotate_r2g(Ealpha, Ebeta, Egamma, nod_x, nod_y,
                                                 data_node_ltm[inds[1],,,],
                                                 data_node_ltm[inds[2],,,], 1)
                data_node_ltm[inds[1],,1,1] <- rotated_coords$u
                data_node_ltm[inds[2],,1,1] <- rotated_coords$v
                rm(rotated_coords)
            }
        }

        ## Preparations1 before calculations
        if (verbose > 1) {
            message(paste0(indent, "Run ", subroutinepath, "/sub_prepare1.r ..."))
        }
        if (exists("tmp")) rm(tmp)
        sub_prepare1(data_node_ltm) # produces tmp
        if (exists("tmp")) {
            data_node_ltm <- tmp
            rm(tmp)
        }

        if (nfiles == 0) {
            if (!exists("data_node_ltm")) {
                message(indent, "Prepare matrix ...")
                data_node_ltm <- array(NA, 
                                       dim=c(1, 
                                             ifelse(dim_tag == "2D", nod2d_n, nod3d_n), 
                                             #1,
                                             1, 1),
                                       dimnames=list(var=varname, node=NULL,
                                                     depth=depths_plot, time=timespan))
            } else {
                stop("what?!?!?!")
                dimnames(data_node_ltm) <- list(var=varname, node=NULL,
                                                depth=depths_plot, time=timespan)
            }
        } # nfiles = 0
      
        
        ## At this point,
        ## dim(data_node_ltm) = c(nvars,nod2d_n,ndepths=1,nrecspf) if dim_tag == "2D"
        ## dim(data_node_ltm) = c(nvars,nod3d_n,ndepths=1,nrecspf) if dim_tag == "3D" 


        ## Save memory by depth averaging data if possible
        if (average_depth && nfiles > 0) {

            if (zave_method == 1) { # level-wise dz                        
                if (verbose > 1) { # rearrange first
                    message(paste0(indent, "Bring data_node_ltm from (nod3d_n=", nod3d_n,
                                 ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
                    if (verbose > 2) {
                        message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                    }
                }
                sub_n3_to_n2xde(data_node_ltm) # produces tmp
                data_vert_ltm <- tmp # dim(data_vert_ltm) = c(nvars,nod2d_n,ndepths,nrecspf)
                rm(tmp)

            } else { # which zave_method
                data_vert_ltm <- data_node_ltm # dim(data_vert_ltm) = c(nvars,nod2d_n,ndepths,nrecspf)
            }

            if (verbose > 1 && ndepths > 1) {
                message(paste0(indent, "Average over ", depths_plot, " m depths ..."))
                if (verbose > 2) {
                    message(paste0(indent, "   run ", subroutinepath, "/sub_vertical_average.r ..."))
                }
            }
            sub_vertical_average(data_vert_ltm) # prduces tmp
            data_node_ltm <- tmp # overwrite old data_node_ltm
            # if (zave_method == 1): dim(data_node_ltm) = c(nvars,nod2d_n,ndepths=1,nrecspf)
            # if (zave_method == 2): dim(data_node_ltm) = c(nvars,nod[23]d_n=1,ndepths=1,nrecspf=1) # special!
            rm(tmp)

        } # if average_depth

        ## Preparations2 before calculations e.g. calc rho, f, bathy, ... if needed
        #print(str(data_node_ltm))
        if (verbose > 1) {
            message(paste0(indent, "Run ", subroutinepath, "/sub_prepare2.r ..."))
        }
        indent_save <- indent; indent <- paste0(indent_save, "   ")
        sub_prepare2(data_node_ltm) # creates data_node
        if (exists("data_node")) {
            data_node_ltm <- data_node
            rm(data_node)
        }
        indent <- indent_save
        #print(str(data_node_ltm))

        if (csec_ltm_out) {
            if (verbose > 1) {
                message(paste0(indent, "For Cross section bring data_node_ltm from (nod3d_n=", nod3d_n,
                             ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
                if (verbose > 1) {
                    message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                }
            }
            sub_n3_to_n2xde(data_node_ltm) # produces tmp
            data_global_vert_ltm <- tmp # dim(data_vert_ltm) = c(nvars,nod2d_n,ndepths,nrecspf=1)
            rm(tmp, data_vert_ltm)
        }

        
        ## At this point,
        ## dim(data_node_ltm) = c(nvars,nod2d_n,ndepths=1,nrecspf) if 
        ##  (dim_tag == "2D") or (dim_tag == "3D" && average_depth && zave_method == 1)
        ## dim(data_node_ltm) = c(nvars,nod3d_n,ndepths=1,nrecspf) if 
        ##  (dim_tag == "3D" && !average_depth)
        ## dim(data_node_ltm) = c(nvars,nod_n=1,ndepths=1,nrecspf) if 
        ##  (dim_tag == "3D" && average_depth && zave_method == 2) # special!

        ## variable specific calculations
        if (verbose > 1) {
            message(paste0(indent, "Run ", subroutinepath, "/sub_calc.r ..."))
        }
        # ltm part
        if (exists("data_node")) stop("data_node: this should not happen")
        indent_save <- indent; indent <- paste0(indent_save, "   ")
        sub_calc(data_node_ltm) # data_node is result of sub_calc()
        if (exists("data_node")) {
            data_node_ltm <- data_node
            rm(data_node)
        }
        if (exists("tmp")) rm(tmp)

        #stop("asd")

        ## set first dimension name to varname if length = 1
        if (is.null(dimnames(data_node_ltm)[[1]]) ||
            (dim(data_node_ltm)[1] == 1 && dimnames(data_node_ltm)[[1]] != varname)) {
            dimnames(data_node_ltm)[1] <- list(var=varname)
        }

        ## Check data so far
        if (verbose > 2) {
            for (i in 1:dim(data_node_ltm)[1]) {
                message(paste0(indent, "   min/max data_node_ltm[", i, ":", 
                               dimnames(data_node_ltm)[[1]][i], ",,,] = ",
                               paste0(range(data_node_ltm[i,,,], na.rm=T), collapse="/")))
            }
        }

        ## integrate vertically
        if (integrate_depth && nfiles > 0) {
            if (verbose > 1) {
                message(paste0(indent, "Integrate between ", depths_plot, " m ..."))
                if (verbose > 2) {
                    message(paste0(indent, "Run ", subroutinepath, "/sub_vertical_integrate.r ..."))
                }
            }
            sub_vertical_integral(data_node_ltm) # produces tmp
            data_node_ltm <- tmp # dim(data_node_ltm) = c(nvars,nod2d_n,ndepths=1,nrecspf=1)
            rm(tmp)

        } # if integrate_depth

        ## Change to proper units if wanted
        if (multfac_out != 1) {
            for (i in 1:dim(data_node_ltm)[1]) {
                if (verbose > 0) {
                    message(paste0(indent, "Multiply data_node_ltm[", i, ":",
                                   dimnames(data_node_ltm)[[1]][i], ",,,] by multfac_out=",
                                   multfac_out, " ..."))
                }
                data_node_ltm[i,,,] <- data_node_ltm[i,,,]*multfac_out
                if (verbose > 0) {
                    message(paste0(indent, "   min/max data_node_ltm[", i, ":", 
                                   dimnames(data_node_ltm)[[1]][i], ",,,] = ",
                                   paste0(range(data_node_ltm[i,,,], na.rm=T), collapse="/"), " ", units_out))
                }
            }
        }

    } # calc variable with ltm data if !all(transient_out, regular_transient_out, sd_out))

    ## At this point
    # TODO
    ## dim(data_node_ltm) = c(nvars,nod2d_n,depths=1,nrecspf=1)
    ## is time-averaged and depth-averaged/-integrated 

    ## Calculate root mean square
    if (rms_out) {

        if (verbose > 1) {
            for (i in 1:dim(data_node_ltm)[1]) { # nvars
                message(paste0(indent, "Calc rms(", dimnames(data_node_ltm)[[1]][i], 
                             ") = sqrt( E[", dimnames(data_node_ltm)[[1]][i], 
                             "^2] ) ..."))
            }
        }
        # data_node_sd = E[X^2]
        data_node_rms <- sqrt(data_node_sd)
        dimnames(data_node_rms)[[1]] <- paste0(dimnames(data_node_ltm)[[1]], "_rms")
    
    } # if rms_out
    
    ## Calculate standard deviation
    if (sd_out) {

        if (sd_method == "default") { # population standard deviation
            if (verbose > 1) {
                for (i in 1:dim(data_node_ltm)[1]) { # nvars
                    message(paste0(indent, "Calc population sd(", dimnames(data_node_ltm)[[1]][i], 
                                 ") = sqrt( E[", dimnames(data_node_ltm)[[1]][i], 
                                 "^2] - E[", dimnames(data_node_ltm)[[1]][i], "]^2 ) ..."))
                }
            }
            # data_node_sd = E[X^2]; data_node_ltm = E[X]
            data_node_sd <- sqrt(data_node_sd - data_node_ltm^2)
            dimnames(data_node_sd)[[1]] <- paste0(dimnames(data_node_ltm)[[1]], "_sd")

        } else if (sd_method == "ackermann83") {

            if (verbose > 1) {
                message(indent, "Calc sd of vector speed\n",
                        indent, "  sd_s(x) = 1/S * sqrt[ mean(u)^2*var(u) + mean(v)^2*var(v) + 2*mean(u)*mean(v)*cov(u,v) ]   (7)\n",
                        indent, "and sd of vector direction\n",
                        indent, "   sd_d(x) = 1/(S^2) * sqrt[ mean(v)^2*var(u) + mean(u)^2*var(v) - 2*mean(u)*mean(v)*cov(u,v) ]   (11)\n",
                        indent, "with\n",
                        indent, "   S = sqrt(E[u]^2 + E[v]^2)   (2)\n",
                        indent, "   mean(u) = E[u]\n",
                        indent, "   var(u) = E[u^2] - E[u]^2\n",
                        indent, "   cov(u,v) = E[uv] - E[u]*E[v]\n",
                        indent, "from Ackermann (1983): Means and Standard Deviations of Horizontal Wind Components\n",
                        indent, "   https://doi.org/10.1175/1520-0450(1983)022<0959:MASDOH>2.0.CO;2")
            }
            
            uv_varinds <- c(1, 2)
            uv_varnames <- dimnames(data_node_ltm)
            uv_sq_varinds <- c(1, 2)
            cov_varinds <- which(regexpr("*", dimnames(data_node_sd)[[1]]) != -1 &
                                 regexpr("_cov", dimnames(data_node_sd)[[1]]) != -1)

            if (length(varinds) != 2) {
                stop(paste0("Which are the u,v components in dimnames(data_node_sd)[[1]]=c(",
                            paste0(dimnames(data_node_sd)[[1]], collapse=","), ")?"))
            }

            # data_node_ltm = E[u], E[v]; data_sd = E[u^2], E[v^2], E[u*v]
            uvar <- data_sd[1,,,] - udata^2
            if (any(uvar < 0)) {
                message(paste0("warning: variance(", varname_fesom[1], ") < 0."))
            }
            vvar <- data_sd[2,,,] - vdata^2
            if (any(uvar < 0)) {
                message(paste0("warning: variance(", varname_fesom[1], ") < 0."))
            }
            # sd of speed
            data_sd_s <- 1/data * sqrt( udata^2*uvar + vdata^2*vvar + 2*udata*vdata*data_sd[3,,,] )
            # sd of direction
            data_sd_d <- 1/(data^2) * sqrt( vdata^2*uvar + udata^2*vvar - 2*udata*vdata*data_sd[3,,,] )

        } # which sd_method

    } # sd_out

    ## append rms and/or sd to data to only have one matrix
    if (rms_out) {
        data_node_ltm <- abind(data_node_ltm, data_node_rms, along=1, use.dnns=T)
        rm(data_node_rms)
    }
    if (sd_out) {
        data_node_ltm <- abind(data_node_ltm, data_node_sd, along=1, use.dnns=T)
        rm(data_node_sd)
    }

    ## At this point
    ## dim(data_node_ltm) = c(nvars+sd(nvars),nod2d_n,depths=1,nrecspf=1)
    ## is time-averaged and depth-averaged/-integrated data and sd of data if wanted

    ## Arrange data_node_ltm (includes sd if rms_out || sd_out) as datamatrix
    if (plot_map 
        || (ltm_out && output_type == "elems") 
        || regular_ltm_out) {

        # rearrange from nod3d_n to nod2d_n x ndepths
        if (!average_depth && !integrate_depth) {
            if (dim_tag == "3D" && dim(data_node_ltm)[2] != nod2d_n && ndepths > 1) {
                if (verbose > 1) { # rearrange first
                    message(paste0(indent, "For regular interpolation bring data_node_ltm from (nod3d_n=", nod3d_n,
                                 ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
                    if (verbose > 2) {
                        message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                    }
                }
                sub_n3_to_n2xde(data_node_ltm) # produces tmp
                data_vert_ltm <- tmp # dim(data_vert_ltm) = c(nvars,nod2d_n,ndepths,nrecspf)
                rm(tmp)
            } else { # already in level space
                data_vert_ltm <- data_node_ltm
            
            } # dim_tag == "3D"
        
        } else {
            data_vert_ltm <- data_node_ltm

        } # if !average_depth && !integrate_depth

        if (verbose > 1) {
            message(paste0(indent, "For regular interpolation rearrange data_vert_ltm from (nod2d_n=", nod2d_n, 
                         " x ndepths=", dim(data_vert_ltm)[3], ") to (3 x elem2d_n=", 
                         elem2d_n, " x ndepths=", dim(data_vert_ltm)[3], ") ...")) 
        }

        if (verbose > 1) {
            message(paste0(indent, "Interpolate on regular grid ('regular_dx'=",
                         sprintf("%.3f", regular_dx), " deg,'regular_dy'=",
                         sprintf("%.3f", regular_dy),
                         " deg) and "))
            message(paste0(indent, "   select regular data in '", area, "' area: ",
                         round(range(map_geogr_lim_lon)[1], 2), " to ",
                         round(range(map_geogr_lim_lon)[2], 2) , " deg longitude and ",
                         round(range(map_geogr_lim_lat)[1], 2), " to ",
                         round(range(map_geogr_lim_lat)[2], 2), " deg latitude ..."))
    

            if (any(plot_map, ltm_out)) {
                message(indent, "   and for plot/ltm_out select irregular data in '", area, "' area from ", appendLF=F)
                if ((plot_map && plot_type == "interp") || 
                    (ltm_out && output_type == "nodes")) {
                    message("'data_vert_ltm':")
                } else if ((plot_map && plot_type == "const") ||
                           (ltm_out && output_type == "elems")) {
                    message("'data_elem_ltm':")
                }
                message(paste0(indent, "   ", round(range(map_geogr_lim_lon)[1], 2), " to ",
                             round(range(map_geogr_lim_lon)[2], 2), " deg longitude and ",
                             round(range(map_geogr_lim_lat)[1], 2), " to ",
                             round(range(map_geogr_lim_lat)[2], 2), " deg latitude ..."))
            }
        } # verbose
           
        # no depth loop needed here
        if (any(plot_map, ltm_out)) { 
            if ((plot_map && plot_type == "interp") || 
                (ltm_out && output_type == "nodes")) {
                datamat_ltm <- data_vert_ltm[,poly_node_inds_geogr,,]
                if (projection != "orthographic") {
                    if (geogr_lims) {
                        if (projection == "rectangular") {
                            xpsur <- xpsur[poly_node_inds_geogr]
                            ypsur <- ypsur[poly_node_inds_geogr]
                        }
                    }
                }
            }
        }

        # create progress bar
        if (dim(data_vert_ltm)[3] > 1) { # ndepths > 1
            pb <- mytxtProgressBar(min=0, max=dim(data_vert_ltm)[3], style=pb_style,
                                   char=pb_char, width=pb_width,
                                   indent=paste0(indent, "   ")) # 5 " " for default message()
        }

        for (di in 1:dim(data_vert_ltm)[3]) { # ndepths

            # old:
            #datamat_ltm[1,,,] <- data[,pos[elem2d[1,]],,]
            #datamat_ltm[2,,,] <- data[,pos[elem2d[2,]],,]
            #datamat_ltm[3,,,] <- data[,pos[elem2d[3,]],,]
            data_elem_ltm <- array(data_vert_ltm[,pos[elem2d],di,], 
                                   dim=c(dim(data_vert_ltm)[1],    # nvars
                                         3,                    # 3 nodes per element
                                         elem2d_n,             # elem2d_n
                                         1,                    # 1 depth
                                         dim(data_vert_ltm)[4]),   # nrecspf
                                   dimnames=c(dimnames(data_vert_ltm)[1],
                                              list(node=1:3, 
                                                   elem=NULL,
                                                   depth=dimnames(data_vert_ltm)[[3]][di]),
                                              dimnames(data_vert_ltm)[4]))

            ## Check data so far
            if (verbose > 2) {
                for (i in 1:dim(data_elem_ltm)[1]) {
                    message(paste0(indent, "   min/max data_elem_ltm[", i, ":", 
                                 dimnames(data_elem_ltm)[[1]][i], ",,,,] = ",
                                 paste0(range(data_elem_ltm[i,,,,], na.rm=T), collapse="/")))
                }
            } 

            ## Interpolation of ltm data on regular grid
            if (regular_ltm_out) {
                if (di == 1) { # initialize matrices
                    datamat_reg_ltm <- array(NA, 
                                         dim=c(dim(data_vert_ltm)[1],              # nvars
                                               length(xinds), length(yinds),   # x, y of _area_
                                               dim(data_vert_ltm)[3:4]),           # ndepths, nrecspf
                                         dimnames=c(dimnames(data_vert_ltm)[1],
                                                    list(xi=round(xi, 2), 
                                                         yi=round(yi, 2)),
                                                    dimnames(data_vert_ltm)[3:4]))
                }
               
                ## interpolate on regular grid
                for (i in 1:dim(data_elem_ltm)[1]) { # nvars
                    if (dim(data_elem_ltm)[1] > 1 && verbose > 2) {
                        message(paste0(indent, "   var = ", 
                                     dimnames(data_elem_ltm)[[1]][i], " ..."))
                    }

                    for (j in 1:dim(data_elem_ltm)[5]) { # nrecspf == 1 here (ltm)
                        if (dim(data_elem_ltm)[5] > 1 && verbose > 2) {
                            message(paste0(indent, "      time = ",
                                         dimnames(data_elem_ltm)[[5]][j], " ..."))
                        }
                   
                        # set values outside "area" to NA before interp
                        # this is possible due to patricks nice interp method
                        tmp <- drop(data_elem_ltm[i,,,,j])
                        nainds <- rep(T, t=elem2d_n)
                        if (!exists("poly_inds_geogr")) {
                            stop("need to update the elem index within area. still depends on used projection")
                        }
                        nainds[poly_inds_geogr] <- F
                        tmp[,nainds] <- NA
                        datamat_reg_ltm[i,,,di,j] <- t(sub_calc_regular_2d_interp(
                                                       I_MAT=IMAT[yinds,xinds], 
                                                       XI=XI[yinds,xinds], 
                                                       YI=YI[yinds,xinds],
                                                       xp=xc_global, yp=yc_global,
                                                       #datamat=drop(data_elem_ltm[i,,,,j])
                                                       datamat=tmp
                                                       ))
                    } # for j nrecspf == 1 here (ltm)
                } # for i nvars
                rm(tmp)
                
                ## Check data so far
                if (verbose > 2) {
                    for (i in 1:dim(datamat_reg_ltm)[1]) {
                        if (!all(is.na(datamat_reg_ltm[i,,,di,]))) {
                            message(paste0(indent, "   min/max datamat_reg_ltm[", i, ":", 
                                         dimnames(datamat_reg_ltm)[[1]][i], ",,,", di, ",] = ",
                                         paste0(range(datamat_reg_ltm[i,,,di,], na.rm=T), collapse="/")))
                        }
                    }
                }
            } # if regular_ltm_out

            ## Select data of irregular data for ltm_out/plot_map
            if (any(plot_map, ltm_out)) { 
                
                if ((plot_map && plot_type == "const") || 
                    (ltm_out && output_type == "elems")) {

                    if (di == 1) { # first depth

                        # save regular interpolated data in area
                        datamat_ltm <- array(NA, 
                                         dim=c(dim(data_vert_ltm)[1],           # nvars
                                               3,                           # 3 nodes per element
                                               length(poly_inds_geogr),     # 2d-elems in area
                                               dim(data_vert_ltm)[3:4]),        # ndepths,nrecspf
                                         dimnames=c(dimnames(data_vert_ltm)[1],
                                                    list(node=1:3,
                                                         elem=NULL),
                                                    dimnames(data_vert_ltm)[3:4]))
                    } # if di == 1

                    if (projection != "orthographic") {
                        if (proj_lims) {
                            datamat_ltm[,,,di,] <- data_elem_ltm[,,poly_inds_geogr,,]
                        } else if (geogr_lims) {
                            if (projection != "rectangular") {
                                datamat_ltm[,,,di,] <- data_elem_ltm[,,poly_inds_proj,,]
                            } else if (projection == "rectangular") {
                                datamat_ltm[,,,di,] <- data_elem_ltm[,,poly_inds_geogr,,]
                            }
                        } # if proj_lims or geogr_lims
                    } else if (projection == "orthographic") {
                        datamat_ltm[,,,di,] <- data_elem_ltm[,,poly_inds_proj,,]
                    } # which projection

                } # if plot_map && plot_type == "interp"
                
            } # if (any(plot_map, ltm_out))
            
            # update progress bar
            if (dim(data_vert_ltm)[3] > 1) {
                setTxtProgressBar(pb, di)
            }

        } # for di
        
        # close progress bar
        if (dim(data_vert_ltm)[3] > 1) {
            close(pb)
        }

        #rm(data_elem_ltm)

        ## Remove NA locations due to coordinate transformation
        if (length(na_inds) > 0) {
            if ((plot_map && plot_type == "interp") ||
                (ltm_out && output_type == "nodes")) {
                stop("update")

            } else if ((plot_map && plot_type == "const") ||
                       (ltm_out && output_type == "elems")) {
                datamat_ltm <- datamat_ltm[,,-na_inds,,]
            }
        }

        ## Check data so far
        if (verbose > 1) {
            for (i in 1:dim(datamat_ltm)[1]) { # nvars
                message(indent, "   min/max datamat_ltm[", i, ":", dimnames(datamat_ltm)[[1]][i], appendLF=F)
                if ((plot_map && plot_type == "interp") ||
                    (ltm_out && output_type == "nodes")) {
                    message(",,,] = ", paste0(range(datamat_ltm[i,,,], na.rm=T), collapse="/"), appendLF=F)
                } else if ((plot_map && plot_type == "const") ||
                           (ltm_out && output_type == "elems")) {
                    message(",,,,] = ", paste0(range(datamat_ltm[i,,,,], na.rm=T), collapse="/"), appendLF=F)
                }
                message(" ", units_out)
            }
        }
        
    } # if plot_map || (ltm_out && output_type == "elems") || regular_ltm_out

    ## At this point
    # TODO
    ## dim(data_node_ltm) = c(nvars,nod2d_n,depths=1,nrecspf=1)
    ## dim(datamat_reg_ltm) = c(nvars,nx_in_area,ny_in_area,ndepths=1,nrecspf=1)
    ## dim(datamat_ltm) = c(nvars,nnodes_in_area,ndepths=1,nrecspf=1)
    ##  if ((plot_map && plot_type == "interp") || output_type == "nodes")
    ## dim(datamat_ltm) = c(nvars,3,nelems_in_area,ndepths=1,nrecspf=1
    ##  if ((plot_map && plot_type == "const") || output_type == "elems")

    ## ltm output start
    if (ltm_out) { # irregular

        if (nfiles > 0) {
            outname <- paste0(ltmpath, "/", runid, "_", setting, "_", output, "_",
                              varname, "_ltm_", out_mode, "_", timespan, depths_fname, "_", area, "_", 
                              projection, "_", output_type,
                              ssh_aviso_correct_fname, 
                              p_ref_suffix, fname_suffix, ".nc")
        } else if (nfiles == 0) {
            outname <- paste0(ltmpath, "/", runid, "_", setting, "_",
                              varname, "_ltm_", out_mode, depths_fname, "_", area, "_",
                              projection, "_", output_type,
                              ssh_aviso_correct_fname, 
                              p_ref_suffix, fname_suffix, ".nc")
        } # if nfiles > 0

        ## remove already existing data to avoid ncdf error:
        ## Error in R_nc4_create: Permission denied (creation mode was 4096)
        if (T) {
            system(paste0("rm ", outname), ignore.stderr=T) # silent
        }

        ## nc out
        if (verbose > 1) {
            message(paste0("   Save irregular ltm file ('outname'):"))
            message(paste0("      ", outname))
        }

        stop("add depth dim")

        if (output_type == "elems") {
            node_dim <- ncdim_def(name="nodespe", 
                                  units="#", 
                                  vals=1:3, 
                                  create_dimvar=T)
            elem_dim <- ncdim_def(name="elem2d", 
                                  units="#", 
                                  vals=1:dim(datamat_ltm)[3], 
                                  create_dimvar=T)
            dim_list <- list(node_dim, elem_dim)
        
        } else if (output_type == "nodes") {
            node_dim <- ncdim_def(name="nod2d", 
                                  units="",
                                  vals=1:dim(datamat_ltm)[2], 
                                  create_dimvar=F)
            dim_list <- node_dim
        } # output_type "elems" or "nodes"
       
        if (F) { # old 
            xp_var <- ncvar_def(name="xp", units="degrees_east", 
                                dim=dim_list, 
                                missval=mv, prec=prec)
            yp_var <- ncvar_def(name="yp", units="degrees_north", 
                                dim=dim_list, 
                                missval=mv, prec=prec)
        }

        data_var <- vector("list", l=dim(datamat_ltm)[1])
        for (i in 1:length(data_var)) {
            name <- dimnames(datamat_ltm)[[1]][i]
            data_var[[i]] <- ncvar_def(name=name, 
                                       units=units_out, 
                                       dim=dim_list,
                                       missval=mv, 
                                       longname=paste0(longname, 
                                                       ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                       prec=prec)
        }

        if (F) { #
            outnc <- nc_create(filename=outname, 
                               vars=c(list(xp_var, yp_var), data_var), 
                               force_v4=force_v4)
        } else {
            outnc <- nc_create(filename=outname,
                               vars=data_var,
                               force_v4=force_v4)
        }

        for (i in 1:length(data_var)) {
            if (output_type == "elems") {
                ncvar_put(outnc, data_var[[i]], drop(datamat_ltm[i,,,1,1]))
            } else if (output_type == "nodes") {
                ncvar_put(outnc, data_var[[i]], drop(datamar_ltm[i,,1,1]))
            } # if output_type
        }

        if (F) { # old 
            if (output_type == "elems") {
                ncvar_put(outnc, xp_var, xp)
                ncvar_put(outnc, yp_var, yp)
            } else if (output_type == "nodes") {
                ncvar_put(outnc, xp_var, xpsur)
                ncvar_put(outnc, yp_var, ypsur)
            }
        }

        ncatt_put(outnc, 0, "runid", runid)
        ncatt_put(outnc, 0, "setting", setting)
        ncatt_put(outnc, 0, "meshid", meshid)
        ncatt_put(outnc, 0, "time", timespan)
        ncatt_put(outnc, 0, "area", area)
        ncatt_put(outnc, 0, "projection", projection)
        ncatt_put(outnc, 0, "longitude_lims_deg", range(poly_geogr_lim_lon), prec=prec)
        ncatt_put(outnc, 0, "latitude_lims_deg", range(poly_geogr_lim_lat), prec=prec)
        if (dim_tag == "3D") {
            ncatt_put(outnc, 0, "depths_m", depths_plot, prec="double")
        }
        if (p_ref_suffix != "") {
            ncatt_put(outnc, 0, paste0("p_ref", ifelse(p_ref != "in-situ", "_dbar", "")), p_ref)
        }

        nc_close(outnc)

    } # if ltm_out

    if (regular_ltm_out) {
        if (nfiles > 0) {
            outname_reg_ltm <- paste0(reg_ltm_outpath, "/", runid, "_", setting, "_", output, "_",
                                  varname, "_ltm_", out_mode, "_", timespan, depths_fname, "_", area, 
                                  "_", projection, "_regular_dx", 
                                  sprintf("%.3f", regular_dx), "_dy",
                                  sprintf("%.3f", regular_dy), 
                                  ssh_aviso_correct_fname, 
                                  p_ref_suffix, fname_suffix, 
                                  ".nc")
        } else if (nfiles == 0) {
            outname_reg_ltm <- paste0(reg_ltm_outpath, "/", runid, "_", setting, "_",
                                  varname, "_ltm_", out_mode, depths_fname, "_", area,
                                  "_", projection, "_regular_dx",
                                  sprintf("%.3f", regular_dx), "_dy",
                                  sprintf("%.3f", regular_dy), 
                                  ssh_aviso_correct_fname, 
                                  p_ref_suffix, fname_suffix,
                                  ".nc")
        } # if nfiles > 0

        ## remove already existing data to avoid ncdf error:
        ## Error in R_nc4_create: Permission denied (creation mode was 4096)
        if (T) {
            system(paste0("rm ", outname_reg_ltm), ignore.stderr=T) # silent
        }

        ## nc out
        if (verbose > 1) {
            message(paste0("   Save regular ", out_mode, " (=out_mode) ltm file:"))
            message(paste0("      ", outname_reg_ltm, " (=outname_reg_ltm)"))
        }
        
        if (F) { # old
            xi_dim <- ncdim_def(name="nxi", units="", 
                                vals=xi, create_dimvar=T)
            yi_dim <- ncdim_def(name="nyi", units="", 
                                vals=yi, create_dimvar=T)

            xi_var <- ncvar_def(name="xi", units="degrees_east", 
                                dim=xi_dim, 
                                missval=mv, prec=prec)
            yi_var <- ncvar_def(name="yi", units="degrees_north", 
                                dim=yi_dim, 
                                missval=mv, prec=prec)
        } else {
            lon_dim <- ncdim_def(name="lon", units="degree_east", vals=xi)
            lat_dim <- ncdim_def(name="lat", units="degree_north", vals=yi)
            if (dim(datamat_reg_ltm)[4] > 1) { # ndepths
                depth_dim <- ncdim_def(name="depth", units="m",
                                       vals=-interpolate_depths)
            }
            
        } # old 
        
        datamat_reg_var <- vector("list", l=dim(datamat_reg_ltm)[1]) # nvars
        for (i in 1:length(datamat_reg_var)) {
            name <- dimnames(datamat_reg_ltm)[[1]][i]
            if (dim(datamat_reg_ltm)[4] > 1) { # ndepths {
                datamat_reg_var[[i]] <- ncvar_def(name=name, 
                                                  units=units_out, 
                                                  dim=list(lon_dim, lat_dim, depth_dim),
                                                  missval=mv, 
                                                  longname=paste0(longname, 
                                                                  ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                  prec=prec)
            } else {
                datamat_reg_var[[i]] <- ncvar_def(name=name, 
                                                  units=units_out, 
                                                  dim=list(lon_dim, lat_dim),
                                                  missval=mv, 
                                                  longname=paste0(longname, 
                                                                  ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                                  prec=prec)
            }
        }
        
        regular_nc <- nc_create(filename=outname_reg_ltm,
                                vars=datamat_reg_var,
                                force_v4=force_v4)
        for (i in 1:length(datamat_reg_var)) {
            ncvar_put(regular_nc, datamat_reg_var[[i]], drop(datamat_reg_ltm[i,,,,1]))
        }
        
        ncatt_put(regular_nc, 0, "setting", setting)
        ncatt_put(regular_nc, 0, "meshid", meshid)
        ncatt_put(regular_nc, 0, "time", timespan)
        ncatt_put(regular_nc, 0, "area", area)
        if (dim_tag == "3D") {
            ncatt_put(regular_nc, 0, "depths_m", depths_plot, prec="double")
        }
        ncatt_put(regular_nc, 0, "regular_dx", sprintf("%.3f", regular_dx))
        ncatt_put(regular_nc, 0, "regular_dy", sprintf("%.3f", regular_dy))
        if (p_ref_suffix != "") {
            ncatt_put(regular_nc, 0, paste0("p_ref", ifelse(p_ref != "in-situ", "_dbar", "")), p_ref)
        }

        nc_close(regular_nc)

    } # if regular_ltm_out

    # ltm moc output
    if (moc_ltm_out) {

        moc_outname <- paste0(ltmpath, "/", runid, "_", setting, "_", output, "_",
                              varname, "_ltm_", timespan, depths_fname, "_", area,
                              p_ref_suffix, fname_suffix, ".nc")

        ## remove already existing data to avoid ncdf error:
        ## Error in R_nc4_create: Permission denied (creation mode was 4096)
        if (T) {
            system(paste0("rm ", moc_outname), ignore.stderr=T) # silent
        }

        ## nc out
        if (verbose > 1) {
            message(paste0("   Save regular MOC file: ('moc_outname')"))
            message(paste0("      ", moc_outname))
        }
       
        # remove possible redundant latitudes and bottom depths with no values
        moc_reg_lat <- moc_reg_lat_global
        if (T) {
            lat_na_inds <- which(apply(moc_topo, 1, function(x) all(x == 1)))
            depth_na_inds <- which(apply(moc_topo, 2, function(x) all(x == 1)))
            if (length(lat_na_inds) > 0) {
                moc_reg_lat <- moc_reg_lat[-lat_na_inds]
                data_node_ltm <- data_node_ltm[,-lat_na_inds,,]
                moc_topo <- moc_topo[-lat_na_inds,]
            }
            if (length(depth_na_inds) > 0) {
                data_node_ltm <- data_node_ltm[,,-depth_na_inds,]
                moc_topo <- moc_topo[,-depth_na_inds]
            }
        }

        # improve this: only use depths where MOC has data
        interpolate_depths <- interpolate_depths[1:dim(data_node_ltm)[3]]
        
        depth_dim <- ncdim_def(name="depth",
                               units="",
                               vals=-interpolate_depths,
                               create_dimvar=T)
        moc_reg_lat_dim <- ncdim_def(name="lat",
                                     units="",
                                     vals=moc_reg_lat,
                                     create_dimvar=T)
        depth_var <- ncvar_def(name="depthvec",
                               units="m",
                               dim=depth_dim,
                               missval=9999,
                               prec="integer")
        moc_reg_lat_var <- ncvar_def(name="moc_reg_lat",
                                     units="degrees north",
                                     dim=moc_reg_lat_dim,
                                     missval=mv,
                                     prec=prec)
        moc_topo_var <- ncvar_def(name="moc_topo",
                                  units="#",
                                  dim=list(moc_reg_lat_dim, depth_dim),
                                  missval=mv,
                                  prec=prec)
        data_fun_var <- vector("list", l=dim(data_node_ltm)[1])
        for (i in 1:length(data_fun_var)) {
            name <- paste0(dimnames(data_node_ltm)[[1]][i], "_", out_mode)
            data_fun_var[[i]] <- ncvar_def(name=name,
                                           units=units_out,
                                           dim=list(moc_reg_lat_dim, depth_dim),
                                           missval=mv,
                                           longname=paste0(longname, 
                                                           ifelse(subtitle == "", "", paste0(", ", subtitle))),
                                           prec=prec)
        }

        ## Create out nc
        outnc <- nc_create(filename=moc_outname, 
                           vars=c(data_fun_var,
                                  list(depth_var, moc_reg_lat_var, moc_topo_var)),
                           force_v4=force_v4)
        ncvar_put(outnc, depth_var, -interpolate_depths)
        ncvar_put(outnc, moc_reg_lat_var, moc_reg_lat)
        ncvar_put(outnc, moc_topo_var, moc_topo)
        for (i in 1:length(data_fun_var)) { # only 1 var, ltm: only 1 rec
            ncvar_put(outnc, data_fun_var[[i]], drop(data_node_ltm[i,,,1]))
        }

        ## Put attributes to nc file
        ncatt_put(outnc, 0, "runid", runid)
        ncatt_put(outnc, 0, "setting", setting)
        ncatt_put(outnc, 0, "meshid", meshid)
        ncatt_put(outnc, 0, "time", timespan)
        ncatt_put(outnc, 0, "area", area)
        ncatt_put(outnc, 0, "longitude_lims_deg", range(map_geogr_lim_lon), prec="double")
        ncatt_put(outnc, 0, "latitude_lims_deg", range(map_geogr_lim_lat), prec="double")
        if (exists("moc_mask_file")) {
            ncatt_put(outnc, 0, "moc_mask", moc_mask_file)
        }
        if (dim_tag == "3D") {
            ncatt_put(outnc, 0, "depths_m", depths_plot, prec="double")
        }
        if (p_ref_suffix != "") {
            ncatt_put(outnc, 0, paste0("p_ref", ifelse(p_ref != "in-situ", "_dbar", "")), p_ref)
        }

        ## Close nc
        nc_close(outnc) 

    } # if moc_ltm_out


    if (csec_ltm_out) {

        stop("not imeplemented yet")

    } # if csec_ltm_out


    if (verbose > 0) {
        message(paste0("   elapsed total: ", round((proc.time() - ptm)[3], 2),
                     " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
        message("==============================================")
    }


    ## 7) Plot
    if (plot_map) { 
       
        ## At this point
        # TODO
        ## dim(datamat_ltm) = c(nvars,nnodes_in_area,ndepths=1,nrecspf=1)
        ##  if ((plot_map && plot_type == "interp") || output_type == "nodes")
        ## dim(datamat_ltm) = c(nvars,3,nelems_in_area,ndepths=1,nrecspf=1
        ##  if ((plot_map && plot_type == "const") || output_type == "elems")

        if (verbose > 0) {
            if (nfiles > 0) {
                message(paste0("7) Plot ltm (timespan=", timespan, ") ", 
                               varname, " (", longname,
                             ") in ", area, " area ..."))  
            } else {
                message(paste0("7) Plot ", varname, " (", longname,
                             ") in ", area, " area ..."))  
            }
        }
        indent <- "   "

        ## 1 plot for every ltm variable
        nplots <- dim(datamat_ltm)[1]
        var_names <- dimnames(datamat_ltm)[[1]]

        for (ploti in 1:nplots) {
            
            varnamei <- dimnames(datamat_ltm)[[1]][ploti]
            if (verbose > 0) {
                message(paste0(indent, "Open plot device for '", varnamei, "' ..."))
            }

            if (plot_type == "interp") {
                # dim(datamat_ltm) =  c(nvars,nnodes_in_area,ndepths,nrecspf=1)
                z <- drop(datamat_ltm[ploti,,,]) # dim = c(nnodes_in_area)
                if (length(dim(z)) != 1) { # dim = c(nnodes_in_area,ndepths)
                    message(indent, "   Note: 'datamat_ltm' has depth dim. For plot, use 1st depth = ", 
                            interpolate_depths[1], "m")
                    depths_fname <- paste0("_", interpolate_depths[1], "m")
                    z <- drop(z[,1]) # dim = nnodes_in_area
                }

            } else if (plot_type == "const") {
                # dim(datamat_ltm) = c(nvars,3,nelems_in_area,ndepths,nrecspf=1)
                z <- drop(datamat_ltm[ploti,,,,]) # dim = c(3,nelems_in_area)
                if (length(dim(z)) != 2) { # dim = c(3,nelems_in_area,ndepths)
                    message(indent, "   Note: 'datamat_ltm' has depth dim. For plot, use 1st depth = ", 
                            interpolate_depths[1], "m")
                    depths_fname <- paste0("_", interpolate_depths[1], "m")
                    z <- drop(z[,,1]) # dim = c(3,nelems_in_area)
                } else if (is.null(dim(z))) { # in case of only one element make a matrix again
                    z <- array(z, c(1, 3))
                }

            } # which plot_type

            if (!dir.exists(paste0(plotpath, "/", varname))) {
                dir.create(paste0(plotpath, "/", varname), recursive=T, showWarnings=F)
            }
            plotname <- paste0(plotpath, "/", varname, "/",  
                               runid, "_", setting, "_", varnamei,
                               timespan_fname, depths_fname, "_", area, "_", 
                               projection, p_ref_suffix, fname_suffix, "_", plot_type, ".", plot_file)
            
            if (plot_file == "png") {
                png(plotname, 
                    width=plot_size[1], height=plot_size[2], 
                    res=dpi, bg=bg_col,
                    family=font_family)
            
            } else if (plot_file == "pdf") {
                pdf(plotname, width=plot_size[1]/dpi, 
                    height=plot_size[2]/dpi, family=font_family)#,pointsize=14)
            }

            par(oma=c(2,2,2,2))
            if (projection == "rectangular") {
                par(mar=c(5,4,4,6)) # leave some space on right border for legend
            } else {
                par(mar=c(5,3,4,6))
            }

            # TO DO:
            # Switch from -180:180 to 0:360 degrees longitude 
            # in case of longitudes crossing the -180 deg 
            # longitude line coming from the east.
            continentdata <- "world"
            if (projection == "rectangular") {
                if (cyclic_plot) {
                    message("need to update that")
                    map_geogr_lim_lon <- c(map_geogr_lim_lon[1], map_geogr_lim_lon[2] + 360)
                    long_inds <- which(xp < 0, arr.ind=T)
                    xp[long_inds] <- xp[long_inds] + 360
                }
            }
            #

            if (xyaxis_labels) {
                xlab <- expression(paste("Longitude [", degree, "]"))
                ylab <- expression(paste("Latitude [", degree, "]"))
            } else {
                xlab <- ""; ylab <- ""
            }

            ## Open plot
            if (projection == "rectangular") {
                # Note: For whatever reason, the default projection 'rectangular' in map() sometimes does 
                # strange things -_- Need to leave out map()-parameters 'proj', 'orient', and 'par' in 
                # case of rectangular projection.

                if (area != "global") {
                    # FIX THAT
                    poly_extreme_coords <- c(max(xp[,which(xp == min(xp), arr.ind=T)[,2]]), # left
                                            min(xp[,which(xp == max(xp), arr.ind=T)[,2]]), # right
                                            max(yp[,which(yp == min(yp), arr.ind=T)[,2]]), # bottom
                                            min(yp[,which(yp == max(yp), arr.ind=T)[,2]])) # top
                    #poly_extreme_coords <- c(min(xp), max(xp), min(yp), max(yp))

                    plot(0, 0, xlim=poly_extreme_coords[1:2], ylim=poly_extreme_coords[3:4], t="n", 
                         xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
            
                } else if (area == "global") {
                    plot(0, 0, xlim=range(map_geogr_lim_lon), ylim=range(map_geogr_lim_lat), t="n",
                         xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
                }
                plot_coords <- par("usr")
                x_at <- pretty(plot_coords[1:2], n=10) #30)#10)
                y_at <- pretty(plot_coords[3:4], n=10) #30)#10)
                x_lab <- x_at
                if (any(regexpr("\\.", x_lab) != -1)) { # there are decimal 
                    pos <- regexpr("\\.", x_lab)
                    inds <- which(pos != -1)
                    pos <- pos[inds]
                    tmp <- max(nchar(substr(x_lab[inds], pos + 1, nchar(x_lab[inds]))))
                    x_lab <- sprintf(paste0("%.", tmp, "f"), x_lab)
                }
                y_lab <- y_at
                if (any(regexpr("\\.", y_lab) != -1)) { # there are decimal 
                    pos <- regexpr("\\.", y_lab)
                    inds <- which(pos != -1)
                    pos <- pos[inds]
                    tmp <- max(nchar(substr(y_lab[inds], pos + 1, nchar(y_lab[inds]))))
                    y_lab <- sprintf(paste0("%.", tmp, "f"), y_lab)
                }

                axis(1, at=x_at, labels=x_lab)
                axis(2, at=y_at, labels=y_lab, las=2)
                mtext(side=1, xlab, line=2.5)
                mtext(side=2, ylab, line=3)

            } else if (projection == "stereographic") {
                success <- load_package("maps")
                if (!success) stop(helppage)
                
                map(continentdata, t="n", 
                    proj=projection, orient=orient, par=projection_par,
                    xlim=range(map_geogr_lim_lon), ylim=range(map_geogr_lim_lat))
                mtext(side=1, xlab, line=2)
                mtext(side=2, ylab, line=1)

            } else if (projection == "orthographic") {
                success <- load_package("maps")
                if (!success) stop(helppage)

                xyp <- map(continentdata, 
                           proj=projection, orient=orient, par=projection_par, 
                           interior=F, bg=bg_col)
                # this generates warning, ok
            }
            usr <- par("usr")

            ## Add title to plot
            if (plot_title) {
                cex.main <- 0.7
                if (verbose > 1) {
                    message(paste0(indent, "   Add title to plot (plot_title=T) ..."))
                }
                if (nfiles > 0) {
                    if (dim_tag == "2D") {
                        title(paste0(runid, " ", setting, " ", varnamei, " ", timespan, 
                                     " ", area), 
                              cex.main=cex.main)
                    } else if (dim_tag == "3D") {
                        title(paste0(runid, " ", setting, " ", varnamei, " ", timespan, 
                                     " ", depths_plot, " m ", area), 
                              cex.main=cex.main)
                    }
                } else if (nfiles == 0) {
                    title(paste0(runid, " ", setting, " ", varnamei, " ", area), 
                          cex.main=cex.main)
                }

                ## Add subtitle to plot  
                if (nchar(subtitle) > 0) mtext(subtitle, line=0.5)

            } # if plot_title

            ## Add landmasses
            if (plot_type == "const") {
                if (continentborders) {
                    if (verbose > 1) {
                        message(paste0(indent, "   Add continentborders from built-in ", 
                                     continentdata, " to plot ..."))
                        message(paste0(indent, "   Note: Thats *not* the way FESOM sees land..."))
                    }
                    
                    success <- load_package("maps")
                    if (!success) stop(helppage)

                    if (projection == "rectangular") {
                        
                        if (fill_continent) {
                            map(continentdata, 
                                xlim=range(map_geogr_lim_lon), 
                                ylim=range(map_geogr_lim_lat),
                                fill=T, col=land_col, border=landborder_col, add=T)
                        } else {
                            map(continentdata, 
                                xlim=range(map_geogr_lim_lon), 
                                ylim=range(map_geogr_lim_lat),
                                fill=F, col=landborder_col, add=T)
                        }
                        
                        if (area == "pacific") {
                            if (fill_continent) {
                                map("world2", regions="USA:Alaska", 
                                    fill=fill, col=land_col, border=landborder_col, add=T)
                            } else {
                                map("world2", regions="USA:Alaska", col=landborder_col, add=T)
                            }
                        }

                    } else if (projection != "rectangular") {
                        map(continentdata, proj=projection, orient=orient, par=projection_par,
                            add=T, fill=T, col=land_col, border=landborder_col)
                    }

                } else if (!continentborders) {
                    if (verbose > 1) {
                        message(paste0(indent, "   Add continents as FESOM sees them by coloring",
                                     " the whole plot plane with 'land_col' (continentborders=F) ..."))
                    }
                    # Create artifical landmass, as FESOM sees it (fill the complete plot plane
                    # with land_col and overlay the data polygons).
                    if (projection == "rectangular" || projection == "stereographic") {
                        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
                             col=land_col, border=NA)

                    } else if (projection == "orthographic") {
                        xyp <- na.omit(data.frame(do.call(cbind, xyp[c("x","y")])))
                        # draw a circle around the points for coloring the ocean
                        polygon(max(xyp$x)*sin(seq(0,2*pi,length.out=100)), 
                                max(xyp$y)*cos(seq(0,2*pi,length.out=100)),
                                col=land_col, border=land_col)
                    }
                } # if continentborders or not

                if (fill_ocean) {
                    if (verbose > 1) {
                        message(paste0(indent, "   Distinguish continents from oceans by",
                                     " coloring all 2D elements with 'ocean_col' ..."))
                    }
                    xc_global_vec <- as.vector(rbind(xc_global, NA))
                    yc_global_vec <- as.vector(rbind(yc_global, NA))
                    if (projection == "rectangular") {
                        polygon(xc_global_vec, yc_global_vec, col=ocean_col, border=NA)
                    } else if (projection != "rectangular") {
                        tmp <- mapproject(xc_global_vec, yc_global_vec, projection=projection, 
                                          orientation=orient, par=projection_par)
                        polygon(tmp$x, tmp$y, col=ocean_col, border=NA)
                    }
                }
            } # if plot_type == "const"


            ## Make Colorbar
            zlim_orig <- range(z, na.rm=T)
            zlim <- zlim_orig 

            if (verbose > 1) {
                message(paste0(indent, "   min/max of z within ", area, " view = ",
                               paste0(zlim_orig, collapse="/"), " ", units_out))
            }

            ## interpolate data for plot
            if (plot_type == "interp") {

                ## spatial interpolation using akima::interp
                if (interp_method == "bilinear") {
                    interp_method_plot <- "Bilinear"
                } else if (interp_method == "bicubuc_spline") {
                    interp_method_plot <- "Bicubic spline"
                }

                if (F) { # for testing
                    interp_dlon_plot <- "auto"
                    interp_dlat_plot <- "auto"
                }

                if (interp_dlon_plot == "auto") {
                    interp_dx_plot <- mean(resolution[poly_inds_geogr])
                    if (resolution_unit == "m") {
                        # dx to dlon
                        interp_dlon_plot <- m2lon(dm=interp_dx_plot, alat=mean(ypsur))
                        interp_dx_plot <- round(interp_dx_plot/1000) # km
                        interp_dx_unit <- "km"
                    } else {
                        stop("not defined")
                    }
                    if (verbose > 1) {
                        message(paste0(indent, "   'interp_dlon_plot'='auto' (namelist.plot.r):"))
                        message(paste0(indent, "      mean(resolution) = ", interp_dx_plot, " ",
                                     interp_dx_unit, " ~ ", round(interp_dlon_plot, 3),
                                     " deg lon at average lat ", round(mean(ypsur), 3), " deg; see m2lon())"))
                    }
                } else {
                    if (!is.numeric(interp_dlon_plot)) {
                        stop("'interp_dlon_plot' must be a number.")
                    }
                } # if interp_dx_plot == "auto" or not

                if (interp_dlat_plot == "auto") {
                    interp_dy_plot <- mean(resolution[poly_inds_geogr])
                    if (resolution_unit == "m") {
                        # dy to dlat
                        interp_dlat_plot <- m2lat(dm=interp_dy_plot, alat=mean(ypsur))
                        interp_dy_plot <- round(interp_dy_plot/1000) # km
                        interp_dy_unit <- "km"
                    } else {
                        stop("not defined")
                    }
                    if (verbose > 1) {
                        message(paste0(indent, "   'interp_dlat_plot'='auto' (namelist.plot.r):"))
                        message(paste0(indent, "      mean(resolution) = ", interp_dy_plot, " ",
                                     interp_dy_unit, " ~ ", round(interp_dlat_plot, 3),
                                     " deg lat at average lat ", round(mean(ypsur), 3), " deg; see m2lat())"))
                    }
                } else {
                    if (!is.numeric(interp_dlat_plot)) {
                        stop("'interp_dlat_plot' must be a number.")
                    }
                } # if interp_dy_plot == "auto" or not

                ## The matlab routine write_mesh_subset.m may introduces
                ## some NaNs in the new sub_mesh.
                ## These need to be removed before the interpolation
                if (ploti == 1) {
                    xpsur_save <- xpsur
                    ypsur_save <- ypsur
                }
                if (any(is.na(z))) {
                    xpsur <- xpsur_save[-which(is.na(z))]
                    ypsur <- ypsur_save[-which(is.na(z))]
                    z <- z[-which(is.na(z))]
                } else {
                    xpsur <- xpsur_save
                    ypsur <- ypsur_save
                }

                xo <- seq(min(xpsur), max(xpsur), b=interp_dlon_plot)
                yo <- seq(min(ypsur), max(ypsur), b=interp_dlat_plot)
                if (verbose > 0) {
                    message(paste0(indent, "   'plot_type'='interp':"))
                    message(paste0(indent, "      ", interp_method_plot, " (='interp_method') interpolation of ",
                                 length(z), " 2D nodes on (nx,ny) = (", length(xo), ",", length(yo), ") regular"))
                    message(paste0(indent, "      ('interp_dlon_plot','interp_dlat_plot') = (",
                                 sprintf("%.3f", interp_dlon_plot), ",",
                                 sprintf("%.3f", interp_dlat_plot),
                                 ") deg grid using akima::interp() ..."))
                }
                interp <- akima::interp(x=xpsur, y=ypsur, z=z,
                                        xo=xo, yo=yo,
                                        linear=ifelse(interp_method == "bilinear", T, F))
                if (F) {
                    if (ploti == 1) interp1 = interp
                    if (ploti == 2) interp2 = interp
                    if (ploti == 3) interp3 = interp
                }

                zlim_interp <- range(interp$z, na.rm=T)
                if (verbose > 1) {
                    message(paste0(indent, "   min/max of ", interp_method, " interpolated data = ",
                                   paste0(zlim_interp, collapse="/"), " ", units_out))
                }
                zlim <- zlim_interp # this is the new zlim!
                
            } # if plot_type == "interp"

            ## Change to proper units
            if (multfac_plot != 1) {
                if (verbose > 1) {
                    message(paste0(indent, "   Multiply z by multfac_plot=",
                                   multfac_plot, " ..."))
                }
                if (plot_type == "interp") {
                    interp$z <- interp$z*multfac_plot
                    zlim <- range(interp$z, na.rm=T)
                } else {
                    z <- z*multfac_plot
                    zlim <- range(z, na.rm=T)
                }
                if (verbose > 1) {
                    message(paste0(indent, "   min/max of z = ",
                                   paste0(zlim, collapse="/"), " ", units_plot))
                }
            } # if multfac_plot != 1


            ## create colorbar
            if (verbose > 1) message(indent, "   Make colorbar using image.plot.pre() ...")

            # overwrite defaults of image.plot.pre() defined in namelist.plot.r
            user_levels_exist <- eval(parse(text=paste0("exists('", varnamei, "_levels')")))
            if (user_levels_exist) { 
                zlevels <- eval(parse(text=paste0(varnamei, "_levels")))
                if (any(!is.numeric(zlevels)) || any(!is.finite(zlevels)) ||
                    (all(min(zlevels) > zlim[2]) || all(max(zlevels) < zlim[1]))) {
                    warning("Do not use your povided '", 
                            varnamei, "_levels'=", "\n", 
                            paste0(zlevels, collapse=","), "\n", 
                            "since ", 
                            ifelse(any(!is.numeric(zlevels)) || any(!is.finite(zlevels)), 
                                   "there are non-numeric or non-finite values included.",
                                   paste0("they are out of range of the actual data min/max=", 
                                          zlim[1], "/", zlim[2])),
                            ".")
                    zlevels <- NULL
                } else {
                    message(indent, "   Use your provided ", varnamei, "_levels=\n",
                            indent, "      ", paste0(round(zlevels, 4), collapse=","))
                    nlevels <- length(zlevels)
                }
            }
            
            user_cols_exist <- eval(parse(text=paste0("exists('", varnamei, "_cols')")))
            if (user_cols_exist) {
                cols <- eval(parse(text=paste0(varnamei, "_cols")))
                message(indent, "   Use your provided ", varnamei, "_cols=\"\n",
                        indent, "      ", paste0(cols, collapse="\",\""), "\"")
                if (!is.null(zlevels) && length(cols) != length(zlevels) - 1) {
                    warning("Reorganize your provided ", varnamei, "_cols=\"", "\n",
                            paste0(user_cols, collapse="\",\""), "\"\n",
                            " since length(", varnamei, "_cols)=", length(cols),
                            " but length(", varnamei, "_levels)=", length(zlevels), ".\n", 
                            "There must be one color less than levels. ...")
                }
            }

            user_palname_exist <- eval(parse(text=paste0("exists('", varnamei, "_palname')")))
            if (user_palname_exist) {
                if (!is.null(cols)) {
                    warning("Do not use your provided palname=", palname, 
                            " since you provided ", varnamei, "_cols.")
                } else {
                    palname <- eval(parse(text=paste0(varnamei, "_palname")))
                    message(indent, "   Use your provided ", varnamei, "_palname=", palname)
                }
            }

            # create color bar
            # source("lib/functions/image.plot.pre.")
            ip <- image.plot.pre(zlim=zlim, nlevels=nlevels, max_labels=max_labels, 
                                 zlevels=zlevels, cols=cols, 
                                 palname=palname,  colors_script=colors_script,
                                 method=method, power_min=power_min,
                                 axis.labels=axis.labels, axis.round=axis.round,
                                 axis.zoom=axis.zoom, axis.addzlims=axis.addzlims,
                                 anom_colorbar=anom_colorbar, center_include=center_include,
                                 verbose=F)

            if (verbose > 1) {
                if (!user_levels_exist) {
                    message(indent, "   Note: you can define your own color levels with e.g.:\n",
                            indent, "      ", varnamei, "_levels <- c(", paste0(ip$axis.labels, collapse=","), ")\n",
                            indent, "      in namelist.var.r.")
                }
                if (!user_palname_exist && !user_cols_exist) {
                    message(indent, "   Note: you can define your own colors with e.g.:\n",
                            indent, "      ", varnamei, "_palname <- \"plasma\" (run color_function() for a demo of available color palettes) or\n", 
                            indent, "      ", varnamei, "_cols <- c(\"", paste0(ip$cols, collapse="\",\""), "\")\n",
                            indent, "      in namelist.var.r.")
                }
            }

            # for testing
            #source("~/scripts/r/mylevels.r")

            if (verbose > 1) {
                message(paste0(indent, "   min/max of color levels = ",
                               paste0(range(ip$levels), collapse="/"), " ", units_plot))
            }
            
            ## Add fesom data to plot
            if (plot_type == "interp") {

                if (F) {
                    message("super slow")
                    xc_lims <- apply(xc_global, 2, range)
                    yc_lims <- apply(yc_global, 2, range)
                    coords <- expand.grid(xo, yo)
                    inds <- rep(F, t=dim(coords)[1])
                    for (i in 1:dim(coords)[1]) {
                        xinds <- which(coords[i,1] >= xc_lims[1,] & coords[i,1] <= xc_lims[2,])
                        yinds <- which(coords[i,2] >= yc_lims[1,] & coords[i,2] <= yc_lims[2,])
                        if (length(xinds) > 0 && length(yinds) > 0) {
                            xyinds <- intersect(xinds, yinds) 
                            if (length(xyinds) > 0) {
                                #message(paste0(i, ": ", coords[i,1], ", ", coords[i,2]))
                                #message(xyinds)
                                inds[i] <- T
                                #progress_function(dim(coords)[1], i, indent=paste0(indent, "      "))
                            }
                        }
                    }
                    interp$z[inds] <- NA
                    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
                         col=land_col, border=NA)
                } # if F
                
                # update zlims to include possibly zlims outside 'zlim' due to interpolation
                # improve this
                breaks <- ip$levels
                if (zlim_interp[1] < breaks[1]) {
                    breaks[1] <- zlim_interp[1]
                }
                if (zlim_interp[2] > breaks[length(breaks)]) {
                    breaks[length(breaks)] <- zlim_interp[2]
                }
                image(interp, col=ip$cols, breaks=breaks,
                      add=T, useRaster=T)

                # add artifical continents
                # to do: only fesoms land
                if (T) {
                    success <- load_package("maps")
                    if (!success) {
                        if (verbose > 0) {
                            message(paste0("Cannot add continents from 'maps' package for 'plot_type=interp' ..."))
                        }
                    } else if (success) {
                        map("world", add=T, fill=T, col=land_col, border=landborder_col)
                    }
                } 

            } else if (plot_type == "interp2") { # testing

                ## Interpolate every polygon (triangle) to regular data.
                # Here, this is done with polygon(). In Matlab this is done with patch()
                # Note: Polygon borders make the plot more blurry in png, set border to NA.
                #       In case of pdf, use the same polygon color for its edge. 
                # https://stackoverflow.com/questions/11777124/three-way-color-gradient-fill-in-r/35687638#35687638
                # https://stackoverflow.com/questions/38725375/matlabs-patch-for-r-fill-polygon-with-color-gradient
                packages <- c("splancs", "phonR")
                for (i in 1:length(packages)) {
                    success <- load_package(packages[i])
                    if (!success) stop(helppage)
                }
                rm(packages)

                interp_res <- 100 # length of interp points of 1 mesh element
                message(paste0(indent, "   Add ", dim(z)[2],
                             " interpolated (interp_res=", interp_res, 
                             ") data polygons to plot ..."))

                #stop("asd")

                # for every 2d element
                cc <- system.time({
                for (i in 1:dim(z)[2]) {
                    
                    #progress_function(dim(z)[2], i, indent=paste0(indent, "      "))
                    
                    x <- xp[,i]
                    y <- yp[,i]
                    z <- z[,,i,1,1]
                    #x <- c(1,2,3)
                    #y <- c(1,2,1)
                    #z <- c(101, 1, 30)
                    #x <- xp[,67]
                    #y <- yp[,67]
                    #z <- datamat[,67]
                    vertices <- cbind(x,y,z)
                    interp_x <- seq(range(x)[1], range(x)[2], l=interp_res)
                    interp_y <- seq(range(y)[1], range(y)[2], l=interp_res)
                    interp_grid <- expand.grid(x=interp_x, y=interp_y)
                    interp_grid$z <- NA
                    interp_grid_indices <- splancs::inpip(interp_grid, vertices[,1:2], bound=T)
                    interp_grid$z[interp_grid_indices] <- with(interp_grid[interp_grid_indices,], 
                                                               phonR:::fillTriangle(x, y, vertices))
                    image(interp_x, interp_y, matrix(interp_grid$z, nrow=length(interp_x)),
                          col=cols, breaks=levels, 
                          add=T, useRaster=T)
                    bla = F
                    if (bla) {
                        image.plot(interp_x, interp_y, matrix(interp_grid$z, nrow=length(interp_x)), 
                                   col=cols, breaks=levels, xlab="Longitude", ylab="Latitude")
                        vertice_cols <- rep("NA", t=3)
                        for (j in 1:3) {
                            # UPDATE THAT    
                            tmp <- which(abs(levels - z[j]) == min(abs(levels - z[j])))[1]
                            if (tmp == ncolors+1) tmp <- ncolors # taking care for values at zlim
                            vertice_cols[j] <- cols[tmp]
                        }
                        segments(vertices[,1], vertices[,2], vertices[c(2,3,1),1], vertices[c(2,3,1),2])
                        points(x, y, pch=21, bg=vertice_cols, cex=2)
                        abline(h=interp_y, lty=3, col="gray")
                        abline(v=interp_x, lty=3, col="gray")
                        text(interp_x, rep(interp_y[1], t=interp_res), 1:interp_res)
                        text(rep(interp_x[1], t=interp_res), interp_y, 1:interp_res)
                        legend("topright", "original mesh element", lty=1, col="black", bty="n")
                    } # end if bla
                } # for i elements
                }) # cc system.time

            } else if (plot_type == "const") {

                ## Add variable polygon-wise to plot using polygon()
                ## The mean of 3 nodes is taken as the polygon value.
                
                # # mean over elements
                data_mean_vec <- apply(z, 2, mean, na.rm=T)

                # arrange all polygons in 1 vector (polygons seperated by a row of NAs)
                xpvec <- as.vector(rbind(xp, NA))
                ypvec <- as.vector(rbind(yp, NA))

                # find levels in data
                col_inds_vec <- findInterval(data_mean_vec, ip$levels, all.inside=T)

                if (verbose > 1) {
                    message(paste0(indent, "   Add ", dim(z)[2],
                                 " constant data polygons to plot (plot_type='const') ..."))
                }
                    
                if (plot_file == "png") {
                    polygon(xpvec, ypvec, col=ip$cols[col_inds_vec], border=elem_border_col)
                } else if (plot_file == "pdf") {
                    if (!is.na(elem_border_col)) {
                        polygon(xpvec, ypvec, col=ip$cols[col_inds_vec], border=ip$cols[col_inds_vec])
                    } else {
                        polygon(xpvec, ypvec, col=ip$cols[col_inds_vec], border=elem_border_col)
                    }
                }

                ## for debugging:
                #map("world", xlim=c(-70, -40), ylim=c(55, 65))
                #polygon(xpvec, ypvec, col=cols[col_inds_vec], border=NA)
                #points(xpvec, ypvec, pch=".", cex=1)

            } # which plot_type


            ## Add zonal & meridional component quivers
            if (uv_out && quiver_tag) {
                
                stop("update")

                if (quiver_mode == 1) {
                    ## 1 quiver for every node:
                    lonquiv <- as.vector(xp)
                    latquiv <- as.vector(yp)
                    uquiv <- as.vector(datamat_ltm[var_inds[ploti],,,,])
                    vquiv <- as.vector(datamat_ltm[var_inds[ploti],,,,])
                    hvelquiv <- as.vector(datamat[var_inds[ploti],,,,])
                } else if (quiver_mode == 2) {
                    ## 1 quiver for every element:
                    lonquiv <- apply(xp, 2, mean)
                    latquiv <- apply(yp, 2, mean)
                    uquiv <- apply(datamat_ltm[var_inds[ploti],,,,], 2, mean)
                    vquiv <- apply(datamat_ltm[var_inds[ploti],,,,], 2, mean)
                    hvelquiv <- apply(datamat[var_inds[ploti],,,,], 2, mean)
                } else if (quiver_mode == 3) {
                    ## 1 quiver for every degree intverall 'quiver_degree_intervall':
                    londegs <- seq(range(map_geogr_lim_lon)[1], 
                                   range(map_geogr_lim_lon)[2], 
                                   b=quiver_degree_intervall)
                    latdegs <- seq(range(map_geogr_lim_lat)[1], 
                                   range(map_geogr_lim_lat)[2], 
                                   b=quiver_degree_intervall)
                    lonquiv_all <- as.vector(xp)
                    latquiv_all <- as.vector(yp)
                    uquiv_all <- as.vector(datamat_ltm[var_inds[ploti],,,,])
                    vquiv_all <- as.vector(datamat_ltm[var_inds[ploti],,,,])

                    inds_all <- c()
                    lonquiv <- inds_all
                    latquiv <- inds_all
                    uquiv <- inds_all
                    vquiv <- inds_all
                    for (i in 1:length(latdegs)) {
                    #for (i in 1:1) {
                        lati <- which(latquiv_all >= latdegs[i]-(quiver_degree_intervall/2) & 
                                      latquiv_all <= latdegs[i]+(quiver_degree_intervall/2))
                        #message(length(lati))
                        for (j in 1:length(londegs)) {
                        #for (j in 1:2) {
                            inds <- which(lonquiv_all[lati] >= londegs[j]-(quiver_degree_intervall/2) & 
                                          lonquiv_all[lati] <= londegs[j]+(quiver_degree_intervall/2))
                            #message(paste0("  ", length(inds)))
                            #points(mean(lonquiv_all[lati[inds]]), mean(latquiv_all[lati[inds]]), cex=2)
                            lonquiv <- c(lonquiv, mean(lonquiv_all[lati[inds]]))
                            latquiv <- c(latquiv, mean(latquiv_all[lati[inds]]))
                            uquiv <- c(uquiv, mean(uquiv_all[lati[inds]]))
                            vquiv <- c(vquiv, mean(vquiv_all[lati[inds]]))
                        }
                    }
                    # Set NaN to NA
                    lonquiv[is.nan(lonquiv)] <- NA
                    latquiv[is.nan(lonquiv)] <- NA
                    uquiv[is.nan(uquiv)] <- NA
                    vquiv[is.nan(vquiv)] <- NA
                    # Calculate norm of u/ and v- component
                    hvelquiv <- sqrt(uquiv^2 + vquiv^2)
                } # end if quiver_mode == 1,2,3
                
                # Apply velocity threshold for quivers
                if (quiver_thr != 0) {
                    quiver_thr_inds <- which(hvelquiv < quiver_thr)
                    if (length(quiver_thr_inds) > 0) {
                        lonquiv <- lonquiv[-quiver_thr_inds]
                        latquiv  <- latquiv[-quiver_thr_inds]
                        uquiv <- uquiv[-quiver_thr_inds]
                        vquiv <- vquiv[-quiver_thr_inds]
                        hvelquiv <- hvelquiv[-quiver_thr_inds]
                    }
                }

               #u=ureg_dat[quiv_plot_inds]/
               #   (sqrt(ureg_dat[quiv_plot_inds]^2 +
               #         vreg_dat[quiv_plot_inds]^2)),
               #v=vreg_dat[quiv_plot_inds]/
               #   (sqrt(ureg_dat[quiv_plot_inds]^2 +
               #         vreg_dat[quiv_plot_inds]^2)),

               # Plot quivers
               if (verbose > 1) {
                   message(indent, "   Add ", length(which(!is.na(hvelquiv))), " quivers ", appendLF=F)
                   if (quiver_thr != 0) {
                       message(">= ", quiver_thr, " ", units_out, appendLF=F)
                       if (multfac_plot != 1) {
                           message(" x ", base^-power_plot, appendLF=F)
                       }
                   }
                   if (quiver_mode == 1) {
                        message(" at every node to plot ...")
                   } else if (quiver_mode == 2) {
                        message(" at every element to plot ...")
                   } else if (quiver_mode == 3) {
                        message(" every ", quiver_degree_intervall, " deg to plot ...")
                   }
               }
               
               quiver_norm <- F
               if (quiver_norm) {
                   arrows(x0=lonquiv, y=latquiv,
                          x1=lonquiv + quiver_scale_fac*uquiv, latquiv + quiver_scale_fac*vquiv,
                          angle=quiver_arrow_edge_angle, length=quiver_arrow_edge_length)
                } else {
                   arrows(lonquiv, latquiv, 
                          lonquiv + quiver_scale_fac*uquiv, latquiv + quiver_scale_fac*vquiv, 
                          angle=quiver_arrow_edge_angle, length=quiver_arrow_edge_length)
                   
                   # Add quiver legend
                   map_extreme_coords <- par("usr")
                   arrows(map_extreme_coords[2], 
                          map_extreme_coords[3] - 2*0.01*diff(map_extreme_coords[3:4]),
                          map_extreme_coords[2] + quiver_scale_fac*quiver_legend_velocity, 
                          map_extreme_coords[3] - 2*0.01*diff(map_extreme_coords[3:4]), 
                          length=quiver_arrow_edge_length, angle=quiver_arrow_edge_angle, xpd=T)
                   text(map_extreme_coords[2], 
                        map_extreme_coords[3] - 7*0.01*diff(map_extreme_coords[3:4]),
                        substitute(paste(quiver_legend_velocity, " m ", unit^-1), 
                                   list(quiver_legend_velocity=quiver_legend_velocity, unit="s")), 
                        xpd=T, adj=c(0,0)) # left aligned text
               }
            } # end if uv_out

            ## Add isobaths
            if (nisobaths > 0) {
                stop("update")
                isobath_cols <- colorRampPalette(c("black", "gray100"))(nisobaths) 
                for (i in 1:nisobaths) {
                    isobath <- isobaths[i]
                    isobath_xy_file <- paste0(meshpath, "/mesh_", meshid, "_",
                                              isobath, "_m_isobath_xy.txt")
                    isobath_xycont_file <- paste0(meshpath, "/mesh_", meshid, "_",
                                                  isobath, "_m_isobath_xycont.txt")
                    if (file.exists(isobath_xycont_file)) {
                        if (verbose == 2 || verbose == 3) {
                            message(paste0("      Add ", isobath, " m isobath to plot ..."))
                        }
                        isobath_coords <- read.table(isobath_xycont_file)
                        # remove points outside plotarea
                        message("Need to update that")
                        iso_area_inds <- splancs::inpip(cbind(isobath_coords$V1, isobath_coords$V2), 
                                                        cbind(rep(map_geogr_lim_lon, e=2), 
                                                              c(map_geogr_lim_lat, rev(map_geogr_lim_lat))))
                        if (runid == "CbSCL") {
                            lines(isobath_coords[iso_area_inds,], lwd=2, col=isobath_cols[i])
                        } else {
                            points(isobath_coords[iso_area_inds,], pch=".", col=isobath_cols[i], cex=2.5)
                        }
                    } else {
                        message(paste0("      Cannot add ", isobath, 
                                     " m isobath to plot: Run script once again with 'drawbathy=T'"))
                    }
                }
                legend("topright", legend=paste0(isobaths, "m"), lty=1,
                                  lwd=2, col=isobath_cols, bty="n")
            }

            ## Add grid lines to plot
            # Note: unfortunately, the labeling of map.grid() is pretty ugly
            if (plot_grid) {
                if (verbose > 1) {
                    message(paste0(indent, "   Add grid to plot (plot_grid=T)..."))
                }
                success <- load_package("maps")
                if (!success) stop(helppage)

                if (projection == "rectangular") {
                    if (area == "pacific") {
                        m <- map("world2", plot=F)
                    } else {
                        m <- map("world", plot=F)
                    }
                    #map.grid(m, pretty=T, col=rgb(0,0,0,0.5), lty=3, labels=grid_labels)
                    #map.grid(c(map_geogr_lim_lon, map_geogr_lim_lat), pretty=T, col=rgb(0,0,0,0.5), lty=3, labels=grid_labels)
                    abline(v=x_at, lty=3, col=rgb(0,0,0,0.5))
                    abline(h=y_at, lty=3, col=rgb(0,0,0,0.5))

                } else if (projection == "stereographic") {
                    m <- map("world", plot=F)
                    if (area == "na2") {
                        nx=50; ny=nx
                    } else if (area == "arctic") {
                        nx=18; ny=20
                    } else {
                        nx=15; ny=15
                    }
                    map.grid(m, nx=nx, ny=ny, col=rgb(0,0,0,0.5), lty=3, labels=grid_labels)

                } else if (projection == "orthographic") {
                    map.grid(pretty=T, col=rgb(0,0,0,0.5), lty=3, labels=grid_labels)
                }
            }

            ## Fill outer space of artifical box with white
            if (projection == "stereographic") {
                if (F) {
                    if (geogr_lims) {
                        map_extreme_coords <- par("usr") # c(left, right, bottom, top)
                        #map_extreme_coords <- c(-180, 180, -90, 90)
                        if (projection == "rectangular") {
                            if (area == "global") {
                                map_extreme_coords <- par("usr") # c(left, right, bottom, top)
                            } else if (area == "lsea") {
                                map_extreme_coords <- c(-180, 180, -90, 90) # c(left, right, bottom, top)
                            }
                        }
                        poly_extreme_coords <- map_extreme_coords 
                        for (i in 1:4) {
                            if (i == 1) { # bottom polygon
                                xxlim <- c(map_extreme_coords[1], map_extreme_coords[1], 
                                           map_extreme_coords[2], map_extreme_coords[2])
                                yylim <- c(map_extreme_coords[3], poly_extreme_coords[3], 
                                           poly_extreme_coords[3], map_extreme_coords[3])
                            } else if (i == 2) { # left polygon
                                xxlim <- c(map_extreme_coords[1], map_extreme_coords[1], 
                                           poly_extreme_coords[1], poly_extreme_coords[1])
                                yylim <- c(map_extreme_coords[3], map_extreme_coords[4], 
                                           map_extreme_coords[4], map_extreme_coords[3])
                            } else if (i == 3) { # top polygon
                                xxlim <- c(map_extreme_coords[1], map_extreme_coords[1], 
                                           map_extreme_coords[2], map_extreme_coords[2])
                                yylim <- c(poly_extreme_coords[4], map_extreme_coords[4], 
                                           map_extreme_coords[4], poly_extreme_coords[4])
                            } else if (i == 4) { # right polygon
                                xxlim <- c(poly_extreme_coords[2], poly_extreme_coords[2], 
                                           map_extreme_coords[2], map_extreme_coords[2])
                                yylim <- c(map_extreme_coords[3], map_extreme_coords[4], 
                                           map_extreme_coords[4], map_extreme_coords[3])
                            }
                        polygon(xxlim, yylim, col="white", border="white")
                        }
                    }
                } # F
            } # end if (projection == "stereographic")

            ## Add grid labels manually
            # Note: unfortunately not provided by map.grid for projected coordinates
            #if (plot_grid && projection == "stereographic") {
            #    for (i in 1:length(grid_coord_lons)) {
            #        lab <- paste0(grid_coord_labs[i], grid_coord_hemi[i]) 
            #	    text(mapproject(grid_coord_lons[i], grid_coord_lats[i], projection=projection, 
            #			            orientation=orient, par=projection_par), lab, xpd=T, cex=1)
            #    }
            #}

            # for testing
            #source("~/scripts/r/myplotstuff.r")
 

            ## Add box
            if (projection == "rectangular") box()
            if (projection == "stereographic" && geogr_lims) box()
            #if (projection == "orthographic") box()

            ## Add colorbar label
            if (plot_file == "png") {
                cex.axis <- 1.2
            } else if (plot_file == "pdf") {
                cex.axis <- 1
            }
            if (!horizontal) {
                mtext(side=4, line=7,
                      var_label_plot, cex=cex.axis)
                legend.mar <- par("mar")[4] + 0.1
            } else {
                line_colorbar_text <- 7.5
                legend.mar <- par("mar")[1] - 5
                mtext(side=1, line=line_colorbar_text,
                      var_label_plot, cex=cex.axis)
            }



            # add colorbar
            image.plot(zlim=ip$zlim,
                       legend.only=T,
                       col=ip$cols,
                       horizontal=horizontal,
                       breaks=1:ip$nlevels,
                       legend.mar=legend.mar,
                       axis.args=list(cex.axis=cex.axis,
                                      at=ip$axis.at.ind,
                                      labels=ip$axis.labels))

            ## Close plot
            dev.off()
            if (verbose > 1) {
                message(paste0(indent, "   Save plot:"))
                message(paste0(indent, "      ", plotname))
            }
        
        } # end for ploti=1:nplots

        if (verbose > 0) {
            message(paste0(indent, "elapsed total: ", round((proc.time() - ptm)[3], 2),
                         " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
            message("==============================================")
        }
    } # if plot_map

} else {
    
    if (verbose > 0) {
        message(paste0("   elapsed total: ", round((proc.time() - ptm)[3], 2),
                     " sec (", round((proc.time() - ptm)[3]/60, 2), " min)"))
        message("==============================================")
    }

} #if any(plot_map, ltm_out, regular_ltm_out, moc_ltm_out, csec_ltm_out)

