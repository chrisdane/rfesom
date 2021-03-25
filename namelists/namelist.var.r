###########################
## User input for rfesom ##
###########################

## Variable-specific things. 
# Check e.g. lib/sub_calc.r how variable-specific calculations are done.

## Defaults 
varname_nc <- NULL
longname <- "longname"
units_out <- "units_out"
units_plot <- "units_plot"
base <- 10
power_out <- 0
multfac_out <- base^power_out # -> 10^0 = 1 
power_plot <- 0
multfac_plot <- base^power_plot # for nicer colorbar range
var_label_plot <- varname
subtitle <- "" # plotted and attached as nc description if not ""
horiz_deriv_tag <- F
rotate_inds <- F
vec <- F
insitudens_tag <- buoyancy_insitudens_tag <- buoyancy_frequency_insitudens_tag <- F
potdens_tag <- buoyancy_potdens_tag <- buoyancy_frequency_potdens_tag <- F
coriolis_tag <- F
fname_suffix <- ""
p_ref_suffix <- ""
diagsuffix <- ""
typesuffix <- ""
csec_cond_vars <- NULL
csec_conds <- NULL
csec_cond_vals <- NULL
csec_cond_units <- NULL

## Overwrite defaults with variable-specific info
if (varname == "tos") { # fesom 1.4
    longname <- "Sea Surface Temperature"
    units_out <- units_plot <- "degC"
    var_label_plot <- expression(paste("SST [", degree, "C]"))
    varname_nc <- "tos"

} else if (varname == "tso") { # fesom 1.4
    longname <- "Sea Surface Temperature snapshot"
    units_out <- units_plot <- "degC"
    var_label_plot <- expression(paste("SST snapshot [", degree, "C]"))
    varname_nc <- "tso"

} else if (varname == "sst") { # fesom 2.0 
    longname <- "Sea Surface Temperature"
    units_out <- units_plot <- "degC"
    var_label_plot <- expression(paste("SST [", degree, "C]"))
    varname_nc <- "sst"

} else if (any(varname == c("temp", "thetao"))) {
    longname <- "Potential Temperature"
    units_out <- units_plot <- "degC"
    var_label_plot <- expression(paste("Potential Temperature [", degree, "C]"))
    if (integrate_depth) {
        units_out <- "degC m"
        power_plot <- -3
        multfac_plot <- base^power_plot
        units_plot <- paste0("degC m x ", multfac_plot)
        var_label_plot <- substitute(paste(integral(), " T dz [",
                                         var1, " ", var2,
                                         "] " %*% " ", base^-power_plot),
                                  list(var1="°C", var2="m",
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m3"
        }
    } else {
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m2"
        }
    }
    typesuffix <- "oce."
    diagsuffix <- ""
    varname_nc <- "temp"
    varname_nc <- "thetao"

} else if (any(varname == c("salt", "so"))) {
    longname <- "Salinity"
    units_out <- "psu"
    units_plot <- "psu"
    var_label_plot <- "Salinity [psu]"
    if (integrate_depth) {
        power_out <- -3
        multfac_out <- base^power_out
        units_out <- "psu m"
        units_plot <- "psu m"
        var_label_plot <- substitute(paste(integral(), " S dz [",
                                         var1, " ", var2,
                                         "] " %*% " ", base^power_out),
                                  list(var1="psu", var2="m",
                                       base=base, power_out=-power_out))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m3"
        }
    } else {
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m2"
        }
    }
    typesuffix <- "oce."
    diagsuffix <- ""
    varname_nc <- "salt"
    varname_nc <- "so"

} else if (varname == "tob") { 
    longname <- "sea_water_potential_temperature_at_sea_floor"
    units_out <- units_plot <- "degC"
    var_label_plot <- expression(paste(T[bottom], " [", degree, "C]"))
    varname_nc <- "tob"

} else if (varname == "sob") { 
    longname <- "sea_water_salinity_at_sea_floor"
    units_out <- units_plot <- "psu"
    var_label_plot <- expression(paste(S[bottom], " [", degree, "C]"))
    varname_nc <- "sob"

} else if (varname == "insitudens") {
    longname <- "In Situ Density"
    units_out <- "kg m-3"
    units_plot <- "kg m-3"
    var_label_plot <- expression(paste(rho["in situ"], " [kg m"^"-3","]"))
    if (integrate_depth) {
        units_out <- "kg m-2"
        units_plot <- "kg m-2"
        var_label_plot <- expression(paste(rho["in situ"], " [kg m"^"-2","]"))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "kg"
        }
    } else {
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m2"
        }
    }
    horiz_deriv_tag <- F
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "rho"
    rotate_inds <- F
    vec <- F

} else if (varname == "potdens") {
    potdens_tag <- T
    p_ref <- 0
    longname <- "Potential Density"
    units_out <- "kg m-3"
    var_label_plot <- expression(paste(sigma[theta], " [kg m"^"-3","]"))
    if (integrate_depth) {
        units_out <- "kg m-2"
        var_label_plot <- expression(paste(sigma[theta], " [kg m"^"-2","]"))
    }

    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")
    varname_nc <- c("thetao", "so")

} else if (varname == "insitub") {
    longname <- "In situ Buoyancy"
    power_out <- 0
    multfac_out <- base^power_out
    units_out <- paste0("m s-2")
    var_label_plot <- substitute(paste("b [", var1, " ", var2^-2,
                                     #"] " %*% " ", base^power_out),
                                     "]"),
                               list(var1="m", var2="s"
                                    #,base=base, power_out=-power_out
                                    ))
    if (integrate_depth) {
        power_out <- -2
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-2 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " b dz [", 
                                         var1^2, " ", var2^-2, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-2")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-2")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-2")
    }
    horiz_deriv_tag <- F
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "rho"
    rotate_inds <- F
    vec <- F

} else if (varname == "potb") {
    longname <- "Potential Buoyancy"
    power_out <- 0
    multfac_out <- base^power_out
    units_out <- paste0("m s-2")
    var_label_plot <- substitute(paste("b [", var1, " ", var2^-2,
                                     #"] " %*% " ", base^power_out),
                                     "]"),
                               list(var1="m", var2="s"
                                    #,base=base, power_out=-power_out))
                                    ))
    if (integrate_depth) {
        power_out <- -2
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-2 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " b dz [",
                                         var1^2, " ", var2^-2,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-2")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-2")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-2")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "tossq") { 
    longname <- "Squared Sea Surface Temperature"
    units_out <- units_plot <- "degC2"
    var_label_plot <- expression(paste("Squared SST [", degree, "C"^"2", "]"))
    varname_nc <- "tossq"

} else if (varname == "u") {
    longname <- "Zonal Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("Zonal Velocity u [m s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "v") {
    longname <- "Meridional Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("Meridional Velocity v [m s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "hvel") {
    longname <- "Horizontal Velocity"
    units_out <- "m s-1"
    multfac_plot <- 100 # m s-1 --> cm s-1
    units_plot <- "cm s-1"
    var_label_plot <- expression(paste("|", bold("u")[h],"| [cm s"^"-1","]"))
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    if (F) {
        varname_nc <- c("uo", "vo")
    } else if (T) { 
        varname_nc <- c("u", "v")
        fpatterns <- c("<runid>.<YYYY>.oce.mean.nc", "<runid>.<YYYY>.oce.mean.nc")
    }
    rotate_inds <- c(1, 2)
    vec <- T
    if (F) {
        hvel_levels <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 100) # cm s-1
    }
    hvel_cols <- c("#dde7f6", "#b2b3da", "#9b98cb", "#cdc774",
                   "#fcee23", "#fdb514", "#f47720", "#ef4023",
                   "darkred")

} else if (varname == "uu") {
    longname <- "Zonal Velocity Squared"
    units_out <- "m2 s-2"
    var_label_plot <- expression(paste("u"^"2", " [m"^"2", " s"^"-2","]"))
    #if (as.numeric(depths[1]) >= 2000) {
    #    multfac_out <- 3.6 # [m s^(-1)] --> [km h^(-1)]
    #    units_out <- "km h^-1"
    #    var_label_plot <- expression(paste("|",bold("u")[h],"| [km h"^"-1","]"))
    #}
    horiz_deriv_tag <- F
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "uu"
    rotate_inds <- F
    vec <- F

} else if (varname == "u_u") {
    longname <- "Mean Zonal Velocity Squared"
    units_out <- "m2 s-2"
    var_label_plot <- expression(paste(bar("u")^"2", " [m"^"2", " s"^"-2","]"))
    #if (as.numeric(depths[1]) >= 2000) {
    #    multfac_out <- 3.6 # [m s^(-1)] --> [km h^(-1)]
    #    units_out <- "km h^-1"
    #    var_label_plot <- expression(paste("|",bold("u")[h],"| [km h"^"-1","]"))
    #}
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "vv") {
    longname <- "Meridional Velocity Squared"
    units_out <- "m2 s-2"
    var_label_plot <- expression(paste("v"^"2", " [m"^"2", " s"^"-2","]"))
    #if (as.numeric(depths[1]) >= 2000) {
    #    multfac_out <- 3.6 # [m s^(-1)] --> [km h^(-1)]
    #    units_out <- "km h^-1"
    #    var_label_plot <- expression(paste("|",bold("u")[h],"| [km h"^"-1","]"))
    #}
    horiz_deriv_tag <- F
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "vv"
    rotate_inds <- F
    vec <- F

} else if (varname == "v_v") {
    longname <- "Mean Meridional Velocity Squared"
    units_out <- "m2 s-2"
    var_label_plot <- expression(paste(bar("v")^"2", " [m"^"2", " s"^"-2","]"))
    #if (as.numeric(depths[1]) >= 2000) {
    #    multfac_out <- 3.6 # [m s^(-1)] --> [km h^(-1)]
    #    units_out <- "km h^-1"
    #    var_label_plot <- expression(paste("|",bold("u")[h],"| [km h"^"-1","]"))
    #}
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "u_geo") {
    longname <- "Zonal Geostrophic Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("Zonal Geostrophic Velocity u", ""[geo], " [m s"^"-1","]"))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.")
    diagsuffix <- c("")
    varname_nc <- c("ssh")
    rotate_inds <- F
    vec <- F

} else if (varname == "v_geo") {
    longname <- "Meridional Geostrophic Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("Meridional Geostrophic Velocity v", ""[geo], " [m s"^"-1","]"))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.")
    diagsuffix <- c("")
    varname_nc <- c("ssh")
    rotate_inds <- F
    vec <- F

} else if (varname == "hvel_geo") {
    longname <- "Horizontal Geostrophic Velocity"
    units_out <- "m s-1"
    multfac_plot <- 100 # m s-1 -->  cm s-1
    units_plot <- "cm s-1"
    var_label_plot <- expression(paste("|", bold("u")[h], ""[","], ""[geo], "| [cm s"^"-1","]"))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.")
    diagsuffix <- c("")
    varname_nc <- c("ssh")
    coriolis_tag <- T
    vec <- T
    hvel_geo_levels <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 100) # cm s-1
    hvel_geo_cols <- c("#dde7f6", "#b2b3da", "#9b98cb", "#cdc774",
                       "#fcee23", "#fdb514", "#f47720", "#ef4023",
                       "darkred")

} else if (varname == "uveddy") {
    longname <- "Horizontal Eddy Momentum Flux"
    power_out <- 4
    multfac_out <- base^power_out
    if (power_out != 0) {
        if (!integrate_depth) {
            units_out <- paste0("m2 s-2 x ", multfac_out)
            var_label_plot <- substitute(paste(bar(paste("u'v'")), " [",
                                             var1^2, " ", var2^-2, 
                                             "] " %*% " ", base^power_out),
                                       list(var1="m", var2="s", 
                                            base=base, power_out=-power_out))

        } else if (integrate_depth) {
        stop("not implemented")    
        }
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.")
    varname_nc <- c("u", "v", "uv")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "usgs") {
    longname <- "SGS Zonal Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("SGS Zonal Velocity [m"," s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_u", "sgs_v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "vsgs") {
    longname <- "SGS Meridional Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("SGS Meridional Velocity [m"," s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_u", "sgs_v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "uvsgs") {
    longname <- "Horizontal SGS Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("SGS Horizontal Velocity [m"," s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_u", "sgs_v")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "utemp") {
    longname <- "Mean Zonal Advective Temperature Flux"
    units_out <- "degC m s-1"
    var_label_plot <- substitute(paste(bar(u), " ", bar(T), " [°C ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "")
    varname_nc <- c("u", "v", "temp")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "vtemp") {
    longname <- "Mean Meridional Advective Flux of Temperature"
    units_out <- "degC m s-1"
    var_label_plot <- substitute(paste(bar(v), " ", bar(T), " [°C ", 
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "")
    varname_nc <- c("u", "v", "temp")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "uvtemp") {
    longname <- "Mean Horizontal Advective Flux Temperature Flux"
    units_out <- units_plot <- "degC m s-1"
    var_label_plot <- substitute(paste("|", bar(bold(u)[h]), " ", bar(T), "| [°C ", 
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "")
    varname_nc <- c("u", "v", "temp")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "uvtemptot") {
    longname <- "Total Horizontal Advective Temperature Flux"
    units_out <- "degC m s-1"
    var_label_plot <- substitute(paste("|", bar(paste(bold(u)[h], "T")), "| [°C ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("utemp", "vtemp")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "uteddy") {
    longname <- "Eddy Zonal Temperature Flux"
    units_out <- "degC m s-1"
    var_label_plot <- substitute(paste(bar(paste("u'T'")), " [°C ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "utemp", "vtemp", "temp")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "vteddy") {
    longname <- "Eddy Meridional Temperature Flux"
    units_out <- "degC m s-1"
    var_label_plot <- substitute(paste(bar(paste("v'T'")), " [°C ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "utemp", "vtemp", "temp")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "uvteddy") {
    longname <- "Eddy Horizontal Temperature Flux"
    power_out <- 3
    multfac_out <- base^power_out
    if (!integrate_depth) {
        units_out <- paste0("degC m s-1 x ", multfac_out)
        var_label_plot <- substitute(paste("|", bar(paste(bold(u)[h], "'T'")), "| [°C ",
                                         var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                   list(var1="m", var2="s",
                                        base=base, power_out=-power_out))
    } else if (integrate_depth) {
        stop("not implemented")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "utemp", "vtemp", "temp")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "usgstemp") {
    stop("not complete")
    longname <- "Total SGS Zonal Temperature Flux"
    units_out <- "degC m s-1"
    var_label_plot <- expression(paste("Total SGS Zonal Temperature Flux [", degree, "C m s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_ut", "sgs_vt")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "vsgstemp") {
    stop("not complete")
    longname <- "Total SGS Meridional Temperature Flux"
    units_out <- "degC m s-1"
    var_label_plot <- expression(paste("Total SGS Meridional Temperature Flux [", degree, "C m s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_ut", "sgs_vt")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "uvsgstemptot") {
    longname <- "Total Horizontal SGS Temperature Flux"
    units_out <- "°C m s-1"
    var_label_plot <- substitute(paste(bar(paste(bold(u)["sgs,h"], "T")),
                                     " [°C ", var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    power_out <- 0
    multfac_out <- base^power_out
    if (integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("°C m2 s-1")
        var_label_plot <- substitute(paste(integral(), bar(paste(bold(u)["sgs,h"], "T")),
                                         " dz [°C ", var1^2, " ", var2^-1, "]"),
                                   list(var1="m", var2="s"))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("°C m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("°C m3 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("°C m4 s-1")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_ut", "sgs_vt")
    rotate_inds <- F #c(1, 2)
    vec <- T

} else if (varname == "usalt") {
    longname <- "Zonal Advective Flux of Salinity"
    units_out <- "psu m s-1"
    var_label_plot <- substitute(paste(bar(u), " ", bar(S), " [psu ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "")
    varname_nc <- c("u", "v", "salt")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "vsalt") {
    longname <- "Meridional Advective Flux of Salinity"
    units_out <- "psu m s-1"
    var_label_plot <- substitute(paste(bar(v), " ", bar(S), " [psu ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "")
    varname_nc <- c("u", "v", "salt")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "uvsalt") {
    longname <- "Horizontal Advective Flux of Salinity"
    units_out <- "psu m s-1"
    var_label_plot <- substitute(paste("|", bar(bold(u)[h]), " ", bar(S), "| [psu ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "")
    varname_nc <- c("u", "v", "salt")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "useddy") {
    longname <- "Zonal Eddy Salinity Flux"
    units_out <- "psu m s-1"
    var_label_plot <- substitute(paste(bar(paste("u'S'")), " [psu ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "usalt", "vsalt", "salt")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "vseddy") {
    longname <- "Meridional Eddy Salinity Flux"
    units_out <- "psu m s-1"
    var_label_plot <- substitute(paste(bar(paste("v'S'")), " [psu ",
                                     var1, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "usalt", "vsalt", "salt")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "uvseddy") {
    longname <- "Norm of Horizontal Eddy Salinity Flux"
    power_out <- 4
    multfac_out <- base^power_out
    if (power_out != 0) {
        if (!integrate_depth) {
            units_out <- paste0("psu m s-1 x ", multfac_out)
            var_label_plot <- substitute(paste("|", bar(paste(bold(u)[h], "'S'")), "| [psu ",
                                             var1, " ", var2^-1,
                                             "] " %*% " ", base^power_out),
                                       list(var1="m", var2="s",
                                            base=base, power_out=-power_out))
        } else if (integrate_depth) {
        stop("not implemented")
        }
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "usalt", "vsalt", "salt")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "usgssalt") {
    stop("not complete")
    longname <- "SGS Zonal Salinity Flux"
    units_out <- "psu m s-1"
    var_label_plot <- expression(paste("SGS Zonal Salinity Flux [psu m s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_us", "sgs_vs")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "vsgssalt") {
    stop("not complete")
    longname <- "SGS Meridional Salinity Flux"
    units_out <- "psu m s-1"
    var_label_plot <- expression(paste("SGS Meridional Salinity Flux [psu m s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_us", "sgs_vs")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "uvsgssalttot") {
    stop("not complete")
    longname <- "Total Horizontal SGS Salinity Flux"
    units_out <- "psu m s-1"
    var_label_plot <- expression(paste("SGS Horizontal Salinity Flux [psu m s"^"-1","]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("sgs_us", "sgs_vs")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "urho") {
    longname <- "Zonal in situ Density Flux"
    units_out <- "kg m-2 s-1"
    var_label_plot <- substitute(paste(bar(u), " ", bar(rho), " [kg ",
                                     var1^-2, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "")
    varname_nc <- c("u", "v", "rho")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "vrho") {
    longname <- "Meridional in situ Density Flux"
    units_out <- "kg m-2 s-1"
    var_label_plot <- substitute(paste(bar(v), " ", bar(rho), " [kg ",
                                     var1^-2, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "")
    varname_nc <- c("u", "v", "rho")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "uvrho") {
    longname <- "Norm of Horizontal in situ Density Flux"
    units_out <- "kg m-2 s-1"
    var_label_plot <- substitute(paste("|", bar(bold(u)[h]), " ", bar(rho), "|  [kg ",
                                     var1^-2, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.")
    varname_nc <- c("u", "v", "rho")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "urhoeddy") {
    longname <- "Zonal Eddy in situ Density Flux"
    units_out <- "kg m-2 s-1"
    var_label_plot <- substitute(paste(bar(paste("u'", rho, "'")), " [kg ",
                                     var1^-2, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "urho", "vrho", "rho")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "vrhoeddy") {
    longname <- "Meridional Eddy in situ Density Flux"
    units_out <- "kg m-2 s-1"
    var_label_plot <- substitute(paste(bar(paste("v'", rho, "'")), " [kg ",
                                     var1^-2, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "urho", "vrho", "rho")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "uvrhoeddy") {
    longname <- "Norm of Horizontal Eddy in situ Density Flux"
    power_out <- 4
    multfac_out <- base^power_out
    if (power_out != 0) {
        if (!integrate_depth) {
            units_out <- paste0("kg m-2 s-1 x ", multfac_out)
            var_label_plot <- substitute(paste("|", bar(paste(bold(u)[h], "'", rho, "'")), "| [kg ",
                                             var1^-2, " ", var2^-1,
                                             "] " %*% " ", base^power_out),
                                       list(var1="m", var2="s",
                                            base=base, power_out=-power_out))
        } else if (integrate_depth) {
        stop("not implemented")
        }
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "urho", "vrho", "rho")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "relvorti") {
    longname <- "Relative Vorticity"
    power_out <- 6
    multfac_out <- base^power_out
    units_out <- paste0("s-1 x ", multfac_out)
    var_label_plot <- substitute(paste("rel. Vort. ", zeta, " = ",
                                     partialdiff[x], "", bar(v), " - ", 
                                     partialdiff[y], "", bar(u), " [", 
                                     var^-1, "] " %*% " ", base^power_out), 
                              list(var="s", base=base, power_out=-power_out))
                    # in FESOM
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "revortif") {
    longname <- "Relative Vorticity / f"
    power_out <- 0
    multfac_out <- base^power_out
    units_out <- "#"
    var_label_plot <- substitute(paste("rel. Vort. ", zeta, " ", f^-1, " = ", f^-1, " (",
                                     partialdiff[x], "", bar(v), " - ", 
                                     partialdiff[y], "", bar(u), ")  [#]"),                                
                               list(f="f"))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "RossbyNo") {
    longname <- "Relative Vorticity / |f|"
    units_out <- "unitless"
    coriolis_tag <- T
    var_label_plot <- substitute(paste(zeta, " |", f, "|", ""^-1, 
                                       #" = |", f^-1, "| ", partialdiff[x], " ", bar(v), " - ",  partialdiff[y], " ", bar(u), 
                                       " [unitless]"),                                
                                 list(f="f"))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)

} else if (varname == "strain_normal") {
    longname <- "Horizontal Strain (normal part)"
    power_out <- 12
    multfac_out <- base^power_out
    units_out <- paste0("s-2 x ", multfac_out)
    var_label_plot <- substitute(paste("Horizontal Strain (normal part) [", 
                                     units_out^-2, "] " %*% " ", base^power_out),
                               list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "strain_shear") {
    longname <- "Horizontal Strain (shear part)"
    power_out <- 12
    multfac_out <- base^power_out
    units_out <- paste0("s-2 x ", multfac_out)
    var_label_plot <- substitute(paste("Horizontal Strain (shear part) [", 
                                     units_out^-2, "] " %*% " ", base^power_out),
                               list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "strain") {
    longname <- "Horizontal Strain"
    power_out <- 12
    multfac_out <- base^power_out
    units_out <- paste0("s-2 x ", multfac_out)
    var_label_plot <- substitute(paste("Horizontal Strain [", units_out^-2, 
                                     "] " %*% " ", base^power_out),
                               list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "relvortisq") {
    longname <- "Squared Relative Vorticity"
    power_out <- 12
    multfac_out <- base^power_out
    units_out <- paste0("s-2 x ", multfac_out)
    var_label_plot <- substitute(paste("Squared Relative Vorticity [", 
                                     units_out^-2, "] " %*% " ", base^power_out),
                               list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "okubo") {
    longname <- "Okubo-Weiss Parameter"
    power_out <- 12
    multfac_out <- base^power_out
    units_out <- paste0("s-2 x ", multfac_out)
    var_label_plot <- substitute(paste("Okubo-Weiss Parameter [", units_out^-2, 
                                     "] " %*% " ", base^power_out),
                              list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "potvorti") {
    longname <- "Ertel Potential Vorticity"
    power_out <- 10
    multfac_out <- base^power_out
    units_out <- paste0("s-3 x ", multfac_out)
    var_label_plot <- substitute(paste("Ertel Potential Vorticity PV [", units_out^-3, 
                                     "] " %*% " ", base^power_out),
                              list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "", "", "")
    varname_nc <- c("u", "v", "w", "temp", "salt")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "potvorti_bc") {
    longname <- "PV_bc"
    power_out <- 10
    multfac_out <- base^power_out
    units_out <- paste0("s-3 x ", multfac_out)
    var_label_plot <- substitute(paste("PV_bc [", units_out^-3,
                                     "] " %*% " ", base^power_out),
                              list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "", "", "")
    varname_nc <- c("u", "v", "w", "temp", "salt")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "potvorti_vert") {
    longname <- "PV_vert"
    power_out <- 10
    multfac_out <- base^power_out
    units_out <- paste0("s-3 x ", multfac_out)
    var_label_plot <- substitute(paste("PV_vert [", units_out^-3,
                                     "] " %*% " ", base^power_out),
                              list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "", "", "")
    varname_nc <- c("u", "v", "w", "temp", "salt")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "richardson") {
    longname <- "Gradient Richardson Number"
    power_out <- 0
    multfac_out <- base^power_out
    units_out <- paste0("#")# x ", multfac_out)
    var_label_plot <- "Gradient Richardson Number Ri [#]"
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "", "")
    varname_nc <- c("u", "v", "temp", "salt")
    rotate_inds <- c(1, 2)
    insitudens_tag <- T
    buoyany_tag <- T
    vec <- F

} else if (varname == "mke") {
    longname <- "Mean Kinetic Energy"
    power_out <- 4
    multfac_out <- base^power_out
    units_out <- paste0("m2 s-2 x ", multfac_out)
    var_label_plot <- substitute(paste("MKE = 1/2 ", bar(bold(u)[h])^2, 
                                     " [", var1^2, " ", var2^-2, "]"),
                               list(var1="m", var2="s"))
    if (integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-2")
        var_label_plot <- substitute(paste("MKE = 1/2 ", integral(), bar(bold(u)[h])^2, 
                                         " dz [", var1^3, " ", var2^-2, "]"),
                                   list(var1="m", var2="s"))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-2")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-2")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m5 s-2")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "tke") {
    longname <- "Total Kinetic Energy"
    power_out <- 4
    multfac_out <- base^power_out
    units_out <- paste0("m2 s-2 x 1e", power_out)
    if (!integrate_depth) {
        var_label_plot <- substitute(paste("TKE = 1/2 ", bar(bold(u)[h]^2),
                                         " [", var1^2, " ", var2^-2, "]"),
                                   list(var1="m", var2="s"))
    } else if (integrate_depth) {
        var_label_plot <- substitute(paste("TKE = 1/2 ", integral(),
                                         " ", bar(bold(u)[h]^2),
                                         " dz [", var1^3, " ", var2^-2, "]"),
                                   list(var1="m", var2="s"))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-2")
    } else if ((out_mode == "meanint" || out_mode == "depthint") && 
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-2")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m5 s-2")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("uu", "vv")

} else if (varname == "eke") {
    longname <- "Eddy Kinetic Energy"
    if (integrate_depth) {
        units_out <- units_plot <- "m3 s-2"
        var_label_plot <- substitute(paste(integral(), " 1/2 ", 
                                           bar(paste(bold(u)[h], "'"^2)),
                                           " dz [", var1^3, " ", var2^-2, "]"),
                                     list(var1="m", var2="s"))
        if (out_mode == "meanint" || out_mode == "depthint") {
            units_out <- "m5 s-2"
        }
    } else {
        units_out <- units_plot <- "m2 s-2"
        var_label_plot <- substitute(paste("1/2 ", bar(paste(bold(u)[h], "'"^2)), 
                                         " [", var1^2, " ", var2^-2, "]"),
                                   list(var1="m", var2="s"))
        if (out_mode == "meanint" || out_mode == "depthint") {
            units_out <- "m4 s-2"
        }
    }
    typesuffix <- c("oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.")
    varname_nc <- c("u", "v", "uu", "vv")

} else if (varname == "HRS") {
    longname <- "Horizontal Reynolds Stress"
    subtitle <- ">0 eddy growth due to barotropic instability"
    units_out <- "m2 s-3"
    units_plot <- units_out
    if (!integrate_depth) {
        power_plot <- 8
        multfac_plot <- base^power_plot
        units_plot <- paste0("m2 s-3 x ", multfac_plot)
        var_label_plot <- substitute(paste("HRS [", 
                                        var1^2, " ", var2^-3, 
                                        "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s", 
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m4 s-3"
        }
    } else if (integrate_depth) {
        units_out <- "m3 s-3"
        units_plot <- units_out
        power_plot <- 4
        multfac_plot <- base^power_plot
        units_plot <- paste0("m3 s-3 x ", multfac_plot)
        var_label_plot <- substitute(paste(integral(), "HRS dz [",
                                        var1^3, " ", var2^-3, 
                                        "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s", 
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m5 s-3"
        }
    }
    horiz_deriv_tag <- T #"rot"
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "diag.")
    varname_nc <- c("u", "v", "uu", "vv", "uv") # rotation NOT allowed since uu, vv, uv are in rotated model coordinates

} else if (varname == "VRS") {
    longname <- "Vertical Reynolds Stress"
    subtitle <- ">0 Kelvin-Helmholtz instability"
    units_out <- "m2 s-3"
    units_plot <- units_out
    if (!integrate_depth) {
        power_plot <- 12
        multfac_plot <- base^power_plot
        units_plot <- paste0("m2 s-3 x ", multfac_plot)
        var_label_plot <- substitute(paste("VRS [",
                                        var1^2, " ", var2^-3, 
                                        "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s", 
                                       base=base, power_plot=-power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m4 s-3"
        }
    } else if (integrate_depth) {
        units_out <- "m3 s-3"
        units_plot <- units_out
        power_plot <- 4
        multfac_plot <- base^power_plot
        units_plot <- paste0("m3 s-3 x ", multfac_plot)
        var_label_plot <- substitute(paste(integral(), "VRS dz [",
                                        var1^3, " ", var2^-3, 
                                        "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s", 
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m5 s-3"
        }
    }
    horiz_deriv_tag <- T #"rot"
    typesuffix <- c("oce.", "oce.", "oce.", "oce.", "oce.")
    diagsuffix <- c("", "", "", "diag.", "diag.")
    varname_nc <- c("u", "v", "w", "uw", "vw") # rotation NOT allowed since uu, vv, uv are in rotated model coordinates

} else if (varname == "KmKe") {
    longname <- "Kinetic Mean -> Kinetic Eddy Conversion"
    if (!integrate_depth) {
        power_out <- 8
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste("K"[m], "K"[e], " [",
                                        var1^2, " ", var2^-3, 
                                        "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    } else if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         "K"[m], "K"[e], " dz [",
                                        var1^3, " ", var2^-3, 
                                        "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "rot"
    typesuffix <- rep("oce.", t=8)
    diagsuffix <- c("", "", "", rep("diag.", t=5))
    varname_nc <- c("u", "v", "w", "uu", "vv", "uv", "uw", "vw") # rotation NOT allowed since uu, vv, uv are in rotated model coordinates

} else if (varname == "wb") {
    longname <- "wb (Potential Mean -> Kinetic Mean Conversion)"
    power_out <- 4
    multfac_out <- base^power_out
    units_out <- paste0("m2 s-3 x ", multfac_out)
    var_label_plot <- substitute(paste(#"P"[m], "K"[m], 
                                     bar(w), bar(b),       
                                     " [", var1^2, " ", var2^-3, 
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s", 
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        units_out <- paste0("m3 s^-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), 
                                         #"P"[m], "K"[m], 
                                         bar(w), bar(b),
                                         " dz [", var1^3, " ", var2^-3, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("diag.", "")
    varname_nc <- c("rho", "w")

} else if (varname == "uvb") {
    longname <- "Norm of horizontal mean buoyancy flux"
    power_out <- 0
    multfac_out <- base^power_out
    if (power_out != 0) {
        if (!integrate_depth) {
            units_out <- paste0("m2 s-3 x ", multfac_out)
            var_label_plot <- substitute(paste(bar(bold(u))[h], " ", bar(b), 
                                             " [", var1^2, " ", var2^-3, 
                                             "] " %*% " ", base^power_out),
                                       list(var1="m", var2="s", 
                                            base=base, power_out=-power_out))
        } else if (integrate_depth) {
            units_out <- paste0("m3 s-3 x ", multfac_out)
            var_label_plot <- substitute(paste(integral(),
                                             bar(bold(u))[h], " ", bar(rho),
                                             " dz [", var1^3, " ", var2^-3, 
                                             "] " %*% " ", base^power_out),
                                      list(var1="m", var2="s", 
                                           base=base, power_out=-power_out))
        }
    } else if (power_out == 0) {
        if (!integrate_depth) {
            units_out <- paste0("m2 s-3")
            var_label_plot <- substitute(paste(bar(bold(u))[h], " ", bar(b),
                                             " [", var1^2, " ", var2^-3,
                                             "]"),
                                       list(var1="m", var2="s"))
        } else if (integrate_depth) {
            units_out <- paste0("m3 s-3")
            var_label_plot <- substitute(paste(integral(),
                                             bar(bold(u))[h], " ", bar(rho),
                                             " dz [", var1^3, " ", var2^-3,
                                             "]"),
                                      list(var1="m", var2="s"))
        }
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- c("", "", "diag.")
    varname_nc <- c("u", "v", "rho")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "PmPe") {
    longname <- "Potential Mean -> Potential Eddy Conversion"
    subtitle <- ">0 eddy growth due to baroclinic instability"
    units_out <- "m2 s-3"
    units_plot <- units_out
    if (integrate_depth) {
        units_out <- "m3 s-3"
        power_plot <- 6
        multfac_plot <- base^power_plot
        units_plot <- paste0("m3 s-3 x ", multfac_plot)
        var_label_plot <- substitute(paste(integral(), 
                                         #"P"[m], "P"[e],
                                         " dz [", var1^3, " ", var2^-3, 
                                         "] " %*% " ", base^-power_plot),
                                   list(var1="m", var2="s", 
                                        base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m5 s-2"
        }
    } else {
        power_plot <- 5
        multfac_plot <- base^power_plot
        units_plot <- paste0("m2 s-3 x ", multfac_plot)
        var_label_plot <- substitute(#paste(#"P"[e], "K"[e], 
                                     paste(-bar(bold(u)[h], "'b'") %.% bold(nabla)[h], bar(b), " ", N^-2,
                                     " [", var1^2, " ", var2^-3, 
                                     "] " %*% " ", base^-power_plot),
                                     list(var1="m", var2="s", N="N",
                                          base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m4 s-2"
        }
    }
    horiz_deriv_tag <- T
    varname_nc <- c("u", "v", "urho", "vrho", "rho", "temp", "salt")
    fpatterns <- c("<runid>.<YYYY>.oce.mean.nc", "<runid>.<YYYY>.oce.mean.nc", 
                   "<runid>.<YYYY>.oce.diag.nc", "<runid>.<YYYY>.oce.diag.nc", "<runid>.<YYYY>.oce.diag.nc",
                   "<runid>.<YYYY>.oce.mean.nc", "<runid>.<YYYY>.oce.mean.nc")
    rotate_inds <- c(1, 2, 3, 4)
    dxvars <- dyvars <- "rho"
    buoyancy_frequency_potdens_tag <- T

} else if (varname == "PmPe_wN2") {
    longname <- "Potential Mean -> Potential Eddy Conversion"
    subtitle <- ">0 eddy growth due to baroclinic instability"
    units_out <- "m2 s-3"
    units_plot <- units_out
    if (integrate_depth) {
        units_out <- "m3 s-3"
        power_plot <- 6
        multfac_plot <- base^power_plot
        units_plot <- paste0("m3 s-3 x ", multfac_plot)
        var_label_plot <- substitute(paste(integral(), 
                                         #"P"[m], "P"[e],
                                         " dz [", var1^3, " ", var2^-3, 
                                         "] " %*% " ", base^-power_plot),
                                   list(var1="m", var2="s", 
                                        base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m5 s-2"
        }
    } else {
        power_plot <- 2
        multfac_plot <- base^power_plot
        units_plot <- paste0("m2 s-3 x ", multfac_plot)
        var_label_plot <- substitute(#paste(#"P"[e], "K"[e], 
                                     paste(-bar(bold(u)[h], "'b'") %.% bold(nabla)[h], bar(b), " ", N^-2,
                                     " [", var1^2, " ", var2^-3, 
                                     "] " %*% " ", base^-power_plot),
                                     list(var1="m", var2="s", N="N",
                                          base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m4 s-2"
        }
    }
    horiz_deriv_tag <- "rot"
    varname_nc <- c("u", "v", "urho", "vrho", "rho", "N2")
    fpatterns <- c("<runid>.<YYYY>.oce.mean.nc", "<runid>.<YYYY>.oce.mean.nc", 
                   "<runid>.<YYYY>.oce.diag.nc", "<runid>.<YYYY>.oce.diag.nc",
                   "<runid>.<YYYY>.oce.diag.nc", "<runid>.<YYYY>.oce.diag.nc")
    rotate_inds <- c(1, 2, 3, 4)

} else if (varname == "wbeddy") {
    longname <- "w'b' (Potential Eddy -> Kinetic Eddy Conversion)"
    subtitle <- ">0 eddy growth due to baroclinic instability"
    units_out <- "m2 s-3"
    units_plot <- units_out
    if (integrate_depth) {
        units_out <- "m3 s-3"
        power_plot <- 6
        multfac_plot <- base^power_plot
        units_plot <- paste0("m3 s-3 x ", multfac_plot)
        var_label_plot <- substitute(paste(integral(), 
                                         #"P"[e], "K"[e],
                                         bar(paste("w'b'")), 
                                         " dz [", var1^3, " ", var2^-3, 
                                         "] " %*% " ", base^-power_plot),
                                   list(var1="m", var2="s", 
                                        base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m5 s-2"
        }
    } else {
        power_plot <- 10
        multfac_plot <- base^power_plot
        units_plot <- paste0("m2 s-3 x ", multfac_plot)
        var_label_plot <- substitute(paste(#"P"[e], "K"[e], 
                                         bar(paste("w'b'")),
                                         " [", var1^2, " ", var2^-3, 
                                         "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s", 
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m4 s-2"
        }
    }
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- c("", "diag.", "diag.")
    varname_nc <- c("w", "rho", "wrho")

} else if (varname == "uvbeddy") {
    longname <- "Norm of Horizontal Eddy Buoyancy Flux"
    if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         "|", bar(paste(bold(u)[h], "'b'")), 
                                         "| dz [", var1^3, " ", var2^-3, 
                                         "] " %*% " ", base^power_out),
                                   list(var1="m", var2="s", 
                                        base=base, power_out=-power_out))
    } else {
        power_out <- 6
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste("|", bar(paste(bold(u)[h], "'b'")),
                                         "| [", var1^2, " ", var2^-3, 
                                         "] " %*% " ", base^power_out),
                                   list(var1="m", var2="s", 
                                        base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- rep("oce.", t=5)
    diagsuffix <- c("", "", "diag.", "diag.", "diag.")
    varname_nc <- c("u", "v", "urho", "vrho", "rho")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "vertvel") {
    longname <- "Vertical Velocity"
    subtitle <- ">0 upwards"
    units_out <- "m s-1"
    if (F) {
        multfac_plot <- 100*3600 # m s-1 --> cm h-1
        units_plot <- "cm h-1"
        var_label_plot <- expression(paste("w [cm h"^"-1","]"))
    } else if (T) {
        multfac_plot <- 86400 # m s-1 --> m day-1
        units_plot <- "m day-1"
        var_label_plot <- expression(paste("w [m day"^"-1","]"))
    } else if (F) {
        multfac_plot <- 100*86400 # m s-1 --> cm day-1
        units_plot <- "cm day-1"
        var_label_plot <- expression(paste("w [cm day"^"-1","]"))
    }
    vertvel_levels <- pretty(c(-10, 10), n=11)
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "m2 s-1"
    }
    if (integrate_depth) {
        units_out <- "m2 s-1"
        units_plot <- units_out
        var_label_plot <- substitute(paste(integral(), " w dz [", 
                                           var1^2, " ", var2^-1, "]"),
                                     list(var1="m", var2="s"))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m3 s-1"
        }
    }
    typesuffix <- "oce."
    diagsuffix <- ""
    varname_nc <- "w"
    varname_nc <- "wo"

} else if (varname == "gradT") {
    longname <- "grad_h T"
    power_out <- 2
    multfac_out_plot <- base^power_out
    units_plot <- paste0("K km-1 x ", multfac_out_plot)
    multfac_out <- multfac_out_plot * 1e3 # 1e3 for m -> km and 'power_out' for better range
    var_label_plot <- substitute(paste("|", bold(nabla)[h], "T|   [K ", units_out^-1, 
                                     "] " %*% " ", base^power_out), 
                              list(var="km", base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.")
    diagsuffix <- c("")
    varname_nc <- c("temp")
    varname_nc <- "thetao"
    rotate_inds <- F
    vec <- F

} else if (varname == "gradB") {
    longname <- "Horizonal buoyancy gradient"
    insitudens_tag <- T # calc in-situ density from T,S
    buoyancy_tag <- T # use buoyancy instead of density
    if (integrate_depth) {
        units_out <- "m s-2"
        power_plot <- 4
        multfac_plot <- base^power_plot
        units_plot <- paste0("m s-2 x ", multfac_plot)
        var_label_plot <- substitute(paste(integral(),
                                           "|", bold(nabla)[h], bar(b), "| dz [", 
                                           var1, " ", var2^-2, "] " %*% " ", base^-power_plot),
                                   list(var1="m", var2="s", 
                                        base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m3 s-2"
        }
    } else {
        units_out <- "s-2"
        power_plot <- 8
        multfac_plot <- base^power_plot
        units_plot <- paste0("s-2 x ", multfac_plot)
        var_label_plot <- substitute(paste("|", bold(nabla)[h], bar(b), "| [", 
                                           var^-2, "] " %*% " ", base^-power_plot),
                                   list(var="s", base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m2 s-2"
        }
    }
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")

} else if (varname == "gradmld") {
    longname <- "grad_h MLD"
    power_out <- 0
    multfac_out_plot <- base^power_out
    units_out <- paste0("m km-1")
    multfac_out <- multfac_out_plot * 1e3 # 1e3 for m -> km and 
    var_label_plot <- substitute(paste("|", bold(nabla)[h], " MLD|   [m ", units_out^-1,
                                     "]"),
                              list(var="km"))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.")
    diagsuffix <- c("diag.")
    varname_nc <- c("mixlay")
    rotate_inds <- F
    vec <- T

} else if (any(varname == c("zos", "ssh"))) {
    longname <- "Sea Surface Height"
    units_out <- "m"
    multfac_plot <- 100 # m --> cm
    units_plot <- "cm"
    var_label_plot <- "Sea Surface Height [cm]"
    #fsuffix <- "oce.mean.nc"
    varname_nc <- "ssh"
    #varname_nc <- "zos"

} else if (varname == "zossq") { 
    longname <- "Squared Sea Surface Height"
    units_out <- units_plot <- "m2"
    var_label_plot <- expression(paste("Squared SSH [m"^"2", "]"))
    varname_nc <- "zossq"

} else if (varname == "mixlay") {
    longname <- "Mixed Layer Depth"
    units_out <- units_plot <- "m"
    var_label_plot <- "MLD [m]"
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "mixlay"

} else if (varname == "mlotst") {
    longname <- "mean Mixed Layer Depth by sigma theta"
    units_out <- units_plot <- "m"
    var_label_plot <- eval(substitute(expression(paste("MLD"[sigma[theta]], " [m]"))))
    varname_nc <- "mlotst"

} else if (varname == "mlotstmax") {
    longname <- "max Mixed Layer Depth by sigma theta"
    units_out <- units_plot <- "m"
    var_label_plot <- eval(substitute(expression(paste("MLD"[sigma[theta]], " max [m]"))))
    varname_nc <- "mlotstmax"

} else if (varname == "mlotstmin") {
    longname <- "min Mixed Layer Depth by sigma theta"
    units_out <- units_plot <- "m"
    var_label_plot <- eval(substitute(expression(paste("MLD"[sigma[theta]], " min [m]"))))
    varname_nc <- "mlotstmin"

} else if (varname == "omldamax") {
    longname <- "max Mixed Layer Depth by mixing scheme"
    units_out <- units_plot <- "m"
    var_label_plot <- eval(substitute(expression(paste("MLD"["MS"], " max [m]"))))
    varname_nc <- "omldamax"

} else if (varname == "Nsquared") {
    longname <- "Buoyancy Frequency Squared"
    #p_ref <- 1000 # [dbar] overwrites default from runscript
    units_out <- "s-2"
    power_plot <- 5
    multfac_plot <- base^power_plot
    var_label_plot <- substitute(paste(var1^2, " = -g/", rho[0], 
                                       " ", partialdiff[z], rho, 
                                     " [", var2^-2, "]"
                                     , " " %*% " ", base^power_plot
                                     ),
                               list(var1="N", var2="s"
                                    , base=base, power_plot=-power_plot
                                    ))
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "m2 s-2"
    }
    if (integrate_depth) {
        units_out <- "m s-2"
        var_label_plot <- substitute(paste(integral(), var1^2, " dz = ", 
                                           integral(), " -g/", rho[0],
                                           " ", partialdiff[z], rho,
                                         " dz [", var2, " ", var3^-2, "]"
                                         , " " %*% " ", base^power_out
                                         ),
                                   list(var1="N", var2="m", var3="s"
                                        , base=base, power_out=-power_out
                                        ))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m3 s-2"
        }
    }
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")
    varname_nc <- c("thetao", "so")

} else if (varname == "c_barotrop") {
    longname <- "Barotropic wavespeed"
    units_plot <- "m s-1"
    var_label_plot <- substitute(paste(c[0], " = ", sqrt(gH), " ",
                                       "[m ", var^-1, "]"
                                       #, " " %*% " ", base^power_out
                                       ),
                                 list(#m=mmode, 
                                      m="m",
                                      var="s"
                                      #, base=base, power_out=-power_out
                                      ))
    multfac_out <- 1
    units_out <- units_plot
    if (any(out_mode == c("meanint", "depthint"))) {
        multfac_out <- 1
        units_out <- "m3 s-1"
    }

} else if (varname == "c_barocline") {
    longname <- "Mode-m baroclinic gravity-wave speed"
    mmodes <- 1:5
    #mmodes <- 6:9
    #mmodes <- c(10, 15, 20, 25, 30)
    #mmodes <- c(40, 50, 60, 70, 80)
    fname_suffix <- paste0("_modes_", paste0(mmodes, collapse="_"))
    power_out <- 5
    multfac_out <- base^power_out
    units_plot <- "m s-1"
    var_label_plot <- substitute(paste(c[m], " " %~~% " (m", pi, ")"^-1, 
                                       " ", integral(), "N(z) dz ",
                                       "[m ", var^-1, "]"
                                       , " " %*% " ", base^power_out
                                       ),
                                 list(#m=mmode, 
                                      m="m",
                                      var="s"
                                      , base=base, power_out=-power_out
                                      ))
    multfac_out <- 1
    units_out <- units_plot
    if (any(out_mode == c("meanint", "depthint"))) {
        multfac_out <- 1
        units_out <- "m3 s-1"
    }
    if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_plot <- "m"
        var_label_plot <- substitute(paste(integral(), c[m], " dz " %~~% " ", 
                                           integral(), "[ (m", pi, ")"^-1,
                                           " ", integral(), "N(z) dz ] dz ",
                                           "[", var1^2, " ", var2^-1, "]"
                                           , " " %*% " ", base^power_out
                                           ),
                                     list(#m=mmode, 
                                          m="m",
                                          var1="m", var2="s"
                                          , base=base, power_out=-power_out
                                          ))
        multfac_out <- 1
        units_out <- units_plot
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m4 s-1"
        }
    }
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")

} else if (varname == "c_long_rossby") {
    longname <- "Mode-m baroclinic long rossby-wave speed"
    mmodes <- 1
    #mmodes <- 1:5
    #mmodes <- 6:9
    #mmodes <- c(10, 15, 20, 25, 30)
    #mmodes <- c(40, 50, 60, 70, 80)
    fname_suffix <- paste0("_modes_", paste0(mmodes, collapse="_"))
    power_out <- 2 # m/s --> cm/s
    multfac_out <- base^power_out
    units_plot <- "cm s-1"
    var_label_plot <- substitute(paste(c[m], " " %~~% "-", beta, " f"^-2, 
                                       " ( (m", pi, ")"^-1,
                                       " ", integral(), "N(z)dz )"^2, " ",
                                       " [cm ", var^-1, "]"
                                       #, " " %*% " ", base^power_out
                                       ),
                                 list(#m=mmode, 
                                      m="m",
                                      var="s"
                                      #, base=base, power_out=-power_out
                                      ))
    multfac_out <- multfac_out
    units_out <- units_plot
    if (any(out_mode == c("meanint", "depthint"))) {
        multfac_out <- 1
        units_out <- "m3 s-1"
    }
    if (integrate_depth) {
        stop("asd")
        power_out <- 4
        multfac_out <- base^power_out
        units_plot <- "m"
        var_label_plot <- substitute(paste(integral(), c[m], " dz " %~~% " ",
                                           integral(), "[ (m", pi, ")"^-1,
                                           " ", integral(), "N(z) dz ] dz ",
                                           "[", var1^2, " ", var2^-1, "]"
                                           , " " %*% " ", base^power_out
                                           ),
                                     list(#m=mmode, 
                                          m="m",
                                          var1="m", var2="s"
                                          , base=base, power_out=-power_out
                                          ))
        multfac_out <- 1
        units_out <- units_plot
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m4 s-1"
        }
    }
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")


} else if (varname == "wkb_hvel_mode") {
    longname <- "Horizontal velocity baroclinic m-mode"
    mmodes <- 1:5
    #mmodes <- 6:9
    #mmodes <- c(10, 15, 20, 25, 30)
    #mmodes <- c(40, 50, 60, 70, 80)
    fname_suffix <- paste0("_modes_", paste0(mmodes, collapse="_"))
    power_out <- 5
    multfac_out <- base^power_out
    units_plot <- "#"
    #units_plot <- c("s-1", "m s-1", "#") # N, c, R 
    var_label_plot <- substitute(paste(R[m], "(z)", " " %~~% " ", "(",
                                       c[m], "N(z) ", var1^-1, ")cos(",
                                       var2[m]^-1, integral(), "N(z) dz) [#]"
                                       , " " %*% " ", base^power_out
                                       ),
                                 list(#m=mmode, 
                                      m="m",
                                      var1="g", var2="c"
                                      , base=base, power_out=-power_out
                                      ))
    multfac_out <- 1
    units_out <- units_plot
    if (any(out_mode == c("meanint", "depthint"))) {
        multfac_out <- 1
        units_out <- "m2"
    }
    if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_plot <- "m"
        #units_plot <- c("m s-1", "m2 s-1", "m")
        var_label_plot <- substitute(paste(integral(), R[m], "(z) dz", " " %~~% " ", 
                                           integral(), " (",
                                           c[m], "N(z) ", var1^-1, ")cos(",
                                           var2[m]^-1, integral(), "N(z) dz) dz [m]"
                                           , " " %*% " ", base^power_out
                                           ),
                                     list(#m=mmode, 
                                          m="m",
                                          var1="g", var2="c"
                                          , base=base, power_out=-power_out
                                          ))
        multfac_out <- 1
        units_out <- units_plot
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m3"
            #units_out <- c("m3 s-1", "m4 s-1", "m3")
        }
    }
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")

} else if (varname == "wkb_vertvel_mode") {
    longname <- "Vertical velocity baroclinic m-mode"
    mmodes <- 1:5
    fname_suffix <- paste0("_modes_", paste0(mmodes, collapse="_"))
    power_out <- 5
    multfac_out <- base^power_out
    units_plot <- "#"
    #units_plot <- c("s-1", "m s-1", "#") # N, c, R 
    var_label_plot <- substitute(paste(S[m], "(z)", " " %~~% " ", var1[m]^0, " sin(",
                                       var2[m]^-1, integral(), "N(z) dz) [#]"
                                       , " " %*% " ", base^power_out
                                       ),
                                 list(#m=mmode, 
                                      m="m",
                                      var1="S", var2="c"
                                      , base=base, power_out=-power_out
                                      ))
    multfac_out <- 1
    units_out <- units_plot
    if (any(out_mode == c("meanint", "depthint"))) {
        multfac_out <- 1
        units_out <- "m2"
    }
    if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_plot <- "m"
        #units_plot <- c("m s-1", "m2 s-1", "m")
        var_label_plot <- substitute(paste(integral(), S[m], "(z) dz", " " %~~% " ", 
                                           integral(), " ", var1[m]^0, " sin(",
                                           var2[m]^-1, integral(), "N(z) dz) dz [#]"
                                           , " " %*% " ", base^power_out
                                           ),
                                     list(#m=mmode, 
                                          m="m",
                                          var1="S", var2="c"
                                          , base=base, power_out=-power_out
                                          ))
        multfac_out <- 1
        units_out <- units_plot
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m3"
            #units_out <- c("m3 s-1", "m4 s-1", "m3")
        }
    }
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")

} else if (varname == "Kv") {
    longname <- "Vertical Diffusivity"
    power_out <- 0
    multfac_out_plot <- base^power_out
    units_out <- paste0("m2 s-1")
    multfac_out <- multfac_out_plot
    var_label_plot <- substitute(paste("Vertical Diffusivity ", K[v], 
                                     " [", var1^2, " ", var2^-1, "]"),
                               list(var1="m", var2="s"))
    if (integrate_depth) {
        power_out <- -4
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         " K", ""[v], " ",
                                         " dz [", var1^3, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.")
    diagsuffix <- c("diag.")
    varname_nc <- c("Kv")
    rotate_inds <- F
    vec <- F

} else if (varname == "Kh") {
    longname <- "Horizontal Diffusivity"
    power_out <- 0
    multfac_out_plot <- base^power_out
    #units_out <- paste0("m^2 s^-1 x ", multfac_out_plot)
    units_out <- paste0("m2 s-1")
    multfac_out <- multfac_out_plot
    #var_label_plot <- substitute(paste("Vertival Diffusivity ", K[v], " [", var1^2, " ", var2^-1, "] " %*% " ", base^power_out),
    #                          list(var1="m", var2="s", base=base, power_out=-power_out))
    var_label_plot <- substitute(paste("Horizontal Diffusivity ", K[h], " [", var1^2, " ", var2^-1, "]"),
                              list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.")
    diagsuffix <- c("diag.")
    varname_nc <- c("Kh")
    rotate_inds <- F
    vec <- F

} else if (varname == "K_GM") {
    longname <- "GM Thickness Diffusivity"
    power_out <- 0
    multfac_out_plot <- base^power_out
    #units_out <- paste0("m^2 s^-1 x ", multfac_out_plot)
    units_out <- paste0("m2 s-1")
    multfac_out <- multfac_out_plot
    #var_label_plot <- substitute(paste("Vertival Diffusivity ", K[v], " [", var1^2, " ", var2^-1, "] " %*% " ", base^power_out),
    #                          list(var1="m", var2="s", base=base, power_out=-power_out))
    var_label_plot <- substitute(paste("GM Thickness Diffusivity ", K[GM], " [", var1^2, " ", var2^-1, "]"),
                              list(var1="m", var2="s"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.")
    diagsuffix <- c("diag.")
    varname_nc <- c("K_GM")
    rotate_inds <- F
    vec <- F

} else if (varname == "ptr1") {
    longname <- "Passive Tracer"
    power_out <- 0
    multfac_out_plot <- base^power_out
    #units_out <- paste0("m^2 s^-1 x ", multfac_out_plot)
    units_out <- paste0("psu")
    multfac_out <- multfac_out_plot
    var_label_plot <- paste("Passive Tracer [", units_out, "]")
    if (integrate_depth) {
        units_out <- "psu m"
        var_label_plot <- paste("Passive Tracer [psu m]")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.")
    diagsuffix <- c("")
    varname_nc <- c("ptr1")
    rotate_inds <- F
    vec <- F
    pal <- colorRampPalette(rev(c("white", "#ccede1", "#99dbc4", "#3ebb75", "#4cbc38",
                                   "#c1df13", "#feef00", "#fabc09", "#f14b1c", "#f47486")))

} else if (varname == "FmKm") {
    longname <- "Mean Wind Stress Energy"
    power_out <- 4
    multfac_out_plot <- base^power_out
    units_out <- paste0("m3 s-3 x ", multfac_out_plot)
    multfac_out <- multfac_out_plot
    var_label_plot <- substitute(paste(rho[0]^-1, " ", bar(bold(u)[h]), 
                                     "" %.% "", bar(bold(tau)[0]), 
                                     "  [", var1^3, " ", units_out^-3, 
                                     "] " %*% " ", base^power_out),
                          list(var1="m", var2="s", 
                               base=base, power_out=-power_out))    
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "forcing.", "forcing.")
    diagsuffix <- c("", "", "diag.", "diag.")
    varname_nc <- c("u", "v", "stress_x", "stress_y")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "FeKe") {
    longname <- "Eddy Wind Stress Energy"
    subtitle <- ">0 eddy growth due to wind anomalies"
    units_out <- "m3 s-3"
    power_plot <- 4
    multfac_plot <- base^power_plot
    units_plot <- paste0("m3 s-3 x ", multfac_plot)
    var_label_plot <- substitute(paste(rho[0]^-1 , " ", 
                                       bar(paste(bold(u)[h], "'" %.% "", bold(tau)[0], "'")),
                                       " [", var1^3, " ", var2^-3, 
                                       "] " %*% " ", base^power_plot),
                                 list(var1="m", var2="s", 
                                      base=base, power_plot=-power_plot))
    typesuffix <- c("oce.", "oce.", "forcing.", "forcing.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "diag.", "diag.")
    varname_nc <- c("u", "v", "stress_x", "stress_y", "tauxu", "tauyv")

} else if (varname == "advh") {
    longname <- "Horizontal advection"
    units_out <- "m s-2"
    power_plot <- 5
    multfac_plot <- base^power_plot
    units_plot <- paste0("m s-2 x ", multfac_plot)
    var_label_plot <- substitute(paste(bold(u)["h"] %.% bold(nabla)[h], bold(u)["h"],
                                     " [m ", var1^-2,
                                     "] " %*% " ", base^-power_plot),
                               list(var1="s",
                                    base=base, power_plot=power_plot))
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "m3 s-2"
    }
    if (integrate_depth) {
        stop("aasd")
        units_out <- "m s-1"
        power_plot <- 4
        multfac_plot <- base^power_plot
        units_plot <- paste0("s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))["h"],
                                         " dz [", var1, " ", var2^-1,
                                         "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s",
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            stop("asd")
            units_out <- "m3 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- rep("", t=2)
    varname_nc <- c("u", "v")
    varname_nc <- c("uo", "vo")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuv") {
    longname <- "Norm of horizontal divergence"
    units_out <- "s-1"
    power_plot <- 5
    multfac_plot <- base^power_plot
    units_plot <- paste0("s-1 x ", multfac_plot)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(bold(u))["h"],
                                     " [", var1^-1,
                                     "] " %*% " ", base^-power_plot),
                               list(var1="s",
                                    base=base, power_plot=power_plot))
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "m2 s-1"
    }
    if (integrate_depth) {
        units_out <- "m s-1"
        power_plot <- 4
        multfac_plot <- base^power_plot
        units_plot <- paste0("s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))["h"],
                                         " dz [", var1, " ", var2^-1,
                                         "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s",
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m3 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- rep("", t=2)
    varname_nc <- c("u", "v")
    varname_nc <- c("uo", "vo")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvrho") {
    longname <- "div_h(u_h rho)"
    power_out <- 3
    multfac_out <- base^power_out
    units_out <- paste0("kg m-3 s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(bold(u))[h], bar(rho),
                                     " [kg ", var1^-3, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 3
        multfac_out <- base^power_out
        units_out <- paste0("kg m-2 s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))[h], bar(rho),
                                         " dz [kg ", var1^-2, " ", var2^-1, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m3 s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo"
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- c("", "", "diag.")
    varname_nc <- c("u", "v", "rho")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvrhoeddy") {
    longname <- "div_h(u_h'rho')"
    power_out <- 7
    multfac_out <- base^power_out
    units_out <- paste0("kg m-3 s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u)[h], "'", rho, "'")),
                                     " [kg ", var1^-3, " ", var2^-1, 
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s", 
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 3
        multfac_out <- base^power_out
        units_out <- paste0("kg m-2 s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(paste(bold(u)[h], "'", rho, "'")),
                                         " dz [kg ", var1^-2, " ", var2^-1, 
                                         "] " %*% " ", base^power_out),
                                   list(var1="m", var2="s", 
                                        base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m3 s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo"
    typesuffix <- rep("oce.", t=5)
    diagsuffix <- c("", "", "diag.", "diag.", "diag.")
    varname_nc <- c("u", "v", "urho", "vrho", "rho")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "divuvb") {
    longname <- "div_h(u_h b)"
    power_out <- 6
    multfac_out <- base^power_out
    units_out <- paste0("m s-3 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(bold(u))[h], bar(b), 
                                     " [", var1, " ", var2^-3, 
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s", 
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 2
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))[h], bar(b),
                                         " dz [", var1^2, " ", var2^-3, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-3")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo"
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- c("", "", "diag.")
    varname_nc <- c("u", "v", "rho")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvbeddy") {
    longname <- "div_h(u_h'b')"
    power_out <- 10
    multfac_out <- base^power_out
    units_out <- paste0("m s-3 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u)[h], "'b'")),
                                     " [", var1, " ", var2^-3, "] " %*% " ", base^power_out),
                               list(var1="m", var2="s", base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 6
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), 
                                         bold(nabla)[h] %.% bar(paste(bold(u)[h], "'b'")), 
                                         " dz [", var1^2, " ", var2^-3, 
                                         "] " %*% " ", base^power_out),
                                   list(var1="m", var2="s", 
                                        base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-3")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo"
    typesuffix <- rep("oce.", t=5)
    diagsuffix <- c("", "", "diag.", "diag.", "diag.")
    varname_nc <- c("u", "v", "urho", "vrho", "rho")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "divuvsgsb") {
    longname <- "div_h(u_sgs_h b)"
    power_out <- 9
    multfac_out <- base^power_out
    units_out <- paste0("m s-3 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(bold(u))["sgs,h"], bar(b),
                                     " [", var1, " ", var2^-3,
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))["sgs,h"], bar(b),
                                         " dz [", var1^2, " ", var2^-3, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-3")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo"
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- rep("diag.", t=3)
    varname_nc <- c("sgs_u", "sgs_v", "rho")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvt") {
    longname <- "Divergence of mean horizontal temperature flux"
    units_out <- "degC s-1"
    power_plot <- 5
    multfac_plot <- base^power_plot
    units_plot <- paste0("degC s-1 x ", multfac_plot)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^-power_plot),
                               list(var1="°C", var2="s",
                                    base=base, power_plot=power_plot))
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "degC m2 s-1"
    }
    if (integrate_depth) {
        units_out <- "degC m s-1"
        power_plot <- 4
        multfac_plot <- base^power_plot
        units_plot <- paste0("degC s-1 x ", multfac_plot)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                         " dz [°C ", var1, " ", var2^-1,
                                         "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s",
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m3 s-1"
        }
    }
    horiz_deriv_tag <- T 
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- rep("", t=3)
    varname_nc <- c("u", "v", "temp")
    varname_nc <- c("uo", "vo", "thetao")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvteddy") {
    longname <- "Divergence of eddy horizontal temperature flux"
    units_out <- units_plot <- "degC s-1"
    power_plot <- 8
    multfac_plot <- base^power_plot
    units_plot <- paste0("degC s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^-power_plot),
                               list(var1="°C", var2="s",
                                    base=base, power_plot=power_plot))
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "degC m2 s-1"
    }
    if (integrate_depth) {
        units_out <- "degC m s-1"    
        power_plot <- 7
        multfac_plot <- base^power_plot
        units_plot <- paste0("degC m s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(paste(bold(u)[h], "'T'")),
                                         " dz [°C ", var1, " ", var2^-1,
                                         "] " %*% " ", base^-power_plot),
                                  list(var1="m", var2="s",
                                       base=base, power_plot=power_plot))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m3 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=5)
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "utemp", "vtemp", "temp")
    if (any(runid == c("Arc22_daily", "Arc22_sub_daily",
                       "Arc22_sub", "Arc22_sub_small"))) {
        varname_nc <- c("u", "v", "ut", "vt", "temp")
    }
    varname_nc <- c("uo", "vo", "uto", "vto", "thetao")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "divuvttot") {
    longname <- "div_h(u_h t)"
    power_out <- 7
    multfac_out <- base^power_out
    units_out <- paste0("degC s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u)["h"], T)),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="°C", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 6
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(paste(bold(u)["h"], T)),
                                         " dz [°C ", var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m3 s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo"
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- rep("diag.", t=2)
    varname_nc <- c("utemp", "vtemp")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvsgsttot") {
    longname <- "Divergence of total horizontal SGS temperature flux"
    units_plot <- "degC s-1"
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u)["sgs,h"], "T")),
                                     " [", var1, " ", var2^-1, "]"
                                     #, " " %*% " ", base^power_out
                                     ),
                               list(var1="°C", var2="s"
                                    #, base=base, power_out=-power_out
                                    ))
    units_out <- units_plot
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "degC m2 s-1"
    }
    if (integrate_depth) {
        units_plot <- "degC m s-1"
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(paste(bold(u)["sgs,h"], "T")),
                                         " dz [°C ", var1, " ", var2^-1, "]"
                                         #, " " %*% " ", base^power_out
                                         ),
                                  list(var1="m", var2="s"
                                       #, base=base, power_out=-power_out
                                       ))
        units_out <- units_plot
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m3 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- rep("diag.", t=2)
    varname_nc <- c("sgs_ut", "sgs_vt")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvsgst") {
    longname <- "Divergence of mean horizontal SGS temperature flux"
    power_out <- 9
    multfac_out <- base^power_out
    units_out <- paste0("degC s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(bold(u))["sgs,h"], bar(T),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="°C", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- "degC m s-1"
        units_plot <- units_out
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))["sgs,h"], bar(T),
                                         " dz [°C ", var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))

        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m3 s-1"
        }
    } else {
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m2 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- c(rep("diag.", t=2), "")
    varname_nc <- c("sgs_u", "sgs_v", "temp")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvsgsteddy") {
    longname <- "Divergence of eddy horizontal SGS temperature flux"
    power_out <- 9
    multfac_out <- base^power_out
    units_out <- paste0("degC s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u), "'", ""["sgs,h"], "T'")),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="°C", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- "degC m s-1"
        units_plot <- units_out
        var_label_plot <- substitute(paste(integral(),
                                           bold(nabla)[h] %.% bar(paste(bold(u), "'", ""["sgs,h"], "T'")),
                                           " dz [°C ", var1, " ", var2^-1,
                                           "] " %*% " ", base^power_out),
                                    list(var1="m", var2="s",
                                         base=base, power_out=-power_out))

        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m3 s-1"
        }
    } else {
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "degC m2 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=5)
    diagsuffix <- c(rep("diag.", t=4), "")
    varname_nc <- c("sgs_u", "sgs_v", "sgs_ut", "sgs_vt", "temp")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "divuvt2") {
    longname <- "grad_laplace_inv_div_h(u_h t)"
    power_out <- 7
    multfac_out <- base^power_out
    units_out <- paste0("m s-1 degC x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h], " ", 
                                     bold(nabla)[h], ""^"-2", "(", 
                                     bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                     ") [", var1, " ", var2^-1,
                                     " °C] " %*% " ", base^power_out),
                               list(var1="m", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-1 degC x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h], " ",
                                         bold(nabla)[h], ""^"-2", "(",
                                         bold(nabla)[h] %.% bar(bold(u))["h"], bar(T),
                                         ") dz [", var1^2, " ", var2^-1,
                                         " °C] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m3 s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo"
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- rep("", t=3)
    varname_nc <- c("u", "v", "temp")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "intz_uvteddy_div") {
    longname <- "Divergent part of depth-integrated eddy temperature flux (eq 4 JM02)"
    units_out <- "degC m2 s-1"
    units_plot <- units_out
    if (F) {
        power_plot <- -8
        multfac_plot <- base^power_plot
        var_label_plot <- substitute(paste("(", integral(), " ", bar(paste(bold(u)[h], "'T'")), " dz)"[div],
                                         " [", degree, var1, " ", var2^2, " ", var3^-1,
                                         "] " %*% base^power_plot),
                                   list(var1="C", var2="m", var3="s",
                                        base=base, power_plot=-power_plot))
    } else if (T) {
        var_label_plot <- substitute(paste("(", integral(), " ", bar(paste(bold(u)[h], "'T'")), " dz)"[div],
                                         " [", degree, var1, " ", var2^2, " ", var3^-1,
                                         "]"),
                                   list(var1="C", var2="m", var3="s"))
    }
    horiz_deriv_tag <- T
    varname_nc <- "dxphi"
    vec <- T

} else if (varname == "dxphi") {
    longname <- "Zonal divergent part of depth-integrated eddy temperature flux (eq 5 JM02)"
    units_out <- "degC m2 s-1"
    units_plot <- units_out
    if (F) {
        power_plot <- -8
        multfac_plot <- base^power_plot
        var_label_plot <- substitute(paste("(", integral(), " ", bar(paste(u, "'T'")), " dz)"[div],
                                         " [", degree, var1, " ", var2^2, " ", var3^-1,
                                         "] " %*% base^power_plot),
                                   list(var1="C", var2="m", var3="s",
                                        base=base, power_plot=-power_plot))
    } else if (T) {
        var_label_plot <- substitute(paste("(", integral(), " ", bar(paste(u, "'T'")), " dz)"[div],
                                         " [", degree, var1, " ", var2^2, " ", var3^-1,
                                         "]"),
                                   list(var1="C", var2="m", var3="s"))
    }
    varname_nc <- "dxphi"

} else if (varname == "dyphi") {
    longname <- "Meridional divergent part of depth-integrated eddy temperature flux (eq 5 JM02)"
    units_out <- "degC m2 s-1"
    units_plot <- units_out
    if (F) {
        power_plot <- -8
        multfac_plot <- base^power_plot
        var_label_plot <- substitute(paste("(", integral(), " ", bar(paste(v, "'T'")), " dz)"[div],
                                         " [", degree, var1, " ", var2^2, " ", var3^-1,
                                         "] " %*% base^power_plot),
                                   list(var1="C", var2="m", var3="s",
                                        base=base, power_plot=-power_plot))
    } else if (T) {
        var_label_plot <- substitute(paste("(", integral(), " ", bar(paste(v, "'T'")), " dz)"[div],
                                         " [", degree, var1, " ", var2^2, " ", var3^-1,
                                         "]"),
                                   list(var1="C", var2="m", var3="s"))
    }
    varname_nc <- "dyphi"
    
} else if (varname == "gradphi") {
    longname <- "Divergent part of depth-integrated eddy temperature flux (eq 5 JM02)"
    units_out <- "degC m2 s-1"
    units_plot <- units_out
    if (F) {
        power_plot <- -8
        multfac_plot <- base^power_plot
        var_label_plot <- substitute(paste("(", integral(), " ", bar(paste(bold(u)[h], "'T'")), " dz)"[div],
                                         " [", degree, var1, " ", var2^2, " ", var3^-1,
                                         "] " %*% base^power_plot),
                                   list(var1="C", var2="m", var3="s",
                                        base=base, power_plot=-power_plot))
    } else if (T) {
        var_label_plot <- substitute(paste("(", integral(), " ", bar(paste(bold(u)[h], "'T'")), " dz)"[div],
                                         " [", degree, var1, " ", var2^2, " ", var3^-1,
                                         "]"),
                                   list(var1="C", var2="m", var3="s"))
    }
    varname_nc <- c("dxphi", "dyphi")
    vec <- T

} else if (varname == "divuvs") {
    longname <- "Divergence of mean horizontal salt flux"
    power_out <- 7
    multfac_out <- base^power_out
    units_plot <- "psu s-1"
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(bold(u))["h"], bar(S),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="psu", var2="s",
                                    base=base, power_out=-power_out))
    multfac_out <- 1
    units_out <- units_plot
    if (any(out_mode == c("meanint", "depthint"))) {
        multfac_out <- 1
        units_out <- "psu m2 s-1"
    }
    if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_plot <- "psu m s-1"
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))["h"], bar(S),
                                         " dz [psu ", var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
        multfac_out <- 1
        units_out <- units_plot
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m3 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- rep("", t=3)
    varname_nc <- c("u", "v", "salt")
    varname_nc <- c("uo", "vo", "so")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvseddy") {
    longname <- "Divergence of eddy horizontal salt flux"
    power_out <- 9
    multfac_out <- base^power_out
    units_plot <- "psu s-1"
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u)[h], "'S'")),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="psu", var2="s",
                                    base=base, power_out=-power_out))
    multfac_out <- 1
    units_out <- units_plot
    if (any(out_mode == c("meanint", "depthint"))) {
        multfac_out <- 1
        units_out <- "psu m2 s-1"
    }
    if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_plot <- "psu m s-1"
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(paste(bold(u)[h], "'S'")),
                                         " dz [psu ", var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
        multfac_out <- 1
        units_out <- units_plot
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m3 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=5)
    diagsuffix <- c("", "", "diag.", "diag.", "")
    varname_nc <- c("u", "v", "usalt", "vsalt", "salt")
    if (any(runid == c("Arc22_daily", "Arc22_sub_daily",
                       "Arc22_sub", "Arc22_sub_small"))) {
        varname_nc <- c("u", "v", "us", "vs", "salt")
    }
    varname_nc <- c("uo", "vo", "uso", "vso", "so")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "divuvsgsstot") {
    longname <- "Divergence of total horizontal SGS salt flux"
    power_out <- 9
    multfac_out <- base^power_out
    units_out <- "psu s-1"
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u)["sgs,h"], "S")),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="psu", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- "psu m s-1"
        units_plot <- units_out
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(paste(bold(u)["sgs,h"], "S")),
                                         " dz [psu ", var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))

        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m3 s-1"
        }
    } else {
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m2 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- rep("diag.", t=2)
    varname_nc <- c("sgs_us", "sgs_vs")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvsgss") {
    longname <- "Divergence of mean horizontal SGS salt flux"
    power_out <- 9
    multfac_out <- base^power_out
    units_out <- paste0("psu s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(bold(u))["sgs,h"], bar(S),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="psu", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- "psu m s-1"
        units_plot <- units_out
        var_label_plot <- substitute(paste(integral(),
                                         bold(nabla)[h] %.% bar(bold(u))["sgs,h"], bar(S),
                                         " dz [psu ", var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))

        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m3 s-1"
        }
    } else {
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m2 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- c(rep("diag.", t=2), "")
    varname_nc <- c("sgs_u", "sgs_v", "salt")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "divuvsgsseddy") {
    longname <- "Divergence of eddy horizontal SGS salt flux"
    power_out <- 9
    multfac_out <- base^power_out
    units_out <- paste0("psu s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% bar(paste(bold(u), "'", ""["sgs,h"], "S'")),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="psu", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- "psu m s-1"
        units_plot <- units_out
        var_label_plot <- substitute(paste(integral(),
                                           bold(nabla)[h] %.% bar(paste(bold(u), "'", ""["sgs,h"], "S'")),
                                           " dz [psu ", var1, " ", var2^-1,
                                           "] " %*% " ", base^power_out),
                                    list(var1="m", var2="s",
                                         base=base, power_out=-power_out))

        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m3 s-1"
        }
    } else {
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "psu m2 s-1"
        }
    }
    horiz_deriv_tag <- T
    typesuffix <- rep("oce.", t=5)
    diagsuffix <- c(rep("diag.", t=4), "")
    varname_nc <- c("sgs_u", "sgs_v", "sgs_us", "sgs_vs", "salt")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- T

} else if (varname == "dzwrho") {
    longname <- "dz(wrho)"
    power_out <- 8
    multfac_out <- base^power_out
    units_out <- paste0("kg m-3 s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(#"P"[m], "K"[m], 
                                    partialdiff[z], " ", bar(w), bar(rho),
                                     " [kg ", var1^-3, " ", var2^-1, 
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s", 
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 2
        multfac_out <- base^power_out
        units_out <- paste0("kg m-2 s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         #"P"[m], "K"[m], 
                                         partialdiff[z], " ", bar(w), bar(rho),
                                         " dz [kg ", var1^-2, " ", var2^-1, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("kg m-2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("kg m-1 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("kg s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("diag.", "")
    varname_nc <- c("rho", "w")
    rotate_inds <- F
    vec <- F

} else if (varname == "dzwb") {
    longname <- "dz(wb)"
    power_out <- 8
    multfac_out <- base^power_out
    units_out <- paste0("m s-3 x ", multfac_out)
    var_label_plot <- substitute(paste(#"P"[m], "K"[m], 
                                    partialdiff[z], " ", bar(w), bar(b),
                                     " [", var1, " ", var2^-3, 
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s", 
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 2
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         #"P"[m], "K"[m], 
                                         partialdiff[z], " ", bar(w), bar(b),
                                         " dz [", var1^2, " ", var2^-3, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-3")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("diag.", "")
    varname_nc <- c("rho", "w")
    rotate_inds <- F
    vec <- F

} else if (varname == "dzwt") {
    longname <- "dz(wT)"
    power_out <- 8
    multfac_out <- base^power_out
    units_out <- paste0("degC s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(partialdiff[z], " ", bar(w), bar(T),
                                     " [", var1, " ", var2^-1, 
                                     "] " %*% " ", base^power_out),
                               list(var1="°C", var2="s", 
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 4
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         partialdiff[z], " ", bar(w), bar(T),
                                         " dz [°C ", var1, " ", var2^-1, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m3 s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "w")
    rotate_inds <- F
    vec <- F

} else if (varname == "dzws") {
    longname <- "dz(wS)"
    power_out <- 8
    multfac_out <- base^power_out
    units_out <- paste0("psu s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(partialdiff[z], " ", bar(w), bar(S),
                                     " [", var1, " ", var2^-1, 
                                     "] " %*% " ", base^power_out),
                               list(var1="psu", var2="s", 
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 2
        multfac_out <- base^power_out
        units_out <- paste0("psu m s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         partialdiff[z], " ", bar(w), bar(S),
                                         " dz [psu ", var1, " ", var2^-1, 
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s", 
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("psu m s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("psu m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("psu m3 s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "")
    varname_nc <- c("salt", "w")
    rotate_inds <- F
    vec <- F

} else if (varname == "dzwbeddy") {
    longname <- "dz(w'b')"
    power_out <- 12
    multfac_out <- base^power_out
    units_out <- paste0("m s-3 x ", multfac_out)
    var_label_plot <- substitute(paste(#"P"[e], "K"[e], 
                                     partialdiff[z], " ", bar(paste("w'b'")),
                                     " [", var1, " ", var2^-3, 
                                     "] " %*% " ", base^power_out),
                              list(var1="m", var2="s", 
                                   base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 8
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(),
                                         #"P"[e], "K"[e],
                                         partialdiff[z], " ", bar(paste("w'b'")),
                                         " dz [", var1^2, " ", var2^-3, 
                                         "] " %*% " ", base^power_out),
                                   list(var1="m", var2="s", 
                                        base=base, power_out=-power_out))
        if (any(out_mode == c("meanint", "depthint"))) {
            units_out <- "m3 s-2"
        }
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-3")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F
    typesuffix <- rep("oce.", t=3)
    diagsuffix <- c("", "diag.", "diag.")
    varname_nc <- c("w", "rho", "wrho")
    rotate_inds <- F
    vec <- F

} else if (varname == "hdiffb") {
    longname <- "Horizontal Buoyancy Diffusion"
    power_out <- 14
    multfac_out <- base^power_out
    units_out <- paste0("m s-3 x ", multfac_out)
    var_label_plot <- substitute(paste(bold(nabla)[h] %.% "K", ""[h], 
                                     " ", bold(nabla)[h], " ", bar(b),
                                     " [", var1, " ", var2^-3,
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 11
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), bold(nabla)[h] %.% "K", ""[h],
                                         bold(nabla)[h], " ", bar(b),
                                         " dz [", var1^2, " ", var2^-3,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo"
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("rho", "Kh")
    rotate_inds <- F
    vec <- F

} else if (varname == "vdiffrho") {
    longname <- "Vertical Density Diffusion"
    power_out <- 5
    multfac_out <- base^power_out
    units_out <- paste0("kg m-3 s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(partialdiff[z], " K", ""[v], " ",
                                     partialdiff[z], " ", bar(rho),
                                     " [kg ", var1^-3, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("kg m-2 s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " ",
                                         partialdiff[z], " K", ""[v], " ",
                                         partialdiff[z], " ", bar(rho),
                                         " dz [kg ", var1^-2, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F # not necessary "geo"
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("rho", "Kv")
    rotate_inds <- F
    vec <- F

} else if (varname == "vdiffb") {
    longname <- "Vertical Buoyancy Diffusion"
    power_out <- 5
    multfac_out <- base^power_out
    units_out <- paste0("m s-3 x ", multfac_out)
    var_label_plot <- substitute(paste(partialdiff[z], " K", ""[v], " ",
                                     partialdiff[z], " ", bar(b),
                                     " [", var1, " ", var2^-3,
                                     "] " %*% " ", base^power_out),
                               list(var1="m", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " ", 
                                         partialdiff[z], " K", ""[v], " ",
                                         partialdiff[z], " ", bar(b),
                                         " dz [", var1^2, " ", var2^-3,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-3")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-3")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F # not necessary "geo"
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("rho", "Kv")
    rotate_inds <- F
    vec <- F

} else if (varname == "vdifft") {
    longname <- "Vertical Temperature Diffusion"
    power_out <- 5
    multfac_out <- base^power_out
    units_out <- paste0("degC s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(partialdiff[z], " K", ""[v], " ",
                                     partialdiff[z], " ", bar(T),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="°C", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " ",
                                         partialdiff[z], " K", ""[v], " ",
                                         partialdiff[z], " ", bar(T),
                                         " dz [°C ", var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("degC m3 s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F # not necessary "geo"
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "diag.")
    varname_nc <- c("temp", "Kv")
    rotate_inds <- F
    vec <- F

} else if (varname == "vdiffs") {
    longname <- "Vertical Salinity Diffusion"
    power_out <- 9
    multfac_out <- base^power_out
    units_out <- paste0("psu s-1 x ", multfac_out)
    var_label_plot <- substitute(paste(partialdiff[z], " K", ""[v], " ",
                                     partialdiff[z], " ", bar(S),
                                     " [", var1, " ", var2^-1,
                                     "] " %*% " ", base^power_out),
                               list(var1="psu", var2="s",
                                    base=base, power_out=-power_out))
    if (integrate_depth) {
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("psu m s-1 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " ",
                                         partialdiff[z], " K", ""[v], " ",
                                         partialdiff[z], " ", bar(S),
                                         " dz [psu ", var1, " ", var2^-1,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    if (!(out_mode == "meanint" || out_mode == "depthint") &&
        integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("psu m s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               !integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("psu m2 s-1")
    } else if ((out_mode == "meanint" || out_mode == "depthint") &&
               integrate_depth) {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("psu m3 s-1")
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- F # not necessary "geo"
    typesuffix <- rep("oce.", t=2)
    diagsuffix <- c("", "diag.")
    varname_nc <- c("salt", "Kv")
    rotate_inds <- F
    vec <- F

} else if (varname == "slopeSx") {
    longname <- "Isoneutral Slope x"
    power_out <- 4
    multfac_out <- base^power_out
    units_out <- paste0("# x ", multfac_out)
    var_label_plot <- substitute(paste(S[x], 
                                     " [#] " %*% " ", base^power_out),
                               list(base=base, power_out=-power_out))
    if (integrate_depth) {
        stop("asd")
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " ",
                                         partialdiff[z], " K", ""[v], " ",
                                         partialdiff[z], " ", bar(b),
                                         " dz [", var1^2, " ", var2^-3,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo" # not necessary "geo"
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "rho"
    rotate_inds <- F
    vec <- F

} else if (varname == "slopeSy") {
    longname <- "Isoneutral Slope y"
    power_out <- 4
    multfac_out <- base^power_out
    units_out <- paste0("# x ", multfac_out)
    var_label_plot <- substitute(paste(S[y],
                                     " [#] " %*% " ", base^power_out),
                               list(base=base, power_out=-power_out))
    if (integrate_depth) {
        stop("asd")
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " ",
                                         partialdiff[z], " K", ""[v], " ",
                                         partialdiff[z], " ", bar(b),
                                         " dz [", var1^2, " ", var2^-3,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo" # not necessary "geo"
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "rho"
    rotate_inds <- F
    vec <- F

} else if (varname == "slopeS") {
    longname <- "Isoneutral Slope"
    power_out <- 4
    multfac_out <- base^power_out
    units_out <- paste0("# x ", multfac_out)
    var_label_plot <- substitute(paste("|", bold(S)[h], "|",
                                     " [#] " %*% " ", base^power_out),
                               list(base=base, power_out=-power_out))
    if (integrate_depth) {
        stop("asd")
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " ",
                                         partialdiff[z], " K", ""[v], " ",
                                         partialdiff[z], " ", bar(b),
                                         " dz [", var1^2, " ", var2^-3,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo" # not necessary "geo"
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "rho"
    rotate_inds <- F
    vec <- T

} else if (varname == "slopeSsq") {
    longname <- "Isoneutral Slope Squared"
    power_out <- 4
    multfac_out <- base^power_out
    units_out <- paste0("# x ", multfac_out)
    var_label_plot <- substitute(paste("|", bold(S)[h], "|"^2,
                                     " [#] " %*% " ", base^power_out),
                               list(base=base, power_out=-power_out))
    if (integrate_depth) {
        stop("asd")
        power_out <- 5
        multfac_out <- base^power_out
        units_out <- paste0("m2 s-3 x ", multfac_out)
        var_label_plot <- substitute(paste(integral(), " ",
                                         partialdiff[z], " K", ""[v], " ",
                                         partialdiff[z], " ", bar(b),
                                         " dz [", var1^2, " ", var2^-3,
                                         "] " %*% " ", base^power_out),
                                  list(var1="m", var2="s",
                                       base=base, power_out=-power_out))
    }
    var_label_plot_roundfac <- 2
    horiz_deriv_tag <- "geo" # not necessary "geo"
    typesuffix <- "oce."
    diagsuffix <- "diag."
    varname_nc <- "rho"
    rotate_inds <- F
    vec <- T

} else if (varname == "opottempmint") {
    longname <- "integral_wrt_depth_of_product_of_sea_water_density_and_potential_temperature"
    units_out <- units_plot <- "degC kg m-2"
    varname_nc <- "opottempmint"
    var_label_plot <- substitute(paste(integral(), " ",
                                       rho, " T dz [°C kg ", var^-2,
                                       "]",
                                       #"] " %*% " ", base^power_out),
                                 list(var="m"
                                      #, base=base, power_out=-power_out
                                      )))

} else if (varname == "somint") {
    longname <- "integral_wrt_depth_of_product_of_sea_water_density_and_salinity"
    units_out <- units_plot <- "g m-2"
    varname_nc <- "somint"
    var_label_plot <- substitute(paste(integral(), " ",
                                       rho, " S dz [g ", var^-2,
                                       "]",
                                       #"] " %*% " ", base^power_out),
                                 list(var="m"
                                      #, base=base, power_out=-power_out
                                      )))

} else if (varname == "uv_bott_force_mean") {
    longname <- "Mean Bottom Stress Energy"
    C_d <- 0.0025 # Quadratic bottom drag coeff.
    subtitle <- paste0("Quadr. bott. drag coeff. C_d=", C_d)
    power_out <- 4
    multfac_out_plot <- base^power_out
    units_out <- paste0("Nm/s3 x ", multfac_out_plot)
    multfac_out <- multfac_out_plot
    var_label_plot <- substitute(paste("Mean Bottom Stress Energy |", bold(u)[h], "" %.% "",
                                     bold(tau)[-h], "|   [N m ", units_out^-3, "] " %*% " ",
                                     base^power_out),
                          list(var="s", base=base, power_out=-power_out))
    var_label_plot_roundfac <- 3
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "uv_bott_force_eddy") {
    longname <- "Eddy Bottom Stress Energy"
    C_d <- 0.0025 # Quadratic bottom drag coeff.
    if (depths != "bottom") stop(paste0("choose 'bottom' as depth for ", varname))
    subtitle <- paste0("Quadr. bott. drag coeff. C_d = ", C_d)    
    power_out <- 0
    multfac_out_plot <- base^power_out
    #units_out <- paste0("m^2 s^-1 x ", multfac_out_plot)
    units_out <- paste0("Nm/s3")
    multfac_out <- multfac_out_plot
    var_label_plot <- substitute(paste("Eddy Bottom Stress Energy |", bold(u)[h], "'" %.% "",
                                     bold(tau)[-h], "'|   [N m ", units_out^-3, "] " %*% " ",
                                     base^power_out),
                          list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "forcing.", "forcing.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "diag.", "diag.")
    varname_nc <- c("u", "v", "stress_x", "stress_y", "tauxu", "tauyv")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "MOCw") {
    longname <- "MOCw"
    regular_dy_moc <- 1/2 # degree
    units_out <- "Sv" 
    var_label_plot <- "MOC [Sv]"
    horiz_deriv_tag <- T # for cluster vol 
    typesuffix <- "oce."
    diagsuffix <- ""
    varname_nc <- "w"
    varname_nc <- "wo"

} else if (varname == "MOCv") {
    longname <- "MOCv"
    regular_dy_moc <- 1/2
    units_out <- "Sv" 
    var_label_plot <- "MOC [Sv]"
    horiz_deriv_tag <- T # for cluster vol 
    typesuffix <- "oce."
    diagsuffix <- ""
    varname_nc <- "v"
    varname_nc <- "vo"

} else if (varname == "Tair") {
    longname <- "Air Temperature 2m"
    units_out <- "degC"
    var_label_plot <- expression(paste("Air Temperature [", degree, "C]"))
    horiz_deriv_tag <- F
    multfac_out <- 1
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("tair")
    rotate_inds <- F
    vec <- F

} else if (varname == "runoff") {
    longname <- "Runoff"
    units_out <- "m a-1"
    var_label_plot <- expression(paste("Runoff [m a"^"-1","]"))
    horiz_deriv_tag <- F
    multfac_out <- 86400*365 # m/s --> m/a
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("runoff")
    rotate_inds <- F
    vec <- F

} else if (varname == "fwflux") {
    longname <- "Freshwater Flux"
    units_out <- "km3 a-1"
    var_label_plot <- expression(paste("FW Flux [km"^"3"," a"^"-1","]"))
    var_label_plot_roundfac <- 4
    horiz_deriv_tag <- "geo"
    multfac_out <- 86400*365/1e9 # m^3/s --> km^3/a
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("runoff")
    rotate_inds <- F
    vec <- F

} else if (varname == "shum") {
    longname <- "Air Specific Humidity"
    units_out <- "g kg-1"
    var_label_plot <- expression(paste("Air Specific Humidity [g kg"^"-1","]"))
    horiz_deriv_tag <- F
    multfac_out <- 1e3 # kg/kg --> g/kg
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("shum")
    rotate_inds <- F
    vec <- F

} else if (varname == "swrd") {
    longname <- "Atmosphere Shortwave Radiation"
    subtitle <- "<0 out of the ocean"
    units_out <- "W m-2"
    var_label_plot <- expression(paste("Atmosphere Shortwave Radiation [W m"^"-2","]"))
    horiz_deriv_tag <- F
    multfac_out <- 1
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("swrd")
    rotate_inds <- F
    vec <- F

} else if (varname == "lwrd") {
    longname <- "Atmosphere Longwave Radiation"
    subtitle <- "<0 out of the ocean"
    units_out <- "W m-2"
    var_label_plot <- expression(paste("Atmosphere Longwave Radiation [W m"^"-2","]"))
    horiz_deriv_tag <- F
    multfac_out <- 1
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("lwrd")
    rotate_inds <- F
    vec <- F

} else if (varname == "olat") {
    longname <- "Latent Heat Flux To Ocean"
    subtitle <- "<0 out of the ocean"
    units_out <- "W m-2"
    var_label_plot <- expression(paste("Latent Heat Flux To Ocean [W m"^"-2","]"))
    horiz_deriv_tag <- F
    multfac_out <- 1
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("olat")
    rotate_inds <- F
    vec <- F

} else if (varname == "osen") {
    longname <- "Sensible Heat Flux To Ocean"
    subtitle <- "<0 out of the ocean"
    units_out <- "W m-2"
    var_label_plot <- expression(paste("Sensible Heat Flux To Ocean [W m"^"-2","]"))
    horiz_deriv_tag <- F
    multfac_out <- 1
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("osen")
    rotate_inds <- F
    vec <- F

} else if (varname == "qnet") {
    longname <- "Net heat flux to ocean"
    subtitle <- "<0 ocean heat loss"
    units_out <- "W m-2"
    var_label_plot <- expression(paste("Q"[net], " [W m"^"-2","]"))
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("qnet")

} else if (varname == "wnet") {
    longname <- "Net freshwater flux to ocean"
    subtitle <- ">0 into ocean"
    units_out <- "m s-1"
    power_out <- 0
    multfac_out <- base^power_out
    var_label_plot <- expression(paste("Wnet [m s"^"-1","]"))
    if (out_mode == "meanint" || out_mode == "depthint") {
        power_out <- -6
        multfac_out <- base^power_out
        #units_out <- paste0("m3 s-1 x ", multfac_out)
        units_out <- "Sv"
    }
    horiz_deriv_tag <- F
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("wnet")
    rotate_inds <- F
    vec <- F

} else if (varname == "wind") {
    longname <- "Wind Speed"
    units_out <- "m s-1"
    units_plot <- units_out
    var_label_plot <- expression(paste("Wind [m s"^"-1","]"))
    typesuffix <- c("forcing.", "forcing.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("uwind", "vwind")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "curlwind") {
    longname <- "Curl of Wind Speed"
    units_out <- "s-1"
    var_label_plot <- expression(paste("Curl of Wind Speed [s"^"-1","]"))
    horiz_deriv_tag <- "geo"
    multfac_out <- 1
    typesuffix <- c("forcing.", "forcing.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("uwind", "vwind")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "taux") {
    longname <- "Meridional Wind Stress"
    units_out <- "N m-2"
    var_label_plot <- expression(paste("Meridional Wind Stress [N m"^"-2","]"))
    horiz_deriv_tag <- F
    multfac_out <- 1
    typesuffix <- c("forcing.", "forcing.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("stress_x", "stress_y")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "tauy") {
    longname <- "Zonal Wind Stress"
    units_out <- "N m-2"
    var_label_plot <- expression(paste("Zonal Wind Stress [N m"^"-2","]"))
    horiz_deriv_tag <- F
    multfac_out <- 1
    typesuffix <- c("forcing.", "forcing.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("stress_x", "stress_y")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "tau") {
    longname <- "Norm of Wind Stress"
    units_out <- "N m-2"
    power_out <- 0
    multfac_out <- base^power_out
    var_label_plot <- expression(paste("Norm of Wind Stress [N m"^"-2","]"))
    if (out_mode == "meanint" || out_mode == "depthint") {
        power_out <- 12
        multfac_out <- base^-power_out
        units_out <- paste0("N x 1e", power_out)
    }
    horiz_deriv_tag <- F
    typesuffix <- c("forcing.", "forcing.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("stress_x", "stress_y")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "curltau") {
    longname <- "Wind Stress Curl"
    units_out <- "N m-3"
    power_plot <- 7
    multfac_plot <- base^power_plot
    units_plot <- paste0("N m-3 x ", multfac_plot)
    var_label_plot <- substitute(paste(bold(k), "" %.% "(", 
                                 bold(nabla), " " %*% " ", bold(tau), ") ",
                                 "[N ", var^-3, "] " %*% " ", base^-power_plot),
                                 list(var="m", base=base, 
                                      power_plot=power_plot))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("forcing.", "forcing.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("stress_x", "stress_y")
    rotate_inds <- c(1, 2)
    curltau_levels <- seq(-2, 2, l=100)
    curltau_palname <- "ncview_blu_red"

} else if (varname == "ekmanP") {
    longname <- "Ekman Pumping"
    subtitle <- ">0 upwelling"
    power_out <- 3
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("kg s-1 m-2 x ", multfac_out_plot)
    var_label_plot <- substitute(paste(bold(k), "" %.% "(", 
                                     bold(nabla), "" %*% "", bold(tau), "/f) ",
                                     "[kg ", var1^-1, " ", var2^-2, 
                                     "] " %*% " ", base^power_out),
                               list(var1="s", var2="m", 
                                    base=base, power_out=-power_out))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("forcing.", "forcing.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("stress_x", "stress_y")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "twindenergy") {
    longname <- "Total Wind Energy"
    power_out <- 0
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("W m-2")
    var_label_plot <- substitute(paste(bar(paste(bold(tau), "" %.% "", bold(u)[h])), " ",
                                     "[W ", var1^-2, " ",
                                     "]"),
                               list(var1="m"))
    if (out_mode == "meanint") {
        power_out <- 12
        multfac_out <- base^-power_out
        units_out <- paste0("TW")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("tauxu", "tauyv")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "mwindenergy") {
    longname <- "Mean Wind Energy"
    power_out <- 0
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("W m-2")
    var_label_plot <- substitute(paste(bar(bold(tau)), "" %.% "", bar(bold(u)[h]), " ",
                                     "[W ", var1^-2, " ",
                                     "]"),
                               list(var1="m"))
    if (out_mode == "meanint") {
        power_out <- 12
        multfac_out <- base^-power_out      
        units_out <- paste0("TW")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "forcing.", "forcing.")
    diagsuffix <- c("", "", "diag.", "diag.")
    varname_nc <- c("u", "v", "stress_x", "stress_y")
    rotate_inds <- c(1, 2, 3, 4)
    vec <- F

} else if (varname == "ewindenergy") {
    longname <- "Eddy Wind Energy"
    power_out <- 0
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("W m-2")
    var_label_plot <- substitute(paste(bar(paste(bold(tau), "'" %.% "", 
                                               bold(u)[h], "'")), " ",
                                     "[W ", var1^-2, " ",
                                     "]"),
                               list(var1="m"))
    if (out_mode == "meanint") {
        power_out <- 12
        multfac_out <- base^-power_out
        units_out <- paste0("TW")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "forcing.", "forcing.", "oce.", "oce.")
    diagsuffix <- c("", "", "diag.", "diag.", "diag.", "diag.")
    varname_nc <- c("u", "v", "stress_x", "stress_y", "tauxu", "tauyv") # in my case, tauxu,tauyv are only surface!
    rotate_inds <- c(1, 2, 3, 4, 5, 6)
    vec <- F

} else if (varname == "virtual_salt") {
    longname <- "Virtual Salt"
    subtitle <- ">0 increases salinity"
    units_out <- "psu m s-1"
    power_plot <- 5
    multfac_plot <- base^power_plot
    units_plot <- paste0("psu m s-1 x ", multfac_plot)
    var_label_plot <- substitute(paste("Virtual Salt Flux [psu m ", var^-1, 
                                     "] " %*% " ", base^-power_plot),
                              list(var="s", 
                                   base=base, power_plot=power_plot))
    if (out_mode == "meanint") {
        #power_out <- 0
        #multfac_out <- base^power_out
        #units_out <- paste0("m3 s-1 psu")
        power_out <- -6
        multfac_out <- base^power_out
        units_out <- "Sv psu"
    }
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("virtual_salt")

} else if (varname == "relax_salt") {
    longname <- "Saltinity Relaxation"
    subtitle <- ">0 increases salinity"
    power_out <- 6
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("psu m s-1 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Salinity Relaxation [psu m ", units_out^-1, 
                                     "] " %*% " ", base^power_out),
                              list(var="s", 
                                   base=base, power_out=-power_out))
    if (out_mode == "meanint") {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-1 psu")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "cdrag") {
    longname <- "Drag Coefficient"
    power_out <- 3
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("# x ", multfac_out_plot)
    var_label_plot <- substitute(paste("C"[D], " [#] " %*% " ", base^power_out),
                              list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("forcing.")
    diagsuffix <- c("diag.")
    varname_nc <- c("cd")
    rotate_inds <- F
    vec <- F

} else if (varname == "Ftemp") {
    longname <- "Temperature flux to ocean"
    subtitle <- ">0 surface temperature gain"
    units_out <- "m s-1 degC"
    power_plot <- 6
    multfac_plot <- base^power_plot
    units_plot <- paste0("m s-1 degC x ", multfac_plot)
    var_label_plot <- substitute(paste("Temperature flux to ocean [°C ",
                                     var1, " ", var2^-1, "] " %*% " ",
                                     base^-power_plot),
                              list(var1="m", var2="s", base=base,
                                   power_plot=power_plot))
    if (out_mode == "meanint") {
        units_out <- "m3 s-1 degC"
    }
    typesuffix <- c("oce.", "forcing.")
    diagsuffix <- c("diag.", "diag.")
    varname_nc <- c("rho", "qnet")

} else if (varname == "Fsalt") {
    longname <- "Salt flux to ocean"
    subtitle <- ">0 surface salt gain"
    power_out <- 6
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m s-1 psu x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Salt flux to ocean [psu ",
                                     var1, " ", var2^-1, "] " %*% " ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    if (out_mode == "meanint") {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-1 psu")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "ice.", rep("forcing.", t=6))
    diagsuffix <- c("", rep("diag.", t=6))
    varname_nc <- c("salt", "thdgr", "snow", "rain", "evap", "runoff", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "Fsalt2") {
    longname <- "Salt flux to ocean"
    subtitle <- ">0 surface salt gain"
    power_out <- 6
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m s-1 psu x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Salt flux to ocean [psu ",
                                     var1, " ", var2^-1, "] " %*% " ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    if (out_mode == "meanint") {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m3 s-1 psu")
    }
    horiz_deriv_tag <- F
    typesuffix <- rep("forcing.", t=2)
    diagsuffix <- rep("diag.", t=2)
    varname_nc <- c("virtual_salt", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "Fthermal") {
    longname <- "Thermal density flux to ocean"
    subtitle <- ">0 surface density gain"
    power_out <- 6
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("kg m-2 s-1 x ", multfac_out_plot) 
    var_label_plot <- substitute(paste("Thermal density flux to ocean [kg ", 
                                     var1^-2, " ", var2^-1, "] " %*% "  ", 
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out)) 
    if (out_mode == "meanint") {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("kg s-1")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "forcing.")
    diagsuffix <- c("", "", "diag.")
    varname_nc <- c("temp", "salt", "qnet")
    rotate_inds <- F
    vec <- F

} else if (varname == "Fthermalbudget") {
    longname <- "Thermal density flux to ocean"
    subtitle <- ">0 surface density gain"
    power_out <- 6
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("kg m-2 s-1 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Thermal density flux to ocean [kg ",
                                     var1^-2, " ", var2^-1, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", rep("forcing.", t=5))
    diagsuffix <- c("", "", rep("diag.", t=5))
    varname_nc <- c("temp", "salt", "swrd", "lwrd", "olwout", "osen", "olat")
    rotate_inds <- F
    vec <- F

} else if (varname == "Fhaline") {
    longname <- "Haline density flux to ocean"
    subtitle <- ">0 surface density gain"
    power_out <- 6
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("kg m-2 s-1 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Haline density flux to ocean [kg ",
                                     var1^-2, " ", var2^-1, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    if (out_mode == "meanint") {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("kg s-1")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "ice.", rep("forcing.", t=6))
    diagsuffix <- c("", "", rep("diag.", t=6))
    varname_nc <- c("temp", "salt", "thdgr", "snow", "rain", "evap", "runoff", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "Fhalinebudget") {
    longname <- "Haline density flux to ocean"
    subtitle <- ">0 surface density gain"
    power_out <- 6
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("kg m-2 s-1 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Haline density flux to ocean [kg ",
                                     var1^-2, " ", var2^-1, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "ice.", rep("forcing.", t=6))
    diagsuffix <- c("", "", rep("diag.", t=6))
    varname_nc <- c("temp", "salt", "thdgr", "snow", "rain", "evap", "runoff", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "Frho") {
    longname <- "Density flux to ocean"
    subtitle <- ">0 surface density gain"
    units_out <- "kg m-2 s-1"
    power_plot <- 6
    multfac_plot <- base^power_plot
    units_plot <- paste0("kg m-2 s-1 x ", base^-power_plot)
    var_label_plot <- substitute(paste("Density flux to ocean [kg ",
                                     var1^-2, " ", var2^-1, "] " %*% " ",
                                     base^-power_plot),
                              list(var1="m", var2="s", base=base,
                                   power_plot=power_plot))
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "kg s-1"
    }
    varname_nc <- c("temp", "salt", "thdgr", "qnet", "snow", "rain", "evap", "runoff", "relax_salt")
    typesuffix <- c("oce.", "oce.", "ice.", rep("forcing.", t=6))
    diagsuffix <- c("", "", rep("diag.", t=7))

} else if (varname == "Frho2") {
    longname <- "Density flux to ocean"
    subtitle <- ">0 surface density gain"
    units_out <- "kg m-2 s-1"
    power_plot <- 6
    multfac_plot <- base^power_plot
    units_plot <- paste0("kg m-2 s-1 x ", base^-power_plot)
    var_label_plot <- substitute(paste("Density flux to ocean [kg ",
                                     var1^-2, " ", var2^-1, "] " %*% " ",
                                     base^-power_plot),
                              list(var1="m", var2="s", base=base,
                                   power_plot=power_plot))
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "kg s-1"
    }
    varname_nc <- c("temp", "salt", "qnet", "virtual_salt", "relax_salt") 
    typesuffix <- c("oce.", "oce.", "forcing.", "forcing.", "forcing.") 
    diagsuffix <- c("", "", "diag.", "diag.", "diag.")

} else if (varname == "Frhobudget") {
    longname <- "Density flux to ocean"
    subtitle <- ">0 surface density gain"
    units_out <- "kg m-s s-1"
    power_plot <- 6
    multfac_plot <- base^power_plot
    units_plot <- paste0("kg m-2 s-1 x ", multfac_plot)
    var_label_plot <- substitute(paste("Density flux to ocean [kg ",
                                     var1^-2, " ", var2^-1, "] " %*% "  ",
                                     base^-power_plot),
                              list(var1="m", var2="s", base=base,
                                   power_plot=power_plot))
    if (any(out_mode == c("meanint", "depthint"))) {
        units_out <- "kg s-1"
    }
    typesuffix <- c("oce.", "oce.", # temp salt 
                     rep("forcing.", t=5), # swrd lwrd olwout osen olat
                     "ice.", # thdgr
                     rep("forcing.", t=5)) # snow rain evap runoff relax_salt
    diagsuffix <- c("", "", 
                     rep("diag.", t=11))
    varname_nc <- c("temp", "salt", 
                        "swrd", "lwrd", "olwout", "osen", "olat",
                        "thdgr", "snow", "rain", "evap", "runoff", "relax_salt")

} else if (varname == "FrhermalB") {
    longname <- "Thermal buoyancy flux to ocean"
    subtitle <- ">0 surface buoyancy gain"
    power_out <- 8
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m2 s-3 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Thermal buoyancy flux to ocean [",
                                     var1^2, " ", var2^-3, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "forcing.")
    diagsuffix <- c("", "", "diag.")
    varname_nc <- c("temp", "salt", "qnet")
    rotate_inds <- F
    vec <- F

} else if (varname == "FthermalBbudget") {
    longname <- "Thermal buoyancy flux to ocean"
    subtitle <- ">0 surface buoyancy gain"
    power_out <- 8
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m2 s-3 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Thermal buoyancy flux to ocean [",
                                     var1^2, " ", var2^-3, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", rep("forcing.", t=5))
    diagsuffix <- c("", "", rep("diag.", t=5))
    varname_nc <- c("temp", "salt", "swrd", "lwrd", "olwout", "osen", "olat")
    rotate_inds <- F
    vec <- F

} else if (varname == "FhalineB") {
    longname <- "Haline buoyancy flux to ocean"
    subtitle <- ">0 surface buoyancy gain"
    power_out <- 8
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m2 s-3 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Haline buoyancy flux to ocean [",
                                     var1^2, " ", var2^-3, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "ice.", rep("forcing.", t=6))
    diagsuffix <- c("", "", rep("diag.", t=6))
    varname_nc <- c("temp", "salt", "thdgr", "snow", "rain", "evap", "runoff", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "FhalineBbudget") {
    longname <- "Haline buoyancy flux to ocean"
    subtitle <- ">0 surface buoyancy gain"
    power_out <- 8
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m2 s-3 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Haline buoyancy flux to ocean [",
                                     var1^2, " ", var2^-3, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "ice.", rep("forcing.", t=6))
    diagsuffix <- c("", "", rep("diag.", t=6))
    varname_nc <- c("temp", "salt", "thdgr", "snow", "rain", "evap", "runoff", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "FrhoB") {
    longname <- "Buoyancy flux to ocean"
    subtitle <- ">0 surface buoyancy gain"
    power_out <- 8
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m2 s-3 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Buoyancy flux to ocean [",
                                     var1^2, " ", var2^-3, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", "ice.", rep("forcing.", t=6))
    diagsuffix <- c("", "", rep("diag.", t=7))
    varname_nc <- c("temp", "salt", "thdgr", "qnet", "snow", "rain", "evap", "runoff", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "FrhoB2") {
    longname <- "Buoyancy flux to ocean"
    subtitle <- ">0 surface buoyancy gain"
    power_out <- 8
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m2 s-3 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Buoyancy flux to ocean [ ",
                                     var1^2, " ", var2^-3, 
                                     "] " %*% " ", base^power_out),
                              list(var1="m", var2="s", 
                                   base=base, power_out=-power_out))
    if (out_mode == "meanint") {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-3")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.", rep("forcing.", t=3))
    diagsuffix <- c("", "", rep("diag.", t=3))
    varname_nc <- c("temp", "salt", "qnet", "virtual_salt", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "FrhoBudget") {
    longname <- "Buoyancy flux to ocean"
    subtitle <- ">0 surface buoyancy gain"
    power_out <- 8
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m2 s-3 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Buoyancy flux to ocean [kg ",
                                     var1^-2, " ", var2^-1, "] " %*% "  ",
                                     base^power_out),
                              list(var1="m", var2="s", base=base,
                                   power_out=-power_out))
    if (out_mode == "meanint") {
        power_out <- 0
        multfac_out <- base^power_out
        units_out <- paste0("m4 s-3")
    }
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.",
                     rep("forcing.", t=5),
                     "ice.",
                     rep("forcing.", t=5))
    diagsuffix <- c("", "",
                     rep("diag.", t=11))
    varname_nc <- c("temp", "salt",
                        "swrd", "lwrd", "olwout", "osen", "olat",
                        "thdgr", "snow", "rain", "evap", "runoff", "relax_salt")
    rotate_inds <- F
    vec <- F

} else if (varname == "uice") {
    longname <- "Sea Ice Zonal Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("Sea Ice Zonal Velocity [m s"^"-1","]"))
    multfac_out <- 1
    horiz_deriv_tag <- F
    typesuffix <- c("ice.", "ice.")
    diagsuffix <- c("", "")
    varname_nc <- c("uice", "vice")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "vice") {
    longname <- "Sea Ice Meridional Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("Sea Ice Meridional Velocity [m s"^"-1","]"))
    multfac_out <- 1
    horiz_deriv_tag <- F
    typesuffix <- c("ice.", "ice.")
    diagsuffix <- c("", "")
    varname_nc <- c("uice", "vice")
    rotate_inds <- c(1, 2)
    vec <- F

} else if (varname == "hvelice") {
    longname <- "Horizontal Ice Velocity"
    units_out <- "m s-1"
    var_label_plot <- expression(paste("Horizontal Ice Velocity [m s"^"-1","]"))
    multfac_out <- 1
    horiz_deriv_tag <- F
    typesuffix <- c("ice.", "ice.")
    diagsuffix <- c("", "")
    varname_nc <- c("uice", "vice")
    rotate_inds <- c(1, 2)
    vec <- T

} else if (varname == "sic") {
    longname <- "Sea Ice Concentration"
    sic_thr <- 0.15 # same units_out as FESOM sea ice concentration variable ('area')
    sic_cond <- ">"
    #subtitle <- paste0("sic ", sic_cond, " ", 100*sic_thr, " %")
    nsidc_iceedge <- F
    if (nsidc_iceedge) {
        nsidc_path <- "/work/ba0941/a270073/data/NSIDC/SMMR/"
        nsidc_thr <- 15 # [%]
        subtitle <- paste0("NSIDC sea ice concentration > ", nsidc_thr, "% climatology")
    }
    power_out <- 2 
    multfac_out <- base^power_out # # [0,1] --> [0,100]
    units_out <- "%"
    var_label_plot <- expression(paste("Sea Ice concentration [%]"))
    horiz_deriv_tag <- F
    typesuffix <- c("ice.")
    diagsuffix <- c("")
    varname_nc <- c("area")
    varname_nc <- "sic"

} else if (varname == "hice") {
    longname <- "Sea Ice Thickness"
    units_out <- "m"
    var_label_plot <- expression(paste("Effective Sea Ice Thickness [m]"))
    multfac_out <- 1
    horiz_deriv_tag <- F
    typesuffix <- c("ice.")
    diagsuffix <- c("")
    varname_nc <- c("hice")
    rotate_inds <- F
    vec <- F

} else if (varname == "iceextent") {
    longname <- "Sea Ice Extent"
    sic_thr <- 0.15 # same units_out as FESOM sea ice concentration variable ('area') 
    sic_cond <- ">"
    sic_cond_fname <- "gt" ## "gt" "ge" "st" "se"
    subtitle <- paste0("sic ", sic_cond, " ", 100*sic_thr, " %")
    power_out <- 6 # [m^2] --> [km^2]
    multfac_out <- base^-power_out
    units_out <- "km2"
    var_label_plot <- substitute(paste("Sea Ice Extent [", units_out^2, "]"),
                                 list(units_out="km"))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("ice.")
    diagsuffix <- c("")
    varname_nc <- c("area")
    varname_nc <- "sisnconc"

} else if (varname == "siextentn") {
    longname <- "NH Sea Ice Extent (15%)"
    varname_nc <- varname
    var_label_plot <- substitute(paste("NH sea ice extent [Mio ", units_out^2, "]"),
                                 list(units_out="km"))

} else if (varname == "siextents") {
    longname <- "SH Sea Ice Extent (15%)"
    varname_nc <- varname
    var_label_plot <- substitute(paste("SH sea ice extent [Mio ", units_out^2, "]"),
                                 list(units_out="km"))

} else if (varname == "siarean") {
    var_label_plot <- substitute(paste("Arctic sea ice extent [Mio ", units_out^2, "]"),
                                 list(units_out="km"))
    varname_nc <- "siarean"

} else if (varname == "icevol") {
    longname <- "Sea Ice Volume"
    sic_thr <- NULL #0.15 # same units_out as FESOM sea ice concentration variable ('area')
    sic_cond <- NULL #">" # choose: ">", ">=", "<", "<="
    sic_cond_fname <- NULL
    #subtitle <- paste0("sic ", sic_cond, " ", 100*sic_thr, " %")
    power_out <- 9 # [m^3] --> [km^3]
    multfac_out <- base^-power_out
    units_out <- paste0("km3")
    var_label_plot <- substitute(paste("Sea Ice Volume [", units_out^3, "]"),
                              list(var="km"))
    horiz_deriv_tag <- "geo"
    typesuffix <- c("ice.", "ice.")
    diagsuffix <- c("", "")
    varname_nc <- c("area", "hice")
    rotate_inds <- F
    vec <- F

} else if (varname == "hsnow") {
    longname <- "Snow Thickness"
    units_out <- "m"
    var_label_plot <- expression(paste("Effective Snow Thickness [m]"))
    multfac_out <- 1
    horiz_deriv_tag <- F
    typesuffix <- c("ice.")
    diagsuffix <- c("")
    varname_nc <- c("hsnow")
    rotate_inds <- F
    vec <- F

} else if (varname == "thdgr") {
    longname <- "Growth Rate of eff. Ice Thickness"
    subtitle <- ">0 sea ice formation"
    power_out <- 8
    multfac_out <- base^power_out
    multfac_out_plot <- base^-power_out
    units_out <- paste0("m s-1 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Thermodynamic Growth Rate of eff. Ice Thickness [m ", 
                                    units_out^-1, "] " %*% " ", base^power_out),
                              list(var="s", base=base, power_out=-power_out))
    horiz_deriv_tag <- F
    typesuffix <- c("ice.")
    diagsuffix <- c("diag.")
    varname_nc <- c("thdgr")
    rotate_inds <- F
    vec <- F

} else if (varname == "transport") {

    ## set 'csec_cond*' if you want to calculate the transport
    ## of  specific water mass if out_mode='csec_mean', e.g.
    ## only water that is denser than 27.8 kg m-3, see examples below.
    # 'csec_cond_vars':
    #   these follow the same naming convention as for the variable 'varname_nc'
    # 'csec_conds':
    #   gt = greather than, ge = greater than or equal to
    #   ge = greater than or equal to
    #   lt = less than
    #   le = less equal or equal to
    #   NULL = no constrain

    ## 3 transport conditions
    if (F) {
        csec_cond_vars <- c("potdens", "potdens", "v")
        csec_conds <- c("gt", "lt", "lt")
        csec_cond_vals <- c(27.8, 27.88, 0)
        csec_cond_units <- c("kgm-3", "kgm-3", "ms-1")
    }
    ## 2 transport conditions
    if (F) {
        csec_cond_vars <- c("potdens", "u")
        csec_conds <- c("gt", "lt") 
        csec_cond_vals <- c(27.8, 0)
        csec_cond_units <- c("kgm-3", "ms-1")
    }
    if (T) {
        csec_cond_vars <- c("potdens", "v")
        csec_conds <- c("gt", "lt")
        csec_cond_vals <- c(27.8, 0)
        csec_cond_units <- c("kgm-3", "ms-1")
    }
    if (F) {
        csec_cond_vars <- c("potdens", "v")
        csec_conds <- c("gt", "lt")
        csec_cond_vals <- c(27.88, 0)
        csec_cond_units <- c("kgm-3", "ms-1")
    }
    ## 1 transport condition
    if (F) {
        csec_cond_vars <- "v"
        csec_conds <- "lt"
        csec_cond_vals <- 0
        csec_cond_units <- "ms-1"
    }
    if (T) {
        csec_cond_vars <- "potdens"
        csec_conds <- "gt"
        #csec_conds <- "lt"
        csec_cond_vals <- 27.8
        csec_cond_units <- "kgm-3"
    }
    if (F) {
        csec_cond_vars <- "rho"
        csec_conds <- "gt"
        csec_cond_vals <- 27.8
        csec_cond_units <- "kgm-3"
    }
    ## No transport conditions
    if (F) {
        csec_conds <- NULL
        csec_cond_vars <- NULL
        csec_cond_vals <- NULL
        csec_cond_units <- NULL
    }

    longname <- "Transport"
    var_label_plot <- expression(paste("Transport [Sv]"))
    units_out <- "Sv"
    multfac_out <- 1
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.") # defeault
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    if (!is.null(csec_conds)) { # if there are conditions
        if (any(csec_cond_vars == "rho")) {
            typesuffix <- c("oce.", "oce.", "oce.")
            diagsuffix <- c("", "", "diag.")
            varname_nc <- c("u", "v", "rho")
        } else if (any(csec_cond_vars == "potdens")) {
            typesuffix <- c("oce.", "oce.", "oce.", "oce.")
            diagsuffix <- c("", "", "", "")
            varname_nc <- c("u", "v", "temp", "salt")
        }
    }
    rotate_inds <- c(1, 2) # u, v
    vec <- F

} else if (varname == "bathy") {
    longname <- "Bathymetry"
    units_out <- "m"
    units_plot <- "m"
    var_label_plot <- "Bathymetry [m]"
    # GEBCO (General Bathymetric Chart of the Oceans) colors:
    bathy_cols <- c("#c5ebdc", "#a0dfda", "#7fd5e8", "#5ecbe6", "#49add9",
                    "#3d82c9", "#3259af", "#26468b", "#212f5c", "#141c34")
    #bathy_cols <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", 
    #                "yellow", "#FF7F00", "red", "#7F0000")
    axis.addzlims <- T

} else if (varname == "gradbathy") {
    longname <- "Norm of horizontal bathymetry gradient"
    units_out <- "#"
    units_plot <- units_out
    var_label_plot <- substitute(paste("|", bold(nabla)[h], " H| [#]"))
    horiz_deriv_tag <- "geo"

} else if (varname == "hvel_dot_gradbathy") {
    longname <- "Scalar product of horizontal velocity and horizontal bathymetry gradient times -1"
    subtitle <- "> 0 upwards"
    units_out <- "m s-1"
    if (F) {
        multfac_plot <- 100*3600 # m s-1 --> cm h-1
        units_plot <- "cm h-1"
        var_label_plot <- substitute(paste(-bold(u)[h] %.% bold(nabla)[h], "H",
                                           " [", var1, " ", var2^-1, "]"),
                                     list(var1="cm", var2="h"))
    } else if (T) {
        multfac_plot <- 100*86400 # m s-1 --> cm day-1
        units_plot <- "cm day-1"
        var_label_plot <- substitute(paste(-bold(u)[h] %.% bold(nabla)[h], "H",
                                           " [", var1, " ", var2^-1, "]"),
                                     list(var1="cm", var2="day"))
    }
    horiz_deriv_tag <- "geo"
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("u", "v")
    varname_nc <- c("uo", "vo")
    rotate_inds <- c(1, 2)

} else if (varname == "foverh") {
    longname <- "coriolis parameter over depth"
    units_out <- "m-1 s-1"
    power_plot <- 6
    multfac_plot <- base^power_plot
    units_plot <- paste0("m-1 s-1 x ", base^-power)
    var_label_plot <- substitute(paste("f ", H^-1, " [", var1^-1, " ", 
                                       var2^-1, "] " %*% " ", base^-power_plot),
                                 list(H="H", var1="m", var2="s", 
                                      base=base, power_plot=power_plot))
    coriolis_tag <- T

} else if (varname == "resolutionkm") {
    longname <- "Resolution"
    units_out <- "km"
    power_out <- -3 ## resolution unit is 'mesh_dist_unit' # m --> km
    multfac_out <- base^power_out
    var_label_plot <- "Mesh Resolution [km]"
    units_plot <- "km"
    horiz_deriv_tag <- "geo"
    axis.addzlims <- T # show min and max resolution in colorbar 
    #resolutionkm_cols <- colorRampPalette(c("plum1", "plum", "orchid4", "slateblue", "royalblue1", "cyan", "aquamarine",
    #                          "seagreen2", "palegreen2", "gold2", "darkorange", "orangered", "red"))
    #resolutionkm_cols <- colorRampPalette(rev(c("orchid4", "slateblue", "royalblue1", "cyan", "aquamarine",
    #                               "seagreen2", "palegreen2", "gold2", "darkorange", "orangered", "red", "darkred")))
    #resolutionkm_levels <- c(5:10, seq(10, 25, b=5), seq(30, 100, b=10), 150, 200, 250)

} else if (varname == "resolutiondeg") {
    longname <- "Resolution"
    units_out <- "deg"
    var_label_plot <- expression(paste("Mesh Resolution [", degree, "]"))
    multfac_out <- 1
    horiz_deriv_tag <- "geo"
    rotate_inds <- F
    vec <- F
    pal <- colorRampPalette(c("plum1", "plum", "orchid4", "slateblue", "royalblue1", "cyan", "aquamarine",
                              "seagreen2", "palegreen2", "gold2", "darkorange", "orangered", "red"))

} else if (varname == "mesharea") {
    longname <- "Mesh Area"
    power_out <- 3
    multfac_out <- base^-power_out * 1e-6 # m^2 --> 1e3 km^2
    multfac_out_plot <- base^power_out
    units_out <- paste0("km2 x ", multfac_out_plot)
    var_label_plot <- substitute(paste("Mesh Area [k", units_out^2, "] " %*% " ", base^power_out),
                              list(var="m", base=base, power_out=power_out))
    horiz_deriv_tag <- "geo"
    rotate_inds <- F
    vec <- F
    pal <- colorRampPalette(c("plum1", "plum", "orchid4", "slateblue", "royalblue1", "cyan", "aquamarine",
                              "seagreen2", "palegreen2", "gold2", "darkorange", "orangered", "red"))

} else if (varname == "rossbyrad") {
    longname <- "First Barolinic Rossby Radius of Deformation"
    power_out <- 3
    multfac_out <- base^-power_out
    units_out <- "km"
    var_label_plot <- expression(paste("Rossby Radius of Deformation [km]"))
    horiz_deriv_tag <- F
    typesuffix <- c("oce.", "oce.")
    diagsuffix <- c("", "")
    varname_nc <- c("temp", "salt")
    insitudens_tag <- T
    buoyancy_tag <- T
    coriolis_tag <- T
    rotate_inds <- F
    vec <- F

} else {
    stop("`varname` \"", varname, "\" is not defined in namelist.var.r")
}

## update units_out
#if (multfac_out != 1) {
    # e.g. units_out = "myunit" and multfac_out = 1e3
    # --> data = data*1e3
    # --> units_out = "myunit x 1e-3" (e.g. the value 1.3 in the data is actually 1.3 x 1e-3)
    #units_out <- paste0(units_out, " x ", base^-power_out) # e.g. psu -> psu x 1e+03
#}

