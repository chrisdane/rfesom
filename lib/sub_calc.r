## R
##
## Variable specific things
##
## input:
##
##   'data_node': 
##     dim=(nvars,nod2d_n,ndepths=1,nrecspf) if dim_tag=="2D"
##     dim=(nvars,nod3d_n,ndepths=1,nrecspf) if dim_tag=="3D"
##
## output:
##  
##   'data_node' got overwritten by or extended 
##   with the calculated variable
##
## notes:
##
##   horizontal derivative
##     'bafux_2d', 'bafuy_2d': dim=(3,elem2d_n)
##     'bafux_3d', 'bafuy_3d': dim=(4,elem3d_n)
##
##   horizontal integral:
##     'cluster_area_2d': dim=nod2d_n 
##     'cluster_vol_3d': dim=nod2d_n 
##
##   vertical derivative/integral:      
##     needs 'data_node'
##

## Notes
# Mean Eddy transport mean(u'T')
# = mean( (u - mean(u))(T - mean(T)) )
# = mean(uT) - mean( u*mean(T) ) - mean( mean(u)*T ) + mean( mean(u)*mean(T) )
# =    1     -          2        -         3         +           4
# 2 = 3 = 4
# => mean(u'T') = mean(uT) - mean(u)*mean(T)
#
# = in FESOM language:
# = mean(utemp_diag.oce) - mean(u_mean.oce)*mean(temp_mean.oce)
# with 'mean()' in the line above indicating the average
# over the time period chosen in this script and 'u_mean.oce'
# and 'uu_diag.oce' indicating the output of FESOM output files
# <id>.<year>.oce.mean.nc and <id>.<year>.oce.diag.nc respectively.

sub_calc <- function(data_node) {

    #data_global <- data_node # for old stuff; need to update
    if (F) {
        message("str(data_node)")
        print(str(data_node))
        if (exists("data_node_ltm")) {
            message("str(data_node_ltm)")
            print(str(data_node_ltm))
        }
    }

    ## Select one component of loaded data
    if (any(varname == c("u", "v", "initudens", "potdens", "insitub", "potb", "gradB"))) {

        if (varname == "gradB") {
            if (insitudens_tag) {
                data_node <- data_node["insitub",,,]
            } else if (potdens_tag) {
                data_node <- data_node["potb",,,]
            }
        } else {
            data_node <- data_node[varname,,,]
        }
        if (verbose > 2) message(indent, "Selected ", dimnames(data_node)[[1]], " from data matrix ...")

    } # select a specific var from data array

    ## get varnames of loaded data
    vars <- dimnames(data_node)[[1]]
    nvars <- length(vars)

    ## norm of vector
    if (any(varname == c("hvel", "wind", "tau", "hvelice"))) {

        varinds <- c(1, 2)
        if (verbose > 1) {
            message(indent, varname, " = sqrt(",
                    vars[varinds[1]], "^2 + ",
                    vars[varinds[2]], "^2) ...")
        }

        tmp <- sqrt(data_node[varinds[1],,,]^2 + data_node[varinds[2],,,]^2)
        if (uv_out || horiz_deriv_tag != F) {
            data_node <- abind(data_node, tmp, along=1, use.dnns=T)
            dimnames(data_node)[[1]][3] <- varname
            dim_tag[length(dim_tag)+1] <- dim_tag[1]
            names(dim_tag)[length(dim_tag)] <- varname
            levelwise[length(levelwise)+1] <- levelwise[1]
            names(levelwise) <- names(dim_tag)
        } else {
            data_node <- tmp
            dimnames(data_node)[[1]] <- varname
        }

    } # "hvel", "wind", "tau", "hvelice"

    ## norm of mean flux
    if (any(varname == c("uvtemp", "uvsalt", "uvrho", "uvb", 
                         "uvsgstemp", "uvsgssalt", "uvsgsrho", "uvsgsb",
                         "divuvt", "divuvs", "divuvrho", "divuvb",
                         "divuvsgst", "divuvsgss", "divuvsgsrho", "divuvsgsb"))) {

        if (any(varname == c("uvtemp", "divuvt"))) {
            varinds <- c(which(vars == "u" | vars == "uo"),
                          which(vars == "v" | vars == "vo"),
                          which(vars == "temp" | vars == "thetao"))
        } else if (any(varname == c("uvsalt", "divuvs"))) {
            varinds <- c(which(vars == "u" | vars == "uo"),
                          which(vars == "v" | vars == "vo"),
                          which(vars == "salt" | vars == "so"))
        } else if (any(varname == c("uvrho", "divuvrho"))) {
            if (insitudens_tag) {
                varinds <- c(which(vars == "u" | vars == "uo"),
                              which(vars == "v" | vars == "vo"),
                              which(vars == "insitudens"))
            } else if (potdens_tag) {
                varinds <- c(which(vars == "u" | vars == "uo"),
                              which(vars == "v" | vars == "vo"),
                              which(vars == "potdens"))
            }
        } else if (any(varname == c("uvb", "divuvb"))) {
            if (insitudens_tag) {
                varinds <- c(which(vars == "u" | vars == "uo"),
                              which(vars == "v" | vars == "vo"),
                              which(vars == "insitub"))
            } else if (potdens_tag) {
                varinds <- c(which(vars == "u" | vars == "uo"),
                              which(vars == "v" | vars == "vo"),
                              which(vars == "potb"))
            }
        } else if (any(varname == c("uvsgstemp", "divuvsgst"))) {
            varinds <- c(which(vars == "sgs_u"),
                          which(vars == "sgs_v"),
                          which(vars == "temp" | vars == "thetao"))
        } else if (any(varname == c("uvsgssalt", "divuvsgst"))) {
            varinds <- c(which(vars == "sgs_u"),
                          which(vars == "sgs_v"),
                          which(vars == "salt" | vars == "so"))
        } else if (any(varname == c("uvsgsrho", "divuvsgsrho"))) {
            if (insitudens_tag) {
                varinds <- c(which(vars == "sgs_u"),
                              which(vars == "sgs_v"),              
                              which(vars == "insitudens"))
            } else if (potdens_tag) {
                varinds <- c(which(vars == "sgs_u"),
                              which(vars == "sgs_v"),                          
                              which(vars == "potdens"))
            }
        } else if (any(varname == c("uvsgsb", "divuvsgsb"))) {
            if (insitudens_tag) {
                varinds <- c(which(vars == "sgs_u"),
                              which(vars == "sgs_v"),           
                              which(vars == "insitub"))
            } else if (potdens_tag) {
                varinds <- c(which(vars == "sgs_u"),
                              which(vars == "sgs_v"),                       
                              which(vars == "potb"))
            }
        }
        if (any(is.na(varinds))) stop("Could not find data.")
       
        if (verbose > 1) {
            message(indent, "Calc ",
                         vars[1], " times ", vars[3], " = ",
                         vars[varinds[1]], "*", vars[varinds[3]], ", ",
                         vars[2], " times ", vars[3], " = ",
                         vars[varinds[2]], "*", vars[varinds[3]], " ...")
        }

        utmp <- data_node[varinds[1],,,]*data_node[varinds[3],,,]
        dimnames(utmp)[[1]] <- paste0(vars[1], "_times_", vars[3])
        vtmp <- data_node[varinds[2],,,]*data_node[varinds[3],,,]
        dimnames(vtmp)[[1]] <- paste0(vars[2], "_times_", vars[3])

        ## norm of mean flux is calculated now
        if (any(varname == c("uvtemp",  "uvsalt", "uvrho", "uvb",
                             "uvsgstemp", "uvsgssalt", "uvsgsrho", "uvsgsb"))) {
        
            if (verbose > 1) {
                message(paste0(indent, varname, " = sqrt[(", 
                             vars[varinds[1]], "*", 
                             vars[varinds[3]], ")^2 + (", 
                             vars[varinds[2]], "*", 
                             vars[varinds[3]], ")^2] ..."))
            }

            tmp <- sqrt(utmp^2 + vtmp^2)
            dimnames(tmp)[[1]] <- varname
            if (uv_out) {
                data_node <- abind(utmp, vtmp, tmp, along=1, use.dnns=T)
            } else {
                data_node <- tmp
            }

        ## norm of mean flux is calculated later
        } else if (any(varname == c("divuvt",  "divuvs", "divuvrho", "divuvb",
                                    "divuvsgst", "divuvsgss", "divuvsgsrho", "divuvsgsb"))) {
            
            data_node <- abind(utmp, vtmp, along=1, use.dnns=T)
            
        } # norm of mean flux calculated nor or later

    } # "uvtemp", "uvsalt", "uvrho", "uvb", 
      # "uvsgstemp", "uvsgssalt", "uvsgsrho", "uvsgsb",
      # "divuvt", "divuvs", "divuvrho", "divuvb",
      # "divuvsgst", "divuvsgss", "divuvsgsrho", "divuvsgsb"

    ## norm of total flux 
    if (any(varname == c("uvtemptot", "uvsalttot", "uvrhotot", "uvbtot",
                         "uvsgstemptot", "uvsgssalttot",
                         "divuvttot", "divuvstot", "divuvrhotot", "divuvbtot",
                         "divuvsgsttot", "divuvsgsstot"))) {

        if (any(varname == c("uvtemptot", "divuvtemptot"))) {
            varinds <- c(which(vars == "utemp" | vars == "uto"),
                          which(vars == "vtemp" | vars == "vto"))
        } else if (any(varname == c("uvsalttot", "divuvsalttot"))) {
            varinds <- c(which(vars == "usalt" | vars == "uso"),
                          which(vars == "vsalt" | vars == "vso"))
        } else if (any(varname == c("uvrhotot", "uvbtot", 
                                    "divuvrhotot", "divuvbtot"))) {
            varinds <- c(which(vars == "urho"),
                          which(vars == "vrho"))
        } else if (any(varname == c("uvsgstemptot", "divuvsgsttot"))) {
            varinds <- c(which(vars == "sgs_ut"),
                          which(vars == "sgs_vt"))
        } else if (any(varname == c("uvsgssalttot", "divuvsgsstot"))) {
            varinds <- c(which(vars == "sgs_us"),
                          which(vars == "sgs_vs"))
        }
        if (any(is.na(varinds))) stop("Could not find data.")

        if (any(varname == c("uvbtot", "divuvbtot"))) {
            data_node[varinds[1],,,] <- -g/rho0*data_node[varinds[1],,,]
            dimnames(data_node)[[1]][varinds[1]] <- "uinsitub"
            data_node[varinds[2],,,] <- -g/rho0*data_node[varinds[2],,,]
            dimnames(data_node)[[1]][varinds[2]] <- "vinsitub"
            vars <- dimnames(data_node)[[1]]
        }

        utmp <- data_node[varinds[1],,,]
        vtmp <- data_node[varinds[2],,,]

        if (any(varname == c("uvtemptot", "uvsalttot", "uvrhotot", "uvbtot",
                             "uvsgstemptot", "uvsgssalttot"))) {
            if (verbose > 1) {
                message(paste0(indent, varname, " = sqrt(",
                             vars[varinds[1]], "^2 + ",
                             vars[varinds[2]], "^2)"))
            }
            tmp <- sqrt(utmp^2 + vtmp^2)
            dimnames(tmp)[[1]] <- varname
            if (uv_out) {
                data_node <- abind(utmp, vtmp, tmp, along=1, use.dnns=T)
            } else {
                data_node <- tmp
            }

        } else if (any(varname == c("divuvttot", "divuvstot", "divuvrhotot", "divuvbtot",
                                    "divuvsgsttot", "divuvsgsstot"))) {
            data_node <- abind(utmp, vtmp, along=1, use.dnns=T)

        }

    } # "uvtemptot", "uvsalttot", "uvrhotot", "uvbtot",
      # "uvsgstemptot", "uvsgssalttot",
      # "divuvttot", "divuvstot", "divuvrhotot", "divuvbtot",
      # "divuvsgsttot", "divuvsgsstot"

    ## norm of eddy flux
    if (any(varname == c("uvteddy", "uvseddy", "uvrhoeddy", "uvbeddy",
                         "uvsgsteddy", "uvsgsseddy",
                         "divuvteddy", "divuvseddy", "diuvrhoeddy", "divuvbeddy",
                         "divuvsgsteddy", "divuvsgsseddy"))) {

        if (any(varname == c("uvteddy", "divuvteddy"))) {
            varinds <- c(which(vars == "u" | vars == "uo"),
                          which(vars == "v" | vars == "vo"),
                          which(vars == "temp" | vars == "thetao"),
                          which(vars == "utemp" | vars == "uto"),
                          which(vars == "vtemp" | vars == "vto"))
        } else if (any(varname == c("uvseddy", "divuvseddy"))) {
            varinds <- c(which(vars == "u" | vars == "uo"),
                          which(vars == "v" | vars == "vo"),
                          which(vars == "salt" | vars == "so"),
                          which(vars == "usalt" | vars == "uso"),
                          which(vars == "vsalt" | vars == "vso"))
        } else if (any(varname == c("uvrhoeddy", "uvbeddy", 
                                    "divuvrhoeddy", "divuvbeddy"))) {
            if (insitudens_tag) {
                varinds <- c(which(vars == "u" | vars == "uo"),
                              which(vars == "v" | vars == "vo"),
                              which(vars == "insitudens"),
                              which(vars == "urho"),
                              which(vars == "vrho"))
            } else if (potdens_tag) {
                stop(paste0("Error: _potential_ density fluxes are not available. 
                             Set 'potdens_tag' to F."))
            }
        } else if (any(varname == c("uvsgsteddy", "divuvsgsteddy"))) {
            varinds <- c(which(vars == "sgs_u"),
                          which(vars == "sgs_v"),
                          which(vars == "temp" | vars == "thetao"),
                          which(vars == "sgs_ut"),
                          which(vars == "sgs_vt"))
        } else if (any(varname == c("uvsgsseddy", "divuvsgsseddy"))) {
            varinds <- c(which(vars == "sgs_u"),
                          which(vars == "sgs_v"),
                          which(vars == "salt" | vars == "so"),
                          which(vars == "sgs_us"),
                          which(vars == "sgs_vs"))
        }
        if (any(is.na(varinds))) stop("Could not find data.")

        if (any(varname == c("uvbeddy", "divuvbeddy"))) {
            data_node[varinds[3],,,] <- -g/rho0*data_node[varinds[3],,,]
            dimnames(data_node)[[1]][varinds[3]] <- "insitub"
            data_node[varinds[4],,,] <- -g/rho0*data_node[varinds[4],,,]
            dimnames(data_node)[[1]][varinds[4]] <- paste0(vars[varinds[1]], "insitub")
            data_node[varinds[5],,,] <- -g/rho0*data_node[varinds[5],,,]
            dimnames(data_node)[[1]][varinds[5]] <- paste0(vars[varinds[2]], "insitub")
            vars <- dimnames(data_node)[[1]]
        }

        if (verbose > 1) {
            message(paste0(indent, "Calc ", 
                         vars[varinds[4]], "_eddy = ",
                         vars[varinds[4]], " - ", vars[varinds[1]], "*", vars[varinds[3]], ", ",
                         vars[varinds[5]], "_eddy = ", 
                         vars[varinds[5]], " - ", vars[varinds[2]], "*", vars[varinds[3]], " ..."))
        }

        # mean(uT) - mean(u)*mean(T); mean() is e.g. monthly mean if monthly model output
        utmp <- data_node[varinds[4],,,] - data_node[varinds[1],,,]*data_node[varinds[3],,,]
        dimnames(utmp)[[1]] <- paste0(vars[varinds[4]], "_eddy")
        vtmp <- data_node[varinds[5],,,] - data_node[varinds[2],,,]*data_node[varinds[3],,,]
        dimnames(vtmp)[[1]] <- paste0(vars[varinds[5]], "_eddy")

        ## norm of eddy flux is calculated now
        if (any(varname == c("uvteddy", "uvseddy", "uvrhoeddy", "uvbeddy",
                             "uvsgsteddy", "uvsgsseddy"))) {

            if (verbose > 1) {
                message(paste0(indent, varname, " = sqrt( [", vars[varinds[4]],
                             " - ", vars[varinds[1]], "*", vars[varinds[3]],
                             "]^2 + [", vars[varinds[5]],  " - ",
                             vars[varinds[2]], "*", vars[varinds[3]], "]^2 )"))
            }

            tmp <- sqrt(utmp^2 + vtmp^2)
            dimnames(tmp)[[1]] <- varname
            if (uv_out) {
                data_node <- abind(utmp, vtmp, tmp, along=1, use.dnns=T)
            } else {
                data_node <- tmp
            }

        ## norm of norm of eddy flux is calculated later
        } else if (any(varname == c("divuvteddy", "divuvseddy", "diuvrhoeddy", "divuvbeddy",
                                    "divuvsgsteddy", "divuvsgsseddy"))) {

            data_node <- abind(utmp, vtmp, along=1, use.dnns=T)

        } # if  norm of eddy flux calculated nor or later 

    } # "uvteddy", "uvseddy", "uvrhoeddy", "uvbeddy",
      # "uvsgsteddy", "uvsgsseddy",
      # "divuvteddy", "divuvseddy", "diuvrhoeddy", "divuvbeddy",
      # "divuvsgsteddy", "divuvsgsseddy"

    ## mean(u)*mean(u), mean(v)*mean(v)
    if (any(varname == c("u_u", "v_v"))) {

        if (varname == "u_u") {
            varinds <- which(vars == "u" | vars == "uo")
        } else if (varname == "v_v") {
            varinds <- which(vars == "v" | vars == "vo")
        }
        if (any(is.na(varinds))) stop("Could not find data.")
        data_node <- data_node[varinds[1],,,] * data_node[varinds[1],,,]
        dimnames(data_node)[[1]] <- varname

    } # "u_u", "v_v"

    if (varname == "wb") {

        varinds <- c(which(vars == "w" | vars == "wo"),
                      which(vars == "insitub"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            message(paste0(indent, varname, " = ", varinds[1], " * ", varinds[2], " ..."))
        }

        data_node <- data_node[varinds[1],,,]*data_node[varinds[2],,,]
        dimnames(data_node)[[1]] <- varname

    } # wb uvb

    if (varname == "wbeddy") {
        varinds <- c(which(vars == "w" | vars == "wo"),
                     which(vars == "rho"),
                     which(vars == "wrho"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            message(paste0(indent, varname, " = mean(", vars[varinds[3]], 
                         ") - mean(", vars[varinds[1]], ")*mean(",
                         vars[varinds[2]], ") ..."))
        }
        data_node <- data_node[varinds[3],,,] - data_node[varinds[1],,,]*data_node[varinds[2],,,]
        if (verbose > 1) {
            message(indent, "bouyancy b = -g/rho0*rho")
        }
        data_node <- -g/rho0*data_node
        dimnames(data_node)[[1]] <- varname

    } # "wbeddy"

    if (varname == "N2_insitudens") {
        
        data_node <- N2_insitudens_node

    } # N2_insitudens
    
    if (varname == "N2_potdens") {

        data_node <- N2_potdens_node

    } # N2_potdens
    
    if (varname == "richardson") {

        if (verbose > 1) {
            message(paste0(indent, varname, " = N^2/[sqrt(dudz^2 + dvdz^2)]^2 ... (Thomas et al. 2008)"))
        }

        # vertical derivative needs 'data_node'
        if (verbose > 1) {
            message(paste0(indent, "Calc global vertical derivative for all depths ..."))
        }

        varinds <- c(which(vars == "u" | vars == "uo"),
                      which(vars == "v" | vars == "vo"))
        if (any(is.na(varinds))) stop("Could not find data.")

        dvardz_node <- array(0, 
                              dim=c(length(varinds), dim(data_node)[3:4]))
        dimnames(dvardz_node)[[1]] <- dimnames(data_node)[[1]][varinds]
        dimnames(dvardz_node)[2:4] <- dimnames(data_node)[2:4]
        
        # vertical derivative
        for (ii in 1:nod2d_n) {
            for (k in 1:(aux3d_n-1)) {
                if (aux3d[k,ii] > 0 && aux3d[k+1,ii] > 0) {
                    node_up <- aux3d[k,ii]
                    node_low <- aux3d[k+1,ii]
                    dz <- nod3d_z[node_up] - nod3d_z[node_low]

                    dvardz_node[,aux3d[k,ii],,] <- (data_node[varinds,node_up,,] -
                                                     data_node[varinds,node_low,,])/dz
                }
            }
        }

        data_node <- N2_node/(sqrt(dvardz_node[1,,,]^2 + dvardz_node[2,,,]^2))^2 
        dimnames(data_node)[[1]] <- varname

    } # richardson
    
    if (varname == "rossbyrad") {

        if (verbose > 1) {
            message(paste0(indent, varname, "_{m=1} = 1/(m*|f|*pi) * int_{z=-H}^{z=0} N dz ..."))
        }

        data_node <- sqrt(N2_node)

        if (verbose > 1) {
            message(paste0(indent, "Run ", subroutinepath, "/sub_vertical_integral.r ..."))
        }
        sub_vertical_integral(data_node) # produces tmp
        data_node <- tmp # dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf)

        data_node <- 1/(abs(coriolis_nod2d)*pi) * data_node # 1st bc rossby rad of def.
        dimnames(data_node)[[1]] <- varname
    
    } # rossbyrad

    if (any(varname == c("c_barocline", "c_long_rossby", 
                         "wkb_hvel_mode", "wkb_vertvel_mode"))) {

        if (!exists("mmodes") || any(mmodes == 0)) {
            stop("Set 'mmodes' to non-zero (e.g. 1 or c(1,2,3)) for variable ", varname, " in namelist.var.r.")
        }

        ## N = sqrt(N2)
        N_node <- N2_node # R does not use more memory here
        N_node[N_node < 0] <- 0 #NA # claudi uses 0 here
        N_node <- sqrt(N_node)
        if (verbose > 2) {
            finite_inds <- is.finite(N_node)
            if (any(finite_inds) && any(N_node > 0)) {
                message(paste0(indent, "   min/max N_node = ", 
                             paste0(range(N_node[finite_inds], na.rm=T), collapse="/"), " s-1"))
            } else {
                stop(paste0(indent, "   Error: all values of N_node = sqrt(N_node) are < 0 and/or infinite.",
                            " Change to another location/time."))
            }
        }
        dimnames(N_node)[[1]] <- "N"
        if (!keep_gsw) rm(N2_node,  envir=.GlobalEnv)

        if (F) {
            N_node_rho0 <- N2_node_rh0
            N_node_rho0[N_node_rho0 < 0] <- 0
            N_node_rho0 <- sqrt(N_node_rho0)
        }

        ## rearrange N
        if (verbose > 1) {
            message(paste0(indent, "Bring N_node from (nod3d_n=", nod3d_n,
                         ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
            if (verbose > 2) {
                message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
            }
        }
        sub_n3_to_n2xde(N_node) # produces tmp
        N_vert <- tmp

        if (F) {
            sub_n3_to_n2xde(N_node_rho0)
            N_vert_rho0 <- tmp
        }

        ## vertical integral of N
        if (F) { # not needed since N is also integrated from depth one step later
            if (verbose > 1) {
                message(paste0(indent, "Integrate N_node between ", depths_plot, " m ..."))
                if (verbose > 2) {
                    message(paste0(indent, "Run ", subroutinepath, "/sub_vertical_integrate.r ..."))
                }
            }
            sub_vertical_integral(N_node) # produces tmp
            N_intz_const <- tmp
            if (verbose > 2) {
                finite_inds <- is.finite(N_intz_const)
                message(paste0(indent, "   min/max N_intz_const = ",
                             paste0(range(N_intz_const[finite_inds], na.rm=T), collapse="/"), " m s-1"))
            }
            dimnames(N_intz_const)[[1]] <- "int_N_dz_const"
        } # F

        ## vertical integral of N from bottom: keep z dim
        if (verbose > 1) {
            message(paste0(indent, "Integrate N_node from ", interpolate_depths[ndepths], 
                         "-", interpolate_depths[1], " m and keep z dim ..."))
            if (verbose > 2) {
                message(paste0(indent, "Run ", subroutinepath, "/sub_vertical_integrate_keepz.r ..."))
            }
        }
        sub_vertical_integral_keepz(N_node)
        N_intz_z <- tmp
        if (!keep_gsw) rm(N_node, envir=.GlobalEnv)
        if (verbose > 2) {
            finite_inds <- is.finite(N_intz_z)
            message(paste0(indent, "   min/max N_intz_z = ",
                         paste0(range(N_intz_z[finite_inds], na.rm=T), collapse="/"), " m s-1"))
        }

        if (F) {
            sub_vertical_integral_keepz(N_node_rho0)
            N_intz_z_rho0 <- tmp
        }
        
        ## rearrange N_intz_z
        if (verbose > 1) {
            message(paste0(indent, "Bring N_intz_z from (nod3d_n=", nod3d_n,
                         ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
            if (verbose > 2) {
                message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
            }
        }
        sub_n3_to_n2xde(N_intz_z) # produces tmp
        N_intz_z_vert <- tmp
        if (!keep_gsw) rm(N_intz_z, envir=.GlobalEnv)
        dimnames(N_intz_z_vert)[[1]] <- "int_N_dz_z"

        if (F) {
            sub_n3_to_n2xde(N_intz_z_rho0)
            N_intz_z_vert_rho0 <- tmp
        }
        
        # for testing
        if (F) {
            N_vert_mean <- N_vert[,poly_node_inds_geogr,,]
            N_intz_const_mean <- N_intz_const[,poly_node_inds_geogr,,]
            N_intz_z_vert_mean <- N_intz_z_vert[,poly_node_inds_geogr,,]
            patch_area <- cluster_area_2d[poly_node_inds_geogr]
            patch_area <- replicate(patch_area, n=dim(N_vert_mean)[3]) # ndepths
            patch_area <- replicate(patch_area, n=dim(N_vert_mean)[4]) # nrecspf
            patch_area <- replicate(patch_area, n=dim(N_vert_mean)[1]) # nvars
            patch_area <- aperm(patch_area, c(4, 1, 2, 3))
            
            # var * area
            N_vert_mean <- N_vert_mean*patch_area
            N_intz_const_mean <- N_intz_const_mean*patch_area[,,1,] # 1 depth
            N_intz_z_vert_mean <- N_intz_z_vert_mean*patch_area
            
            # sum over nodes (var * area)
            N_vert_mean <- apply(N_vert_mean, c(1, 3, 4), sum, na.rm=T) # c(var, recs, depth)
            N_intz_const_mean <- apply(N_intz_const_mean, c(1, 3, 4), sum, na.rm=T)
            N_intz_z_vert_mean <- apply(N_intz_z_vert_mean, c(1, 3, 4), sum, na.rm=T)
            
            # where there are no values at depth
            N_vert_mean[N_vert_mean == 0] <- NA
            N_intz_const_mean[N_intz_const_mean == 0] <- NA
            N_intz_z_vert_mean[N_intz_z_vert_mean == 0] <- NA
           
            # divide through area for all depths
            for (i in 1:dim(N_vert_mean)[2]) { # for all depths
                if (rec_tag && leap_tag && is.leap(year)) {
                    tmp_area <- sum(patch_area_leap[1,,i,1]) # NA positions do not change in time
                } else {
                    tmp_area <- sum(patch_area[1,,i,1])
                }
                if (T && verbose > 2) {
                    message(paste0(indent, "         area in ", interpolate_depths[i], " m depth = ", tmp_area, " m^2"))
                }

                N_vert_mean[,i,] <- N_vert_mean[,i,]/tmp_area
                if (i == 1) N_intz_const_mean[,i,] <- N_intz_const_mean[,i,]/tmp_area
                N_intz_z_vert_mean[,i,] <- N_intz_z_vert_mean[,i,]/tmp_area
            } # for i ndepths

            # average over time
            N_vert_mean <- apply(N_vert_mean, c(1, 2), mean, na.rm=T)
            N_intz_const_mean <- apply(N_intz_const_mean, c(1, 2), mean, na.rm=T) # single number
            N_intz_z_vert_mean <- apply(N_intz_z_vert_mean, c(1, 2), mean, na.rm=T)

            xlim <- range(N_vert_mean,
                           N_intz_const_mean,
                           N_intz_z_vert_mean,
                           na.rm=T)
            ylim <- rev(range(interpolate_depths))
            dev.new()
            plot(N_vert_mean[1,], interpolate_depths, t="l",
                 xlim=xlim, ylim=ylim)
            lines(N_intz_z_vert_mean[1,], interpolate_depths, col="red")
            abline(v=N_intz_const_mean[1,1], col="blue")
            legend("top", c("N_vert", "N_intz_z_vert", "N_intz_const"),
                   col=c("black", "red", "blue"), lty=1, bty="n", cex=1.5)
        } # F for testing
        #stop("asd")

        # mode-m baroclinic gravity-wave speed
        if (verbose > 0) {
            message(paste0(indent, "Calc mode-m baroclinic gravity-wave speed (Killworth et al. 1997; Chelton et al. 1998; Ferrari et al. 2010 Appendix; Vallis 2017 p. 117) ..."))
        }
        c_vert <- array(NA, dim=c(length(mmodes), dim(N_intz_z_vert)[2], 1, dim(N_intz_z_vert)[4]),
                         dimnames=c(list(paste0("c_", mmodes)),
                                    dimnames(N_intz_z_vert)[2],
                                    list(depths=paste0("int", depths_fname)),
                                    dimnames(N_intz_z_vert)[4]))
        if (F) {
            c_vert_rho0 <- c_vert
        }

        for (i in 1:length(mmodes)) {
            if (verbose > 0) {
                message(paste0(indent, "   c_", mmodes[i], " = 1/(", mmodes[i], " * pi) * int_z N dz"))
            }
            #c_vert[i,,,] <- 1/(mmodes[i]*pi) * N_intz_const
            c_vert[i,,,] <- 1/(mmodes[i]*pi) * N_intz_z_vert[,,1,] # same at the surface
            if (verbose > 2) {
                message(paste0(indent, "   min/max c_vert[", i, ",,,] = ",
                             paste0(range(c_vert[i,,,], na.rm=T), collapse="/"), " m s-1"))
            }
            if (F) {
                c_vert_rho0[i,,,] <- 1/(mmodes[i]*pi) * N_intz_z_vert_rho0[,,1,]
            }
        } # for i mmodes

        if (varname == "c_barocline") {
            data_node <- c_vert

        } else if (any(varname == c("c_long_rossby", "internal_rossbyrad"))) {

            f_vert <- 2*2*pi/86400*sin(ycsur*pi/180)
            beta_vert <- 2*2*pi/86400*cos(ycsur*pi/180)/Rearth
            f_vert <- replicate(f_vert, n=dim(c_vert)[1]) # nvars
            f_vert <- replicate(f_vert, n=dim(c_vert)[3]) # ndepths = 1
            f_vert <- replicate(f_vert, n=dim(c_vert)[4]) # nrecspf
            f_vert <- aperm(f_vert, c(2, 1, 3, 4))
            beta_vert <- replicate(beta_vert, n=dim(c_vert)[1]) # nvars
            beta_vert <- replicate(beta_vert, n=dim(c_vert)[3]) # ndepths = 1
            beta_vert <- replicate(beta_vert, n=dim(c_vert)[4]) # nrecspf
            beta_vert <- aperm(beta_vert, c(2, 1, 3, 4))

            if (verbose > 0) {
                message(paste0(indent, "internal_rossbyrad = c_barocline/|f|               for  |phi| >= 5° latitude"))
                message(paste0(indent, "                   = sqrt( c_barocline/(2*beta) )  for  |phi| <= 5° latitude (Chelton et al. 1998) ..."))
            }
            int_rossbyrad_node <- c_vert/abs(f_vert)
            eq_inds <- which(abs(ycsur) <= 5)
            if (length(eq_inds) > 0) {
                int_rossbyrad_node[,eq_inds,,] <- sqrt( c_vert[,eq_inds,,]/(2*beta_vert[,eq_inds,,]) )
            }
            #rm(eq_inds, envir=.GlobalEnv)

            if (F) {
                int_rossbyrad_node_rho0 <- c_vert_rho0/abs(f_vert)
            }

            if (varname == "internal_rossbyrad") {
                data_node <- int_rossbyrad_node
                dimnames(data_node)[[1]] <- paste0(varname, "_", mmodes)
            
            } else if (varname == "c_long_rossby") {
                if (verbose > 0) {
                    message(paste0(indent, varname, " = -beta * internal_rossbyrad^2 = -beta/f^2 * c_barocline^2 (Killworth et al. 1997; Chelton et al. 1998) ..."))
                }
                data_node <- -beta_vert * int_rossbyrad_node^2
                dimnames(data_node)[[1]] <- paste0(varname, "_", mmodes)

                if (F) {
                    data_node_rho0 <- -beta_vert * int_rossbyrad_node_rho0^2
                    data_node <- data_node_rho0 # differences between /rho0 and /rho are very small
                }

            } # "c_long_rossby" "internal_rossbyrad"

            rm(f_vert, beta_vert, envir=.GlobalEnv)

        } else if (any(varname == c("wkb_hvel_mode", "wkb_vertvel_mode"))) {

            ## repeat c_vert for matrix multiplication
            c_vert <- adrop(c_vert, drop=3) # drop depth placeholder dim
            c_vert <- replicate(c_vert, n=dim(N_vert)[3]) # ndepths
            c_vert <- aperm(c_vert, c(1, 2, 4, 3)) # c(mmodes,nod2d_n,ndepths,nrecspf)

            ## S0
            # Chelton et al. 1998: S0_C98 = (N/c)^(-1/2)
            # Ferrari et al. 2010: "S0 must have an inverse relation to the buoyancy frequency"
            # Vallis 2017 (p. 117): S0_V17 = (c/N)^(1/2) 
            # --> S0_C98 = S0_V17 !!!
           
            if (varname == "wkb_hvel_mode") {

                # mode-m baroclinic horizontal velocity
                if (verbose > 0) {
                    message(paste0(indent, "Calc baroclinic horizontal velocity modes (Ferrari et al. 2010 Appendix; Vallis 2017 p. 117) ..."))
                }
                R_vert <- array(NA, dim=dim(c_vert),
                                 dimnames=dimnames(c_vert))
                dimnames(R_vert)[[1]] <- paste0("R_", mmodes)
                for (i in 1:length(mmodes)) {
                    if (verbose > 0) {
                        message(paste0(indent, "   R_", mmodes[i], "(z) = -[c_", 
                                     mmodes[i], " * N(z) * S_", mmodes[i], "^0 / g] * cos(int_z N(z) dz / c_", 
                                     mmodes[i], ")"))
                        message(paste0(indent, "      with S_", mmodes[i], "^0 = (c_", mmodes[i], "/N)^(1/2)"))
                    }
                    S0 <- (c_vert[i,,,]/N_vert)^(1/2) # from Vallis 2017
                    R_vert[i,,,] <- -(c_vert[i,,,] * N_vert[1,,,] / g * S0) * cos(N_intz_z_vert[1,,,] / c_vert[i,,,]) # [#]
                    if (verbose > 2) {
                        message(paste0(indent, "   min/max R_vert[", i, ",,,] = ",
                                     paste0(range(R_vert[i,,,], na.rm=T), collapse="/"), " [#]"))
                    }
                } # for i mmodes
                dimnames(R_vert)[[1]] <- paste0(varname, mmodes)

                # cat everything together
                #data_node <- abind(N_vert, c_vert, R_vert, along=1, use.dnns=T)
                data_node <- R_vert
                
                if (!keep_gsw) rm(R_vert, envir=.GlobalEnv)

            } else if (varname == "wkb_vertvel_mode") {

                # mode-m baroclinic vertical velocity
                if (verbose > 0) {
                    message(paste0(indent, "Calc baroclinic vertical velocity modes (Chelton et al. 1998 Appendix; Ferrari et al. 2010 Appendix; Vallis 2017 p. 117) ..."))
                }
                S_vert <- array(NA, dim=dim(c_vert),
                                 dimnames=dimnames(c_vert))
                dimnames(S_vert)[[1]] <- paste0("S_", mmodes)
                for (i in 1:length(mmodes)) {
                    if (verbose > 0) {
                        # ferrari = chelton since (a/b)^(1/2) = (b/a)^(-1/2)
                        if (T) { # ferrari
                            message(paste0(indent, "   S_", mmodes[i], "(z) = S_", mmodes[i], 
                                         "^0 sin(int_z N(z) dz / c_", mmodes[i], ")"))
                        } else if (F) { # chelton
                            message(paste0(indent, "   S_", mmodes[i], "(z) = [N(z)/c_", mmodes[i], "]^(-1/2) ",
                                         " sin(int_z N(z) dz / c_", mmodes[i], ")"))
                        }
                        message(paste0(indent, "      with S_", mmodes[i], "^0 = (c_", mmodes[i], "/N)^(1/2)"))
                    }
                    S0 <- (c_vert[i,,,]/N_vert)^(1/2) # from Vallis 2017
                    # ferrari = chelton since (a/b)^(1/2) = (b/a)^(-1/2)
                    if (T) { # ferrari
                        S_vert[i,,,] <- S0 * sin(N_intz_z_vert[1,,,] / c_vert[i,,,]) # [#]
                    } else if (F) { # chelton (B = 1)
                        S_vert[i,,,] <- (N_vert[1,,,]/c_vert[i,,,])^(-1/2) * sin(N_intz_z_vert[1,,,] / c_vert[i,,,])
                    }
                    if (verbose > 2) {
                        message(paste0(indent, "   min/max R_vert[", i, ",,,] = ",
                                     paste0(range(R_vert[i,,,], na.rm=T), collapse="/"), " [#]"))
                    }
                } # for i mmodes
                dimnames(S_vert)[[1]] <- paste0(varname, mmodes)

                if (F) {
                    S_vert_mean <- apply(S_vert[,poly_node_inds_geogr,,], c(1, 3, 4), mean, na.rm=T)
                    S_vert_mean <- apply(S_vert_mean, c(1, 2), mean, na.rm=T)
                    dev.new()
                    plot(S_vert_mean[1,], interpolate_depths, t="n", 
                         xlim=range(S_vert_mean, na.rm=T), ylim=rev(range(interpolate_depths)))
                    abline(v=0, col="gray")
                    for (i in 1:dim(S_vert_mean)[1]) lines(S_vert_mean[i,], interpolate_depths, col=i)
                    legend("bottomleft", paste0("S", mmodes),
                           lty=1, col=mmodes, bty="n", cex=1.5)
                    stop("asd")
                }

                #stop("asd")

                # cat everything together
                #data_node <- abind(N_vert, c_vert, R_vert, along=1, use.dnns=T)
                data_node <- S_vert

                if (!keep_gsw) rm(S_vert, envir=.GlobalEnv)

            } # wkb_hvel_mode or wkb_vertvel_mode

            if (!keep_gsw) rm(S0, envir=.GlobalEnv)

        } # "wkb_hvel_mode", "wkb_vertvel_mode"

        if (!keep_gsw) rm(c_vert, N_intz_z_vert, N_vert, envir=.GlobalEnv)

    } # "c_barocline", "c_long_rossby", "wkb_hvel_mode", "wkb_vertvel_mode"

    if (varname == "mke") {

        varinds <- c(which(vars == "u" | vars == "uo"),
                      which(vars == "v" | vars == "vo"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            message(paste0(indent, "MKE = 1/2 mean(", vars[varinds[1]], 
                         "^2 + ", vars[varinds[2]], "^2) ..."))
        }

        data_node <- 1/2*(data_node[varinds[1],,,]^2 + 
                          data_node[varinds[2],,,]^2)
        dimnames(data_node)[[1]] <- varname

    } # "mke"

    if (varname == "tke") {

        varinds <- c(which(vars == "uu" | vars == "u2o"),
                      which(vars == "vv" | vars == "v2o"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            message(indent, "TKE = 1/2 mean(", vars[varinds[1]], 
                    " + ", vars[varinds[2]], ") ...")
        }

        data_node <- 1/2*(data_node[varinds[1],,,] +
                          data_node[varinds[2],,,])
        dimnames(data_node)[[1]] <- varname

    } # "tke"

    if (varname == "eke") {
        
        varinds <- c(which(vars == "uu" | vars == "u2o"),
                     which(vars == "u" | vars == "uo"),
                     which(vars == "vv" | vars == "v2o"),
                     which(vars == "v" | vars == "vo"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            message(paste0(indent, "EKE = 1/2 [ mean(",
                         vars[varinds[1]], ") - mean(",
                         vars[varinds[2]], ")^2 + mean(",
                         vars[varinds[3]], ") - mean(",
                         vars[varinds[4]], ")^2 ] ..."))
        }
        
        # Eddy kinetic energy (EKE)
        # = 1/2 mean( \vec{v'_h}^2 ) (from kinetic energy definition)
        # with \vec{v'_h} = (\vec{i}*u' + \vec{j}*v')
        #               u = mean(u) + u' <=> u' = u - mean(u)
        #               v = mean(v) + v' <=> v' = v - mean(v)
        # = 1/2 [ mean( (u-mean(u))^2 ) + mean( (v-mean(v))^2 ) ]
        # = 1/2 [ mean(u^2) - mean(2 mean(u) u) + mean(mean(u)^2) +
        #         mean(v^2) - mean(2 mean(v) v) + mean(mean(v)^2) ]
        # = 1/2 [ mean(u^2) - 2 mean(u)^2 + mean(u)^2 +
        #         mean(v^2) - 2 mean(v)^2 + mean(v)^2 ]
        # = 1/2 [ mean(u^2) - mean(u)^2 + mean(v^2) - mean(v)^2 ]
        #
        # = in FESOM language:
        # = 1/2 [ mean(uu_diag.oce) - mean(u_mean.oce)^2 + mean(vv_diag.oce) - mean(v_mean.oce)^2 ]
        # with 'mean()' in the line above indicating the average
        # over the time period chosen in this script and 'u_mean.oce'
        # and 'uu_diag.oce' indicating the output of FESOM output files
        # <id>.<year>.oce.mean.nc and <id>.<year>.oce.diag.nc respectively.
        data_node <- 1/2*(data_node[varinds[1],,,] - data_node[varinds[2],,,]^2 + 
                          data_node[varinds[3],,,] - data_node[varinds[4],,,]^2) # [m^2/s^2]
        dimnames(data_node)[[1]] <- varname

    } # "eke"

    if (varname == "twindenergy") {

        varinds <- c(which(vars == "utaux"),
                      which(vars == "vtauy"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 2) {
            message(paste0(indent, "TWE = ", vars[varinds[1]], 
                         " + ", vars[varinds[2]], " ... (Waterhouse et al. 2014)"))
        }

        data_node <- data_node[varinds[1],,,] + data_node[varinds[2],,,]
        dimnames(data_node)[[1]] <- varname

    } # "twindenergy"

    if (any(varname == c("mwindenergy", "FmKm"))) {
        varinds <- c(which(vars == "u" | vars == "uo"),
                      which(vars == "v" | vars == "vo"),
                      which(vars == "stress_x" | vars == "tauuo"),
                      which(vars == "stress_y" | vars == "tauvo"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            if (varname == "mwindenergy") {
                message(paste0(indent, "MWE = mean(", vars[varinds[1]], ")*mean(", vars[varinds[3]], 
                             ") + mean(", vars[varinds[2]], ")*mean(", vars[varinds[4]], 
                             ") ... (Waterhouse et al. 2014)"))
            } else if (varname == "FmKm") {
                message(paste0(indent, varname, " = 1/rho0 * mean(", vars[varinds[1]], 
                             ")*mean(", vars[varinds[3]], ") + mean(", vars[varinds[2]], 
                             ")*mean(", vars[varinds[4]], ") ... (Renault et al. 2016)"))
            }
        }

        data_node <- data_node[varinds[1],,,]*data_node[varinds[3],,,] +
                      data_node[varinds[2],,,]*data_node[varinds[4],,,]
        if (varname == "FmKM") {
            data_node <- 1/rho0*data_node
        }
        dimnames(data_node)[[1]] <- varname

    } # "mwindenergy", "FmKm"

    if (any(varname == c("ewindenergy", "FeKe"))) {
        varinds <- c(which(vars == "u" | vars == "uo"),
                     which(vars == "v" | vars == "vo"),
                     which(vars == "stress_x" | vars == "tauuo"),
                     which(vars == "stress_y" | vars == "tauvo"),
                     which(vars == "tauxu"),
                     which(vars == "tauyv"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            if (varname == "ewindenergy") {
                message(paste0(indent, "EWE = (", vars[varinds[5]], " - mean(", 
                             vars[varinds[1]], ")*mean(", vars[varinds[3]], 
                             ") + ", vars[varinds[6]], " - mean(", vars[varinds[2]], 
                             ")*mean(", vars[varinds[4]], ") ... (Waterhouse et al. 2014)"))
            } else if (varname == "FeKe") {
                message(indent, varname, " = 1/rho0 * (", vars[varinds[5]], " - mean(",
                        vars[varinds[1]], ")*mean(", vars[varinds[3]],
                        ") + ", vars[varinds[6]], " - mean(", vars[varinds[2]],
                        ")*mean(", vars[varinds[4]], ") ... (Renault et al. 2016)")
            }
        }

        data_node <- data_node[varinds[5],seq_len(nod2d_n),,] - 
                        data_node[varinds[1],seq_len(nod2d_n),,]*data_node[varinds[3],seq_len(nod2d_n),,] + 
                     data_node[varinds[6],seq_len(nod2d_n),,] - 
                        data_node[varinds[2],seq_len(nod2d_n),,]*data_node[varinds[4],seq_len(nod2d_n),,]
        if (varname == "FeKe") {
            data_node <- 1/rho0*data_node
        }
        dimnames(data_node)[[1]] <- varname

    } # "ewindenergy", "FeKe"

    if (varname == "uv_bott_force_mean") {
        
        # mean bottom drag -Cd|u|u \cdot u
        varinds <- c(which(vars == "u" | vars == "uo"),
                     which(vars == "v" | vars == "vo"))
        if (length(varinds) != 2) stop("could not find all necessary vars")
        message(indent, "uv_bott_force_mean = -C_d * |\\vec{u}_h| * \\vec{u}_h \\cdot \\vec{u}_h = -C_d * ",
                "sqrt[mean(", vars[varinds[1]], ")^2 + mean(", vars[varinds[2]], ")^2] * ",
                "(mean(", vars[varinds[1]], ")^2 + mean(", vars[varinds[2]], ")^2)")
        tmp <- -C_d*sqrt(data_node[varinds[1],,,]^2 + data_node[varinds[2],,,]^2)  # -C_d*|u|
        data_node <- tmp * (data_node[varinds[1],,,]^2 + data_node[varinds[2],,,]) # -C_d*|u| u \cdot u
        rm(tmp)

    } # uv_bott_force_mean

    if (varname == "uv_bott_force_eddy") {
        
        # eddy bottom drag -Cd|u|u \cdot u
        varinds <- c(which(vars == "uu" | vars == "u2o"),
                     which(vars == "u" | vars == "uo"),
                     which(vars == "vv" | vars == "v2o"),
                     which(vars == "v" | vars == "vo"))
        if (length(varinds) != 4) stop("could not find all necessary vars")
        message(indent, "uv_bott_force_eddy = -C_d * |\\vec{u}_h'| * \\vec{u}_h' \\cdot \\vec{u}_h' = -C_d * ",
                "sqrt[mean(", vars[varinds[1]], ") - mean(", vars[varinds[2]], ")^2 + mean(", vars[varinds[3]], ") - mean(", vars[varinds[4]], ")^2] * ",
                "(mean(", vars[varinds[1]], ") - mean(", vars[varinds[2]], ")^2 + mean(", vars[varinds[3]], ") - mean(", vars[varinds[4]], ")^2)")
        tmp <- -C_d*sqrt(data_node[varinds[1],,,] - data_node[varinds[2],,,]^2 + data_node[varinds[3],,,] - data_node[varinds[4],,,]^2)    # -C_d*|u'|
        data_node <- tmp * (data_node[varinds[1],,,] - data_node[varinds[2],,,]^2 + data_node[varinds[3],,,] - data_node[varinds[4],,,]^2) # -C_d*|u'| u' \cdot u'
        rm(tmp)

    } # uv_bott_force_eddy

    ## vertical mean flux divergence
    if (any(varname == c("dzwt", "dzws", "dzwrho", "dzwb"))) {

        # vertical derivative needs 'data_node'
        if (varname == "dzwt") {
            varinds <- c(which(vars == "w" | vars == "wo"),
                         which(vars == "temp" | vars == "thetao"))
        } else if (varname == "dzws") {
            varinds <- c(which(vars == "w" | vars == "wo"),
                          which(vars == "salt" | vars == "so"))
        } else if (varname == "dzwrho") {
            if (insitudens_tag) {
                varinds <- c(which(vars == "w" | vars == "wo"),
                              which(vars == "insitudens"))
            } else if (potdens_tag) {
                varinds <- c(which(vars == "w" | vars == "wo"),
                              which(vars == "potdens"))
            }
        } else if (varname == "dzwb") {
            if (insitudens_tag) {
                varinds <- c(which(vars == "w" | vars == "wo"),
                              which(vars == "insitub"))
            } else if (potdens_tag) {
                varinds <- c(which(vars == "w" | vars == "wo"),
                              which(vars == "potb"))
            }
        }
        if (any(is.na(varinds))) stop("Could not find data.")

        if (verbose > 1) {
            message(paste0(indent, varname, " = d(", vars[varinds[1]], "*", 
                         vars[varinds[2]], ")/dz ..."))
        }

        data_node <- data_node[varinds[1],,,]*data_node[varinds[2],,,]
        dimnames(data_node)[[1]] <- paste0(vars[varinds[1]], "*", vars[varinds[2]])

        # vertical derivative
        if (verbose > 1) {
            message(paste0(indent, "Calc global vertical derivative of '",
                         dimnames(data_node)[[1]], "' for all depths ..."))
        }

        dvardz <- array(0, dim=dim(data_node))
        dimnames(dvardz)[[1]] <- varname
        dimnames(dvardz)[2:4] <- dimnames(data_node)[2:4]

        # create progress bar
        pb <- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                char=pb_char, width=pb_width,
                                indent=paste0("     ", indent)) # 5 " " for default message()

        for (i in 1:(aux3d_n-1)) {
            nodes_up <- aux3d[i,]
            nodes_low <- aux3d[i+1,]
            inds <- nodes_up > 0 & nodes_low > 0
            if (any(!is.na(inds))) {
                dz <- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                dvardz[,nodes_up[inds],,] <- (data_node[,nodes_up[inds],,] -
                                               data_node[,nodes_low[inds],,])/dz
            }

            # update progress bar
            setTxtProgressBar(pb, i)

        } # for i aux3d_n-1

        # close progress bar
        close(pb)

        data_node <- dvardz

    } # "dzwt", "dzws", "dzwrho", "dzwb"

    ## vertical eddy flux divergence
    if (any(varname == c("dzwbeddy"))) {

        if (varname == "dzwbeddy") {
            if (insitudens_tag) {
                varinds <- c(which(vars == "w" | vars == "wo"),
                              which(vars == "rho"),
                              which(vars == "wrho"))
            } else if (potdens_tag) {
                stop(paste0("Error: _potential_ density fluxes are not available. 
                             Set 'potdens_tag' to F."))
            }
        }
        if (any(is.na(varinds))) stop("Could not find data.")

        if (varname == "dzwbeddy") {
            data_node[varinds[3],,,] <- -g/rho0*data_node[varinds[3],,,]
            dimnames(data_node)[[1]][varinds[3]] <- paste0(vars[varinds[1]], "insitub")
            vars <- dimnames(data_node)[[1]]
        }

        if (verbose > 1) {
            message(paste0(indent, varname, " = d(", vars[varinds[3]], " - ", 
                         vars[varinds[1]], "*", vars[varinds[2]], ")/dz ..."))
        }

        data_node <- data_node[varinds[3],,,] - data_node[varinds[1],,,]*data_node[varinds[2],,,]
        dimnames(data_node)[[1]] <- paste0(vars[varinds[3]], "_eddy")

        # vertical derivative
        if (verbose > 1) {
            message(paste0(indent, "Calc global vertical derivative of '",
                         dimnames(data_node)[[1]], "' for all depths ..."))
        }

        dvardz <- array(0, dim=dim(data_node))
        dimnames(dvardz)[[1]] <- varname
        dimnames(dvardz)[2:4] <- dimnames(data_node)[2:4]

        # create progress bar
        pb <- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                char=pb_char, width=pb_width,
                                indent=paste0("     ", indent)) # 5 " " for default message()

        for (i in 1:(aux3d_n-1)) {
            nodes_up <- aux3d[i,]
            nodes_low <- aux3d[i+1,]
            inds <- nodes_up > 0 & nodes_low > 0
            if (any(!is.na(inds))) {
                dz <- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                dvardz[,nodes_up[inds],,] <- (data_node[,nodes_up[inds],,] -
                                               data_node[,nodes_low[inds],,])/dz
            }

            # update progress bar
            setTxtProgressBar(pb, i)

        } # for i aux3d_n-1

        # close progress bar
        close(pb)

        data_node <- dvardz

    } # "dzwbeddy"

    ## vertical tracer diffusion
    if (any(varname == c("vdifft", "vdiffs", "vdiffrho", "vdiffb"))) {

        message(paste0(indent, "Warning: Kv is probably snapshot data!!!!"))

        varinds <- which(vars == "Kv")
        if (varname == "vdifft") {
            varinds <- c(varinds, which(vars == "temp" | vars == "thetao"))
        } else if (varname == "vdiffs") {
            varinds <- c(varinds, which(vars == "salt" | vars == "so"))
        } else if (varname == "vdiffrho") {
            if (insitudens_tag) {
                varinds <- c(varinds, which(vars == "insitudens"))
            } else if (potdens_tag) {
                varinds <- c(varinds, which(vars == "potdens"))
            }
        } else if (varname == "vdiffb") {
            if (insitudens_tag) {
                varinds <- c(varinds, which(vars == "insitub"))
            } else if (potdens_tag) {
                varinds <- c(varinds, which(vars == "potb"))
            }
        }
        if (any(is.na(varinds))) stop("Could not find data.")

        if (verbose > 1) {
            message(paste0(indent, varname, " = d[", vars[varinds[1]], 
                         " d(", vars[varinds[2]], ")/dz]/dz ..."))
        }

        # 1st vertical derivative
        if (verbose > 1) {
            message(paste0(indent, "Calc global vertical derivative of '",
                         dimnames(data_node)[[1]][varinds[2]], "' for all depths ..."))
        }

        dvardz1 <- array(0, dim=c(1, dim(data_node)[2:4]))
        dimnames(dvardz1)[[1]] <- paste0("dz_", vars[varinds[2]])
        dimnames(dvardz1)[2:4] <- dimnames(data_node)[2:4]

        # create progress bar
        pb <- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                char=pb_char, width=pb_width,
                                indent=paste0("     ", indent)) # 5 " " for default message()

        for (i in 1:(aux3d_n-1)) {
            nodes_up <- aux3d[i,]
            nodes_low <- aux3d[i+1,]
            inds <- nodes_up > 0 & nodes_low > 0
            if (any(!is.na(inds))) {
                dz <- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                dvardz[,nodes_up[inds],,] <- (data_node[varinds[2],nodes_up[inds],,] -
                                               data_node[varinds[2],nodes_low[inds],,])/dz
            }

            # update progress bar
            setTxtProgressBar(pb, i)

        } # for i aux3d_n-1

        # close progress bar
        close(pb)

        # Kv times first vertical derivative of tracer
        Kv_dvardz1 <- data_node[varind[1],,,] * dvar1dz
        dimnames(Kv_dvardz1)[[1]] <- paste0(vars[varind[1]], "*", dimnames(dvardz1)[[1]])

        # 2nd vertical derivative
        if (verbose > 1) {
            message(paste0(indent, "Calc global vertical derivative of '",
                         dimnames(Kv_dvardz1)[[1]], "' for all depths ..."))
        }

        dvardz2 <- array(0, dim=dim(Kv_dvardz1))
        dimnames(dvardz2)[[1]] <- varname
        dimnames(dvardz2)[2:4] <- dimnames(data_node)[2:4]

        # create progress bar
        pb <- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                char=pb_char, width=pb_width,
                                indent=paste0("     ", indent)) # 5 " " for default message()

        for (i in 1:(aux3d_n-1)) {
            nodes_up <- aux3d[i,]
            nodes_low <- aux3d[i+1,]
            inds <- nodes_up > 0 & nodes_low > 0
            if (any(!is.na(inds))) {
                dz <- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                dvardz2[,nodes_up[inds],,] <- (Kv_dvardz1[,nodes_up[inds],,] -
                                                Kv_dvardz1[,nodes_low[inds],,])/dz
            }

            # update progress bar
            setTxtProgressBar(pb, i)
        
        } # for i aux3d_n

        # close progress bar
        close(pb)

        data_node <- dvardz2

    } # "vdifft", "vdiffs", "vdiffrho", "vdiffb"

    ## vertical reynolds stress
    if (any(varname == c("VRS", "KmKe"))) {

        # vertical derivative needs 'data_node'

        # eddy flux u'w'
        varinds <- c(which(vars == "u" | vars == "uo"),
                      which(vars == "w" | vars == "wo"),
                      which(vars == "uw"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            message(paste0(indent, "u'w' = ", vars[varinds[3]], " - ", 
                         vars[varinds[1]], " * ", vars[varinds[2]], " ..."))
        }
        utmp <- data_node[varinds[3],,,] - data_node[varinds[1],,,]*data_node[varinds[2],,,]
        dimnames(utmp)[[1]] <- paste0(dimnames(data_node)[[1]][varinds[3]], "_eddy")

        # eddy flux v'w'
        varinds <- c(which(vars == "v" | vars == "vo"),
                      which(vars == "w" | vars == "wo"),
                      which(vars == "vw"))
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            message(paste0(indent, "v'w' = ", vars[varinds[3]], " - ", 
                         vars[varinds[1]], " * ", vars[varinds[2]], " ..."))
        }
        vtmp <- data_node[varinds[3],,,] - data_node[varinds[1],,,]*data_node[varinds[2],,,]
        dimnames(vtmp)[[1]] <- paste0(vars[varinds[3]], "_eddy")

        # vertical derivative
        varinds <- c(which(vars == "u" | vars == "uo"),
                      which(vars == "v" | vars == "vo")) 
        if (any(is.na(varinds))) stop("Could not find data.")
        if (verbose > 1) {
            message(paste0(indent, "Calc global vertical derivative of '",
                         paste0(vars[varinds], collapse="','"),
                         "' for all depths ..."))
        }
        dvardz <- array(0, 
                         dim=c(length(varinds), dim(data_node)[2:4]))
        dimnames(dvardz)[[1]] <- paste0("dz_", vars[varinds])
        dimnames(dvardz)[2:4] <- dimnames(data_node)[2:4]

        # create progress bar
        pb <- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                char=pb_char, width=pb_width,
                                indent=paste0("     ", indent)) # 5 " " for default message()

        for (i in 1:(aux3d_n-1)) {
            nodes_up <- aux3d[i,]
            nodes_low <- aux3d[i+1,]
            inds <- nodes_up > 0 & nodes_low > 0
            if (any(!is.na(inds))) {
                dz <- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                dvardz[,nodes_up[inds],,] <- (data_node[varinds,nodes_up[inds],,] -
                                               data_node[varinds,nodes_low[inds],,])/dz
            }

            # update progress bar
            setTxtProgressBar(pb, i)

        } # for i aux3d_n-1

        # close progress bar
        close(pb)

        # calc VRS
        if (verbose > 1) {
            message(paste0(indent, "VRS = - u'w'*dudz - v'w'*dvdz"))
        }
        vrs <- -utmp*dvardz[1,,,] - vtmp*dvardz[2,,,]
        dimnames(vrs)[[1]] <- "VRS"
        if (varname == "VRS") {
            data_node <- vrs
        }

    } # "VRS", "KmKe"

    ## slopeSx, slopeSy, slopeS, slopeSsq
    if (any(varname == c("slopeSx", "slopeSy", "slopeS", "slopeSsq"))) {

        # vertical derivative of rho
        stop("not yet")

    } # "slopeSx", "slopeSy", "slopeS", "slopeSsq"

    if (any(varname == c("bathy", "gradbathy", "hvel_dot_gradbathy", "c_barotrop", "foverh"))) {

        if (varname == "hvel_dot_gradbathy") {
            data_node_uv <- data_node # save uv for later
        }

        # bathy_node was calculated in sub_prepare2.r
        data_node <- bathy_node
        if (varname == "c_barotrop") {
            if (verbose > 0) {
                message(paste0(indent, varname, " = sqrt(gH) ..."))
            }
            data_node <- sqrt(g*data_node)
        } else if (varname == "foverh") {
            if (verbose > 1) {
                message(paste0(indent, varname, " = f/H ..."))
            }
            data_node <- coriolis_node/data_node
        }

        dimnames(data_node)[1] <- list(var=varname)
        if (any(varname == c("gradbathy", "hvel_dot_gradbathy"))) {
            dimnames(data_node)[1] <- list(var="bathy") # grad not applied yet
        }

    } # "bathy", "gradbathy", "hvel_dot_gradbathy", "c_barotrop", "foverh"

    # at this point, data_node needs to be well defined
    # with the variables in the first dim: c(var,node,depth,rec)
    
    # define variables to calc horizontal derivative from
    if (horiz_deriv_tag != F) {

        if (verbose > 2) {
            message(indent, "`horiz_deriv_tag` = ", horiz_deriv_tag, " is not false")
        }
        vars_avail <- dimnames(data_node)[[1]] # not equal `varname_nc` through additional sub_prepare() steps...

        ## x,y-derivatives of which variables?
        # user provided which horizontal derivatives of which variables should be calculated
        if (!exists("dxinds") && !exists("dyinds") &&
            !exists("dxvars") && !exists("dyvars")) {
            
            dxinds <- dyinds <- dxvars <- dxvars <- NULL
            #stop("neither `dxinds` and/or `dxvars` nor `dyinds` and/or `dyvars` are defined.")
            
        } else { # user provided any dx/dy information

            ## dx
            # inds missing but names given 
            if (!exists("dxinds") && exists("dxvars")) {
                message(indent, "   `dxinds` is not given")
                if (!is.character(dxvars)) stop("given `dxvars` is not of type character")
                message(indent, "   `dxvars` = \"", paste(dxvars, collapse="\",\""), "\"")
                if (any(vars_avail %in% dxvars)) {
                    dxinds <- which(vars_avail %in% dxvars)
                    message(indent, "      --> found ", length(dxinds), 
                            " ", ifelse(length(dxinds) == 1, "index", "indices"), 
                            " for dx calculation: ", paste(dxinds, collapse=","))
                } else {
                    message("data_node:")
                    stop("did not any such variable in available data array\n",
                         cat(capture.output(str(data_node, vec.len=dim(data_node)[1])), sep="\n"))
                }

            # inds given but names missing
            } else if (exists("dxinds") && !exists("dxvars")) {
                message(indent, "   `dxvars` is not given")
                if (!is.finite(dxinds)) stop("given `dxinds` is not finite")
                message(indent, "   `dxinds` = ", paste(dxinds, collapse=","), "")
                if (any(seq(vars_avail) %in% dxinds)) {
                    dxvars <- vars_avail[dxinds]
                    message(indent, "      --> found ", length(dxvars), 
                            " varnames for dx calculation: \"", paste(dxvars, collapse="\",\""), "\"")
                } else {
                    message("data_node:")
                    stop("did not any such variable index in available data array\n",
                         cat(capture.output(str(data_node, vec.len=dim(data_node)[1])), sep="\n"))
                }

            # both inds and names given
            } else if (exists("dxinds") && exists("dxvars")) {
                if (length(dxinds) != length(dxvars)) {
                    stop("given `dxinds` = ", paste(dxinds, collapse=","), 
                         " and given `dxvars` = \"", paste(dxvars, collapse="\",\""), 
                         "\" are of different lengths: ", length(dxinds), " != ", length(dxvars), ".")
                }
            # neither inds or names are given
            } else if (!exists("dxinds") && !exists("dxvars")) {
                dxinds <- NULL
                dxvars <- NULL
            }

            ## dy
            # inds missing but names given 
            if (!exists("dyinds") && exists("dyvars")) {
                message(indent, "   `dyinds` is not given")
                if (!is.character(dyvars)) stop("given `dyvars` is not of type character")
                message(indent, "   `dyvars` = \"", paste(dyvars, collapse="\",\""), "\"")
                if (any(vars_avail %in% dyvars)) {
                    dyinds <- which(vars_avail %in% dyvars)
                    message(indent, "      --> found ", length(dyinds), 
                            " ", ifelse(length(dyinds) == 1, "index", "indices"),
                            " for dy calculation: ", paste(dyinds, collapse=","))
                } else {
                    message("data_node:")
                    stop("did not any such variable in available data array\n",
                         cat(capture.output(str(data_node, vec.len=dim(data_node)[1])), sep="\n"))
                }

            # inds given but names missing
            } else if (exists("dyinds") && !exists("dyvars")) {
                message(indent, "   `dyvars` is not given")
                if (!is.finite(dyinds)) stop("given `dyinds` is not finite")
                message(indent, "   `dyinds` = ", paste(dyinds, collapse=","), "")
                if (any(seq(vars_avail) %in% dyinds)) {
                    dyvars <- vars_avail[dyinds]
                    message(indent, "      --> found ", length(dyvars), 
                            " varnames for dy calculation: \"", paste(dyvars, collapse="\",\""), "\"")
                } else {
                    message("data_node:")
                    stop("did not any such variable index in available data array\n",
                         cat(capture.output(str(data_node, vec.len=dim(data_node)[1])), sep="\n"))
                }

            # both inds and names given
            } else if (exists("dyinds") && exists("dyvars")) {
                if (length(dyinds) != length(dyvars)) {
                    stop("given `dyinds` = ", paste(dyinds, collapse=","), 
                         " and given `dyvars` = \"", paste(dyvars, collapse="\",\""), 
                         "\" are of different lengths: ", length(dyinds), " != ", length(dyvars), ".")
                }
            # neither inds or names are given
            } else if (!exists("dyinds") && !exists("dyvars")) {
                dyinds <- NULL
                dyvars <- NULL
            }
            
        } # dxinds dxvars dyinds dyvars checked

        ## from here, dxinds dxvars dyinds dyvars can be used

        # my phd stuff for backwards compatibility
        if (any(varname == c("u_geo", "slopeSy"))) {
            dyinds <- 1 # = dy of variable 1 in data_node array
        }
        if (any(varname == c("v_geo", "slopeSx"))) {
            dxinds <- 1
        }
        if (any(varname == c("gradT", "gradB", "gradmld", 
                             "hvel_geo", "hdiffb", 
                             "slopeS", "slopeSsq"))) {
            dxinds <- 1
            dyinds <- 1
        }
        if (any(varname == c("divuv",
                             "divuvt", "divuvttot", "divuvteddy",
                             "divuvsgst", "divuvsgsttot", "divuvsgsteddy",
                             "divuvs", "divuvstot", "divuvseddy",
                             "divuvsgss", "divuvsgsstot", "divuvsgsseddy",
                             "divuvrho", "divuvrhotot", "divuvrhoeddy",
                             "divuvsgsrho", "divuvsgsrhotot", "divuvsgsrhoeddy",
                             "divuvb", "divuvbtot", "divuvbeddy",
                             "divuvsgsb", "divuvsgsbtot", "divuvsgsbeddy",
                             "strain_normal", "strain", "okubo",
                             "HRS", "KmKe",
                             "divuvt2"))) {
            dxinds <- 1
            dyinds <- 2
        }
        if (varname == "advh") {
            dxinds <- c(1, 2)
            dyinds <- c(1, 2)
        }
        if (any(varname == c("relvorti", "relvortisq", "RossbyNo",
                             "curlwind", "curltau",
                             "ekmanP", "ekmanP_ms",
                             "strain_shear", "strain", "okubo",
                             "HRS", "KmKe"))) {
            if (!is.null(dxinds)) {
                dxinds <- c(dxinds, 2)
            } else {
                dxinds <- 2
            }
            if (!is.null(dyinds)) {
                dyinds <- c(dyinds, 1)
            } else {
                dyinds <- 1
            }
        }
        if (varname == "intz_uvteddy_div") {
            dxinds <- 1
            dyinds <- 1
        }
        if (any(varname == c("gradbathy", "hvel_dot_gradbathy"))) {
            dxinds <- 1
            dyinds <- 1
        }
        
        ndxy <- length(dxinds) + length(dyinds)

    } # if (horiz_deriv_tag != F) {

    ## calc horizontal derivative
    if (horiz_deriv_tag != F && 
        (!is.null(dxinds) || !is.null(dyinds))) {

        # check
        if (any(is.na(dxinds)) || any(is.na(dyinds))) {
            stop(paste0("Could not find data_node to take the horizontal derivative from."))
        }

        ## do stuff before horizontal derivative
        if (any(varname == c("ekmanP", "ekmanP_ms"))) {
            if (verbose > 1) {
                message(indent, varname, " = d(", varname_nc[2], "/f)", "dx - d(", varname_nc[1], "/f)dy ...")
            }
            # divide data through f
            for (i in seq_len(nvars)) {
                data_node[i,,,] <- data_node[i,,,]/coriolis_node
            }
        }

        ## horizontal derivative in node-space (the correct way...)
        if (horiz_deriv_node3d) { # which method?

            if (!is.null(dxinds)) {
                # dim(bafux_2d) = c(3,elem2d_n)
                if (F) {
                    bafux_2d_time_depth <- replicate(bafux_2d, n=dim(data_node)[3]) # ndepths=1 
                    bafux_2d_time_depth <- replicate(bafux_2d_time_depth, n=dim(data_node)[4]) # nrecspf
                    bafux_2d_time_depth <- replicate(bafux_2d_time_depth, n=length(dxinds)) # nvars
                    bafux_2d_time_depth <- aperm(bafux_2d_time_depth, c(5, 1, 2, 3, 4))
                    # dim(bafux_2d_time_depth) = c(nvars,3,elem2d_n,ndepths=1,nrecspf)
                } else if (T) {
                    bafux_2d_time_depth <- replicate(bafux_2d, n=length(dxinds)) # nvars
                    bafux_2d_time_depth <- replicate(bafux_2d_time_depth, n=dim(data_node)[4]) # nrecspf
                    bafux_2d_time_depth <- aperm(bafux_2d_time_depth, c(3, 1, 2, 4))
                }
                dvardx_node3d <- array(0,
                                        dim=c(length(dxinds), dim(data_node)[2:4]),
                                        dimnames=c(list(var=paste0("dx_", dimnames(data_node)[[1]][dxinds])),
                                                   dimnames(data_node)[2:4]))
                dvardx_node3d_cnt <- dvardx_node3d[1,,,]
            } # if !is.null(dxinds)
            if (!is.null(dyinds)) {
                bafuy_2d_time_depth <- replicate(bafuy_2d, n=length(dyinds)) # nvars
                bafuy_2d_time_depth <- replicate(bafuy_2d_time_depth, n=dim(data_node)[4]) # nrecspf
                bafuy_2d_time_depth <- aperm(bafuy_2d_time_depth, c(3, 1, 2, 4))
                dvardy_node3d <- array(0,
                                        dim=c(length(dyinds), dim(data_node)[2:4]),
                                        dimnames=c(list(var=paste0("dy_", dimnames(data_node)[[1]][dyinds])),
                                                   dimnames(data_node)[2:4]))
                dvardy_node3d_cnt <- dvardy_node3d[1,,,]
            } # if !is.null(dyinds)

            if (verbose > 1) {
                message(indent, "Calc horizontal derivatives ", appendLF=F)
                if (!is.null(dxinds)) {
                    message(paste0("dx_", dimnames(data_node)[[1]][dxinds], collapse=", "), appendLF=F)
                    if (!is.null(dyinds)) message(", ", appendLF=F)
                }
                if (!is.null(dyinds)) {
                    message(paste0("dy_", dimnames(data_node)[[1]][dyinds], collapse=", "), appendLF=F)
                }
                message(" over ", elem2d_n, " 2D elems", 
                        ifelse(ndepths > 1, paste0(" and ", ndepths, " depths"), ""),
                        " ...")
                if (verbose > 3) {
                    message("data_node:")
                    cat(capture.output(str(data_node)), sep="\n")
                    if (!is.null(dxinds)) {
                        message("dvardx_node3d:")
                        cat(capture.output(str(dvardx_node3d)), sep="\n")
                        message("bafux_2d_time_depth:")
                        cat(capture.output(str(bafux_2d_time_depth)), sep="\n")
                    }
                    if (!is.null(dyinds)) {
                        message("dvardy_node3d:")
                        cat(capture.output(str(dvardy_node3d)), sep="\n")
                        message("bafuy_2d_time_depth:")
                        cat(capture.output(str(bafuy_2d_time_depth)), sep="\n")
                    }
                }
            }

            #time1 <- array(NA, c(aux3d_n, elem2d_n))
            #time2 <- time1
            `[` <- fctbackup # restore to R's default for loop

            #stop("asd")
            # create progress bar
            if (ndepths > 1) {
                pb <- mytxtProgressBar(min=0, max=ndepths, style=pb_style,
                                        char=pb_char, width=pb_width,
                                        indent=paste0("   ", indent)) # 5 " " for default message()
            } else {
                pb <- mytxtProgressBar(min=0, max=elem2d_n, style=pb_style,
                                        char=pb_char, width=pb_width,
                                        indent=paste0("   ", indent)) # 5 " " for default message()
            }

            #for (i in 1:aux3d_n) {
            for (i in 1:ndepths) {
                for (j in 1:elem2d_n) {
                    if (all(dim_tag == "3D" & levelwise == F)) {
                        nds_surf <- elem2d[,j]
                        nds_layer <- aux3d[i,nds_surf]
                    } else if (all(dim_tag == "3D" & levelwise == T)) {
                        stop("not yet")
                    } else if (all(dim_tag == "2D")) {
                        nds_layer <- elem2d[,j]
                    } else {
                        stop("mixed 2D and 3D vars not implemented here")
                    }
                    if (all(nds_layer != -999)) {

                        #stop("asd")

                        if (!is.null(dxinds)) {
                            #message(paste0(i, " ", j, " xxx"))
                            #aux <- adrop(bafux_2d_time_depth[,,j,,], drop=3)*data_node[dxinds,nds_layer,,] 
                            # c(nvars,3,ndepths=1,nrecspf)*c(nvars,3,ndepths=1,nrecspf)
                           
                            if (F) {
                                ptm <- proc.time()[3]
                                aux <- bafux_2d_time_depth[,,j,]*data_node[dxinds,nds_layer,,]
                                aux <- apply(aux, 4, sum) # sum over all nodes of 2d element
                                aux <- replicate(aux, n=length(dxinds))
                                aux <- replicate(aux, n=3) # 3 nodes per 2d element
                                aux <- replicate(aux, n=1) # ndepths
                                dvardx_node3d[,nds_layer,,] <- dvardx_node3d[,nds_layer,,] + aperm(aux, c(4, 3, 2, 1))
                                time1[i,j] <- proc.time()[3] - ptm
                            } else if (F) {
                                ptm <- proc.time()[3]
                                aux <- bafux_2d_time_depth[,,j,]*data_node[dxinds,nds_layer,,]
                                tmp <- array(0, dim=dim(aux))
                                aux <- apply(aux, c(1, 3, 4), sum) # keep nvars,ndepths=1,nrecspf
                                tmp[,1,,] <- aux
                                tmp[,2,,] <- aux
                                tmp[,3,,] <- aux
                                dvardx_node3d[,nds_layer,,] <- dvardx_node3d[,nds_layer,,] + tmp
                                time1[i,j] <- proc.time()[3] - ptm
                            ## for some reason this is the fastest:
                            } else if (T) { # with original `[`
                                #ptm <- proc.time()[3]
                                aux <- bafux_2d_time_depth[,,j,,drop=F]*data_node[dxinds,nds_layer,,,drop=F]
                                for (k in 1:dim(aux)[1]) { # for all vars
                                    aux2 <- apply(aux[k,,,,drop=F], c(1, 4), sum)[1,]
                                    dvardx_node3d[k,nds_layer,,] <- dvardx_node3d[k,nds_layer,1,] + t(array(aux2, c(length(aux2), 3)))
                                }
                                #time1[i,j] <- proc.time()[3] - ptm
                            ## for some reason this is not correct:
                            } else if (F) { # with original `[`
                                ptm <- proc.time()[3]
                                for (k in 1:dim(data_node)[4]) { # nrecspf
                                    for (l in 1:length(dxinds)) { # nvars
                                        aux <- bafux_2d_time_depth[l,,j,k]*data_node[dxinds[l],nds_layer,1,k]
                                        dvardx_node3d[l,nds_layer,1,k] <- dvardx_node3d[l,nds_layer,1,k] + aux
                                    }
                                }
                                time1[i,j] <- proc.time()[3] - ptm
                            }
                            dvardx_node3d_cnt[,nds_layer,,] <- dvardx_node3d_cnt[,nds_layer,,] + 1
                        } # if !is.null(dxinds)

                        if (!is.null(dyinds)) {
                            if (F) {
                                aux <- bafuy_2d_time_depth[,,j,]*data_node[dyinds,nds_layer,,]
                                aux <- apply(aux, 4, sum) # sum over all nodes of 2d element
                                aux <- replicate(aux, n=length(dyinds))
                                aux <- replicate(aux, n=3) # 3 nodes per 2d element
                                aux <- replicate(aux, n=1) # ndepths
                                dvardy_node3d[,nds_layer,,] <- dvardy_node3d[,nds_layer,,] + aperm(aux, c(4, 3, 2, 1))
                            } else if (F) {
                                aux <- bafuy_2d_time_depth[,,j,]*data_node[dyinds,nds_layer,,]
                                tmp <- array(0, dim=dim(aux))
                                aux <- apply(aux, c(1, 3, 4), sum) # keep nvars,ndepths=1,nrecspf
                                tmp[,1,,] <- aux
                                tmp[,2,,] <- aux
                                tmp[,3,,] <- aux
                                dvardy_node3d[,nds_layer,,] <- dvardy_node3d[,nds_layer,,] + tmp
                            } else if (T) {
                                #ptm <- proc.time()[3]
                                aux <- bafuy_2d_time_depth[,,j,,drop=F]*data_node[dyinds,nds_layer,,,drop=F]
                                for (k in 1:dim(aux)[1]) {
                                    aux2 <- apply(aux[k,,,,drop=F], c(1, 4), sum)[1,]
                                    dvardy_node3d[k,nds_layer,,] <- dvardy_node3d[k,nds_layer,1,] + t(array(aux2, c(length(aux2), 3)))
                                }
                                #time2[i,j] <- proc.time()[3] - ptm
                            } else if (F) {
                                ptm <- proc.time()[3]
                                for (k in 1:dim(data_node)[4]) { # nrecspf
                                    for (l in 1:length(dyinds)) { # nvars
                                        aux <- bafuy_2d_time_depth[l,,j,k]*data_node[dyinds[l],nds_layer,1,k]
                                        dvardy_node3d[l,nds_layer,1,k] <- dvardy_node3d[l,nds_layer,1,k] + aux
                                    }   
                                }   
                                time2[i,j] <- proc.time()[3] - ptm
                            }
                            dvardy_node3d_cnt[,nds_layer,,] <- dvardy_node3d_cnt[,nds_layer,,] + 1
                        } # if !is.null(dyinds)

                    } # if not -999
                
                    if (ndepths == 1) {
                        setTxtProgressBar(pb, j)
                    }

                } # for j elem2d_n

                # update progress bar
                if (ndepths > 1) {
                    setTxtProgressBar(pb, i)
                }

            } # for i ndepths

            # close progress bar
            close(pb)

            fctbackup <- `[`; `[` <- function(...) { fctbackup(..., drop=F) }

            if (!is.null(dxinds)) {
                for (k in 1:length(dxinds)) {
                    dvardx_node3d[k,,,] <- dvardx_node3d[k,,,]/dvardx_node3d_cnt
                }
                dvardx_node3d[is.na(dvardx_node3d)] <- 0
                
                if (verbose > 2) {
                    for (k in 1:length(dxinds)) {
                        message(indent, "   min / max 'dvardx_node3d[", k, ",,,]' = ", 
                                paste(range(dvardx_node3d[k,,,]), collapse=" / "))
                    }
                    if (verbose > 3) {
                        message("dvardx_node3d:")
                        cat(capture.output(str(dvardx_node3d)), sep="\n")
                    }
                }
            } # if !is.null(dxinds)

            if (!is.null(dyinds)) {
                for (k in 1:length(dyinds)) {
                    dvardy_node3d[k,,,] <- dvardy_node3d[k,,,]/dvardy_node3d_cnt
                }
                dvardy_node3d[is.na(dvardy_node3d)] <- 0
                
                if (verbose > 2) {
                    for (k in 1:length(dyinds)) {
                        message(indent, "   min / max 'dvardy_node3d[", k, ",,,]' = ", 
                                paste(range(dvardy_node3d[k,,,]), collapse=" / "))
                    }
                    if (verbose > 3) {
                        message("dvardy_node3d:")
                        cat(capture.output(str(dvardy_node3d)), sep="\n")
                    }
                }
            }

        } # if horiz_deriv_node3d # which method

        ## horizontal derivative in level space which is much faster than in node-space
        ## but somehow it doesnt work yet ...
        if (horiz_deriv_elem2d) {
        
            ## bring data_node on level space
            if (dim_tag == "2D" 
                || average_depth
                || (dim_tag == "3D" && ndepths == 1)) {

                # nothing to do
                data_vert <- data_node # dim(data_vert) = c(nvars,nod2d_n,ndepths=1,nrecspf)

            } else {

                if (verbose > 1) { # rearrange first
                    message(paste0(indent, "Bring data_node from (nod3d_n=", nod3d_n,
                                 ") on (nod2d_n=", nod2d_n, " x ndepths=", ndepths, ") ..."))
                    message(paste0(indent, "   run ", subroutinepath, "/sub_n3_to_n2xde.r ..."))
                }
                sub_n3_to_n2xde(data_node) # produces tmp
                data_vert <- tmp # dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)

            } # if dim_tag == "3D" --> rearrange before horiz deriv

            ## bring derivative and data_node on same dimensions
            # dim(bafux_2d) = c(3,elem2d_n)
            bafux_2d_vert <- replicate(bafux_2d, n=1) # only once for nvars
            bafuy_2d_vert <- replicate(bafuy_2d, n=1)
            bafux_2d_vert <- aperm(bafux_2d_vert, c(3, 1, 2)) # c(1, 3, elem2d_n)
            bafuy_2d_vert <- aperm(bafuy_2d_vert, c(3, 1, 2))
            bafux_2d_vert <- replicate(bafux_2d_vert, n=dim(data_vert)[3]) # ndepths
            bafuy_2d_vert <- replicate(bafuy_2d_vert, n=dim(data_vert)[3])
            bafux_2d_vert <- replicate(bafux_2d_vert, n=dim(data_vert)[4]) # nrecspf
            bafuy_2d_vert <- replicate(bafuy_2d_vert, n=dim(data_vert)[4])
            # dim(bafux_2d_vert) = c(1,3,elem2d_n,ndepths,nrecspf)
            ## note: avoid using huge bafux_3d: dim=c(4,elem3d_n) (4 nodes of tetrahedral 3D element)

            ## put data_node from nodes on elems
            if (verbose > 1) {
                message(paste0(indent, "Bring data_node from (nod2d_n=", nod2d_n, " x ndepths=", 
                             dim(data_vert)[3], ") on (3 x elem2d_n=", elem2d_n, " x ndepths=", 
                             dim(data_vert)[3], ") for horizontal derivative ..."))
            }
            data_elem <- array(data_vert[c(dxinds,dyinds),pos[elem2d],,],
                                dim=c(ndxy, 3, elem2d_n,           # nvars, 3 nodes per 2D-element, nelem2d
                                      dim(data_vert)[3:4]),        # ndepth, nrecspf
                                dimnames=c(list(var=dimnames(data_vert)[[1]][c(dxinds,dyinds)]),
                                           list(node=1:3, elem=NULL), # do not name elem dim saves memory
                                           dimnames(data_vert)[3:4]))
            # dim(data_elem) = c(nvars,3,elem2d_n,ndepths,nrecspf)

            ## horizontal derivatives
            if (!is.null(dxinds)) {
                if (verbose > 1) {
                    message(paste0(indent, "Calc dx for interpolated depths ..."))
                }
                dvardx_elem <- array(0, 
                                      dim=c(length(dxinds), 1, dim(data_elem)[3:5]),
                                      dimnames=c(list(var=paste0("dx_", dimnames(data_elem)[[1]][dxinds], "_elem2d")),
                                                 list(node=NULL),
                                                 dimnames(data_elem)[3:5]))
                # dim(data_elem)     = c(nvars, 3,elem2d_n,ndepths,nrecspf)
                # dim(bafux_2d_vert) = c(1,     3,elem2d_n,ndepths,nrecspf)
                # dim(dvardx_elem)   = c(dxinds,1,elem2d_n,ndepths,nrecspf)
                for (i in 1:length(dxinds)) { # for all variables
                    for (j in 1:3) { # for all 3 nodes of a 2D-element
                        dvardx_elem[i,1,,,] <- dvardx_elem[i,1,,,] + data_elem[dxinds[i],j,,,]*bafux_2d_vert[1,j,,,]
                    }
                }
            }
            if (!is.null(dyinds)) {
                if (verbose > 1) {
                    message(paste0(indent, "Calc dy for interpolated depths ..."))
                }
                dvardy_elem <- array(0,
                                      dim=c(length(dyinds), 1, dim(data_elem)[3:5]),
                                      dimnames=c(list(var=paste0("dy_", dimnames(data_elem)[[1]][dyinds], "_elem2d")),
                                                 list(node=NULL),
                                                 dimnames(data_elem)[3:5]))
                for (i in 1:length(dyinds)) {
                    for (j in 1:3) {
                        dvardy_elem[i,1,,,] <- dvardy_elem[i,1,,,] + data_elem[dyinds[i],j,,,]*bafuy_2d_vert[1,j,,,]
                    }
                }
            }

            ## bring derivative back from (elem2d_n x ndepths) to (nod2d_n x ndepths)
            if (verbose > 1) {
                message(paste0(indent, "Bring derivative back from (3 x elem2d_n=", elem2d_n, " x ndepths=", 
                             dim(dvardy_elem)[4], ") on (nod2d_n=", nod2d_n, " x ndepths=", 
                             dim(dvardy_elem)[4], ") ..."))
                message(paste0(indent, "   run ", subroutinepath, "/sub_e2xde_to_n2xde.r ..."))
            }
            if (!is.null(dxinds)) {
                sub_e2xde_to_n2xde(dvardx_elem) # produces tmp
            }
            dvardx_vert <- tmp # dim(dvardx_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
            if (!is.null(dyinds)) {
                sub_e2xde_to_n2xde(dvardy_elem) 
            }
            dvardy_vert <- tmp


            ## bring derivative back from (nod2d_n x ndepths) to (nod3d_n) if possible
            if (dim_tag == "2D" 
                || average_depth
                || (dim_tag == "3D" && ndepths == 1)) { # nothing to do
                
                if (!is.null(dxinds)) {
                    dvardx_node <- dvardx_vert # dim(dvardx_node) = c(nvars,nod2d_n,ndepths=1,nrecspf)
                }
                if (!is.null(dyinds)) {
                    dvardy_node <- dvardy_vert
                }

            } else {

                if (!is.null(dxinds)) {
                    if (verbose > 1) {
                        message(paste0(indent, "Bring dvardx_vert back from (nod2d_n=", nod2d_n,
                                     " x ndepths=", dim(dvardx_vert)[3], ") on (nod3d_n=", nod3d_n, ") ..."))
                        message(paste0(indent, "   run ", subroutinepath, "/sub_n2xde_to_n3.r ..."))
                    }
                    sub_n2xde_to_n3(dvardx_vert) # produces tmp
                    dvardx_node <- tmp # dim(dvardx_node) = c(nvars,nod3d_n,ndepths=1,nrecspf)
                    #stop("asd")
                    if (F) {
                        for (i in 1:dim(dvardx_vert)[3]) {
                            indz <- ((i-1)*nod2d_n + 1):(i*nod2d_n)
                            message(paste0(i, ": ", min(indz), "/", max(indz)))
                            d <- dvardx_node[,indz,,1] - dvardx_vert[,,i,1]
                            message(range(d, na.rm=T))
                        }
                    }
                }
                if (!is.null(dyinds)) {
                    if (verbose > 1) {
                        message(paste0(indent, "Bring dvardy_vert back from (nod2d_n=", nod2d_n,
                                     " x ndepths=", dim(dvardy_vert)[3], ") on (nod3d_n=", nod3d_n, ") ..."))
                        message(paste0(indent, "   run ", subroutinepath, "/sub_n2xde_to_n3.r ..."))
                    }
                    sub_n2xde_to_n3(dvardy_vert) 
                    dvardy_node <- tmp 
                }
            }

        } # if horiz_elem_2d

        ## At this point,
        ##  if dim_tag == "2D":
        ##      dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf)
        ##      dim(dvardx_node) = c(nvars,nod2d_n,ndepths=1,nrecspf)
        ##  if dim_tag == "3D":       
        ##      dim(data_node) = c(nvars,nod3d_n,ndepths=1,nrecspf)
        ##      dim(dvardx_node) = c(nvars,nod3d_n,ndepths=1,nrecspf)
        ##  if horiz_deriv_node3d 
        ##      dim(dvardx_node3d) = c(nvars,nod3d_n,ndepths=1,nrecpsf)


        ## Do stuff after horizontal derivative
        if (any(varname == c("gradT", "gradmld", "gradB", "gradbathy"))) {
            message(paste0(indent, varname, " = sqrt[ (", 
                           dimnames(dvardx_node3d)[[1]][1], ")^2 + (",
                           dimnames(dvardy_node3d)[[1]][1], ")^2 ) ] ..."))
            data_node <- sqrt(dvardx_node3d[1,,,]^2 + dvardy_node3d[1,,,]^2)
        }

        if (varname == "hvel_dot_gradbathy") {
            message(paste0(indent, varname, " = -1*(u*", 
                           dimnames(dvardx_node3d)[[1]][1], " + v*",
                           dimnames(dvardy_node3d)[[1]][1], ") ..."))
            data_node <- -1*(data_node_uv[1,,,]*dvardx_node3d[1,,,] + 
                              data_node_uv[2,,,]*dvardy_node3d[1,,,])
            dimnames(data_node)[1] <- list(var=varname)
        }

        if (any(varname == c("u_geo", "v_geo", "hvel_geo"))) {
            if (any(varname == c("u_geo", "hvel_geo"))) {
                if (verbose > 1) {
                    message(paste0(indent, "u_geo = -g/f dSSH/dy ..."))
                }
                u_geo <- -g/coriolis_node * dvardy_node3d
                dimnames(u_geo)[[1]] <- "u_geo"
                if (varname == "u_geo") {
                    data_node <- u_geo
                }
            }
            if (any(varname == c("v_geo", "hvel_geo"))) {
                if (verbose > 1) {
                    message(paste0(indent, "v_geo = g/f dSSH/dx ..."))
                }
                v_geo <- g/coriolis_node * dvardx_node3d
                dimnames(v_geo)[[1]] <- "v_geo"
                if (varname == "v_geo") {
                    data_node <- v_geo
                }
            }
            if (varname == "hvel_geo") {
                message(paste0(indent, varname, " = sqrt(u_geo² + v_geo²) ..."))
                tmp <- sqrt(u_geo^2 + v_geo^2)
                dimnames(tmp)[[1]] <- varname
                if (uv_out) {
                    data_node <- abind(u_geo, v_geo, tmp, along=1, use.dnns=T)  
                } else {
                    data_node <- tmp
                }
                rm(tmp, envir=.GlobalEnv) 
            }
        } # "u_geo", "v_geo", "hvel_geo"

        if (varname == "hdiffb") {
            message(paste0(indent, varname, " = d(K_h*dbdx)/dx + d(K_h*dbdy)/dy ..."))
        }

        # divergence of horizontal flux
        if (any(varname == c("divuv", 
                             "divuvt", "divuvteddy", 
                             "divuvsgst", "divuvsgsteddy", "divuvsgsttot",
                             "divuvs", "divuvseddy",
                             "divuvsgss", "divuvsgsseddy", "divuvsgsstot"))) {

            varinds <- c(1, 1)
           
            if (horiz_deriv_elem2d) {
                if (verbose > 1) {
                    message(paste0(indent, varname, " = ", dimnames(dvardx_node)[[1]][varinds[1]], 
                                 " + ", dimnames(dvardy_node)[[1]][varinds[2]], " ..."))
                }
                tmp <- dvardx_node[varinds[1],,,] + dvardy_node[varinds[2],,,]
                dimnames(tmp)[[1]] <- paste0(varname, "_elem2d")
                if (uv_out) {
                    data_node <- abind(dvardx_node, dvardy_node, tmp, along=1, use.dnns=T)
                } else {
                    data_node <- tmp
                }
            }

            if (horiz_deriv_node3d) {
                if (verbose > 1) {
                    message(paste0(indent, varname, " = ", dimnames(dvardx_node3d)[[1]][varinds[1]],
                                 " + ", dimnames(dvardy_node3d)[[1]][varinds[2]], " ..."))
                }
                tmp <- dvardx_node3d[varinds[1],,,] + dvardy_node3d[varinds[2],,,]
                dimnames(tmp)[[1]] <- varname
                if (uv_out) {
                    data_node3d <- abind(dvardx_node3d, dvardy_node3d, tmp, along=1, use.dnns=T)
                } else {
                    data_node3d <- tmp
                }
            }

            # for comparison cat both derivatives together ...
            if (horiz_deriv_elem2d && horiz_deriv_node3d) {
                data_node <- abind(data_node, data_node3d, along=1, use.dnns=T)
            } else if (!horiz_deriv_elem2d && horiz_deriv_node3d) {
                data_node <- data_node3d
            }
        
        } # divergence of horizontal flux
          # "divuvt", "divuvteddy",
          # "divuvsgst", "divuvsgsteddy", "divuvsgsttot",
          # "divuvs", "divuvseddy",
          # "divuvsgss", "divuvsgsseddy", "divuvsgsstot"

        # horizontal advection
        if (any(varname == c("advh"))) {
            # https://en.wikipedia.org/wiki/Derivation_of_the_Navier%E2%80%93Stokes_equations#Momentum_equation
            stop("asd")

        } # if advh

        if (any(varname == c("relvorti", "relvortisq", 
                             "curlwind", "curltau", 
                             "ekmanP", "ekmanP_ms"))) {
            message(indent, varname, " = ", dimnames(dvardx_node3d)[[1]], " - ", dimnames(dvardy_node3d)[[1]], " ...")
            data_node <- dvardx_node3d - dvardy_node3d
            if (varname == "ekmanP_ms") {
                message(indent, varname, " = ekmanP/rho0 = ekmanP/", rho0) 
                data_node <- data_node/rho0
            }
            dimnames(data_node)[[1]] <- varname
        }
        
        if (varname == "RossbyNo") {
            message(paste0(indent, varname, " = (", dimnames(dvardx_node3d)[[1]],
                           " - ", dimnames(dvardy_node3d)[[1]], ") / abs(f) ..."))
            data_node <- (dvardx_node3d - dvardy_node3d) / abs(coriolis_node)
            dimnames(data_node)[[1]] <- varname
        }

        if (any(varname == c("potvorti_bc", "potvorti_vert", "potvorti"))) {

            if (varname == "potvorti_bc") {
                message(paste0(indent, varname, " = db/dx(dw/dy - dv/dz) + db/dy(du/dz - dw/dx) ..."))
            }
            if (varname == "potvorti_vert") {
                message(paste0(indent, varname, " = db/dz(f + dv/dx - du/dy) ..."))
            }
            if (varname == "potvorti") {
                message(paste0(indent, varname, " = (vec{k}f + vec{nabla} x vec{u}) cdot vec{nabla} b ..."))
            }

            # Put everything together
            if (any(varname == c("potvorti_bc", "potvorti"))) {
                term1 <- dvar4dx_node * (dvar3dy_node - dvar2dz)
                term2 <- dvar4dy_node * (dvar1dz - dvar3dx_node)
            }

            if (any(varname == c("potvorti_vert", "potvorti"))) {
                if (ltm) f_mat <- f_mat[,,1,1]
                term3 <- dvar3dz * (dvar2dx_node - dvar1dy_node + f_mat)
            }

            if (varname == "potvorti_bc") {
                data_node <- term1 + term2
            }

            if (varname == "potvorti_vert") {
                data_node <- term3
            }

            if (varname == "potvorti") {
                data_node <- term1 + term2 + term3
            }

        } # "potvorti_bc", "potvorti_vert", "potvorti"

        if (any(varname == c("strain_normal", "strain_shear", "strain", "okubo"))) {
            if (any(varname == c("strain_normal", "strain", "okubo"))) {
            message(paste0(indent, varname, " = (d", varname_nc[1],
                         "dx + d", varname_nc[2], "dy)² ..."))

                strain_normal <- (dvar1dx + dvar2dy)^2
                if (varname == "strain_normal") {
                    data_node <- strain_normal
                }
            }
            if (any(varname == c("strain_shear", "strain", "okubo"))) {
            message(paste0(indent, varname, " = (d", varname_nc[2],
                         "dx + d", varname_nc[1],  "dy)² ..."))

                strain_shear <- (dvar2dx + dvar1dy)^2

                if (varname == "strain_shear") {
                    data_node <- strain_shear
                }
            }
            if (any(varname == c("strain", "okubo"))) {
            message(paste0(indent, varname, " = (d",
                         varname_nc[1], "dx + d", varname_nc[2], "dy)² + (d",
                         varname_nc[2], "dx + d", varname_nc[1], "dy)² ..."))

                strain <- strain_normal + strain_shear
                if (varname == "strain") {
                    data_node <- strain
                }
            }
            if (varname == "okubo") {
            message(paste0(indent, varname, " = (d",
                         varname_nc[1], "dx + d", varname_nc[2], "dy)² + (d",
                         varname_nc[2], "dx + d", varname_nc[1], "dy)² - (d",
                         varname_nc[2], "dx - d", varname_nc[1], "dy)² ..."))

                okubo <- strain_normal + strain_shear - relvortisq
                data_node <- okubo
            }

            # todo: okubo_budget ...

            dimnames(data_node)[1] <- list(var=varname)

        } # if "strain_normal", "strain_shear", "strain", "okubo"

        # baroclinic energy conversion (mean potential -> eddy potential)
        if (any(varname == c("PmPe", "PmPe_wN2"))) {
            
            message(indent, varname, " = -1/N2 [ u'b' * dx_b + v'b' * dy_b ] (Olbers et al. 2012, p. 379)\n",
                    indent, rep(" ", t=nchar(varname)), 
                    " = -1/N2 * (g/rho0)^2 * [ u'rho' * dx_rho + v'rho' * dy_rho ]\n",
                    indent, "   with g = ", g, ", rho0 = ", rho0, " ...")
            term1 <<- (data_node[which(varname_nc == "urho"),,,] -
                      data_node[which(varname_nc == "u"),,,]*
                      data_node[which(varname_nc == "rho"),,,])*dvardx_node3d["dx_rho",,,]
            term2 <<- (data_node[which(varname_nc == "vrho"),,,] -
                      data_node[which(varname_nc == "v"),,,]*
                      data_node[which(varname_nc == "rho"),,,])*dvardy_node3d["dy_rho",,,]
            if (any(data_node == Inf)) message("jaaa")
            if (any(data_node == -Inf)) message("-jaaa")
            if (varname == "PmPe") {
                data_node <- -1/N2_potdens_node * (g/rho0)^2 * (term1 + term2)
            } else if (varname == "PmPe_wN2") {
                data_node <- -1/data_node[which(varname_nc == "N2"),,,] * (g/rho0)^2 * (term1 + term2)
            }
            if (any(data_node == Inf)) message("jaaa2")
            if (any(data_node == -Inf)) message("-jaaa2")

        } # PmPe

        if (varname == "HRS" || varname == "KmKe") {

            message(paste0(indent, varname, " = - u'u'*dudx - u'v'*dudy - u'v'*dvdx - v'v'*dvdy"))

            ## hrs = -u'u'*dudx - u'v'*dudy - u'v'*dvdx - v'v'*dvdy
            term1 <- -(data_node[which(varname_nc == "uu"),,,] -
                        data_node[which(varname_nc == "u"),,,]^2)*dvardx_node3d["dx_u",,,]
            term2 <- -(data_node[which(varname_nc == "uv"),,,] -
                        data_node[which(varname_nc == "u"),,,]*
                        data_node[which(varname_nc == "v"),,,])*(dvardy_node3d["dy_u",,,] + dvardx_node3d["dx_v",,,])
            term3 <- -(data_node[which(varname_nc == "vv"),,,] -
                        data_node[which(varname_nc == "v"),,,]^2)*dvardy_node3d["dy_v",,,] 
            hrs <- term1 + term2 + term3

            if (varname == "HRS") {
                data_node <- hrs
            }

        } # hrs || KmKe

        if (varname == "KmKe") {
            message(paste0(indent, varname, " = HRS + VRS"))
            message(paste0(indent, paste0(rep(" ", t=nchar(varname)), collapse=""),
                         " = - u'u'*dudx - u'v'*dudy - u'v'*dvdx - v'v'*dvdy - u'w'*dudz - v'w'*dvdz"))
        }
        
        if (varname == "slopeSx") {
            message(paste0(indent, varname, " = -[(drho/dx)/(drho/dz)] ..."))
        }
        
        if (varname == "slopeSy") {
            message(paste0(indent, varname, " = -[(drho/dy)/(drho/dz)] ..."))
        }
        
        if (varname == "slopeS") {
            message(paste0(indent, varname, " = sqrt{[-(drho/dx)/(drho/dz)]^2 + [-(drho/dy)/(drho/dz)]^2} ..."))
        }
        
        if (varname == "slopeSsq") {
            message(paste0(indent, varname, " = (sqrt{[-(drho/dx)/(drho/dz)]^2 + [-(drho/dy)/(drho/dz)]^2})^2 ..."))
        }

        if (varname == "intz_uvteddy_div") {
            data_node <- abind(dvardx_node3d, dvardy_node3d, along=1)
                
            print("str(data_node)")
            print(str(data_node))

            stop("asd")
            if (F) {
                if (F) {
                    if (F) {
                        falnc = nc_open("/mnt/lustre01/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948-2009.phi_rotated_grid_false.nc")
                        trunc = nc_open("/mnt/lustre01/pf/a/a270073/scripts/fortran/strfcn/bin/out/Low01.1948-2009.phi_rotated_grid_true.nc")
                    } else if (T) {
                        falnc = nc_open("/mnt/lustre01/pf/a/a270073/scripts/fortran/strfcn/bin/Low01.1948.phi_sum_u_times_dx.nc")
                        trunc = nc_open("/mnt/lustre01/pf/a/a270073/scripts/fortran/strfcn/bin/Low01.1948.phi_sum_udx.nc")
                    }
                    falx=drop(apply(ncvar_get(falnc, "dxphi"), 1, mean))
                    faly=drop(apply(ncvar_get(falnc, "dyphi"), 1, mean))
                    trux=drop(apply(ncvar_get(trunc, "dxphi"), 1, mean))
                    truy=drop(apply(ncvar_get(trunc, "dyphi"), 1, mean))
                    #tes <- sqrt(falx^2 + faly^2)
                    #tes <- sqrt(trux^2 + truy^2)
                    tes = sqrt(falx^2 + faly^2) - sqrt(trux^2 + truy^2)
                } else if (F) {
                    tes <- drop(data_node_ltm[3,,1,1])
                } else if (F) {
                    dxdyphinc <- nc_open("/pf/a/a270073/scripts/fortran/strfcn/bin/Low01.1948.phi.nc")
                    dxphi <- ncvar_get(dxdyphinc, "dxphi")
                    dyphi <- ncvar_get(dxdyphinc, "dyphi")
                }
                if (T) {
                    ip <- image.plot.pre(range(tes, na.rm=T))
                } else if (F) {
                    zlim <- c(0, 3*summary(tes)[3])
                    ip <- image.plot.pre(zlim)
                }
                col_inds_vec <- findInterval(tes, ip$levels, all.inside=T)
                dev.new()
                plot(xcsur,ycsur,t="n")
                points(xcsur,ycsur,col=ip$cols[col_inds_vec], cex=0.5, pch=16)
            }

            # Rotate vector components
            if (T && rotate_mesh) {
                inds <- c(1, 2)
                if (verbose > 1) {
                    message(paste0(indent, "Rotate global ", 
                                 dimnames(data_node)[[1]][inds[1]], " and ", 
                                 dimnames(data_node)[[1]][inds[2]], 
                                 " back to geographic coordinates ... "))
                }
                rotated_coords <- vec_rotate_r2g(Ealpha, Ebeta, Egamma, nod_x, nod_y, 
                                                 data_node[inds[1],,,], 
                                                 data_node[inds[2],,,], 1)
                #data_node[inds[1],,,] <- rotated_coords$u
                #data_node[inds[2],,,] <- rotated_coords$v
                #rm(rotated_coords, envir=.GlobalEnv) 
                data_node <- abind(rotated_coords$u, rotated_coords$v, along=1)
                print(str(data_node))
                dimnames(data_node)[[1]] <- c(dimnames(dvardx_node3d)[[1]][1],
                                              dimnames(dvardy_node3d)[[1]][1])
                rm(rotated_coords)
            } # if rotate_mesh

            # add vector norm
            data_node <- abind(data_node,
                               sqrt(data_node[1,,,]^2 + data_node[2,,,]^2),
                               along=1)
            dimnames(data_node)[[1]][3] <- varname
            
            data_node <- data_node
            print("str(data_node)")
            print(str(data_node))

        } # intz_uvteddy_div

        if (varname == "divuvt2") { ## special       
              
            stop("update")
            message(paste0(indent, varname, " = \vec{nabla_h} { laplace^-2 [ \vec{nabla_h} cdot ( \vec{u}*t ) ] } ..."))

            ## Retain vector information of tracer flux divergence by taking 
            ## first the inverse laplacian and then the gradient. 
            ## see Jayne and Marotzke 2002
            ## https://doi.org/10.1175/1520-0485(2002)032<3328:TOEHT>2.0.CO;2
            if (verbose > 1) {
                message(paste0(indent, "Calc inverted laplacian for interpolated depths only ..."))
            }
            if (T) {
                message("data")
                message(str(data_node))
            }

            ## Laplacian (dim = 3 x e2)
            laplace_2d <- bafux_2d^2 + bafuy_2d^2

            ## Invert laplacian using singular value decomposition (SVD)
            ## of Matrix X = U D V'
            ## with U = left singular vectors of X  (dim = 3 x 3)
            ##      V = right singular vectors of X (dim = e2 x 3)
            ##      D = singular values of X        (length = 3)
            laplace_2d_svd <- svd(laplace_2d) 

            ## Diagonal matrix with inverted singular values of X (dim = 3 x 3)
            laplace_2d_diag <- diag(1/laplace_2d_svd$d) 

            ## Psuedoinverse of X is X^(-1) = V D^(-1) U' (dim = 3 x e2) 
            ## e.g. https://en.wikipedia.org/wiki/Generalized_inverse
            laplace_2d_inv <- laplace_2d_svd$v %*% laplace_2d_diag %*% t(laplace_2d_svd$u)

            ## Repeat inverted laplacian in depth and time
            laplace_2d_inv_time <- replicate(laplace_2d_inv, n=dim(data_global)[3]) # ntime per year
            laplace_2d_inv_time <- replicate(laplace_2d_inv_time, n=dim(data_node)[4]) # ndepths

            ## Bring div_h(tracer flux) from elems to nodes
            if (verbose > 1) {
                message(paste0(indent, "Bring div_h(tracer flux) back from elem2d to nod2d ..."))
            }
            data_node <- array(0,
                               dim=c(dim(data_node)[1],
                                     nod2d_n, dim(data_node)[3:4]),
                               dimnames=c(dimnames(data_node)[1],
                                          list(node=1:nod2d_n),
                                          dimnames(data_node)[3:4]))
            inds <- data_node

            # create progress bar
            pb <- mytxtProgressBar(min=0, max=elem2d_n, style=pb_style,
                                    char=pb_char, width=pb_width,
                                    indent=paste0("     ", indent)) # 5 " " for default message()

            ## put element values on 3 nodes
            for (i in 1:elem2d_n) {

                #progress_function(elem2d_n, i, indent=paste0(indent, "   "))
                elnodes <- elem2d[,i]

                if (dim(data_node)[3] == 1 &&
                    dim(data_node)[4] == 1) { # 1 depth and 1 time
                    data_node[,elnodes,,] <- data_node[,elnodes,,] +
                                                rep(data[,i,,], t=3)

                } else if (dim(data_node)[3] == 1 &&
                           dim(data_node)[4] != 1) { # several depths but 1 time
                    data_node[,elnodes,,] <- data_node[,elnodes,,] +
                                                t(array(data[,i,,], c(ndepths, 3)))

                } else if (dim(data_node)[3] != 1 &&
                           dim(data_node)[4] == 1) { # 1 depth but several times
                    data_node[,elnodes,,] <- data_node[,elnodes,,] +
                                                t(array(data[,i,,], c(dim(data_node)[3], 3)))

                } else {
                    data_node[,elnodes,,] <- data_node[,elnodes,,] +
                                                aperm(replicate(data[,i,,], n=3),
                                                      c(3,1,2))

                }
                inds[,elnodes,,] <- inds[,elnodes,,] + 1

                # update progress bar
                setTxtProgressBar(pb, i)

            } # for i elem2d_n

            # close progress bar
            close(pb)

            data_node <- data_node/inds
            if (T) {
                message("data_node")
                message(str(data_node))
            }

            ## Bring div_h(tracer flux) from nodes to elements
            if (verbose > 1) {
                message(paste0(indent, "Bring div_h(tracer flux) from nodes on elements for inverted laplacian ..."))
            }
            var_elem <- array(0, c(1, dim(elem2d), dim(data_node)[3:4]))
            dimnames(var_elem)[1] <- list(var=dimnames(data_node)[[1]][1])
            for (i in 1:3) {
                var_elem[1,i,,,] <- data_node[1,elem2d[i,],,] # here 'data_node' is the tracer flux divergence
            }

            if (F) {
                message("var_elem")
                message(str(var_elem))
                message("bafux_2d")
                message(str(bafux_2d))
                message("bafux_2d_time")
                message(str(bafux_2d_time))
                message("laplace_2d_inv")
                message(str(laplace_2d_inv))
                message("laplace_2d_inv_time")
                message(str(laplace_2d_inv_time))
            }

            ## Apply inverted laplacian
            laplace_inv_var_elem <- array(0, c(1, dim(var_elem)[3:5]),
                                           dimnames=list(paste0("laplace_inv_", varname),
                                                         1:dim(var_elem)[3],
                                                         dimnames(data_node)[[3]],
                                                         dimnames(data_node)[[4]]))
            for (i in 1:3) {
                laplace_inv_var_elem[1,,,] <- laplace_inv_var_elem[1,,,] + var_elem[1,i,,,]*laplace_2d_inv_time[,i,,]
            }

            ## Bring laplace^-2 [ div_h(tracer flux) ] from elems to nodes
            if (verbose > 1) {
                message(paste0(indent, "Bring laplacian^-2 [ div_h(tracer flux) ] back from elem2d to nod2d ..."))
            }
            laplace_inv_var_node <- array(0,
                                           dim=c(dim(laplace_inv_var_elem)[1], 
                                                 nod2d_n, dim(laplace_inv_var_elem)[3:4]),
                                           dimnames=c(dimnames(laplace_inv_var_elem)[1],
                                                      list(node=1:nod2d_n),
                                                      dimnames(laplace_inv_var_elem)[3:4]))
            inds <- laplace_inv_var_node

            # create progress bar
            pb <- mytxtProgressBar(min=0, max=elem2d_n, style=pb_style,
                                    char=pb_char, width=pb_width,
                                    indent=paste0("     ", indent)) # 5 " " for default message()

            ## put element values on 3 nodes
            for (i in 1:elem2d_n) {

                #progress_function(elem2d_n, i, indent=paste0(indent, "   "))
                elnodes <- elem2d[,i]

                if (dim(laplace_inv_var_elem)[3] == 1 && 
                    dim(laplace_inv_var_elem)[4] == 1) { # 1 depth and 1 time
                    laplace_inv_var_node[,elnodes,,] <- laplace_inv_var_node[,elnodes,,] +
                                                rep(laplace_inv_var_elem[,i,,], t=3)

                } else if (dim(laplace_inv_var_elem)[3] == 1 && 
                           dim(laplace_inv_var_elem)[4] != 1) { # several depths but 1 time
                    laplace_inv_var_node[,elnodes,,] <- laplace_inv_var_node[,elnodes,,] +
                                                t(array(laplace_inv_var_elem[,i,,], c(ndepths, 3)))

                } else if (dim(laplace_inv_var_elem)[3] != 1 && 
                           dim(laplace_inv_var_elem)[4] == 1) { # 1 depth but several times
                    laplace_inv_var_node[,elnodes,,] <- laplace_inv_var_node[,elnodes,,] +
                                                t(array(laplace_inv_var_elem[,i,,], c(dim(data_node)[3], 3)))

                } else {
                    laplace_inv_var_node[,elnodes,,] <- laplace_inv_var_node[,elnodes,,] +
                                                aperm(replicate(laplace_inv_var_elem[,i,,], n=3),
                                                      c(3,1,2))

                }
                inds[,elnodes,,] <- inds[,elnodes,,] + 1

                # update progress bar
                setTxtProgressBar(pb, i)

            } # for i elem2d_n

            # close progress bar
            close(pb)

            laplace_inv_var_node <- laplace_inv_var_node/inds
            
            if (F) {
                message("laplace_inv_var_elem")
                message(str(laplace_inv_var_elem))
            }

            ## Bring laplace^-2 [ div_h(tracer flux) ] from nodes to elements
            if (verbose > 1) {
                message(paste0(indent, "Bring laplacian^-2 [ div_h(tracer flux) ] from nodes on elements for horizontal derivative ..."))
            }
            var_elem <- array(0, c(1, dim(elem2d), dim(data_node)[3:4]))
            dimnames(var_elem)[1] <- list(var=dimnames(laplace_inv_var_node)[[1]][1])
            for (i in 1:3) {
                var_elem[1,i,,,] <- laplace_inv_var_node[1,elem2d[i,],,]
            }

            ## Apply gradient to retain vector information
            if (verbose > 1) {
                message(paste0(indent, "Calc horizontal gradients for interpolated depths only ..."))
            }
            dvardx <- array(0, c(1, dim(var_elem)[3:5]),
                            dimnames=list(paste0("d_", dimnames(var_elem)[[1]][1], "_dx"),
                                          1:dim(var_elem)[3],
                                          dimnames(data_node)[[3]],
                                          dimnames(data_node)[[4]]))
            dvardy <- array(0, c(1, dim(var_elem)[3:5]),
                            dimnames=list(paste0("d_", dimnames(var_elem)[[1]][1], "_dy"),
                                          1:dim(var_elem)[3],
                                          dimnames(data_node)[[3]],
                                          dimnames(data_node)[[4]]))
            for (i in 1:3) {
                dvardx[1,,,] <- dvardx[1,,,] + var_elem[1,i,,,]*bafux_2d_time[,i,,]
                dvardy[1,,,] <- dvardy[1,,,] + var_elem[1,i,,,]*bafuy_2d_time[,i,,]
            }

            ## Save vector information of tracer flux divergence
            if (uv_out) {
                udata_node <- dvardx
                vdata_node <- dvardy
            }

            if (F) {
                message("udata")
                message(str(udata_node))
            }

        } # divuvt2

    } # if (horiz_deriv_tag != F && (!is.null(dxinds) || !is.null(dyinds)))


    ## Fsalt: upper boundary condition for salinity
    if (varname == "Fsalt") {
        if (verbose > 0) {
            message(paste0(indent, "Fsalt = SSS/(1-SSS/1000)*(Evap - Snow - Rain - Runoff + ThdGr + ThdGrSn) + relax_salt_term ..."))
            message(paste0(indent, "   e.g. Josey (2003): doi:10.1029/2003JC001778"))
        }
        EminusP <- data_node[which(varname_nc == "snow"),,,]*-1 +   # all in m s-1
                   data_node[which(varname_nc == "rain"),,,]*-1 +
                   data_node[which(varname_nc == "evap"),,,] +      
                   data_node[which(varname_nc == "runoff"),,,]*-1 + 
                   data_node[which(varname_nc == "thdgr"),,,] +     # thermodynamic growth rate of eff. ice thickness
                   data_node[which(varname_nc == "thdgrsn"),,,]     # melting rate of snow thickness

        #denom <- 1 - data_node[which(varname_nc == "salt"),,,]/1e6 # /1e3 or /1e6 almost no diff
        denom <- 1
        data_node <- data_node[which(varname_nc == "salt"),,,]*EminusP/denom + # psu m s-1
                     data_node[which(varname_nc == "relax_salt"),,,] # psu m s-1
    } # Fsalt

    if (varname == "Fsalt2") {
        if (verbose > 0) {
            message(paste0(indent, "Fsalt2 = virtual_salt + relax_salt"))
        }
        data_node <- data_node[which(varname_nc == "virtual_salt"),,,] + 
                     data_node[which(varname_nc == "relax_salt"),,,]
    } # Fsalt2

    # upper boundary conditions which need sea water functions
    if (varname == "Ftemp") {
        if (verbose > 0) {
            message(paste0(indent, "Ftemp = Qnet/(rho*cp)"))
        }
        if (F) { # to do: non-constant cp
            data_node <- data_node[which(varname_nc == "qnet"),,,]/(cp_node*data_node[which(varname_nc == "rho"),,,])
        } else if (T) {
            #fu <<- data_node
            #stop("asd")
            data_node <- data_node[which(varname_nc == "qnet"),seq_len(nod2d_n),,]/
                            (cp*data_node[which(varname_nc == "rho"),seq_len(nod2d_n),,])
        }
    } # Ftemp

    if (any(varname == c("Fthermal", "Fthermalbudget", 
                         "Fhaline", "Fhalinebudget",
                         "Frho", "Frhobudget",
                         "FthermalB", "FthermalBbudget",
                         "FhalineB", "FhalineBbudget",
                         "FrhoB", "FrhoBbudget",
                         "Frho2", "FrhoB2"))) {
        
        if (sea_water == "TEOS10") {
            success <- load_package("gsw")
            if (!success) stop()
        }
        success <- load_package("abind")
        if (!success) stop()
        
        # here, the 3d fields temp and salt were already rearranged to ndepths x nod2d_n
        # and selected from the wanted depth "depths"
        
        if (total_rec == 0 || (!transient_out && !regular_transient_out)) {
            if (sea_water == "EOS80") {
                stop("asd")  
                ycsur_mat <- replicate(ycsur, n=ndepths)
                if (ndepths == 1) {
                    depth_mat <- replicate(depth_mat, n=1)
                } else {
                    depth_mat <- aperm(depth_mat, c(2,1))
                }
                pres_mat <- sw_pres(DEPTH=depth_mat, LAT=ycsur_mat)
                pres_mat <- replicate(pres_mat, n=dim(data_global_vert)[3])
                pres_mat <- replicate(pres_mat, n=1)
                pres_mat <- aperm(pres_mat, c(4,1,3,2))
            } else if (sea_water == "TEOS10") {
                depths_node <- replicate(interpolate_depths, n=nod2d_n)
                p_node <- gsw::gsw_p_from_z(z=depths_node, latitude=ycsur)
                assign('p_node', p_node, envir=.GlobalEnv)
            }
            sea_water_fname <- paste0("_", sea_water)

        } # if total_rec == 0
        
        saltind <- which(vars == "salt" | vars == "so" | vars == "sos")
        if (is.na(saltind)) stop("could not find variable salt, so or sos.")
        tempind <- which(vars == "temp" | vars == "thetao" | vars == "tos")
        if (is.na(tempind)) stop("could not find variable temp, thetao or tos.")
        
        # cp from TEOS10
        if (is.character(cp) && cp == "gsw_cp_t_exact") {
            if (verbose > 0) {
                message(indent, "cp = 'gsw_cp_t_exact' (change to finite number in runscript if wanted) ...")
            }
            if (verbose > 1) {
                message(paste0(indent, "   SA = gsw_SA_from_SP(SP, p, longitude, latitude)"))
                message(paste0(indent, "      with SP        Practical Salinity (PSS-78) [unitless]"))
                message(paste0(indent, "           p         sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar"))
                message(paste0(indent, "                     = gsw_p_from_z(z, latitude)"))
                message(paste0(indent, "           longitude longitude in decimal degrees, positive to the east of Greenwich."))
                message(paste0(indent, "           latitude  latitude in decimal degrees, positive to the north of the equator."))
            }
            SA_node <- array(NA, dim=dim(data_node[1,,,]),
                              dimnames=dimnames(data_node[saltind,,,]))
            dimnames(SA_node)[[1]] <- "SA"
            for (i in 1:dim(data_node)[4]) { # for nrecspf
                SA_node[,,,i] <- gsw::gsw_SA_from_SP(SP=drop(data_node[saltind,,,i]), 
                                                     p=p_node,
                                                     longitude=nod_x, latitude=nod_y)
            }
            if (verbose > 1) {
                message(paste0(indent, "   t = gsw_t_from_pt0(SA, pt0, p)"))
                message(paste0(indent, "      with SA   Absolute Salinity [ g/kg ]"))
                message(paste0(indent, "           pt0  potential temperature with reference sea pressure (pr) = 0 dbar. [ deg C ]"))
                message(paste0(indent, "           p    sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar"))
            }
            t_node <- array(NA, dim=dim(data_node[1,,,]),
                              dimnames=dimnames(data_node[tempind,,,]))
            dimnames(t_node)[[1]] <- "t"
            for (i in 1:dim(data_node)[4]) { # for nrecspf
                t_node[,,,i] <- gsw::gsw_SA_from_SP(SA=drop(SA_node[,,,i]), 
                                                    pt0=drop(data_node[tempind,,,i]),
                                                    p=p_node)
            }
            if (verbose > 0) {
                message(indent, "cp = gsw::gsw_cp_t_exact(SA, t, p)")
                message(paste0(indent, "   with SA Absolute Salinity [g/kg]"))
                message(paste0(indent, "        t  in-situ temperature [degC]"))
                message(paste0(indent, "        p  sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar"))
            }
            cp_node <- array(NA, dim=dim(data_node[1,,,]),
                             dimnames=dimnames(data_node[tempind,,,]))
            dimnames(cp_node)[[1]] <- "cp"
            for (i in 1:dim(data_node)[4]) { # for nrecspf
                cp_node[,,,i] <- gsw::gsw_cp_t_exact(SA=drop(SA_node[,,,i]), 
                                                     t=drop(t_node[,,,i]),
                                                     p=p_node)
            }
        # or constant cp
        } else {
            if (!is.finite(cp)) stop("'cp' either needs to be a finite numeric or 'gsw_cp_t_exact'.")
            cp_node <- cp
        }

        # forcing data which needs alpha, beta, rho
        if (any(varname == c("Fthermal", "Fthermalbudget", 
                             "Fhaline", "Fhalinebudget",
                             "Frho", "Frhobudget",
                             "FthermalB", "FthermalBbudget",
                             "FhalineB", "FhalineBbudget",
                             "FrhoB", "FrhoBbudget",
                             "Frho2", "FrhoB2"))) {

            # alpha beta
            if (sea_water == "EOS80") {
                if (verbose > 1) {
                    message(indent, "alpha = sw_alpha(S,T,P,keyword='ptmp') in K-1 ...\n",
                            indent, "beta = sw_beta(S,T,P,keyword='ptmp') in psu-1 ...")
                }
                alpha_node <- sw_alpha(S=data_node[saltind,,,],
                                        T=data_node[tempind,,,],
                                        P=pres_mat, keyword="ptmp")
                beta_node <- sw_beta(S=data_node[saltind,,,],
                                      T=data_node[tempind,,,],
                                      P=pres_mat, keyword="ptmp")
            
            } else if (sea_water == "TEOS10") {
                if (verbose > 1) {
                    message(indent, "alpha = gsw_alpha(SA,CT,p) in K-1 ...\n",
                            indent, "beta = gsw_beta(SA,CT,p) in psu-1 ...")
                }
                SA_node <- array(NA, dim=dim(data_node[1,,,]),
                                 dimnames=dimnames(data_node[saltind,,,]))
                dimnames(SA_node)[[1]] <- "SA"
                for (i in 1:dim(data_node)[4]) { # for nrecspf
                    SA_node[,,,i] <- gsw::gsw_SA_from_SP(SP=drop(data_node[saltind,,,i]), 
                                                         p=p_node,
                                                         longitude=xcsur, latitude=ycsur)
                }
                CT_node <- array(NA, dim=dim(data_node[1,,,]),
                                 dimnames=dimnames(data_node[tempind,,,]))
                dimnames(CT_node)[[1]] <- "CT"
                for (i in 1:dim(data_node)[4]) { # for nrecspf
                    CT_node[,,,i] <- gsw::gsw_CT_from_pt(SA=drop(SA_node[,,,i]), 
                                                         pt=drop(data_node[tempind,,,i]))
                }   
                alpha_node <- array(NA, dim=dim(data_node[1,,,]),
                                    dimnames=dimnames(data_node[tempind,,,]))
                dimnames(alpha_node)[[1]] <- "alpha"
                for (i in 1:dim(data_node)[4]) { # for nrecspf
                    alpha_node[,,,i] <- gsw::gsw_alpha(SA=drop(SA_node[,,,i]), 
                                                       CT=drop(CT_node[,,,i]),
                                                       p=p_node)
                }   
                beta_node <- array(NA, dim=dim(data_node[1,,,]),
                                   dimnames=dimnames(data_node[tempind,,,]))
                dimnames(beta_node)[[1]] <- "beta"
                for (i in 1:dim(data_node)[4]) { # for nrecspf
                    beta_node[,,,i] <- gsw::gsw_beta(SA=drop(SA_node[,,,i]), 
                                                      CT=drop(CT_node[,,,i]),
                                                      p=p_node)
                }   
            } # which sea_water 

            # in-situ density
            if (any(varname == c("Fhaline", "Fhalinebudget",
                                 "Frho", "Frhobudget", 
                                 "FhalineB", "FhalineBbudget",
                                 "FrhoB", "FrhoBbudget",
                                 "Frho2", "FrhoB2"))) {
                if (sea_water == "EOS80") {
                    if (verbose > 1) {
                        message(indent, "rho = sw_dens(S,T,p) in kg m-3 ...")
                    }
                    rho_node <- sw_dens(S=data_node[which(varname_nc == "salt"),,,],
                                        T=data_node[which(varname_nc == "temp"),,,],
                                        P=pres_mat)
                } else if (sea_water == "TEOS10") {
                    if (verbose > 1) {
                        message(indent, "rho = gsw_dens(SA,CT,p) in kg m-3 ...")
                    }
                    rho_node <- array(NA, dim=dim(data_node[1,,,]),
                                           dimnames=dimnames(data_node[tempind,,,]))
                    dimnames(rho_node)[[1]] <- "rho"
                    for (i in 1:dim(data_node)[4]) { # for nrecspf
                        rho_node[,,,i] <- gsw::gsw_rho(SA=SA_node[,,,i],
                                                       CT=CT_node[,,,i],
                                                       p=p_node)
                    }
                } # which sea_water
            } # if density is needed
           
            # Fthermal is needed
            if (any(varname == c("Fthermal", "Fthermalbudget",
                                 "Frho", "Frhobudget",   
                                 "FthermalB", "FthermalBbudget",
                                 "FrhoB", "FrhoBbudget",
                                 "Frho2", "FrhoB2"))) {
                if (verbose > 1) {
                    message(paste0(indent, "Fthermal = -rho*alpha*Ftemp = -alpha/cp*Qnet ..."))
                    message(paste0(indent, "   with Qnet = swrd + lwrd + olwout + osen + olat"))
                    message(paste0(indent, "   from Josey (2003): doi:10.1029/2003JC001778"))
                }

                if (any(varname == c("Fthermal", "Frho",
                                     "FthermalB", "FrhoB",
                                     "Frho2", "FrhoB2"))) {
                   
                    qnetind <- which(varname_nc == "qnet")
                    if (is.na(qnetind)) stop("could not find variable qnet.")

                    Fthermal_node <- -alpha_node/cp_node*data_node[qnetind,,,]
                    if (varname == "Fthermal") {
                        data_node <- Fthermal_node
                        dimnames(data_node)[1] <- list(varname)
                    } else if (varname == "FthermalB") {
                        data_node <- -g/rho0*Fthermal_node
                        dimnames(data_node)[1] <- list(varname)
                    }
                    assign('Fthermal_node', Fthermal_node, envir=.GlobalEnv)
                }
               
                # Fthermalbudget Frhobudget
                if (any(varname == c("Fthermalbudget", "Frhobudget",
                                     "FthermalBbudget", "FrhoBbudget"))) {
                    
                    inds <- match(c("swrd", "lwrd", "olwout", "osen", "olat"), 
                                   varname_nc)
                    if (any(is.na(inds))) stop("could not find some variables of swrd lwrd olwout oswn olat.")

                    # replicate alpha for all vars
                    tmp <- alpha_node
                    for (i in 1:(length(inds) - 1)) {
                        tmp <- abind(tmp, alpha_node, along=1, use.dnns=T)
                    }
                    alpha_node <- tmp
                    Fthermalbudget_node <- -alpha_node/cp_node * data_node[inds,,,]
                    if (any(varname == c("Fthermalbudget", "Frhobudget"))) {
                        dimnames(Fthermalbudget_node)[1] <- list(paste0("Fth_rho_", varname_nc[inds]))
                    } else if (any(varname == c("FthermalBbudget", "FrhoBbudget"))) {
                        dimnames(Fthermalbudget_node)[1] <- list(paste0("Fth_b_", varname_nc[inds]))
                    }

                    # add Fthermal as sum of all components
                    if (any(varname == c("Fthermalbudget", "FthermalBbudget"))) {
                        Fthermalbudget_node <- abind(Fthermalbudget_node,
                                                      array(apply(Fthermalbudget_node, c(2, 3, 4), sum), # sum of all components
                                                          c(1, dim(Fthermalbudget_node)[2:4])), # restore dimension
                                                      along=1, use.dnns=T)
                        dimnames(Fthermalbudget_node)[[1]][dim(Fthermalbudget_node)[1]] <- varname
                    
                        if (varname == "Fthermalbudget") {
                            data_node <- Fthermalbudget_node
                        } else if (varname == "FthermalBbudget") {
                            data_node <- -g/rho0*Fthermalbudget_node
                        }
                    } # Fthermalbudget 
                } # Fthermalbudget Frhobudget
            } # if Fthermal is needed

            # Fhaline is needed
            if (any(varname == c("Fhaline", "Fhalinebudget",
                                 "Frho", "Frhobudget",
                                 "FhalineB", "FhalineBbudget",
                                 "FrhoB", "FrhoBbudget",
                                 "Frho2", "FrhoB2"))) {
               
                if (any(varname == c("Fhaline", "Fhalinebudget",
                                     "Frho", "Frhobudget",
                                     "FhalineB", "FhalineBbudget",
                                     "FrhoB", "FrhoBbudget"))) {

                    if (verbose > 0) {
                        message(indent, "Fhaline = Ffac * (E - P) + relax_salt_term ...\n",
                                indent, "   with            Ffac = rho * beta * SSS / (1 - SSS/1000) = kg m-3 * psu-1 * psu\n",
                                indent, "                  E - P = -1*snow + -1*rain + evap + -1*runoff + thdgr + thdgrsn (all in m s-1)\n",
                                indent, "        relax_salt_term = rho * beta * relax_salt = kg m-3 * psu-1 * psu m s-1 = \n",
                                indent, "                  thdgr = thermodynamic growth rate of eff. sea ice thickness\n",
                                indent, "                thdgrsn = melting rate of snow thickness\n",
                                indent, "   from Josey (2003): doi:10.1029/2003JC001778")
                    }

                    #FhalineFac_node <- rho_node * beta_node *  data_node[saltind,,,] / (1 -  data_node[saltind,,,]) # after Schmitt et al. 1989
                    FhalineFac_node <- rho_node * beta_node *  data_node[saltind,,,] / (1 - data_node[saltind,,,]/1000) # after Josey et al. 2003
                    dimnames(FhalineFac_node)[1] <- list(var="FhalineFac")

                    relax_salt_ind <- which(varname_nc == "relax_salt") # psu m s-1
                    if (is.na(relax_salt_ind)) stop("could not find variable relax_salt.")

                    salt_relax_term <- rho_node * beta_node * data_node[relax_salt_ind,,,]
                    dimnames(salt_relax_term)[1] <- list(var="salt_relax_term")
                  
                    # no budget
                    if (any(varname == c("Fhaline", "Frho",
                                         "FhalineB", "FrhoB"))) {
                        
                        inds <- match(c("snow", "rain", "evap", "runoff", "thdgr", "thdgrsn"), 
                                       varname_nc)
                        if (any(is.na(inds))) stop("could not find some variables of snow rain evap runoff thdgr thdgrsn")
                        
                        Fhaline_node <- data_node[which(varname_nc == "snow"),,,]*-1 + # all m s-1
                                         data_node[which(varname_nc == "rain"),,,]*-1 +
                                         data_node[which(varname_nc == "evap"),,,] +
                                         data_node[which(varname_nc == "runoff"),,,]*-1 +
                                         data_node[which(varname_nc == "thdgr"),,,] +
                                         data_node[which(varname_nc == "thdgrsn"),,,]
                        Fhaline_node <- FhalineFac_node * Fhaline_node + salt_relax_term

                        if (varname == "Fhaline") {
                            data_node <- Fhaline_node
                            dimnames(data_node)[1] <- list(varname)
                        } else if (varname == "FhalineB") {
                            data_node <- -g/rho0*Fhaline_node
                            dimnames(data_node)[1] <- list(varname)
                        }
                        assign('Fhaline_node', Fhaline_node, envir=.GlobalEnv)
                    }
                } # if Fhaline is needed

                if (any(varname == c("Frho2", "FrhoB2"))) {
                    if (verbose > 0) {
                        message(paste0(indent, "Fhaline = rho * beta * (virtual_salt + relax_salt) ..."))
                    }
                    Fhaline_node <- rho_node * beta_node * 
                                        (data_node[which(varname_nc == "virtual_salt"),,,] + 
                                         data_node[which(varname_nc == "relax_salt"),,,])
                    assign('Fhaline_node', Fhaline_node, envir=.GlobalEnv)
                } # Frho2 FrhoB2

                if (any(varname == c("Fhalinebudget", "Frhobudget",
                                     "FhalineBbudget", "FrhoBbudget"))) {

                    inds <- match(c("snow", "rain", "evap", "runoff", "thdgr", "thdgrsn"),
                                   varname_nc)
                    if (any(is.na(inds))) stop("could not find some variables of snow rain evap runoff thdgr thdgrsn")
                    
                    # apply sign convention
                    neg_inds <- match(c("snow", "rain", "runoff"),
                                       dimnames(data_node)[[1]])
                    #neg_inds <- c(which(dimnames(data_node)[[1]] == "snow"),
                    #               which(dimnames(data_node)[[1]] == "rain"),
                    #               which(dimnames(data_node)[[1]] == "runoff"))
                    if (length(neg_inds) == 0 || any(is.na(neg_inds))) stop("what?!")
                    if (verbose > 2) {
                        message(indent, "Multiply ", 
                                paste0(dimnames(data_node)[[1]][neg_inds], collapse=","), 
                                " *-1")
                    }
                    #print(data_node[9,1,1,1])
                    data_node[neg_inds,,,] <- -1*data_node[neg_inds,,,]
                    #print(data_node[9,1,1,1])
                    
                    # replicate FhalineFac for all inds
                    if (!any(search() == "package:abind")) library(abind)
                    tmp <- FhalineFac_node
                    for (i in 1:(length(inds) - 1)) {
                        tmp <- abind(tmp, FhalineFac_node, along=1, use.dnns=T)
                    }
                    FhalineFac_node <- tmp
                    
                    Fhalinebudget_node <- FhalineFac_node * data_node[inds,,,]
                    if (any(varname == c("Fhalinebudget", "Frhobudget"))) {
                        dimnames(Fhalinebudget_node)[1] <- list(paste0("Fha_rho_", varname_nc[inds]))
                    } else if (any(varname == c("FhalineBbudget", "FrhoBbudget"))) {
                        dimnames(Fhalinebudget_node)[1] <- list(paste0("Fha_b_", varname_nc[inds]))
                    }

                    # add salt_relax term without FahlineFac
                    Fhalinebudget_node <- abind(Fhalinebudget_node, salt_relax_term, 
                                                 along=1, use.dnns=1)

                    if (any(varname == c("Fhalinebudget", "FhalineBbudget"))) {
                        # add Fhaline as sum of all components
                        Fhalinebudget_node <- abind(Fhalinebudget_node,
                                                     array(apply(Fhalinebudget_node, c(2, 3, 4), sum), # sum of all components
                                                           c(1, dim(Fhalinebudget_node)[2:4])), # restore dimension
                                                     along=1, use.dnns=T)
                        dimnames(Fhalinebudget_node)[[1]][dim(Fhalinebudget_node)[1]] <- varname

                        if (varname == "Fhalinebudget") {
                            data_node <- Fhalinebudget_node
                        } else if (varname == "FhalineBbudget") {
                            data_node <- -g/rho0*Fhalinebudget_node
                        }
                    } # Fhalinebudget
                } # Fhalinebudget or Frhobudget
            } # Fhaline is needed

            # Frho is needed
            if (any(varname == c("Frho", "Frhobudget",
                                 "FrhoB", "FrhoBbudget",
                                 "Frho2", "FrhoB2"))) {
                if (verbose > 1) {
                    message(paste0(indent, "Frho = Fthermal + Fhaline"))
                }
                
                if (any(varname == c("Frho", "FrhoB", "Frho2", "FrhoB2"))) {
                    data_node <- Fthermal_node + Fhaline_node
                    
                    if (varname == "FrhoB" || varname == "FrhoB2") {
                        if (verbose > 1) message(indent, varname, " = -g/rho0 * Frho")
                        data_node <- -g/rho0*data_node
                    }
                    
                    dimnames(data_node)[1] <- list(varname)
                } # "Frho", "FrhoB", "Frho2", "FrhoB2"
                
                # Frhobudget
                if (varname == "Frhobudget" || varname == "FrhoBbudget") {
                    #print(str(Fthermalbudget_node))
                    #print(str(Fhalinebudget_node))

                    data_node <- abind(Fthermalbudget_node, Fhalinebudget_node,
                                        along=1, use.dnns=T)
                    #print(str(data_node))

                    # add Frho as sum of all components
                    data_node <- abind(data_node, 
                                        array(apply(data_node, c(2, 3, 4), sum), # sum of all components
                                              dim=c(1, dim(data_node)[2:4])), # restore dimension
                                        along=1, use.dnns=T)
                    
                    if (varname == "FrhoBbudget") data_node <- -g/rho0*data_node
                    
                    # name of last entry = sum of all components
                    dimnames(data_node)[[1]][dim(data_node)[1]] <- varname
                    #print(str(data_node))
                    #print(dimnames(data_node)[[1]])
                } # Frhobudget
            } # Frho Frhobudget
        } # forcing data which needs alpha, beta, rho
    } # forcing stuff

    if (regexpr("MOC", varname) != -1) {
       
        if (verbose > 0) {
            message(indent, varname, "(lat,z) = cumsum_{lat}[sum_{nod3d_at_lat}(", 
                    varname_nc[1], "*vol)]", appendLF=F)
            if (varname == "MOCw") {
                message(" (like in fpost1.4)", appendLF=F)
            }
            message("")
        }

        # initialize array for every year
        moc <- array(0, 
                      dim=c(1, length(moc_reg_lat_global), 
                            #aux3d_n, # todo: ndepths or aux3d_n here?
                            ndepths,
                            dim(data_node)[4])) # c(nvars,nreg_lat,ndepths,ntime)
        if (all(total_rec == 0)) {
            moc_topo <- array(1, 
                               dim=c(length(moc_reg_lat_global), 
                                     #aux3d_n
                                     ndepths
                                     ))
        } # only once

        # if MOC calculation based on vertical velocity as in fpost
        if (varname == "MOCw") {
            
            if (F) {
                success <- load_package("Rcpp", indent=indent)
                if (!success) {
                    Rcpp_tag <- F
                    message(indent, "note: a much faster C version of the following task is available via the Rcpp package.\n",
                            indent, "      Consider installing it with install.packages(\"Rcpp\").\n",
                            indent, "      ", helppage)
                } else if (success) {
                    Rcpp_tag <- T
                }
            }
            
            Rcpp_tag <- F # Rcpp routine not yet finished!
            if (Rcpp_tag == T) {
                #ttime <- system.time({sourceCpp("lib/sub_e2_to_n2.cpp", cacheDir=subroutinepath)}) # 18 sec!!! 
                if (levelwise == T) {
                    tmp <- dyn.load(paste0(subroutinepath, "/sourceCpp/sub_calc_MOCw_levelwise.so"))
                    sub_calc_MOCw_levelwise <- Rcpp:::sourceCppFunction(function(elem2d, # 3 x elem2d_n
                                                                                 voltriangle, # elem2d_n
                                                                                 ycsur, # nod2_n
                                                                                 moc_reg_lat_global, # ny_reg
                                                                                 moc_mask, # nod2d_n
                                                                                 moc_topo, # ny_reg x ndepths 
                                                                                 data_node, # nvars x nod2d_n x ndepth x nrecspf
                                                                                 total_rec # integer
                                                                                 ) {}, 
                                                              isVoid=F, dll=tmp, symbol='sourceCpp_1_sub_e2_to_n2')
                    tmp <- sub_e2_to_n2(elem2d, resolution, nod2d_n) 
                } else if (levelwise == F) {
                    stop("noasdaossdhaodh")
                }

            } else if (Rcpp_tag == F) { # not Rcpp

                # create progress bar
                pb <- mytxtProgressBar(min=0, max=elem2d_n, style=pb_style,
                                        char=pb_char, width=pb_width,
                                        indent=paste0("   ", indent)) 

                for (i in seq_len(elem2d_n)) {
                #for (i in c(1, elem2d_n)) {
                    elnodes <- elem2d[,i]
                    m <- moc_mask[elnodes] # 1=inside, 0=outside
                    
                    if (any(m == 1)) { # element is in area for moc calculation
                        #stop("asd")
                        #x <- mean(xcsur[elnodes]) # not needed
                        y <- mean(ycsur[elnodes])
                        vol <- voltriangle[i] # area of i-th 2d-element in `mesh_dist_unit`^2 (default: m^2)
                        yind <- which(moc_reg_lat_global > y)[1] # first index within latitude bin from south
                        if (is.na(yind)) { # find closest
                            yind <- which(abs(moc_reg_lat_global - y) == min(abs(moc_reg_lat_global - y)))
                        }
                        #for (di in 1:aux3d_n) { # calculate in aux3d space
                        for (di in seq_len(ndepths)) {
                            if (levelwise == F) {
                                elnode3 <- aux3d[di,elnodes]
                                if (all(elnode3 > 0)) { 
                                    # if no -999 entries --> element is completely within polygon
                                    vel <- data_node[,elnode3,,] # dim(vel)=c(nvar=1,nodes=3,depth=1,nrecspf)
                                    m <- array(moc_mask[elnodes], dim(vel)) # repeat mask in time dim
                                    # mean over 3 3d-nodes; keep time dim4
                                    moc[1,yind,di,] <- moc[1,yind,di,] + vol*apply(vel*m, 4, mean)*1.e-6 # m3/s --> Sv
                                    if (all(total_rec == 0)) moc_topo[yind,di] <- NA # = 1 at land and NA at water
                                } # if element has no -999 entries in aux3d
                            } else if (levelwise == T) {
                                if (all(!is.na(data_node[,elnodes,di,]))) { 
                                    # if no -999 entries --> element is completely within polygon 
                                    vel <- data_node[,elnodes,di,] # dim(vel)=c(nvar=1,nodes=3,depth=1,nrecspf)
                                    m <- array(moc_mask[elnodes], dim(vel)) # repeat mask in time dim
                                    # mean over 3 3d-nodes; keep time dim4
                                    moc[1,yind,di,] <- moc[1,yind,di,] + vol*apply(vel*m, 4, mean)*1.e-6 # m3/s --> Sv 
                                    if (all(total_rec == 0)) moc_topo[yind,di] <- NA # = 1 at land and NA at water
                                }
                            }
                        } # for di aux3d_n/ndepths
                    } # if elem2d is in area for moc calculation
                
                    # update progress bar
                    setTxtProgressBar(pb, i)
                
                } # for i elem2d_n
                
                # close progress bar
                close(pb)
            
            } # if Rcpp or not

        } # if MOCw or different MOC variable
    
        # cumsum meridionally: from north-south / south-north
        # dim(moc) = c(1,nregy,ndepths,nrecspf)
        moc_save <- moc
        if (area == "global_ocean") { # global moc
            for (i in 2:length(moc_reg_lat_global)) { 
                moc[1,i,,] <- moc[1,i,,] + moc[1,i-1,,]
            }
        } else {
            #moc <- apply(moc, 2, cumsum)
            for (i in (length(moc_reg_lat_global) - 1):1) {
                if (varname == "MOCw") moc[1,i,,] <- -moc[1,i,,] + moc[1,i+1,,]
                if (varname == "MOCv") moc[1,i,,] <- moc[1,i,,] + moc[1,i+1,,]
            }
        } # if global moc or not
        
        data_node <- moc
        dimnames(data_node)[2:4] <- list(lat=moc_reg_lat_global, 
                                         depth=paste0(interpolate_depths, "m"),
                                         rec=timei)
        assign('moc_topo', moc_topo, envir=.GlobalEnv)

    } # "MOCw"

    ## variables that need cluster area
    if (any(varname == c("resolutionkm", "resolutiondeg", "mesharea",
                         "fwflux", 
                         "siarea", "siextent", "icevol"))) {
        
        # dim(cluster_area_2d) = nod2d_n  
        cluster_area_2d_vert <- replicate(cluster_area_2d, n=1) # nvars
        if (varname == "mesharea") {
            cluster_area_2d_vert <- replicate(cluster_area_2d_vert, n=1) # nrecs
            cluster_area_2d_vert <- replicate(cluster_area_2d_vert, n=1) # ndepths
        } else {
            cluster_area_2d_vert <- replicate(cluster_area_2d_vert, n=dim(data_node)[3]) # ndepths
            cluster_area_2d_vert <- replicate(cluster_area_2d_vert, n=dim(data_node)[4]) # nrecspf
        }
        cluster_area_2d_vert <- aperm(cluster_area_2d_vert, c(2, 1, 3, 4)) 
        # dim(cluster_area_2d_vert) = c(nvars,nod2d_n,ndepths=1,nrecspf)

        if (varname == "mesharea") {
            data_node <- cluster_area_2d_vert

        } else if (varname == "fwflux") {
            if (verbose > 1) {
                message(paste0(indent, varname, " = runoff*cluster_area_2d"))
            }
            varinds <- which(vars == "runoff")
            if (any(is.na(varinds))) stop("Could not find data.")

            data_node <- data_node[varinds[1],,,]*cluster_area_2d_vert

        } else if (any(varname == c("resolutionkm", "resolutiondeg"))) {
            
            ## bring derivative back from (elem2d_n x ndepths) to (nod2d_n x ndepths)
            if (verbose > 1) {
                message(indent, "Bring resolution from elem2d_n=", elem2d_n, 
                        " to nod2d_n=", nod2d_n, " ...")
            }
            
            success <- load_package("Rcpp", indent=indent)
            if (!success) {
                Rcpp_tag <- F
                message(indent, "note: a much faster C version of the following task is available via the Rcpp package.\n",
                        indent, "      Consider installing it with install.packages(\"Rcpp\").\n",
                        indent, "      ", helppage)
            } else if (success) {
                Rcpp_tag <- T
            }
            # check if Rcpp
            if (Rcpp_tag) {
                #ttime <- system.time({sourceCpp("lib/sub_e2_to_n2.cpp", cacheDir=subroutinepath)}) # 18 sec!!! 
                dll <- paste0(subroutinepath, "/sourceCpp/sub_e2_to_n2.so")
                if (verbose > 0) message(indent, "Load dynamic lib base::dyn.load(", dll, ") ...")
                tmp <- base::dyn.load(paste0(subroutinepath, "/sourceCpp/sub_e2_to_n2.so"))
                if (verbose > 0) message(indent, "Run Rcpp:::sourceCppFunction(sourceCpp_1_sub_e2_to_n2) ...")
                sub_e2_to_n2 <- Rcpp:::sourceCppFunction(function(elem2d, data_elem2d, nod2d_n) {}, 
                                                         isVoid=F, dll=tmp, symbol='sourceCpp_1_sub_e2_to_n2')
                tmp <- sub_e2_to_n2(elem2d, resolution, nod2d_n) 
            
            } else if (!Rcpp_tag) {
                tmp <- rep(0, t=nod2d_n)
                inds <- tmp
                # dim(resolution) = elem2d_n
                pb <- mytxtProgressBar(min=0, max=elem2d_n, style=pb_style,
                                       char=pb_char, width=pb_width,
                                       indent=paste0(indent, "  ")) # 5 " " for default message()
                for (i in 1:elem2d_n) {
                    elnodes <- elem2d[,i]
                    tmp[elnodes] <- tmp[elnodes] + rep(resolution[i], t=3)
                    inds[elnodes] <- inds[elnodes] + 1
                    setTxtProgressBar(pb, i)
                }
                close(pb)
                tmp <- tmp/inds # dim(data_node) = c(nvars,nod2d_n,ndepths=1,nrecspf=1)
            
            } # if Rcpp_tag or not

            # correct dims
            data_node <- array(tmp, dim=dim(data_node),
                               dimnames=dimnames(data_node))

            if (varname == "resolutiondeg") {
                
                stop("how?!")
                # matlab: km2deg(km,radius) = rad2deg( km2rad(km,radius) );
                #   with: km2rad(km,radius): rad = km/radius;
                #         rad2deg(R): D = R*180/pi;
                #data_node <- resolution/Rearth*180/pi # resolution and Rearth have the same unit ([m] by default)

                a <- (cos(yc[2,1]*pi/180)*sin(abs(diff(xc[1:2,1]))*pi/180))^2
                b <- (cos(yc[1,1]*pi/180)*sin(yc[2,1]*pi/180) - sin(yc[1,1]*pi/180)*cos(yc[2,1]*pi/180)*cos(abs(diff(xc[1:2,1]))*pi/180))^2
                c <- sin(yc[1,1]*pi/180)*sin(yc[2,1]*pi/180) + cos(yc[1,1]*pi/180)*cos(yc[2,1]*pi/180)*cos(abs(diff(xc[1:2,1]))*pi/180)
                #ds <- atan2((sqrt(a+b)/c)*pi/180)
                ds <- atan((sqrt(a+b)/c)*pi/180)

            } # "resolutiondeg"

        } else if (any(varname == c("siarea", "siextent", "icevol"))) {
            if (any(varname == c("siarea", "siextent"))) {
                varinds <- c("siconc"=which(vars == "area" | vars == "sic"))
            } else if (varname == "icevol") {
                varinds <- c("siconc"=which(vars == "area" | vars == "sic"),
                             "sithick"=which(vars == "hice" | vars == "sithick"))
            }
            if (any(is.na(varinds))) stop("Could not find data.")

            if (verbose > 1) {
                if (varname == "siarea") {
                    message(indent, varname, " = ", vars[varinds["siconc"]], " * cluster_area_2d ...")
                } else if (varname == "siextent") {
                    message(indent, varname, " = cluster_area_2d(", vars[varinds["siconc"]], " thr) ...")
                } else if (varname == "icevol") {
                    message(indent, varname, " = ", vars[varinds["siconc"]], " * ", vars[varinds["sithick"]], " * cluster_area_2d ...")
                }
                if (!is.null(sic_cond)) {
                    message(indent, "   with sea ice concentration ", sic_cond, 
                            " (='sic_cond') ", sic_thr, " (='sic_thr').")
                    message(indent, "   Change these variables to 'NULL' in namelist.var.r if you dont want a threshold ...")
                }
            } # verbose
           
            si_area <- cluster_area_2d_vert
            if (!is.null(sic_cond)) {
                if (sic_cond == ">") {
                    sic_inds <- data_node[varinds["siconc"],,,] > sic_thr
                    sic_cond_fname <- "gt"
                
                } else if (sic_cond == ">=") {
                    sic_inds <- data_node[varinds[1],,,] >= sic_thr
                    sic_cond_fname <- "ge"
                
                } else if (sic_cond == "<") {
                    sic_inds <- data_node[varinds[1],,,] < sic_thr
                    sic_cond_fname <- "lt"
                
                } else if (sic_cond == "<=") {
                    sic_inds <- data_node[varinds[1],,,] <= sic_thr
                    sic_cond_fname <- "le"
                
                } else {
                    stop(indent, "sea ice concentration condition '", sic_cond, 
                         "' not defined. Choose among '>', '>=', '<', or '<='")
                }
                si_area[!sic_inds] <- 0
            } # !is.null(sic_cond) 

            if (varname == "siarea") {
                data_node <- si_area * data_node[varinds["siconc"],,,]
                
            } else if (varname == "siextent") {
                data_node <- si_area

            } else if (varname == "icevol") {
                data_node <- si_area * data_node[varinds["sithick"],,,]

            }
        
        } # "siarea", "siextent", "icevol"
        
        dimnames(data_node)[[1]] <- list(varname)

    } # "resolutionkm", "resolutiondeg", "mesharea", "fwflux", "siarea", "siextent", "icevol"

    ## recom
    if (varname == "NPPtot") {

        inds <- match(varname_nc, c("diags3d01", "diags3d02"))
        if (anyNA(inds)) stop("did not find varnames \"diags3d01\" and \"diags3d02\" in `varname_nc`")
        message(indent, "Calc ", varname, " = ", varname_nc[inds[1]], " + ", varname_nc[inds[2]], " ...")
        data_node <- data_node[inds[1],,,] + data_node[inds[2],,,]
        dimnames(data_node)[[1]] <- list(varname)

    } # NPPtot

    if (varname == "export_detC_100m") {

        # data is already interpolated to 100 m depth
        if (ndepths != 1) stop("rerun with `depths <- 100`")
        if (depths != 100) stop("rerun with `depths <- 100`")

        # sinking velocity detritus as a function of depth
        vdet_m_day <- 20 + 0.0288*depths # m day-1 at 100 m depth

        # carbon export by detritus 
        if (verbose > 1) message(indent, "Calc ", varname, " = ", varname_nc, "(z=", depths, " m) * vdet(z=", depths, " m)\n",
                                 indent, "   with vdet = 20 m day-1 + 0.0288 day-1 * ", depths, " m = ", vdet_m_day, " m day-1 ...")
        data_node <- data_node * vdet_m_day # mmolC m-3 * m day-1 = mmolC m-2 day-1

    } # export_detC_100m

    # overwrite global data_node with the result data_node from within this function
    assign('data_node', data_node, envir=.GlobalEnv)
    assign('dim_tag', dim_tag, envir=.GlobalEnv)
    assign('levelwise', levelwise, envir=.GlobalEnv)

} # sub_calc function

