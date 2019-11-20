## R

sub_prepare2 <- function(data_node) {

    ## check user input
    if (insitudens_tag && potdens_tag) {
        stop("set either 'insitudens_tag' OR 'potdens_tag' to TRUE.")
    }
    if (insitudens_tag && ("rho" %in% varname_fesom)) {
        # in situ density calculation not necessary since it was already loaded from fesom data.
        insitudens_tag <<- F
    }

    ## load or calculate in situ or potential density if needed
    ## at the end of this block your data_node has a further entry
    ## with the wanted type of density if needed.
    if (insitudens_tag || potdens_tag || buoyancy_tag) {
     
        ## use rho as calculated by fesom
        if ("rho" %in% varname_fesom) { # rho is in situ rho in fesom ocean only

            #dimnames(data_node)[[1]] <<- "insiturho"

            if (potdens_tag) {
                if (verbose > 0) {
                    message("Warning: dont know how to transfer fesoms insitu density to potential density. use in situ instead ...")
                }
            } # if potdens_tag

            if (buoyancy_tag) {
                if (verbose > 1) {
                    message(indent, "bouyancy b = -g/rho0*rho")
                }
                data_node["rho",,,] <<- -g/rho0*data_node["rho",,,]
                dimnames(data_node)[[1]][which(dimnames(data_node)[[1]] == "rho")] <<- "insitub"
            } # if buoyancy_tag

        ## density needs to be calculated from temp and salt
        } else if (!("rho" %in% varname_fesom)) {

            ## "temp","thetao","thetaoga" are potential temperatures; "tso" is surface
            ## "salt","so","soga" are practical salinities; "sos" is surface
            if (!any(c("temp", "thetao", "thetaoga", "tso") %in% varname_fesom) &&
                !any(c("salt", "so", "soga", "sos") %in% varname_fesom)) {
                message(paste0("Cannot calculate density."))
                message(paste0("Provide temperature (i.e. 'temp','thetao','thetaoga' or 'tso') and"))
                message(paste0("salinity (i.e. 'salt','so','soga' or 'sos') in 'varname_fesom'."))
                stop()
            }

            ## which temp and salt to use for density calculation?
            tempnames <- c("temp", "thetao", "thetaoga", "tso")
            saltnames <<- c("salt", "so", "soga", "sos")
            tempind <<- which(tempnames %in% varname_fesom)
            saltind <<- which(saltnames %in% varname_fesom)
            if (verbose > 0 && length(tempind) > 1) {
                    message(paste0(indent, "Note: you provided more than 1 temperature data in 'varname_fesom':",
                                 paste0(varname_fesom[tempind], collapse=","), "."))
            }
            if (verbose > 0 && length(saltind) > 1) {
                    message(paste0(indent, "Note: you provided more than 1 salinity data in 'varname_fesom':",
                                 paste0(varname_fesom[saltind], collapse=","), "."))
            }   
            tempind <<- which(varname_fesom == tempnames[tempind[1]])
            saltind <<- which(varname_fesom == saltnames[saltind[1]])
            if (verbose > 0) {
                message(paste0(indent, "Use '", varname_fesom[tempind], "','", 
                               varname_fesom[saltind], "' for density calculation ..."))
            }

            ## check if both temp and salt have same dimensions
            if (length(data_node[tempind,,,]) != length(data_node[saltind,,,])) {
                message(paste0("Error: provide temperature and salinity with same dimensions, i.e."))
                message(paste0("either both 1D ('thetaoga' and 'soga') or both 2D ('tso' and 'sos')"))
                message(paste0("or both 3D ('temp' and 'salt' if cpl_tag=F or 'thetao' and 'so' if cpl_tag=T)."))
                stop()
            }
              
            ## which sea water routine?
            if (sea_water != "TEOS10") {
                stop(paste0("set 'sea_water' to 'TEOS10' ('EOS80' is outdated)."))
            } else if (sea_water == "TEOS10") {
                success <<- load_package("gsw")
                if (!success) stop()
            }

            ## load abind package
            success <<- load_package("abind")
            if (!success) stop()

            ## If 3d, calculate pressure from height (75-term equation)
            ## gsw_p_from_z(z, latitude)
            ##  z           height, zero at surface and positive upwards [ m ]
            ##  latitude    latitude in decimal degrees, positive to the north of the equator.
            if (!exists("p_node")) { # only once
                if (potdens_tag) {
                    if (!is.finite(p_ref)) {
                        stop("cannot calculate potential density referenced to given non-finite \"p_ref\" = ", p_ref, ".")
                    }
                    p_ref_node <<- rep(p_ref, t=length(nod_z)) # nod2d or nod3d
                }
                if (dim_tag == "3D") {
                    if (verbose > 0) {
                        message(paste0(indent, "Calc pressure p = gsw::gsw_p_from_z(z, latitude) ..."))
                        if (verbose > 1) {
                            message(paste0(indent, "   with z        height, zero at surface and positive upwards [ m ]"))
                            message(paste0(indent, "        latitude latitude in decimal degrees, positive to the north of the equator."))
                        }
                    }
                    p_node <<- gsw::gsw_p_from_z(z=nod_z, latitude=nod_y)
                } else if (dim_tag == "2D") {
                    p_node <<- rep(0, t=nod2d_n)
                }

            } # if !exists("p_node")

            ## Calculate Absolute Salinity from Practical Salinity, pressure, longitude, and latitude.
            ## gsw_SA_from_SP(SP, p, longitude, latitude)
            ##  SP          Practical Salinity (PSS-78) [ unitless ]
            ##  p           sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar
            ##  longitude   longitude in decimal degrees, positive to the east of Greenwich.
            ##  latitude    latitude in decimal degrees, positive to the north of the equator.
            if (verbose > 0) {
                message(paste0(indent, "Calc absolute salinity SA = gsw::gsw_SA_from_SP(SP, p, longitude, latitude) ..."))
                if (verbose > 1) {
                    message(paste0(indent, "   with SP        Practical Salinity (PSS-78) [ unitless ]"))
                    message(paste0(indent, "        p         sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar"))
                    message(paste0(indent, "        longitude longitude in decimal degrees, positive to the east of Greenwich."))
                    message(paste0(indent, "        latitude  latitude in decimal degrees, positive to the north of the equator."))
                }
            }
            # note: gsw::* functions do not keep the matrix dimensions!!!
            SA_node <<- array(NA, dim=dim(data_node[saltind,,,]),
                              dimnames=dimnames(data_node[saltind,,,]))
            dimnames(SA_node)[[1]] <<- "SA"
            for (i in 1:dim(data_node)[4]) { # for nrecspf
                SA_node[,,,i] <<- gsw::gsw_SA_from_SP(SP=drop(data_node[saltind,,,i]), 
                                                      p=p_node,
                                                      longitude=nod_x, latitude=nod_y)
            }
            
            ## Calculate Conservative Temperature from Potential Temperature
            ## gsw_CT_from_pt(SA, pt)
            ##  SA  Absolute Salinity [ g/kg ]
            ##  pt  potential temperature (ITS-90) [ degC ]
            if (verbose > 0) {
                message(paste0(indent, "Calc Conservative Temperature CT = gsw::gsw_CT_from_pt(SA, pt) ..."))
                if (verbose > 1) {
                    message(paste0(indent, "   with SA Absolute Salinity [ g/kg ]"))
                    message(paste0(indent, "        pt potential temperature (ITS-90) [ degC ]"))
                }
            }
            # note: gsw::* functions do not keep the matrix dimensions!!!
            CT_node <<- array(NA, dim=dim(data_node[tempind,,,]),
                              dimnames=dimnames(data_node[tempind,,,]))
            dimnames(CT_node)[[1]] <<- "CT"
            for (i in 1:dim(data_node)[4]) { # for nrecspf
                CT_node[,,,i] <<- gsw::gsw_CT_from_pt(SA=drop(SA_node[,,,i]), 
                                                      pt=drop(data_node[tempind,,,i]))
            }

            ## calc density
            ## gsw_rho(SA, CT, p)
            ##  SA  Absolute Salinity [ g/kg ]
            ##  CT  Conservative Temperature [ degC ]
            ##  p   sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar
            if (verbose > 0) {
                message(indent, "Calc ", appendLF=F)
                if (insitudens_tag) {
                    message("in-situ ", appendLF=F)
                } else if (potdens_tag) {
                    message("potential ", appendLF=F) 
                }
                message("density = gsw::gsw_rho(SA, CT, p=", appendLF=F)
                if (insitudens_tag) {
                    message("p", appendLF=F)
                } else if (potdens_tag) {
                    message(p_ref, " (='p_ref')", appendLF=F) 
                }
                message(") ...")
                message(indent, "   with SA Absolute Salinity [g/kg]")
                message(indent, "        CT Conservative Temperature [degC]")
                if (insitudens_tag) {
                    message(indent, "        p  sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar")
                } else if (potdens_tag) {
                    message(indent, "        p  ", p_ref, " dbar (='p_ref')")
                }
            }

            if (insitudens_tag) {
                # note: gsw::* functions do not keep the matrix dimensions!!!
                insitudens_node <<- array(NA, dim=dim(data_node[1,,,]),
                                          dimnames=dimnames(data_node[tempind,,,]))
                dimnames(insitudens_node)[[1]] <<- "insitudens"
                for (i in 1:dim(data_node)[4]) { # for nrecspf
                    insitudens_node[,,,i] <<- gsw::gsw_rho(SA=SA_node[,,,i],
                                                           CT=CT_node[,,,i],
                                                           p=p_node)
                }
                if (buoyancy_tag) {
                    if (verbose > 0) {
                        message(indent, "insitub = -g/rho0*insitudens (from namelist.config.r: g=", g, ", rho0=", rho0, ")")
                    }
                    insitudens_node <<- -g/rho0*insitudens_node
                    dimnames(insitudens_node)[[1]] <<- "insitub"
                }

                # add dens to data
                data_node <<- abind(data_node, insitudens_node,
                                    along=1, use.dnns=T)
                rm(insitudens_node, envir=.GlobalEnv)

            } else if (potdens_tag) {
                ## note to teos10 functions: 
                # gsw_rho:
                #   potential density with respect to reference pressure, p_ref
                # gsw_sigma1:   
                #   potential density anomaly with reference pressure
                #   of 1000 dbar, this being this particular potential
                #   density minus 1000 kg/m^3 (75-term equation)
                # comparison:
                if (F) {
                    sa=37; ct=4
                    gsw_rho(sa, ct, p=0)        # 1029.237
                    gsw_sigma0(sa, ct) + 1000   # 1029.237
                    gsw_rho(sa, ct, p=1000)     # 1033.819
                    gsw_sigma1(sa, ct) + 1000   # 1033.819
                    gsw_rho(sa, ct, p=2000)     # 1038.299
                    gsw_sigma2(sa, ct) + 1000   # 1038.299
                    gsw_rho(sa, ct, p=3000)     # 1042.679
                    gsw_sigma3(sa, ct) + 1000   # 1042.679
                    gsw_rho(sa, ct, p=4000)     # 1046.96
                    gsw_sigma4(sa, ct) + 1000   # 1046.96
                } # same!
                # note: gsw::* functions do not keep the matrix dimensions!!!
                potdens_node <<- array(NA, dim=dim(data_node[1,,,]),
                                       dimnames=dimnames(data_node[tempind,,,]))
                dimnames(potdens_node)[[1]] <<- "potdens"
                for (i in 1:dim(data_node)[4]) { # for nrecspf
                    potdens_node[,,,i] <<- gsw::gsw_rho(SA=SA_node[,,,i],
                                                        CT=CT_node[,,,i],
                                                        p=p_ref_node)
                }
                if (buoyancy_tag) {
                    if (verbose > 0) message(indent, "potb = -g/rho0*potdens (g=", g, ",rho0=", rho0, ")")
                    potdens_node <<- -g/rho0*potdens_node
                    dimnames(potdens_node)[[1]] <<- "potb"
                } # if buoyancy_tag

                # add dens to data
                data_node <<- abind(data_node, potdens_node,
                                    along=1, use.dnns=T)
                rm(potdens_node, envir=.GlobalEnv)

            } # which density

        } # if "rho" is not in varname_fesom

    } # if (insitudens_tag || potdens_tag)

    ## calculate coriolis if needed
    if (coriolis_tag) {

        # only once necessary
        if (!exists("coriolis_node")) {
            if (verbose > 1) {
                message(indent, "Calc f = 2*omega*sin(y) with omega = ", omega, " ...")
            }
        
            # corliolis in node space (default)
            coriolis_node <<- array(2*omega*sin(ycsur*pi/180),
                                    dim=c(1, nod2d_n, dim(data_node)[3:4]))
            dimnames(coriolis_node)[[1]] <<- list("f")
        
        } # if !exists("coriolis_node")

    } # if coriolis_tag

    ## read bathymetry data for 2D variables (dim_tag = 2D") obtained from mesh files (nfiles = 0)
    if (any(varname == c("bathy", "gradbathy", "hvel_dot_gradbathy",
                         "c_barotrop", "foverh"))) {
        
        if (varname != "hvel_dot_gradbathy") ndepths <<- 1

        ## how to determine depths correctly?
        use_depth <<- "model" # "model" or "real"
        # real depth (depth.out):  -129 -112  -83  -66  -28  -17
        # model depth (nod3d.out): -125 -125  -80  -80  -30  -20 

        if (use_depth == "model") {
            if (verbose > 1) {
                message(paste0(indent, "Get model bathymetry (last depth before -999 in aux3d.out) ..."))
            }
            
            if (varname != "hvel_dot_gradbathy") {
                fid <<- paste0(meshpath, "/nod3d.out")
                nod3d_n <<- as.numeric(readLines(fid, n=1))
                if (verbose > 1) {
                    message(paste0(indent, "   read ", nod3d_n, 
                                 " 3D nodes from nod3d.out ..."))
                }
                if (!fread_tag) {
                    tmp <<- scan(fid, skip=1, quiet=T)
                    nod3d <<- matrix(tmp, nrow=nod3d_n, byrow=T)
                } else if (fread_tag) {
                    tmp <<- fread(fid, skip=1, showProgress=ifelse(verbose > 1, T, F))
                    nod3d <<- as.matrix(tmp)
                }
                nod_z <<- drop(nod3d[,4])
                rm(tmp, nod3d, envir=.GlobalEnv)
                
                fid <<- paste0(meshpath, "/aux3d.out")
                aux3d_n <<- as.numeric(readLines(fid, n=1))
                if (verbose > 1) {
                    message(paste0(indent, "   read ", aux3d_n*nod2d_n, 
                                 " 3D node indices from aux3d.out ..."))
                }
                if (!fread_tag) {
                    tmp <<- scan(fid, skip=1, nlines=aux3d_n*nod2d_n, quiet=T)
                    aux3d <<- matrix(tmp, nrow=aux3d_n, ncol=nod2d_n)
                } else if (fread_tag) {
                    tmp <<- fread(fid, skip=1, nrows=aux3d_n*nod2d_n, 
                                 showProgress=ifelse(verbose > 1, T, F))
                    aux3d <<- matrix(tmp$V1, nrow=aux3d_n, ncol=nod2d_n)
                }
                rm(tmp, envir=.GlobalEnv)
            } # if varname != "hvel_dot_gradbathy"

            inds <<- apply(aux3d, 2, function(x) x[which(x == -999)[1] - 1])
            bathy_node <<- abs(nod_z[inds])
            rm(inds, envir=.GlobalEnv)
            
            # model depths
            fesom_depths <<- c(0, sort(unique(abs(bathy_node)))) # = same as in z column of nod3d.out 
        
        } else if (use_depth == "real") {
            if (verbose > 1) {
                message(paste0(indent, "Read original bathymetry information from depth.out ..."))
            }
            fid <<- paste0(meshpath, "/depth.out")
            topo <<- scan(fid, quiet=T)
            bathy_node <<- abs(topo)
        
        } # which depth to use?
        
        #bathy_node <<- replicate(bathy_node, n=1) # nvar
        #bathy_node <<- replicate(bathy_node, n=1) # ndepth == 1
        #bathy_node <<- replicate(bathy_node, n=1) # nrecspf == 1 (ltm)
        #bathy_node <<- aperm(bathy_node, c(2, 1, 3, 4))
        bathy_node <<- array(bathy_node,
                             dim=c(1, nod2d_n, dim(data_node)[3:4])) # nvar,n2,ndepth,rec
        dimnames(bathy_node)[1] <<- list(var="bathy")

        #if (varname != "hvel_dot_gradbathy") {
        #    data_node_ltm <<- bathy_node
        #}

    } # "bathy", "gradbathy", "hvel_dot_gradbathy", "c_barotrop", "foverh"

} # sub_prepare2 function

