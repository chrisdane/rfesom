## R

sub_prepare2 <<- function(data_node) {

    ## gsw calculation  
    if (insitudens_tag || buoyancy_insitudens_tag || buoyancy_frequency_insitudens_tag ||
        potdens_tag || buoyancy_potdens_tag || buoyancy_frequency_potdens_tag) {
   
        # which sea water routine?
        if (sea_water != "TEOS10") {
            stop("set 'sea_water' to 'TEOS10' ('EOS80' is outdated).")
        } else if (sea_water == "TEOS10") {
            success <<- load_package("gsw")
            if (!success) stop()
        }

        # load abind package
        success <<- load_package("abind")
        if (!success) stop()
        
        # load or calculate in situ or potential density if needed
        tempnames <<- c("temp", "thetao", "tso") # "temp", "thetao", are potential temperatures; "tso" is surface
        saltnames <<- c("salt", "so", "sos") # "salt", "so", are practical salinities; "sos" is surface
        insitudensnames <<- c("rho") # so far only "rho" to my knowledge
        potdensnames <<- NA # so far not available fesom output to my knowledge

        tempind <<- saltind <<- insitudensind <<- potdensind <<- NULL # default

        # find temp, salt, (in-situ density) indices in data if in-situ density is needed 
        if (insitudens_tag || buoyancy_insitudens_tag || buoyancy_frequency_insitudens_tag) {

            if (verbose > 0) {
                message(indent, appendLF=F)
                if (insitudens_tag) message(" `insitudens_tag`", appendLF=F)
                if (buoyancy_insitudens_tag) message(" `buoyancy_insitudens_tag`", appendLF=F)
                if (buoyancy_frequency_insitudens_tag) message(" `buoyancy_frequency_insitudens_tag`", appendLF=F)
                message(" = T")
            }
            
            # use in-situ density already loaded from fesom data
            if (any(insitudensnames %in% varname_nc)) { 

                # fesoms in-situ `rho` can be used 
                insitudensind <<- which(insitudensnames %in% varname_nc)
                if (verbose > 0 && length(insitudensind) > 1) {
                    message(indent, "Note: you provided more than 1 in-situ density data in 'varname_nc':",
                            paste0(varname_nc[insitudensind], collapse=","), ".")
                }   
                insitudensind <<- which(varname_nc == insitudensnames[insitudensind[1]])
                if (verbose > 0) {
                    message(indent, "   --> use nc variable '", varname_nc[insitudensind], "' for calculation ...")
                }

            # need to calculate in-situ density from temp and salt 
            } else { 

                if (verbose > 0) {
                    message(indent, "   --> did not find in-situ density in `varname_nc` ",
                            "based on known in-situ density variable names\n",
                            indent, "      `potdensnames` = \"", paste(potdensnames, collapse="\",\""), "\"\n",
                            indent, "   --> find temperature and salinity variables ...")
                }
                if (!any(tempnames %in% varname_nc) &&
                    !any(saltnames %in% varname_nc)) {
                    stop("Provide temperature (i.e. \"", paste(tempnames, collapse="\",\""), "\" and ",
                         "salinity (i.e. \"", paste(saltnames, collapse="\",\""), "\" in 'varname_nc' ",
                         "to calculate andy kind of density.")
                }
                tempind <<- which(tempnames %in% varname_nc)
                saltind <<- which(saltnames %in% varname_nc)
                if (verbose > 0 && length(tempind) > 1) {
                    message(indent, "Note: you provided more than 1 temperature data in 'varname_nc':",
                            paste0(varname_nc[tempind], collapse=","), ".")
                }
                if (verbose > 0 && length(saltind) > 1) {
                    message(indent, "Note: you provided more than 1 salinity data in 'varname_nc':",
                            paste0(varname_nc[saltind], collapse=","), ".")
                }   
                tempind <<- which(varname_nc == tempnames[tempind[1]])
                saltind <<- which(varname_nc == saltnames[saltind[1]])
                if (verbose > 0) {
                    message(indent, "   --> use nc variables '", varname_nc[tempind], "','", 
                            varname_nc[saltind], "' for in-situ density calculation ...")
                }

            } # if in-situ density variable is directly available in fesom output or not

        } # if insitudens_tag || buoyancy_insitudens_tag || buoyancy_frequency_insitudens_tag

        # find temp, salt, (potential density) indices in data if potential density is needed 
        if (potdens_tag || buoyancy_potdens_tag || buoyancy_frequency_potdens_tag) {

            if (verbose > 0) {
                message(indent, appendLF=F)
                if (potdens_tag) message(" `potdens_tag`", appendLF=F)
                if (buoyancy_potdens_tag) message(" `buoyancy_potdens_tag`", appendLF=F)
                if (buoyancy_frequency_potdens_tag) message(" `buoyancy_frequency_potdens_tag`", appendLF=F)
                message(" = T")
            }

            # use potential density already loaded from fesom data
            if (any(potdensnames %in% varname_nc)) { 

                # fesoms potential density can be used
                if (verbose > 0) {
                    message(indent, "   --> check `varname_nc` for known potential density variable names\n",
                            indent, "      `potdensnames` = \"", paste(potdensnames, collapse="\",\""), "\"")
                }
                potdensind <<- which(potdensnames %in% varname_nc)
                if (verbose > 0 && length(potdensind) > 1) {
                    message(indent, "Note: you provided more than 1 potential density data in 'varname_nc':",
                            paste0(varname_nc[potdensind], collapse=","), ".")
                }   
                potdensind <<- which(varname_nc == potdensnames[potdensind[1]])
                if (verbose > 0) {
                    message(indent, "   --> use nc variable '", varname_nc[potdensind], "' for calculation ...")
                }

            # need to calculate potential density from temp and salt 
            } else { 

                if (verbose > 0) {
                    message(indent, "   --> did not find potential density in `varname_nc` ",
                            "based on known potential density variable names\n",
                            indent, "      `potdensnames` = \"", paste(potdensnames, collapse="\",\""), "\"\n",
                            indent, "   --> find temperature and salinity variables ...")
                }
                
                # temp and salt indices were not already found in first case above
                if (is.null(tempind) && is.null(saltind)) { 
                    if (!any(c("temp", "thetao", "tso") %in% varname_nc) &&
                        !any(c("salt", "so", "sos") %in% varname_nc)) {
                        stop("Provide temperature (i.e. 'temp', 'thetao', or 'tso') and ",
                             "salinity (i.e. 'salt','so' or 'sos') in 'varname_nc' ",
                             "to calculate andy kind of density.")
                    }
                    tempind <<- which(tempnames %in% varname_nc)
                    saltind <<- which(saltnames %in% varname_nc)
                    if (verbose > 0 && length(tempind) > 1) {
                        message(indent, "Note: you provided more than 1 temperature data in 'varname_nc':",
                                paste0(varname_nc[tempind], collapse=","), ".")
                    }
                    if (verbose > 0 && length(saltind) > 1) {
                        message(indent, "Note: you provided more than 1 salinity data in 'varname_nc':",
                                paste0(varname_nc[saltind], collapse=","), ".")
                    }   
                    tempind <<- which(varname_nc == tempnames[tempind[1]])
                    saltind <<- which(varname_nc == saltnames[saltind[1]])
                    if (verbose > 0) {
                        message(indent, "   --> use nc variables '", varname_nc[tempind], "','", 
                                varname_nc[saltind], "' for potential density calculation ...")
                    }
                } # if temp and salt indices were not already found in first case above

            } # if potential density variable is directly available in fesom output or not

        } # if potdens_tag || buoyancy_potdens_tag || buoyancy_frequency_potdens_tag

        # check if both temp and salt have same dimensions
        if (length(data_node[tempind,,,]) != length(data_node[saltind,,,])) {
            stop("provide temperature and salinity with same dimensions, i.e.\n",
                 "either both 2D ('tso' and 'sos') or both 3D ('temp' and 'salt')\n",
                 "(old naming convention) or 'thetao' and 'so' (new nameing convention).")
        }

        ## If 3d, calculate pressure from height (75-term equation)
        ## gsw_p_from_z(z, latitude)
        ##  z           height, zero at surface and positive upwards [ m ]
        ##  latitude    latitude in decimal degrees, positive to the north of the equator.
        if (!exists("p_node")) { # only once
            if (potdens_tag || buoyancy_potdens_tag || buoyancy_frequency_potdens_tag) {
                if (!is.finite(p_ref)) {
                    stop("cannot calculate potential density referenced to given non-finite \"p_ref\" = ", p_ref, ".")
                }
                p_ref_node <<- array(p_ref, dim=c(1, dim(data_node)[2:3], 1)) # nvar=1, nnode, ndepth, nrec=1
                p_ref_suffix <<- paste0("_p_ref_", p_ref, "_dbar") # put this to output file name
            }
            if (dim_tag == "3D") {
                if (verbose > 0) {
                    message(paste0(indent, "Calc pressure p = gsw::gsw_p_from_z(z, latitude) ..."))
                    if (verbose > 1) {
                        message(indent, "   with z        height, zero at surface and positive upwards [ m ]\n",
                                indent, "        latitude latitude in decimal degrees, positive to the north of the equator.\n",
                                indent, "   http://www.teos-10.org/pubs/gsw/html/gsw_p_from_z.html")
                    }
                }
                p_node <<- gsw::gsw_p_from_z(z=nod_z, latitude=nod_y)
            } else if (dim_tag == "2D") {
                p_node <<- rep(0, t=nod2d_n)
            }
            p_node <- array(p_node, dim=c(1, dim(data_node)[2:3], 1)) # nvar=1, nnode, ndepth, nrec=1
        
            if (verbose > 2) {
                message(indent, "   min / max 'p_node' = ", 
                        paste(range(p_node), collapse=" / "))
            }
        
        } # if !exists("p_node")

        ## Calculate Absolute Salinity from Practical Salinity, pressure, longitude, and latitude.
        ## gsw_SA_from_SP(SP, p, longitude, latitude)
        ##  SP          Practical Salinity (PSS-78) [ unitless ]
        ##  p           sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar
        ##  longitude   longitude in decimal degrees, positive to the east of Greenwich.
        ##  latitude    latitude in decimal degrees, positive to the north of the equator.
        if (verbose > 0) {
            message(indent, "Calc absolute salinity SA = gsw::gsw_SA_from_SP(SP, p, longitude, latitude)\n",
                    indent, "   with SP        Practical Salinity (PSS-78) [ unitless ]\n",
                    indent, "        p         sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar\n",
                    indent, "        longitude longitude in decimal degrees, positive to the east of Greenwich.\n",
                    indent, "        latitude  latitude in decimal degrees, positive to the north of the equator.\n",
                    indent, "        http://www.teos-10.org/pubs/gsw/html/gsw_SA_from_SP.html")
        }
        # note: gsw::* functions do not keep the matrix dimensions!!!
        SA_node <<- array(NA, dim=dim(data_node[saltind,,,]),
                          dimnames=dimnames(data_node[saltind,,,]))
        dimnames(SA_node)[[1]] <<- "SA"
        for (i in 1:dim(data_node)[4]) { # for nrecspf
            SA_node[,,,i] <<- gsw::gsw_SA_from_SP(SP=data_node[saltind,,,i], 
                                                 p=p_node,
                                                 longitude=nod_x, latitude=nod_y)
        }
        
        if (verbose > 2) {
            message(indent, "   min / max 'SA_node' = ", 
                    paste(range(SA_node), collapse=" / "))
        }
        
        ## Calculate Conservative Temperature from Potential Temperature
        ## gsw_CT_from_pt(SA, pt)
        ##  SA  Absolute Salinity [ g/kg ]
        ##  pt  potential temperature (ITS-90) [ degC ]
        if (verbose > 0) {
            message(indent, "Calc Conservative Temperature CT = gsw::gsw_CT_from_pt(SA, pt)\n",
                    indent, "   with SA Absolute Salinity [ g/kg ]\n",
                    indent, "        pt potential temperature (ITS-90) [ degC ]\n",
                    indent, "        http://www.teos-10.org/pubs/gsw/html/gsw_CT_from_pt.html")
        }
        # note: gsw::* functions do not keep the matrix dimensions!!!
        CT_node <<- array(NA, dim=dim(data_node[tempind,,,]),
                          dimnames=dimnames(data_node[tempind,,,]))
        dimnames(CT_node)[[1]] <<- "CT"
        for (i in 1:dim(data_node)[4]) { # for nrecspf
            CT_node[,,,i] <<- gsw::gsw_CT_from_pt(SA=SA_node[,,,i], 
                                                 pt=data_node[tempind,,,i])
        }

        if (verbose > 2) {
            message(indent, "   min / max 'CT_node' = ", 
                    paste(range(CT_node), collapse=" / "))
        }
        
        ## calc density
        ## gsw_rho(SA, CT, p)
        ##  SA  Absolute Salinity [ g/kg ]
        ##  CT  Conservative Temperature [ degC ]
        ##  p   sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar

        # calc in-situ density if needed and not directly available in fesom output
        if (insitudens_tag || buoyancy_insitudens_tag || buoyancy_frequency_insitudens_tag) {
            
            if (is.null(insitudensind)) {

                if (verbose > 0) {
                    message(indent, "Calc in-situ density = gsw::gsw_rho(SA, CT, p)\n",
                            indent, "   with SA Absolute Salinity [ g/kg ]\n",
                            indent, "        CT Conservative Temperature [degC]\n",
                            indent, "        p  sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar\n",
                            indent, "        http://www.teos-10.org/pubs/gsw/html/gsw_rho.html")
                }
                # note: gsw::* functions do not keep the matrix dimensions!!!
                insitudens_node <<- array(NA, dim=dim(data_node[1,,,]),
                                          dimnames=dimnames(data_node[tempind,,,]))
                dimnames(insitudens_node)[[1]] <<- "insitudens"
                for (i in 1:dim(data_node)[4]) { # for nrecspf
                    insitudens_node[,,,i] <<- gsw::gsw_rho(SA=SA_node[,,,i],
                                                          CT=CT_node[,,,i],
                                                          p=p_node)
                }
                
                if (verbose > 2) {
                    message(indent, "   min / max 'insitudens_node' = ", 
                            paste(range(insitudens_node), collapse=" / "))
                }
                
                # add insitudens to data
                if (insitudens_tag) { 
                    data_node <<- abind(data_node, insitudens_node, along=1, use.dnns=T)
                }
            
            } else if (!is.null(insitudensind)) {
            
                insitudens_node <- data_node[insitudensind,,,]
            
            } # if insitudens was already loaded or not

        } # if (insitudens_tag || buoyancy_insitudens_tag || buoyancy_frequency_insitudens_tag) && is.null(insitudensind) 

        # calc potential density if needed and not directly available in fesom output
        if (potdens_tag || buoyancy_potdens_tag || buoyancy_frequency_potdens_tag) {

            if (is.null(potdensind)) {

                if (verbose > 0) {
                    message(indent, "Calc potential density = gsw::gsw_rho(SA, CT, p)\n",
                            indent, "   with SA Absolute Salinity [ g/kg ]\n",
                            indent, "        CT Conservative Temperature [degC]\n",
                            indent, "        p = ", p_ref, " dbar (='p_ref')\n",
                            indent, "        http://www.teos-10.org/pubs/gsw/html/gsw_rho.html")
                }
                
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
                    
                if (verbose > 2) {
                    message(indent, "   min / max 'potdens_node' = ", 
                            paste(range(potdens_node), collapse=" / "))
                }

                # add potdens to data
                if (potdens_tag) { 
                    data_node <<- abind(data_node, potdens_node, along=1, use.dnns=T)
                }

            } else if (!is.null(potdensind)) {

                potdens_node <- data_node[potdensind,,,]

            } # if potdens was already loaded or not

        } # if (potdens_tag || buoyancy_potdens_tag || buoyancy_frequency_potdens_tag) && is.null(potdensind)

        ## from here, in-situ (`insitudens_node`) and/or potential (`potdens_node`) density are available
        
        # use buoyancy instead of density
        if (buoyancy_insitudens_tag) { 
            if (verbose > 0) {
                message(indent, "`buoyancy_insitudens_tag`=T --> buoyancy_insitu = -g/rho0*insitudens = -", g, "/", rho0, "*insitudens")
            }
            buoyancy_insitudens_node <<- -g/rho0*insitudens_node
            dimnames(buoyancy_insitudens_node)[[1]] <<- "buoyancy_insitudens"
        
            # add buoyancy_insitudens to data
            data_node <<- abind(data_node, buoyancy_insitudens_node, along=1, use.dnns=T)
        }
        
        if (buoyancy_potdens_tag) { 
            if (verbose > 0) {
                message(indent, "`buoyancy_potdens_tag`=T --> buoyancy_potdens = -g/rho0*potdens = -", g, "/", rho0, "*potdens")
            }
            buoyancy_potdens_node <<- -g/rho0*potdens_node
            dimnames(buoyancy_potdens_node)[[1]] <<- "buoyancy_potdens"
            
            # add buoyancy_potdens to data
            data_node <<- abind(data_node, buoyancy_potdens_node, along=1, use.dnns=T)
        }

        if (buoyancy_frequency_insitudens_tag) {

            if (F) {
                # use gsw::gsw_Nsquared --> seems not to work maybe due to irregular dz?
                ## The result is computed based on first-differencing a computed density with respect pressure, and
                ## this can yield noisy results with CTD data_node that have not been smoothed and decimated. It also yields
                ## infinite values, for repeated adjacent pressure (e.g. this occurs twice with the ctd dataset provided
                ## in the oce package)
                # www.teos-10.org/pubs/gsw/html/gsw_Nsquared.html
                if (verbose > 1) {
                    message(indent, "N2 =  gsw_Nsquared(SA, CT, p, latitude)\n",
                            indent, "   with SA       Absolute Salinity [g/kg]\n",
                            indent, "        CT       Conservative Temperature [degC]\n",
                            indent, "        p        sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar\n",
                            indent, "        latitude latitude in decimal degrees, positive to the north of the equator\n",
                            indent, "        www.teos-10.org/pubs/gsw/html/gsw_Nsquared.html")
                }
                # note: gsw::* functions do not keep the matrix dimensions!!!
                N2_node <<- array(NA, dim=dim(data_node[1,,,]),
                                  dimnames=dimnames(data_node[tempind,,,]))
                dimnames(N2_node)[[1]] <<- "N2"
                for (i in 1:dim(data_node)[4]) { # for nrecspf
                    tmp <<- gsw::gsw_Nsquared(SA=SA_node[,,,i],
                                             CT=CT_node[,,,i],
                                             p=p_node, latitude=nod_y)
                    # how to treat Inf and -Inf values?
                    tmp$N2[tmp$N2 == Inf] <<- NA
                    tmp$N2[tmp$N2 == -Inf] <<- NA
                    # replace 1st depth value
                    tmp$N2 <<- c(NA, tmp$N2)
                    N2_node[,,,i] <<- tmp$N2 # $p_mid
                }
            } # gsw_Nsquared
            
            # vertical derivative
            if (verbose > 0) {
                message(indent, "`buoyancy_frequency_insitudens_tag`=T --> calc vertical derivative of insitudens ...")
            }
            pb <<- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                   char=pb_char, width=pb_width,
                                   indent=paste0(indent, "   ")) 
            drhodz_insitu <<- insitudens_node
            drhodz_insitu[] <<- 0
            for (i in 1:(aux3d_n-1)) {
                nodes_up <<- aux3d[i,]
                nodes_low <<- aux3d[i+1,]
                inds <<- nodes_up > 0 & nodes_low > 0
                if (any(!is.na(inds))) {
                    dz <<- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                    drhodz_insitu[,nodes_up[inds],,] <<- (insitudens_node[,nodes_up[inds],,] -
                                                         insitudens_node[,nodes_low[inds],,])/dz
                }
                # update progress bar
                setTxtProgressBar(pb, i)
            } # for i aux3d_n-1
            # close progress bar
            close(pb)

            if (verbose > 2) {
                message(indent, "   min / max 'drhodz_insitu' = ", 
                        paste(range(drhodz_insitu), collapse=" / "))
            }

            if (verbose > 0) {
                message(indent, "N2_insitu = -g/rho0*dz(insitudens) = -", g, "/", rho0, "*dz(insitudens)")
            }
            #N2_insitudens_node <<- -g/insitudens_node*drhodz_insitu
            N2_insitudens_node <<- -g/rho0*drhodz_insitu
            dimnames(N2_insitudens_node)[[1]] <<- "N2_insitudens"
            
            if (verbose > 2) {
                message(indent, "   min / max 'N2_insitudens_node' = ", 
                        paste(range(N2_insitudens_node), collapse=" / "))
            }
            
            # add N2_insitudens to data
            data_node <<- abind(data_node, N2_insitudens_node, along=1, use.dnns=T)
        
        } # buoyancy_frequency_insitu
        
        if (buoyancy_frequency_potdens_tag) {
            
            # vertical derivative
            if (verbose > 0) {
                message(indent, "`buoyancy_frequency_potdens_tag`=T --> calc vertical derivative of potdens ...")
            }
            pb <<- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                   char=pb_char, width=pb_width,
                                   indent=paste0(indent, "   "))
            drhodz_potdens <<- potdens_node
            drhodz_potdens[] <<- 0
            for (i in 1:(aux3d_n-1)) {
                nodes_up <<- aux3d[i,]
                nodes_low <<- aux3d[i+1,]
                inds <<- nodes_up > 0 & nodes_low > 0
                if (any(!is.na(inds))) {
                    dz <<- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                    drhodz_potdens[,nodes_up[inds],,] <<- (potdens_node[,nodes_up[inds],,] -
                                                          potdens_node[,nodes_low[inds],,])/dz
                }
                # update progress bar
                setTxtProgressBar(pb, i)
            } # for i aux3d_n-1
            # close progress bar
            close(pb)
            
            if (verbose > 2) {
                message(indent, "   min / max 'drhodz_potdens' = ", 
                        paste(range(drhodz_potdens), collapse=" / "))
            }

            if (verbose > 0) {
                message(indent, "N2_potdens = -g/rho0*dz(potdens) = -", g, "/", rho0, "*dz(potdens)")
            }
            #N2_potdens_node <<- -g/potdens_node*drhodz_potdens
            N2_potdens_node <<- -g/rho0*drhodz_potdens
            dimnames(N2_potdens_node)[[1]] <<- "N2_potdens"
            
            if (verbose > 2) {
                message(indent, "   min / max 'N2_potdens_node' = ", 
                        paste(range(N2_potdens_node), collapse=" / "))
            }
            
            # add N2_potdens to data
            data_node <<- abind(data_node, N2_potdens_node, along=1, use.dnns=T)
            
        } # if buoyancy_frequency_potdens_tag
                    
        #rm(insitudens_node, envir=.GlobalEnv)

    } # if insitudens_tag || buoyancy_insitudens_tag || buoyancy_frequency_insitudens_tag ||
      #    potdens_tag || buoyancy_potdens_tag || buoyancy_frequency_potdens_tag 
       

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

