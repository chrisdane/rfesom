## R

## 

sub_prepare2 <- function(data) {

    ## check user input
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

            dimnames(data_node)[[1]] <<- "insiturho"

            if (potdens_tag) {
                if (verbose > 0) {
                    message("Warning: dont know how to transfer fesoms insitu density to potential density. use in situ instead ...")
                }
            } # if potdens_tag

            if (buoyancy_tag) {
                data_node["rho",,,] <<- -g/rho0*data_node["rho",,,]
                dimnames(data_node)[[1]] <<- "insitub"
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
            tempind <<- which(c("temp", "thetao", "thetaoga", "tso") %in% varname_fesom)
            if (length(tempind) > 1) {
                if (verbose > 0) {
                    message(paste0(indent, "Note: you provided more than 1 temperature data in 'varname_fesom':",
                                 paste0(varname_fesom[tempind], collapse=","), "."))
                }
            }
            tempind <<- tempind[1]
            if (verbose > 0) {
                message(paste0(indent, "Use '", varname_fesom[tempind], "' for density calculation ..."))
            }
            saltind <<- which(c("salt", "so", "soga", "sos") %in% varname_fesom)
            if (length(tempind) > 1) {
                if (verbose > 0) {
                    message(paste0(indent, "Note: you provided more than 1 salinity data in 'varname_fesom':",
                                 paste0(varname_fesom[tempind], collapse=","), "."))
                }
            }   
            tempind <<- tempind[1]
            if (verbose > 0) {
                message(paste0(indent, "Use '", varname_fesom[saltind], "' for density calculation ..."))
            }

            ## check if both temp and salt have same dimensions
            if ((varname_fesom[tempind] == "tso" && varname_fesom[saltind] != "sos") ||
                (varname_fesom[saltind] == "sos" && varname_fesom[tempind] != "tso")) {
                message(paste0("Error: provide temperature and salinity with same dimensions, i.e."))
                message(paste0("either both 1D ('thetaoga' and 'soga') or both 2D ('tso' and 'sos')"))
                message(paste0("or both 3D ('temp' and 'salt' if cpl_tag=F or 'thetao' and 'so' if cpl_tag=T)."))
                stop()
            }
              
            ## which sea water routine?
            if (sea_water != "TEOS10") {
                stop(paste0("error: 'sea_water' must be 'TEOS10' ('EOS80' is outdated)."))
            } else if (sea_water == "TEOS10") {
                success <<- load_package("gsw")
                if (!success) stop()
            }

            ## load abind package
            success <<- load_package("abind")
            if (!success) stop()

            ## Calculate pressure from height (75-term equation)
            ## gsw_p_from_z(z, latitude)
            ##  z           height, zero at surface and positive upwards [ m ]
            ##  latitude    latitude in decimal degrees, positive to the north of the equator.
            ## only once
            if (!exists("z_node")) {
                if (verbose > 0) {
                    message(paste0(indent, "Calc pressure ..."))
                    if (verbose > 1) {
                        message(paste0(indent, "   p = gsw_p_from_z(z, latitude)"))
                        message(paste0(indent, "   with z        height, zero at surface and positive upwards [ m ]"))
                        message(paste0(indent, "        latitude latitude in decimal degrees, positive to the north of the equator."))
                    }
                }
                z_node <<-  
                latitude_node <<- ...
                p_node <<- gsw::gsw_p_from_z(z_node, latitude_node)
            } # if !exists("z_node")

            ## Calculate Absolute Salinity from Practical Salinity, pressure, longitude, and latitude.
            ## gsw_SA_from_SP(SP, p, longitude, latitude)
            ##  SP          Practical Salinity (PSS-78) [ unitless ]
            ##  p           sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar
            ##  longitude   longitude in decimal degrees, positive to the east of Greenwich.
            ##  latitude    latitude in decimal degrees, positive to the north of the equator.
            if (verbose > 0) {
                message(paste0(indent, "Calc absolute salinity ..."))
                if (verbose > 1) {
                    message(paste0(indent, "   SA = gsw_SA_from_SP(SP, p, longitude, latitude)"))
                    message(paste0(indent, "   with SP        Practical Salinity (PSS-78) [ unitless ]"))
                    message(paste0(indent, "        p         sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar"))
                    message(paste0(indent, "        longitude longitude in decimal degrees, positive to the east of Greenwich."))
                    message(paste0(indent, "        latitude  latitude in decimal degrees, positive to the north of the equator."))
                }
            }
            longitude_node <<- ...
            SA_node <<- gsw::gsw_SA_from_SP(data_node[saltind,,,], p_node, longitude_node, latitude_node)
            
            ## Calculate Conservative Temperature from Potential Temperature
            ## gsw_CT_from_pt(SA, pt)
            ##  SA  Absolute Salinity [ g/kg ]
            ##  pt  potential temperature (ITS-90) [ degC ]
            if (verbose > 0) {
                message(paste0(indent, "Calc Conservative Temperature ..."))
                if (verbose > 1) {
                    message(paste0(indent, "   CT = gsw_CT_from_pt(SA, pt)"))
                    message(paste0(indent, "   with SA Absolute Salinity [ g/kg ]"))
                    message(paste0(indent, "        pt potential temperature (ITS-90) [ degC ]"))
                }
            }
            CT_node <<- gsw::gsw_CT_from_pt(SA_node, data_node[tempind,,,])

            if (insitudens_tag) {
                
                ## In-situ density, using the 75-term equation for specific volume.
                ## gsw_rho(SA, CT, p)
                ##  SA  Absolute Salinity [ g/kg ]
                ##  CT  Conservative Temperature [ degC ]
                ##  p   sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar
                if (verbose > 0) {
                    message(paste0(indent, "Calc insitudens ..."))
                    if (verbose > 1) {
                        message(paste0(indent, "   insitudens = gsw::gsw_rho(SA, CT, p)"))
                        message(paste0(indent, "   with SA Absolute Salinity [ g/kg ]"))
                        message(paste0(indent, "        CT Conservative Temperature [ degC ]"))
                        message(paste0(indent, "        p  sea pressure [dbar], i.e. absolute pressure [dbar] minus 10.1325 dbar"))
                    }
                }
                
                insitudens_node <<- gsw::gsw_rho(SA_node, CT_node, p_node)
                dimnames(insitudens_node)[[1]] <<- "insitudens"

                if (buoyancy_tag) {
                    insitudens_node <<- -g/rho0*insitudens_node
                    dimnames(insitudens_node)[[1]] <<- "insitub"
                }

                # add dens to data
                data_node <<- abind(data_node, insitudens_node,
                                    along=1, use.dnns=T)
                rm(insitudens_node)

            } else if (potdens_tag) {

                ## check reference density
                if (!any(p_ref %in% c(0, 1000, 2000, 3000, 4000))) {
                    stop(paste0("error: 'p_ref' must be 0, 1000, 2000, 3000 or 4000 kg m-3"))
                }

                ## This uses the 75-term density equation, and returns potential 
                ## density referenced to a pressure of 'p_ref' dbar, minus 1000 kg/m^3.
                if (verbose > 0) {
                    message(paste0(indent, "Calc potdens ..."))
                    if (verbose > 1) {
                        message(paste0(indent, "   potdens = gsw::sigma", substr(p_ref, 1, 1), "(SA, CT)"))
                        message(paste0(indent, "   with SA Absolute Salinity [ g/kg ]"))
                        message(paste0(indent, "        CT Conservative Temperature [ degC ]"))
                    }
                }
                potdens_node <<- eval(parse(text=paste0("gsw::sigma", substr(p_ref, 1, 1), 
                                                        "(SA_node, CT_node)")))
                dimnames(potdens_node)[[1]] <<- "potdens"

                if (buoyancy_tag) {
                    potdens_node <<- -g/rho0*potdens_node
                    dimnames(potdens_node)[[1]] <<- "potb"
                } # if buoyancy_tag

                # add dens to data
                data_node <<- abind(data_node, potdens_node,
                                    along=1, use.dnns=T)
                rm(potdens_node)

            } # which density

        } # if "rho" %in% varname_fesom

    } #if (insitudens_tag || potdens_tag)


    ## calculate coriolis if needed
    if (coriolis_tag) {

        # only once necessary
        if (!exists("coriolis_nod2d")) {
            # corliolis in node space (default)
            coriolis_node <<- ...
            dimnames(coriolis_node)[[1]] <<- "f"
            data_node <<- abind(data_node, coriolis_node, along=1, use.dnns=T)
        } # if !exists("coriolis_node")

    } # if coriolis_tag

} # sub_prepare2 function

