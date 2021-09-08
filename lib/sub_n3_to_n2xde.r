## R

sub_n3_to_n2xde <- function(data_in) {

    # input: if any(dim_tag == "3D" & !levelwise)
    #           dim(data_in) = c(nvars,nod3d_n,ndepths=1,nrecspf)
    #        else
    #           dim(data_in) = c(nvars,nod2d_n,ndepths,nrecspf)
    # output: dim(tmp) = c(nvars,nod2d_n,ndepths,nrecspf)

    tmp <<- array(NA, 
                  dim=c(dim(data_in)[1], # nvars 
                        nod2d_n, ndepths, # nod2d_n (was overwritten in case of cross section), ndepths
                        dim(data_in)[4]), # nrecspf
                  dimnames=c(dimnames(data_in)[1:2],
                             list(depth=paste0(interpolate_depths, "m")), 
                             dimnames(data_in)[4]))

    # if cross section, name all crossed nodes (needs more memory)
    if (F) { # for bedugging
        if (any(out_mode == c("csec_mean", "csec_depth"))) {
            dimnames(tmp)[2] <<- list(node=which(csec_crossed_nodes == 1))
        }
    }

    ## Rearrange to chosen depth levels (variable 'depths')
    for (vari in seq_len(dim(tmp)[1])) {
        
        if (verbose > 1) message(indent, "   ", varname_nc[vari], ": ", appendLF=F)
        
        if (dim_tag[vari] == "2D") {
            
            if (verbose > 1) message("nothing to do")
            tmp[vari,,1,] <<- data_in[vari,,,] # nothing happens

        } else if (dim_tag[vari] == "3D") {
            
            # if !levelwise, rearrange from (n3) --> (n2,nd) for all needed depths and 
            # apply vertical interpolation coefficients if needed (for both levelwise and !levelwise)
            if (ndepths == 1 && depths == "bottom") {
                if (verbose > 1) message("bottom")
                if (levelwise[vari]) {
                    stop("implement")
                } else if (!levelwise[vari]) {
                    inds <<- !is.na(indsurf[1,])
                    tmp[vari,pos[indsurf[1,inds]],1,] <<- data_in[vari,indbottom[1,inds],,]
                }

            } else {
            
                for (i in seq_len(ndepths)) { # ndepths = 1 if length(depths) = 1, i.e. user wants one specific depth level 

                    z <<- interpolate_depths[i]
                    if (verbose > 1) msg <- paste0(z, "m ")

                    inds <<- !is.na(indsurf[i,]) # only where are values (i.e. where aux3d is not -999)
                 
                    if (any(fesom_depths == z)) { # wanted level equals one of the fesom levels
                        # --> if !levelwise, just rearrange from nod3d_n to ndepths x nod2d_n
                        # --> if levelwise, do nothing
                        
                        if (verbose > 1) message(msg, appendLF=F)

                        if (any(out_mode == c("csec_mean", "csec_depth"))) {
                            if (levelwise[vari]) {
                                stop("implement")
                            } else if (!levelwise[vari]) {
                                tmp[vari,inds,i,] <<- data_in[vari,indlevel[i,inds],,]
                            }
                        } else {
                            if (levelwise[vari]) {
                                tmp[vari,pos[indsurf[i,inds]],i,] <<- data_in[vari,indsurf[i,inds],i,]
                            } else if (!levelwise[vari]) {
                                tmp[vari,pos[indsurf[i,inds]],i,] <<- data_in[vari,indlevel[i,inds],,]
                            }
                        }
                        
                    } else { # wanted level is between some fesom levels
                        # --> apply vertical interpolation coefficients between user and model levels
                        # --> if !levelwise, rearrange from nod3d_n to ndepths x nod2d_n
                        
                        # replicate interpolation coefficients
                        if (F) { # without vari loop
                            indcoef_tmp <<- replicate(indcoef_tmp, n=dim(data_in)[1]) # nvars
                        } else if (T) { # with vari loop
                            indcoef_tmp <<- indcoef[i,inds] # c(1,ninds)
                        }
                        indcoef_tmp <<- replicate(indcoef_tmp, n=1) # nvars
                        indcoef_tmp <<- replicate(indcoef_tmp, n=dim(data_in)[4]) # nrecspf
                        indcoef_tmp <<- aperm(indcoef_tmp, c(3, 2, 1, 4))

                        if (verbose > 1) {
                            msg <- paste0(msg, "= ", 
                                          abs(nod3d_z[indupper[i,inds][which(!is.na(indupper[i,inds]))[1]]]),"m+",
                                          indcoef_tmp[which(!is.na(indcoef_tmp))[1]], 
                                          "*(", abs(nod3d_z[indlower[i,inds][which(!is.na(indlower[i,inds]))[1]]]),
                                          "m-", abs(nod3d_z[indupper[i,inds][which(!is.na(indupper[i,inds]))[1]]]),
                                          "m) ")
                            message(msg, appendLF=F)
                        }

                        if (any(out_mode == c("csec_mean", "csec_depth"))) {
                            if (levelwise[vari]) {
                                stop("implement")
                            } else if (!levelwise[vari]) {
                                tmp[vari,inds,i,] <<- data_in[vari,indupper[i,inds],,] +
                                                             #indcoef[i,inds]*
                                                             indcoef_tmp*
                                                                (data_in[vari,indlower[i,inds],,] -
                                                                 data_in[vari,indupper[i,inds],,])
                            }
                        } else {
                            if (levelwise[vari]) {
                                if (i == 1) {
                                    depthinds_upper_lower <- c(1, 2)
                                } else {
                                    depthinds_upper_lower <- c(i-1, i)
                                }
                                #message("i = ", i, ", depthinds_upper_lower = ", paste(depthinds_upper_lower, collapse=", "))
                                tmp[vari,pos[indsurf[i,inds]],i,] <<- data_in[vari,inds,depthinds_upper_lower[1],] + # upper
                                                                             #indcoef[i,inds]*
                                                                             indcoef_tmp*
                                                                                (data_in[vari,inds,depthinds_upper_lower[2],] - # lower
                                                                                 data_in[vari,inds,depthinds_upper_lower[1],]) # upper
                            } else if (!levelwise[vari]) {
                                tmp[vari,pos[indsurf[i,inds]],i,] <<- data_in[vari,indupper[i,inds],,] +
                                                                             #indcoef[i,inds]*
                                                                             indcoef_tmp*
                                                                                (data_in[vari,indlower[i,inds],,] - 
                                                                                 data_in[vari,indupper[i,inds],,])
                            } # if levelwise or not
                        
                        } # csec or not
                        
                    } # if user level needs to be interpolated or just rearranged from nod3d_n to ndepths x nod2d_n

                } # for i ndepths

            } # if bottom or not
        
        } # if 2D or 3D

        message()

    } # for vari nvars
                    
} # end sub_n3_to_n2xde           
