## R

sub_n3_to_n2xde <- function(data_nod3d) {

    ## input: dim(data_nod3d) = c(nvars,nod3d_n,ndepths=1,nrecspf)
    ## output: dim(tmp) = c(nvars,nod2d_n,ndepths,nrecspf)

    tmp <<- array(NA, 
                  dim=c(dim(data_nod3d)[1], # nfiles 
                        nod2d_n, ndepths, # nod2d_n (was overwritten in case of cross section), ndepths
                        dim(data_nod3d)[4]), # nrecspf
                  dimnames=c(dimnames(data_nod3d)[1],
                             list(node=NULL, # dont name node dim, saves memory
                                  depth=interpolate_depths), 
                             dimnames(data_nod3d)[4]))

    # if cross section, name all crossed nodes (needs more memory)
    if (F) { # for bedugging
        if (any(out_mode == c("csec_mean", "csec_depth"))) {
            dimnames(tmp)[2] <<- list(node=which(csec_crossed_nodes == 1))
        }
    }

    ## Rearrange to chosen depth levels (variable 'depths')
    if (ndepths == 1 && depths == "bottom") {
        if (verbose > 1) message(indent, "   bottom")
        inds <<- !is.na(indsurf[1,])
        tmp[,pos[indsurf[1,inds]],1,] <<- data_nod3d[,indbottom[1,inds],,]

    } else {
        
        for (i in 1:ndepths) { # ndepths may equal 1 here

            z <<- interpolate_depths[i]

            if (verbose > 1) {
                if (ndepths > 1) {
                    if (i == 1) {
                        message(indent, "   ", appendLF=F)
                    } 
                    message(z, "m ", appendLF=F)
                    if (i == ndepths) {
                        message("") 
                    }
                } else if (ndepths == 1) {
                    message(indent, "   ", z, "m")
                }
            }

            inds <<- !is.na(indsurf[i,]) # only where are values
         
            # just rearrange from nod3d_n to ndepths x nod2d_n
            if (any(fesom_depths == z)) {
                
                if (any(out_mode == c("csec_mean", "csec_depth"))) {
                    tmp[,inds,i,] <<- data_nod3d[,indlevel[i,inds],,]
                } else {
                    tmp[,pos[indsurf[i,inds]],i,] <<- data_nod3d[,indlevel[i,inds],,]
                }
                
            # interpolate user level between model levels
            } else {

                if (verbose > 1) {
                    message(paste0(indent, "   x[", z, "m] = x[", fesom_depths[i-1], "m] + c*(x[", 
                                 fesom_depths[i], "m] - x[", fesom_depths[i-1], "m]) ..."))
                }
                
                # replicate interpolation coefficients
                indcoef_tmp <<- indcoef[i,inds] # c(1,ninds)
                indcoef_tmp <<- replicate(indcoef_tmp, n=dim(data_node)[1]) # nvars
                indcoef_tmp <<- replicate(indcoef_tmp, n=dim(data_node)[4]) # nrecspf
                indcoef_tmp <<- aperm(indcoef_tmp, c(3, 2, 1, 4))

                if (any(out_mode == c("csec_mean", "csec_depth"))) {
                    tmp[,inds,i,] <<- data_nod3d[,indupper[i,inds],,] +
                                                 #indcoef[i,inds]*
                                                 indcoef_tmp*
                                                    (data_nod3d[,indlower[i,inds],,] -
                                                     data_nod3d[,indupper[i,inds],,])
                } else {
                    tmp[,pos[indsurf[i,inds]],i,] <<- data_nod3d[,indupper[i,inds],,] +
                                                                 #indcoef[i,inds]*
                                                                 indcoef_tmp*
                                                                    (data_nod3d[,indlower[i,inds],,] - 
                                                                     data_nod3d[,indupper[i,inds],,])
                }
                
            } # if user level needs to be interpolated or just rearranged from nod3d_n to ndepths x nod2d_n

        } # for i ndepths

    } # if bottom or not

} # end sub_n3_to_n2xde           
