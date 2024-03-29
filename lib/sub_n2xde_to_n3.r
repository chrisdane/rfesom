## R

sub_n2xde_to_n3 <- function(data_n2xnd) {

    ## input: dim(data_n2xnd) = c(nvars,nod2d_n,ndepths,nrecspf)
    ## output: dim(tmp) = c(nvars,nod3d_n,ndepths=1,nrecspf)

    tmp <<- array(NA,
                  dim=c(dim(data_n2xnd)[1],       # nvars 
                        nod3d_n, 1,               # nod3d_n, ndepths=1
                        dim(data_n2xnd)[4]),      # nrecspf
                  dimnames=c(dimnames(data_n2xnd)[1],
                             list(node=NULL),     # dont name node dim, saves memory
                             list(depth=depths_plot),
                             dimnames(data_n2xnd)[4]))

    # note: at this point interpolation between user and model depths levels
    #       was already done

    if (F) {
        for (i in 1:ndepths) {

            inds <<- which(aux3d[i,] > 0) # not the -999 nodes
            if (T) {
                message("lhs") 
                message(range(aux3d[i,inds]))
                message("rhs")
                message(range(inds))
            }
            if (length(inds) > 0) {
                tmp[,aux3d[i,inds],,] <<- data_n2xnd[,inds,i,]
            }
        
        } # for i ndephts
    
    } else if (T) {
            
        `[` <<- fctbackup # restore to R's default for loop
        
        for (i in 1:ndepths) { 

            z <<- interpolate_depths[i]

            if (verbose > 1) {
                if (ndepths > 1) {
                    if (i == 1) {
                        message(indent, appendLF=F)
                    } 
                    message(z, "m ", appendLF=F)
                    if (i == ndepths) {
                        message("") 
                    }
                } else if (ndepths == 1) {
                    message(indent, z, "m")
                }
            }

            inds <<- !is.na(indsurf[i,]) # only where are values

            if (any(fesom_depths == z)) {

                if (any(out_mode == c("csec_mean", "csec_depth"))) {
                    stop("asd")
                } else {
                    if (any(inds)) {
                        #message(i)
                        #message(paste0("data_n2xnd: ", min(data_n2xnd[,indsurf[i,inds],i,], na.rm=T), "/", max(data_n2xnd[,indsurf[i,inds],i,], na.rm=T)))
                    }
                    tmp[,indlevel[i,inds],,] <<- data_n2xnd[,pos[indsurf[i,inds]],i,]
                    if (any(inds)) {
                        #message(paste0("tmp: ", min(tmp[,indlevel[i,inds],,], na.rm=T), "/", max(tmp[,indlevel[i,inds],,], na.rm=T)))
                    }
                }

            # interpolate user level between model levels
            } else {

                if (verbose > 1) {
                    # message(paste0(indent, "   x[low] = (x[z] - c[up] - c*x[up]) / c ..."))
                    message(indent, "   x[", fesom_depths[i], "m] = (x[", z, "m] - x[", 
                            fesom_depths[i-1], "m] + c*x[", fesom_depths[i-1], "m]) / c...")
                }
                
                # replicate interpolation coefficients
                indcoef_tmp <<- indcoef[i,inds,drop=F] # c(1,ninds)
                indcoef_tmp <<- replicate(indcoef_tmp, n=dim(data_n2xnd)[1]) # nvars
                indcoef_tmp <<- replicate(indcoef_tmp, n=dim(data_n2xnd)[4]) # nrecspf
                indcoef_tmp <<- aperm(indcoef_tmp, c(3, 2, 1, 4))

                if (any(out_mode == c("csec_mean", "csec_depth"))) {
                    stop("asd")
                    tmp[,inds,i,] <<- data_nod3d[,indupper[i,inds],,] +
                                                 indcoef[i,inds]*
                                                    (data_nod3d[,indlower[i,inds],,] -
                                                     data_nod3d[,indupper[i,inds],,])
                } else {
                    tmp[,indlevel[i,inds],,] <<- data_n2xnd[,indsurf[i,inds],i,] +
                                                                 #indcoef[i,inds]*
                                                                 indcoef_tmp*
                                                                    (data_n2xnd[,indlower[i,inds],i,] -
                                                                     data_n2xnd[,indupper[i,inds],i,])
                }

                stop("asd")

            } # if user level needs to be interpolated or just rearranged from nod3d_n to ndepths x nod2d_n

        } # for i ndepths
            
        fctbackup <<- `[`; `[` <- function(...) { fctbackup(..., drop=F) }
 
    } # if T/F

} # end sub_n2xde_to_n3
