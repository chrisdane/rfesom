## R

sub_vertical_average <- function(data_vert) {

    ## input: dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
    ## output: 
    ##  if (zave_method == 1): dim(tmp) = c(nvars,nod2d_n,ndepths=1,nrecspf)
    ##  if (zave_method == 2): dim(tmp) = c(nvars,nod_n=1,ndepths=1,nrecspf) # special !
    
    if (ndepths == 1) { # no matter if any(dim_tag == "3D") or not; possible interpolation between depth layers was already done in sub_n3_to_n2xde.r
        tmp <- data_vert

    } else if (ndepths > 1) { # implies that `any(dim_tag == "3D")` is true

        ## Which method for averaging over depth levels?
        # 1 = for i all depths: data[inds_2d] <- data[inds_2d] + data_vert[inds_2d]*deltaz[i]
        # 2 = sum(data[inds_3d]*cluster_vol_3d[inds_3d])
        if (zave_method == 1) {

            tmp <- array(0, dim=c(dim(data_vert)[1:2], 1, dim(data_vert)[4])) # nvar, nod2d_n, ndepth=1, nrecspf
            dimnames(tmp)[c(1, 2, 4)] <- dimnames(data_vert)[c(1, 2, 4)]
            names(dimnames(tmp)) <- names(dimnames(data_vert)) # line before does not keep names of dims, dont know why
            dimnames(tmp)[[3]] <- paste0(depths_plot, "m")
            dep_total <- tmp
           
            for (vari in seq_len(dim(tmp)[1])) { # for all vars; necessary to distinguish between 2D and 3D vars in data
                
                if (verbose > 0) message(indent, "   ", varname_nc[vari], ": ", appendLF=F)
                
                if (dim_tag[vari] == "2D") {
                    if (verbose > 0) message("vertical average not necessary")
                    tmp[vari,,,] <- data_vert[vari,,,]

                } else if (dim_tag[vari] == "3D") {
                    
                    # integrate over irregular depths
                    for (i in seq_len(ndepths)) {
                        if (verbose > 1) {
                            message(interpolate_depths[i], "m ", appendLF=ifelse(i == ndepths, T, F))
                        }
                        if (F) { # without vari loop
                            aux <- data_vert[,,i,] # c(nvars,nod2d_n,ndepths,nrecspf)
                            inds <- !is.na(aux)
                            tmp[inds] <- tmp[inds] + aux[inds]*deltaz[i]
                            dep_total[inds] <- dep_total[inds] + deltaz[i]
                        } else if (T) { # with vari loop
                            aux <- data_vert[vari,,i,] # if nrecspf=1 dim(aux)=nod2d_n else (nod2d_n,nrecspf)
                            inds <- !is.na(aux)
                            tmp[vari,,,][inds] <- tmp[vari,,,][inds] + aux[inds]*deltaz[i]
                            dep_total[vari,,,][inds] <- dep_total[vari,,,][inds] + deltaz[i]
                        }
                    } # for i ndepths
                    
                    # divide through total depth
                    tmp[vari,,,] <- tmp[vari,,,]/dep_total[vari,,,]

                    if (F) { # special # soga = 34.72394 psu
                        patch_area <- replicate(cluster_area_2d, n=dim(tmp)[4])
                        area_sum <- apply(tmp[1,,1,] * patch_area, 2, sum)
                        area_mean <- area_sum/sum(cluster_area_2d) # 34.59856 psu (0.12538 psu fresher)
                        stop("asd")
                    }

                } # if dim_tag[vari] == "2D" or "3D"
            } # for vari nvars
                
            if (verbose > 0) message()
            
            #rm(dep_total, envir = .GlobalEnv) # if exported before
            rm(dep_total)

        } else if (F && zave_method == 2) { # not necessary here; done later in main_fesom.r

            tmp <- array(NA,
                         dim=dim(data_vert),
                         dimnames=dimnames(data_vert))
            
            for (vari in seq_len(dim(tmp)[1])) { # for all vars; necessary to distinguish between 2D and 3D vars in data
                
                if (verbose > 0) message(indent, "   ", varname_nc[vari], ": ", appendLF=F)
                
                if (dim_tag[vari] == "2D") {
                    if (verbose > 0) message("vertical average not necessary")
                    tmp[vari,,,] <- data_vert[vari,,,]

                } else if (dim_tag[vari] == "3D") {

                    if (!exists("patch_vol")) { # only once
                        patch_vol <- cluster_vol_3d # dim = nod3d_n
                        patch_vol <- replicate(patch_vol, n=dim(data_node)[3]) # ndepths = 1
                        patch_vol <- replicate(patch_vol, n=dim(data_node)[4]) # nrecspf
                        patch_vol <- replicate(patch_vol, n=dim(data_node)[1]) # nvars
                        patch_vol <- aperm(patch_vol, c(4, 1, 2, 3)) # dim(patch_vol) = c(nvars,nod3d_n,ndepths=1,nrecspf)
                        nod3d_z_inds <- which(abs(nod3d_z) >= interpolate_depths[1] &
                                              abs(nod3d_z) <= interpolate_depths[ndepths])
                        volo <- sum(cluster_vol_3d[nod3d_z_inds]) # total volume
                    }

                    # data*volume
                    tmp[vari,nod3d_z_inds,,] <- data_node[vari,nod3d_z_inds,,]*patch_vol[vari,nod3d_z_inds,,] # X*vol
                    if (verbose > 0) message("ok")
                
                } # if dim_tag[vari] == "2D" or "3D"
            } # for vari
            
            # sum over 2nd dim (nodes) and keep 1st (nvars), 3rd (ndepths=1) and 4th (nrecspf) dimensions
            tmp <- apply(tmp, c(1, 3, 4), sum) # sum(X*vol)
            
            # divide through total volume
            tmp <- tmp/volo # sum(X*vol)/sum(vol)

        } # which zave_method

    } # if ndepths > 1

    # export
    tmp[is.nan(tmp)] <- NA
    tmp <<- tmp

} # end sub_vertical_average
                
