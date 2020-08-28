## R

sub_vertical_average <- function(data_vert) {

    ## input: dim(data_vert) = c(nvars,nod2d_n,ndepths,nrecspf)
    ## output: 
    ##  if (zave_method == 1): dim(tmp) = c(nvars,nod2d_n,ndepths=1,nrecspf)
    ##  if (zave_method == 2): dim(tmp) = c(nvars,nod_n=1,ndepths=1,nrecspf) # special !
    
    if (ndepths == 1) {

        # possible interpolation between depth layers was already done before
        # nothing to do
        tmp <<- data_vert

    } else if (ndepths > 1) {

        ## Which method for averaging over depth levels?
        # 1 = for i all depths: data[inds_2d] <<- data[inds_2d] + data_vert[inds_2d]*deltaz[i]
        # 2 = sum(data[inds_3d]*cluster_vol_3d[inds_3d])
        if (zave_method == 1) {

            if (verbose > 2) {
                message(paste0(indent, "   using zave_method=1: dz ..."))
            }

            tmp <<- array(0, dim=c(dim(data_vert)[1:2], 1, dim(data_vert)[4]))
            dimnames(tmp)[c(1, 2, 4)] <<- dimnames(data_vert)[c(1, 2, 4)]
            dimnames(tmp)[[3]] <<- paste0(depths_plot, "m")
            dep_total <<- tmp
           
            # vertical average
            for (i in 1:ndepths) {

                if (verbose > 1) {
                    z <- interpolate_depths[i]
                    if (i == 1) message(indent, "   ", appendLF=F)
                    message(z, "m ", appendLF=F)
                    if (i == ndepths) message("") 
                }

                aux <<- data_vert[,,i,] # c(nvars,nod2d_n,ndepths,nrecspf)
                inds <<- !is.na(aux)
                tmp[inds] <<- tmp[inds] + aux[inds]*deltaz[i]
                dep_total[inds] <<- dep_total[inds] + deltaz[i]

            } # i ndepths

            tmp <<- tmp/dep_total
            tmp[is.nan(tmp)] <<- NA
            rm(dep_total, envir = .GlobalEnv)

            if (F) { # special # soga = 34.72394 psu
                patch_area <<- replicate(cluster_area_2d, n=dim(tmp)[4])
                area_sum <<- apply(tmp[1,,1,] * patch_area, 2, sum)
                area_mean <<- area_sum/sum(cluster_area_2d) # 34.59856 psu (0.12538 psu fresher)
                stop("asd")
            } # 

        } else if (zave_method == 2) {

            if (verbose > 2) {
                message(paste0(indent, "   using zave_method=2: cluster_vol_3d ..."))
            }

            tmp <<- array(NA,
                          dim=dim(data_vert),
                          dimnames=dimnames(data_vert))

            if (!exists("patch_vol")) { # only once
                patch_vol <<- cluster_vol_3d # dim = nod3d_n
                patch_vol <<- replicate(patch_vol, n=dim(data_node)[3]) # ndepths = 1
                patch_vol <<- replicate(patch_vol, n=dim(data_node)[4]) # nrecspf
                patch_vol <<- replicate(patch_vol, n=dim(data_node)[1]) # nvars
                patch_vol <<- aperm(patch_vol, c(4, 1, 2, 3)) # dim(patch_vol) = c(nvars,nod3d_n,ndepths=1,nrecspf)
                nod3d_z_inds <<- which(abs(nod_z) >= interpolate_depths[1] &
                                      abs(nod_z) <= interpolate_depths[ndepths])
                # total volume
                volo <<- sum(cluster_vol_3d[nod3d_z_inds])
            }

            # data*volume
            tmp[,nod3d_z_inds,,] <<- data_node[,nod3d_z_inds,,]*patch_vol[,nod3d_z_inds,,]
            
            # sum and keep 1st (nvars), 3rd (ndepths=1) and 4th (nrecspf) dimensions
            tmp <<- apply(tmp, c(1, 3, 4), sum)
            
            # divide through total volume
            tmp <<- tmp/volo 

        } # which zave_method

    } # if ndepths > 1

} # end sub_vertical_average
                
