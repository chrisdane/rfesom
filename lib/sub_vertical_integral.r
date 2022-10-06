## R

sub_vertical_integral <- function(data_nod3) {

    ## input: dim(data_nod3) = c(nvars,nod3d_n,ndepths=1,nrecspf)
    ## output: dim(tmp) = c(nvars,nod2d_n,ndepths=1,nrecspf)

    #stop("update dim_tag (and zave?)")

    tmp <<- array(0, 
                  dim=c(dim(data_nod3)[1], nod2d_n, dim(data_nod3)[3:4]),
                  dimnames=c(dimnames(data_nod3)[1],
                             list(node=NULL),
                             dimnames(data_nod3)[3:4]))
    #dep_total <<- tmp[1,,,]

    # create progress bar
    pb <<- mytxtProgressBar(min=0, max=ndepths, style=pb_style,
                            char=pb_char, width=pb_width, 
                            indent=paste0("     ", indent)) # 5 " " for default message()
    #timei <<- rep(NA, t=ndepths)

    if (F) {
        for (i in 1:ndepths) {
            inds <-  which(aux3d[i,] != -999)
            message(length(which(is.na(N_node[1,drop(aux3d[i,inds]),,]))))
        }
    }

    for (i in seq_len(ndepths)) {

        ptmi <<- proc.time()[3]

        if (any(aux3d[i,] > 0)) { # not the -999 nodes (missing values)

            inds <<- which(aux3d[i,] != -999) # vector; dim=NULL

            if (F) {
                for (i in 1:ndepths) {
                    inds <<- which(aux3d[i,] != -999)
                    message(i)
                    message(paste0(range(inds), collapse=","))
                    message(paste0(range(drop(indsurf[i,inds])), collapse=","))
                }
            }

            #message(paste0(i, ",", j))
            #stop("asd")
            aux <<- data_nod3[,drop(aux3d[i,inds]),,] # c(nvars,nnod=1,ndepths=1,nrecspf) 

            # only add non-NA values
            #nonainds <<- !is.na(aux)

            #if (any(nonainds)) {

                # if integrate between specific depths
                if (length(depths) == 2) {
                    
                    z_i <<- array(interpolate_depths[i], dim(aux))

                    if (depths[2] == "MLD") {
                        # dim(mld_node) c(nvars=1,nod2d_n,ndephts=1,nrecspf)
                        z_mld <<- mld_node[,drop(indsurf[i,inds]),,]
                        z_mld <<- adrop(z_mld, drop=1)
                        z_mld <<- replicate(z_mld, n=dim(aux)[1]) # nvars
                        z_mld <<- aperm(z_mld, c(4, 1, 2, 3))
                        z_inds <<- z_i <= z_mld

                    } else {
                        #z_max <<- array(interpolate_depths[ndepths], dim(aux))
                        #z_inds <<- z_i <= z_max # all TRUE
                        z_inds <<- array(T, dim=dim(aux))
                    }
                    #message(str(z_inds)) 

                    # if some depths between levels
                    if (any(z_inds)) {

                        #message(paste0(length(which(is.na(aux*z_inds))), " NA inds ..."))

                        #message(sum(aux*deltaz[i]))
                        #message(sum(z_inds*aux*deltaz[i]))
                        tmp[,drop(indsurf[i,inds]),,] <<- tmp[,drop(indsurf[i,inds]),,] + z_inds*aux*deltaz[i]
                        #dep_total[,drop(indsurf[i,inds]),,] <<- dep_total[,drop(indsurf[i,inds]),,] + z_inds*deltaz[i]

                    }

                # else integrate between all depths
                } else {

                    tmp[,drop(indsurf[i,inds]),,] <<- tmp[,drop(indsurf[i,inds]),,] + aux*deltaz[i]
                    #dep_total[,drop(indsurf[i,inds]),,] <<- dep_total[,drop(indsurf[i,inds]),,] + deltaz[i]

                } # integrate between specific or all depths

            #} # data not all NA

        } # if not -999

        #timei[i] <<- proc.time()[3] - ptmi

        # update progress bar
        setTxtProgressBar(pb, i)

    } # for i depths

    # close progress bar
    close(pb)

    if (F) {
        message("special /dep_total")
        tmp <<- tmp/dep_total
        stop("asd")
    }

    if (F) {
        types <- c(1, 2, 3)
        typenames <- c("som", "non", "all")
        ns <- rep(NA, t=length(types))
        for (i in 1:length(types)) {
            inds <- which(typeij == types[i])
            ns[i] <- length(inds)
        }
        for (i in 1:length(types)) {
            inds <- which(typeij == types[i])
            message(paste0(typenames[i], ": (n=", sprintf(paste0("%", nchar(max(ns)), "i"), ns[i]), ") ", mean(as.vector(timeij)[inds]), " s --> ", sum(as.vector(timeij)[inds]), " total s"))
        }
        message(paste0("total: ", sum(timeij, na.rm=T)))
    } # F
            

} # end sub_vertical_integral
