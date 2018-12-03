## R

sub_vertical_integral <- function(data_nod3) {

    ## input: dim(data_nod3) = c(nvars,nod3d_n,ndepths=1,nrecspf)
    ## output: dim(tmp) = c(nvars,nod2d_n,ndepths=1,nrecspf)

    tmp <<- array(0, 
                  dim=c(dim(data_nod3)[1], nod2d_n, dim(data_nod3)[3:4]),
                  dimnames=c(dimnames(data_nod3)[1],
                             list(node=NULL),
                             dimnames(data_nod3)[3:4]))
    dep_total <<- tmp[1,,,]

    # create progress bar
    pb <<- txtProgressBar(min=0, max=nod2d_n, style=pb_style, 
                          char=pb_char, width=pb_width)
    
    for (i in 1:nod2d_n) {

        #progress_function(nod2d_n, i, indent=paste0(indent, "      "))

        #for (j in 1:(aux3d_n-1)) {
        for (j in 1:length(deltaz)) {

            if (aux3d[j,i] > 0) { # not the -999 nodes (missing values)

                aux <<- data_nod3[,aux3d[j,i],,] # c(nvars,nnod=1,ndepths=1,nrecspf) 

                # if integrate between specific depths
                if (length(depths) == 2 && depths[2] != "max") {

                    if (depths[2] == "MLD") {
                        # dim(mld_node) c(nvars=1,nod2d_n,ndephts=1,nrecspf)
                        z_inds <<- which(abs(nod3d_z[aux3d[j,i]]) <= mld_node[,i,,]) 
                        # length(z_inds) = nrecspf
                        
                    } else {
                        z_inds <<- which(abs(nod3d_z[aux3d[j,i]]) <= rep(depths[2], t=dim(aux)[4])) # repeat in time

                    }
                    #print(z_inds) 

                    # only some depths within MLD at some timepoints per year
                    if (rec_tag && length(z_inds) > 0 && length(z_inds) != dim(aux)[4]) {

                        # set values at depths greater than MLD to zero
                        aux[,,,recs[-z_inds]] <<- 0

                    # all depths deeper than MLD at all timepoints
                    } else if (length(z_inds) == 0) {
                        aux[,,,] <<- 0

                    # all depths within MLD at all timepoints per year
                    } else if ((rec_tag && length(z_inds) == dim(aux)[4]) ||
                               (!rec_tag && length(z_inds) == 1)) {
                        #print(paste0(paste0(z_inds, collapse=","),
                        #             ": all depths within MLD..."))
                        # nothing to do. j aux as it is
                    }

                } # if integrate between specific depths

                tmp[,i,,] <<- tmp[,i,,] + aux*deltaz[j]
                dep_total[,i,,] <<- dep_total[,i,,] + deltaz[j]

            } # if not -999

        } # for j depths

        # update progress bar
        setTxtProgressBar(pb, i)

    } # for i 2d nodes

    # close progress bar
    close(pb)

    if (F) {
        print("special /dep_total")
        tmp <<- tmp/dep_total
        stop("asd")
    }

} # end sub_vertical_integral
