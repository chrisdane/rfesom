## R

## special preparation if necessary

sub_prepare1 <- function(data_nod3d) {

    # if ocean only and sgs_u or sgs_v
    if (!cpl_tag && any(!is.na(match(c("sgs_u", "sgs_v"), varname_fesom)))) {

        ## sgs_u and sgs_v are saved as vertical integral
        ## --> replace data with vertical derivative
        sgsinds <<- match(c("sgs_u", "sgs_v"), varname_fesom)
        sgsinds <<- sgsinds[!is.na(sgsinds)]
        if (length(sgsinds) == 0 || any(is.na(sgsinds))) {
            stop("something strange here")
        }

        if (verbose > 0) {
            print(paste0(indent, "Calc vertical derivative of '",
                         paste0(varname_fesom[sgsinds], collapse="','"), 
                         "' because they are saved 'vertically integrated from bottom' ..."))
        }

        tmp <<- data_node
        tmp[sgsinds,,,] <<- 0 # for vertical derivative

        # vertical derivative (new and old yield the same result)
        if (F) { # old
            # create progress bar
            pb <<- mytxtProgressBar(min=0, max=nod2d_n, style=pb_style,
                                    char=pb_char, width=pb_width,
                                    indent=paste0("     ", indent)) # 5 " " for default print()
            for (i in 1:nod2d_n) {
                for (j in 1:(aux3d_n-1)) {
                    if (aux3d[j,i] > 0 && aux3d[j+1,i] > 0) {
                        node_up <<- aux3d[j,i]
                        node_low <<- aux3d[j+1,i]
                        dz <<- nod3d_z[node_up] - nod3d_z[node_low]
                        tmp[sgsinds,aux3d[j,i],,] <<- (data_nod3d[sgsinds,node_up,,] -
                                                       data_nod3d[sgsinds,node_low,,])/dz
                    } # 
                } # j
                # update progress bar
                setTxtProgressBar(pb, i)
            } # i
        
        } else if (T) { # new
            # create progress bar
            pb <<- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                    char=pb_char, width=pb_width,
                                    indent=paste0("     ", indent)) # 5 " " for default print()
            for (i in 1:(aux3d_n-1)) {
                nodes_up <<- aux3d[i,]
                nodes_low <<- aux3d[i+1,]
                inds <<- nodes_up > 0 & nodes_low > 0
                if (any(!is.na(inds))) {
                    dz <<- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                    #print(dz)
                    tmp[sgsinds,nodes_up[inds],,] <<- (data_nod3d[sgsinds,nodes_up[inds],,] -
                                                       data_nod3d[sgsinds,nodes_low[inds],,])/dz
                }

                # update progress bar
                setTxtProgressBar(pb, i)

            } # for i aux3d_n-1
        } # F

        # close progress bar
        close(pb)

        data_node <<- tmp # replace data with vertical derivative

    } # if ocean only and SGS variable

} # sub_prepare1 function
