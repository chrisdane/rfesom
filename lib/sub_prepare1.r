## R

## special preparation if necessary

sub_prepare1 <- function(data_node) {

    # vertical derivative of data saved "vertically integrated from bottom"
    if (!cpl_tag && any(!is.na(match(c("sgs_u", "sgs_v"), varname_nc)))) {

        ## sgs_u and sgs_v are saved as vertical integral
        ## --> replace data with vertical derivative
        sgsinds <<- match(c("sgs_u", "sgs_v"), varname_nc)
        sgsinds <<- sgsinds[!is.na(sgsinds)]
        if (length(sgsinds) == 0 || any(is.na(sgsinds))) {
            stop("something strange here")
        }

        if (verbose > 0) {
            message(paste0(indent, "Calc vertical derivative of '",
                         paste0(varname_nc[sgsinds], collapse="','"), 
                         "' because they are saved 'vertically integrated from bottom' ..."))
        }

        tmp <<- data_node
        tmp[sgsinds,,,] <<- 0 # for vertical derivative

        # vertical derivative (new and old yield the same result)
        # create progress bar
        pb <<- mytxtProgressBar(min=0, max=aux3d_n-1, style=pb_style,
                                char=pb_char, width=pb_width,
                                indent=paste0("     ", indent)) # 5 " " for default message()
        for (i in 1:(aux3d_n-1)) {
            nodes_up <<- aux3d[i,]
            nodes_low <<- aux3d[i+1,]
            inds <<- nodes_up > 0 & nodes_low > 0
            if (any(!is.na(inds))) {
                dz <<- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                #message(dz)
                tmp[sgsinds,nodes_up[inds],,] <<- (data_node[sgsinds,nodes_up[inds],,] -
                                                   data_node[sgsinds,nodes_low[inds],,])/dz
            }

            # update progress bar
            setTxtProgressBar(pb, i)

        } # for i aux3d_n-1

        # close progress bar
        close(pb)

    } # if ocean only and SGS variable

} # sub_prepare1 function
