## R

## special preparation if necessary

sub_prepare1 <- function(data) {

    data_node <<- data

    if (# if ocean only
        !cpl_tag 
        # and a SGS (sub grid scale) variable 
        && any(regexpr("sgs", varname_fesom) != -1)) {

        ## SGS variables are saved as vertical integral
        ## --> replace data with vertical derivative
        sgsinds <<- which(regexpr("sgs", varname_fesom) != -1)
        if (verbose > 0) {
            print(paste0(indent, "Calc vertical derivative of 'varname_fesom'=",
                         paste0(varname_fesom[sgsinds], collapse=","), 
                         " because they are saved 'vertically integrated from bottom' ..."))
        }

        for (i in 1:length(sgsinds)) {
            # vertical derivative
            for (k in 1:(aux3d_n-1)) {
                nodes_up <<- aux3d[k,]
                nodes_low <<- aux3d[k+1,]
                inds <<- nodes_up > 0 & nodes_low > 0
                if (any(!is.na(inds))) {
                    dz <<- nod3d_z[nodes_up[inds]] - nod3d_z[nodes_low[inds]]
                    data_node[sgsinds[i],nodes_up[inds],,] <<- (data_node[sgsinds[i],nodes_low[inds],,] -
                                                                data_node[sgsinds[i],nodes_up[inds],,])/dz
                }
            }
        }

    } # if ocean only and SGS variable

} # sub_prepare1 function
