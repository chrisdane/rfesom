## R

sub_e2xde_to_n2xde <- function(data_elem2d) {

    ## input: dim(data_elem2d) = c(nvars,1,elem2d_n,ndepths,nrecspf)
    ## output: dim(tmp) = c(nvars,nod2d_n,ndepths,nrecspf)
    
    success <<- load_package("abind")
    if (!success) stop()

    tmp <<- array(0, 
                  dim=c(dim(data_elem2d)[1], nod2d_n, 1, dim(data_elem2d)[4:5]),
                  dimnames=c(dimnames(data_elem2d)[1], 
                             list(node=NULL, elem=NULL),
                             dimnames(data_elem2d)[4:5]))
    inds <<- tmp

    # create progress bar
    pb <<- mytxtProgressBar(min=0, max=elem2d_n, style=pb_style,
                            char=pb_char, width=pb_width,
                            indent=paste0("     ", indent)) # 5 " " for default message()

    ## put 2D-element value on 3 nodes
    for (i in 1:elem2d_n) {

        #progress_function(elem2d_n, i, indent=paste0(indent, "      "))
        elnodes <<- elem2d[,i]
        aux <<- data_elem2d[,,i,,] # 2D-element value of ith elem; dim=c(nvars,node=1,elem=1,ndepths,nrecspf)
        if (is.null(dim(drop(aux)))) { # only 1 var, 1 depth, 1 rec
            aux <- array(drop(aux), c(1, 1, 3, 1, 1))
        } else {
            aux <<- replicate(aux, n=3) # repeat 2D-element value for all 3 nodes; dim=c(nvars,elem=1,node=1,ndepths,nrecspf,3)
            aux <<- adrop(aux, drop=2) # drop element placeholder dimension; dim=c(nvars,elem=i,ndepths=1,nrecspf=12,nodes=3)
            aux <<- aperm(aux, c(1, 5, 2, 3, 4))  # reorder dimensions; dim=c(nvars,nodes=3,elem=i,ndepths,nrecs)
        }
        #message(str(aux))

        tmp[,elnodes,,,] <- aux
        inds[,elnodes,,,] <<- inds[,elnodes,,,] + 1

        # update progress bar
        setTxtProgressBar(pb, i)

    } # for i elem2d

    # close progress bar
    close(pb)
    
    tmp <<- tmp/inds

    ## remove placeholder elem dimension
    tmp <<- adrop(tmp, drop=3)

} # end sub_elem2d_to_nod2d

