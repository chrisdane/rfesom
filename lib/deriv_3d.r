## R-function for calculatind the derivative ...
deriv_3d_function <- function(elem3d, nod_x, nod_y, nod_z,
                              meshid, mv, deriv_3d_fname) {
    
    library(ncdf4)
    if (!is.finite(Rearth)) { # overwrite possibly wrong user input
        Rearth <- 6367.5 * 10^3
        mesh_dist_unit <- "m"
    }

    rad <- pi/180
    domain_len <- 360*rad
    elem3d_n <- dim(elem3d)[2]
    bafux_3d <- array(0, c(4, elem3d_n))
    bafuy_3d <- bafux_3d
    bafuz_3d <- bafux_3d
    voltetra <- inds_el2 <- rep(0, t=elem3d_n)
    cluster_vol_3d <- rep(0, t=length(nod_x))

    if (F) {
        derivative_stdbafu_x_3D <- array(0, c(3,4))
        for (j in 1:4) {
            for (i in 1:3) {
                if (j ==1) derivative_stdbafu_x_3D[i,j] <- -1
                if (i == j-1) derivative_stdbafu_x_3D[i,j] <- 1
            }
        }
    } else if (T) {
        derivative_stdbafu_x_3D <- cbind(rep(-1, t=3), diag(3))
    }

    pb <- mytxtProgressBar(min=0, max=elem3d_n, style=pb_style,
                           char=pb_char, width=pb_width,
                           indent=paste0("   ", indent)) # 5 " " for default message()

    for (i in seq_len(elem3d_n)) {

        #if (i %% 1e5 == 0) message(i, "/", elem3d_n)
                                   
        # cartesian coordinates on elements
        # oce_mesh_setup.F90:local_element_def_3D()
        node <- elem3d[,i] # 1:nod3
        local_cart <- array(NA, c(3, 4))
        local_cart[1,] <- nod_x[node]*rad
        local_cart[2,] <- nod_y[node]*rad*Rearth
        local_cart[3,] <- nod_z[node]

        # transformation matrix Xl(k) = jacobian(j,k)*Xs(i)
        jacobian3D <- array(0, c(3, 3))
        for (j in seq_len(3)) { 
            jacobian3D[,j] <- local_cart[,j+1] - local_cart[,1]
            # check cyclic boundary
            if (jacobian3D[1,j] > domain_len/2.0) { 
                jacobian3D[1,j] <- jacobian3D[1,j] - domain_len
            }
            if (jacobian3D[1,j] < -domain_len/2.0) {
                jacobian3D[1,j] <- jacobian3D[1,j] + domain_len
            }
        }

        # scale
        # oce_mesh_setup.F90:mesh_scaling():
        # do i=1, myDim_elem2D  
        #    cos_elem2D(i)=sum(cos(coord_nod2D(2,elem2D_nodes(:,i))))/3.0
        # end do
        # oce_mesh_setup.F90:local_element_def_2D():
        # meancos=cos_elem2D(element)
        # oce_mesh_setup.F90:local_element_def_3D():
        # meancos=cos_elem2D(elem2D_corresp_to_elem3D(element))
        if (F) { # wrong but almost no difference
            meancos <- 1/4*sum(cos(nod_y[node]*rad))
        } else if (F) { # mimic elem2D_corresp_to_elem3D; too slow
            inds_nod2 <- base::match(node, aux3d) # aux3d=1:nod3; depth x nod2
            inds_nod2 <- base::arrayInd(inds_nod2, dim(aux3d)) # depth x nod2
            dupl_ind <- which(duplicated(inds_nod2[,2])) # same node over two depths = wanted 2d element
            if (length(dupl_ind) != 1) stop("should not happen")
            ind_el2 <- inds_nod2[dupl_ind,2]
            inds_el2[i] <- ind_el2
            meancos <- 1/3*sum(cos(ycsur[elem2d[,ind_el2]]*rad)) # elem2d=1:nod2
        } else if (T) {
            # fast version: compare latitudes directly
            # problem: numerical precision; following pairs should be equal:
            # 1
            # 2642323: -59.3684763249
            # 2725278: -59.368476325
            # 2
            # 2642322: -59.1204916575
            # 2725277: -59.120491658
            # 3
            # 2714568: -61.6841834335
            # 2796425: -61.684183433
            # but those pairs must differ:
            # 1
            # 2576413: 80.9964291248
            # 2635027: 80.9963944595
            #if (F) {
            #    ii <- seq(0.99*elem3d_n, elem3d_n)
            #    for (i in ii) {
            #        node <- elem3d[,i]
                    dupl_ind <- which(duplicated(nod3d_y[node]))
                    if (length(dupl_ind) != 1) { 
                        dupl_ind <- which(duplicated(round(nod3d_y[node], 11)))
                        if (length(dupl_ind) != 1) { 
                            dupl_ind <- which(duplicated(round(nod3d_y[node], 10)))
                            if (length(dupl_ind) != 1) { 
                                dupl_ind <- which(duplicated(round(nod3d_y[node], 9)))
                                if (length(dupl_ind) != 1) { 
                                    dupl_ind <- which(duplicated(round(nod3d_y[node], 8)))
                                    if (length(dupl_ind) != 1) { 
                                        dupl_ind <- which(duplicated(round(nod3d_y[node], 7)))
                                        if (length(dupl_ind) != 1) { 
                                            dupl_ind <- which(duplicated(round(nod3d_y[node], 6)))
                                            if (length(dupl_ind) != 1) { 
                                                dupl_ind <- which(duplicated(round(nod3d_y[node], 5)))
                                                if (length(dupl_ind) != 1) { 
                                                    print(cbind(node, nod3d_y[node]), 12)
                                                    stop("should not happen")
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
            #    } # for i 
            #}
            meancos <- 1/3*sum(cos(nod3d_y[node][-dupl_ind]*rad))
        }
        jacobian3D[1,] <- jacobian3D[1,] * meancos * Rearth

        # inverse jacobian
        jacobian3D_inv <- solve(jacobian3D)

        # derivaive of local basis function
        derivative_locbafu_x_3D <- t(t(derivative_stdbafu_x_3D) %*% jacobian3D_inv)
        bafux_3d[,i] <- derivative_locbafu_x_3D[1,]
        bafuy_3d[,i] <- derivative_locbafu_x_3D[2,]
        bafuz_3d[,i] <- derivative_locbafu_x_3D[3,]

        # mesh volume in unit of `Rearth`^3
        # earth global sum = 1.386e9 km3 = 1.386e18 m3
        voltetra[i] <- 1/6*abs(det(jacobian3D)) # elem-space
        cluster_vol_3d[node] <- cluster_vol_3d[node] + 1/4*voltetra[i] # node-space
                
        # update progress bar
        setTxtProgressBar(pb, i)

    } # for i elem3d_n
    
    # close progress bar
    close(pb)

    ## output
    node_per_prism_dim <- ncdf4::ncdim_def("nodes_per_prism", "", 1:4, create_dimvar=F)
    node_dim <- ncdf4::ncdim_def("nod3d", "", seq_along(nod_x), create_dimvar=F)
    elem_dim <- ncdf4::ncdim_def("elem3d", "", seq_along(voltetra), create_dimvar=F)

    bafux_3d_var <- ncdf4::ncvar_def("bafux_3d", paste0(mesh_dist_unit, "-1"), list(node_per_prism_dim, elem_dim), mv)
    bafuy_3d_var <- ncdf4::ncvar_def("bafuy_3d", paste0(mesh_dist_unit, "-1"), list(node_per_prism_dim, elem_dim), mv)
    bafuz_3d_var <- ncdf4::ncvar_def("bafuz_3d", paste0(mesh_dist_unit, "-1"), list(node_per_prism_dim, elem_dim), mv)
    voltetra_var <- ncdf4::ncvar_def("voltetra", paste0(mesh_dist_unit, "3"), list(elem_dim), mv)
    cluster_vol_3d_var <- ncdf4::ncvar_def("cluster_vol_3d", paste0(mesh_dist_unit, "3"), node_dim, mv)

    nc <- ncdf4::nc_create(deriv_3d_fname,
                           list(bafux_3d_var, bafuy_3d_var, bafuz_3d_var, 
                                voltetra_var, cluster_vol_3d_var),
                           force_v4=force_v4)
    ncdf4::ncvar_put(nc, bafux_3d_var, bafux_3d)
    ncdf4::ncvar_put(nc, bafuy_3d_var, bafuy_3d)
    ncdf4::ncvar_put(nc, bafuz_3d_var, bafuz_3d)
    ncdf4::ncvar_put(nc, voltetra_var, voltetra)
    ncdf4::ncvar_put(nc, cluster_vol_3d_var, cluster_vol_3d)
    ncdf4::nc_close(nc)

    return(list(#bafux_3d=bafux_3d, 
                #bafuy_3d=bafuy_3d, 
                #bafuz_3d=bafuz_3d,
                voltetra=voltetra,
                cluster_vol_3d=cluster_vol_3d
                , inds_el2=inds_el2
                ))

} # deriv_3d_function 

