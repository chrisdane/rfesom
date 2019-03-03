## R-function for calculatind the derivative ...
deriv_3d_function <- function(elem3d, nod_x, nod_y, nod_z,
                              meshid, mv, deriv_3d_fname) {

if (!exists("Rearth")) Rearth <- 6367.5 * 10^3 # [m]

rad <- pi/180
domain_len <- 360*rad
elem3d_n <- dim(elem3d)[2]
bafux_3d <- array(0, c(4, elem3d_n))
bafuy_3d <- bafux_3d
bafuz_3d <- bafux_3d
voltetra <- rep(0, t=elem3d_n)
cluster_vol_3d <- rep(0, t=length(nod_x))

#derivative_stdbafu_x_3D <- array(0, c(3,4))
#for (j in 1:4) {
#    for (i in 1:3) {
#        if (j ==1) derivative_stdbafu_x_3D[i,j] <- -1
#        if (i == j-1) derivative_stdbafu_x_3D[i,j] <- 1
#    }
#}
derivative_stdbafu_x_3D <- cbind(rep(-1, t=3), diag(3))

for (i in 1:elem3d_n) {
    if (i %% 1e5 == 0) message(paste0(i, "/", elem3d_n))
                               
    # cartesion coordinates on elements
    node <- elem3d[,i]
    local_cart <- array(NA, c(3, 4))
    local_cart[1,] <- nod_x[node]*rad
    local_cart[2,] <- nod_y[node]*rad*Rearth
    local_cart[3,] <- nod_z[node]*rad

    # transformation matrix Xl(k) = jacobian(j,k)*Xs(i)
    jacobian3D <- array(0, c(3, 3))

    # check cyclic boundary
    for (j in 1:3) {
        jacobian3D[,j] <- local_cart[,j+1] - local_cart[,1]
        if (jacobian3D[1,j] > domain_len/2.0) {
            jacobian3D[1,j] <- jacobian3D[1,j] - domain_len
        }
        if (jacobian3D[1,j] < -domain_len/2.0) {
            jacobian3D[1,j] <- jacobian3D[1,j] + domain_len
        }
    }

    # scale
    meancos <- 1/4*sum(cos(nod_y[node]*rad))
    jacobian3D[1,] <- jacobian3D[1,] * meancos * Rearth

    # inverse jacobian
    jacobian3D_inv <- solve(jacobian3D)

    # derivaive of local basis function
    derivative_locbafu_x_3D <- t(t(derivative_stdbafu_x_3D)%*%jacobian3D_inv)
    bafux_3d[,i] <- derivative_locbafu_x_3D[1,]
    bafuy_3d[,i] <- derivative_locbafu_x_3D[2,]
    bafuz_3d[,i] <- derivative_locbafu_x_3D[3,]

    voltetra[i] <- 1/6*abs(det(jacobian3D))

    # mesh area in (unit of 'Rearth')^3
    cluster_vol_3d[node] <- cluster_vol_3d[node] + 1/4*voltetra[i]

} # end i in 1:elem3d_n

## output
ncdf4 <- T
if (ncdf4) {
    library(ncdf4)
    node_per_prism_dim <- ncdim_def(paste0("nodes_per_prism"), "", 1:4,
                                   create_dimvar=F)
    node_dim <- ncdim_def(paste0("mesh_", meshid, "_n3Dnod"), "",
                             1:length(nod_x), create_dimvar=F)
    elem_dim <- ncdim_def(paste0("mesh_", meshid, "_n3Delem"), "",
                             1:dim(bafux_3d)[2], create_dimvar=F)

    bafux_3d_var <- ncvar_def("bafux_3d", "m^-1",
                              list(node_per_prism_dim, elem_dim), mv)
    bafuy_3d_var <- ncvar_def("bafuy_3d", "m^-1",
                              list(node_per_prism_dim, elem_dim), mv)
    bafuz_3d_var <- ncvar_def("bafuz_3d", "m^-1",
                              list(node_per_prism_dim, elem_dim), mv)
    voltetra_var <- ncvar_def("voltetra", "#",
                              list(elem_dim), mv)
    cluster_vol_3d_var <- ncvar_def("cluster_vol_3d", "m^3", node_dim, mv)

    nc <- nc_create(deriv_3d_fname,
                    list(bafux_3d_var, bafuy_3d_var, bafuz_3d_var, 
                         voltetra_var, cluster_vol_3d_var),
                    force_v4=force_v4)
    ncvar_put(nc, bafux_3d_var, bafux_3d)
    ncvar_put(nc, bafuy_3d_var, bafuy_3d)
    ncvar_put(nc, bafuz_3d_var, bafuz_3d)
    ncvar_put(nc, voltetra_var, voltetra)
    ncvar_put(nc, cluster_vol_3d_var, cluster_vol_3d)
    nc_close(nc)
}

#return(list(bafux_3d=bafux_3d, 
#            bafuy_3d=bafuy_3d, 
#            bafuz_3d=bafuz_3d,
#            voltetra=voltetra,
#            cluster_vol_3d=cluster_vol_3d))

}
