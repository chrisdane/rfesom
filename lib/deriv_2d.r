## R-function for calculatind the derivative ...
deriv_2d_function <- function(elem2d, xcsur, ycsur,
                              meshid, mv, deriv_2d_fname) {

# all mesh resolutions/areas get their unit from this variable:
if (!is.finite("Rearth")) { # overwrite possibly wrong user input
    mesh_dist_unit <- "m"
    Rearth <- 6367.5 * 10^3
}

rad <- pi/180
domain_len <- 360*rad
elem2d_n <- dim(elem2d)[2]
bafux_2d <- array(0, c(3, elem2d_n))
bafuy_2d <- bafux_2d
voltriangle <- rep(0, t=elem2d_n) # m2
resolution <- voltriangle # m
cluster_area_2d <- rep(0, t=length(xcsur)) # m2

derivative_stdbafu_x_2D <- array(0, c(2,3))
derivative_stdbafu_x_2D[,1]= -1
derivative_stdbafu_x_2D[1,2]= 1
derivative_stdbafu_x_2D[2,3]= 1

for (i in 1:elem2d_n) {
    if (i %% 1e5 == 0) message(paste0(i, "/", elem2d_n))

    # cartesion coordinates on elements
    node <- elem2d[,i]
    local_cart <- array(NA, c(2, 3))
    local_cart[1,] <- xcsur[node]*rad
    local_cart[2,] <- ycsur[node]*rad*Rearth

    # transformation matrix Xl(k) = jacobian(j,k)*Xs(i)
    # old:
    #jacobian2D <- array(0, c(2,3))
    #jacobian2D <- local_cart[,2:3] - cbind(local_cart[,1], local_cart[,1])
    # new:
    jacobian2D <- array(0, c(2, 2))

    # check cyclic boundary
    for (j in 1:2) {
        jacobian2D[,j] <- local_cart[,j+1] - local_cart[,1]
        if (jacobian2D[1,j] > domain_len/2.0) {
            jacobian2D[1,j] <- jacobian2D[1,j] - domain_len
        }
        if (jacobian2D[1,j] < -domain_len/2.0) {
            jacobian2D[1,j] <- jacobian2D[1,j] + domain_len
        }
    }

    # scale
    # old:
    #jacobian2D <- jacobian2D*Rearth
    #jacobian2D[1,] <- jacobian2D[1,]*mean(cos(local_cart[2,]))
    # new:
    meancos <- 1/3*sum(cos(ycsur[node]*rad))
    jacobian2D[1,] <- jacobian2D[1,] * meancos * Rearth

    # inverse jacobian
    jacobian2D_inv <- solve(jacobian2D)

    # derivaive of local basis function
    derivative_locbafu_x_2D <- t(t(derivative_stdbafu_x_2D)%*%jacobian2D_inv)
    bafux_2d[,i] <- derivative_locbafu_x_2D[1,]
    bafuy_2d[,i] <- derivative_locbafu_x_2D[2,]

    voltriangle[i] <- 1/2*abs(det(jacobian2D)) # m2

    # mesh area in (unit of 'Rearth')^2
    cluster_area_2d[node] <- cluster_area_2d[node] + 1/3*voltriangle[i] # m2

    # mesh resolution in unit of 'Rearth'
    # from sein et al. 2017:
    #   "The mesh resolution, defined as the square root of twice the area of the triangles ..."
    # from oce_rhs_tra.F90:
    #   res2=voltriangle(elem2)*1.73e-6
    #   res=sqrt(res2)  !in km
    # from c. wekerle: 
    #   h(i)=sqrt(voltriangle(i)*sqrt(3))/1000;
    # from p. scholz:
    #   x_kart=Rearth*1000.*cosd(aux_yc).*cosd(aux_xc);
    #   y_kart=Rearth*1000.*cosd(aux_yc).*sind(aux_xc);
    #   z_kart=Rearth*1000.*sind(aux_yc);
    #   vek1=[x_kart(2,:)-x_kart(1,:);y_kart(2,:)-y_kart(1,:);z_kart(2,:)-z_kart(1,:)];
    #   vek2=[x_kart(1,:)-x_kart(3,:);y_kart(1,:)-y_kart(3,:);z_kart(1,:)-z_kart(3,:)];
    #   vek3=[x_kart(3,:)-x_kart(2,:);y_kart(3,:)-y_kart(2,:);z_kart(3,:)-z_kart(2,:)];
    #   %_______________________________________________________________
    #   %Calculate Area of triangles over vectorproduct: A=1/2*|axb|
    #   resol=[sqrt(vek1(1,:).^2 + vek1(2,:).^2 + vek1(3,:).^2);...
    #          sqrt(vek2(1,:).^2 + vek2(2,:).^2 + vek2(3,:).^2);...  
    #          sqrt(vek3(1,:).^2 + vek3(2,:).^2 + vek3(3,:).^2)];
    resolution[i] <- sqrt(voltriangle[i]*sqrt(3)) #sqrt(3)=1.73

} # end i in 1:elem2d_n

## output
ncdf4 <- T
if (ncdf4) { 
    library(ncdf4)
    node_per_elem_dim <- ncdim_def(paste0("nodes_per_element"), "", 1:3,
                                      create_dimvar=F)
    node_dim <- ncdim_def(paste0("mesh_", meshid, "_n2Dnod"), "",
                             1:length(xcsur), create_dimvar=F)
    elem_dim <- ncdim_def(paste0("mesh_", meshid, "_n2Delem"), "",
                             1:dim(bafux_2d)[2], create_dimvar=F)

    bafux_2d_var <- ncvar_def("bafux_2d", paste0(mesh_dist_unit, "-1"), 
                                 list(node_per_elem_dim, elem_dim), mv)
    bafuy_2d_var <- ncvar_def("bafuy_2d", paste0(mesh_dist_unit, "-1"),
                                 list(node_per_elem_dim, elem_dim), mv)
    voltriangle_var <- ncvar_def("voltriangle", paste0(mesh_dist_unit, 2),
                                 list(elem_dim), mv)
    cluster_area_2d_var <- ncvar_def("cluster_area_2d", paste0(mesh_dist_unit, 2), node_dim, mv)
    resolution_var <- ncvar_def("resolution", mesh_dist_unit, elem_dim, mv)

    nc <- nc_create(deriv_2d_fname,
                    list(bafux_2d_var, bafuy_2d_var, voltriangle_var,
                         cluster_area_2d_var, resolution_var),
                    force_v4=force_v4)
    ncvar_put(nc, bafux_2d_var, bafux_2d)
    ncvar_put(nc, bafuy_2d_var, bafuy_2d)
    ncvar_put(nc, voltriangle_var, voltriangle)
    ncvar_put(nc, cluster_area_2d_var, cluster_area_2d)
    ncvar_put(nc, resolution_var, resolution)
    nc_close(nc)
}

#return(list(bafux_2d=bafux_2d, 
#            bafuy_2d=bafuy_2d, 
#            voltriangle=voltriangle,
#            cluster_area_2d=cluster_area_2d,
#            resolution=resolution))
}
