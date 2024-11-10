## R-function for calculatind the derivative ...
deriv_2d_function <- function(elem2d, xcsur, ycsur,
                              meshid, mv, deriv_2d_fname) {

    library(ncdf4)
    # all mesh resolutions/areas get their unit from this variable:
    if (!is.finite(Rearth)) { # overwrite possibly wrong user input
        Rearth <- 6367.5 * 1e3
        mesh_dist_unit <- "m"
    }

    rad <- pi/180
    domain_len <- 360*rad
    elem2d_n <- dim(elem2d)[2]
    bafux_2d <- array(0, c(3, elem2d_n))
    bafuy_2d <- bafux_2d
    voltriangle <- rep(0, t=elem2d_n) # m2
    resolution <- voltriangle # m
    cluster_area_2d <- rep(0, t=length(xcsur)) # m2

    derivative_stdbafu_x_2D <- array(0, c(2, 3))
    derivative_stdbafu_x_2D[,1] <- -1
    derivative_stdbafu_x_2D[1,2] <- 1
    derivative_stdbafu_x_2D[2,3] <- 1

    pb <- mytxtProgressBar(min=0, max=elem2d_n, style=pb_style,
                           char=pb_char, width=pb_width,
                           indent=paste0("   ", indent)) # 5 " " for default message()

    # for every 2d element
    for (i in seq_len(elem2d_n)) {
        
        #if (i %% 1e5 == 0) message(paste0(i, "/", elem2d_n))

        # cartesion coordinates on elements
        node <- elem2d[,i]
        local_cart <- array(NA, c(2, 3))
        local_cart[1,] <- xcsur[node]*rad
        local_cart[2,] <- ycsur[node]*rad*Rearth

        # transformation matrix Xl(k) = jacobian(j,k)*Xs(i)
        jacobian2D <- array(0, c(2, 2))
        for (j in seq_len(2)) { 
            jacobian2D[,j] <- local_cart[,j+1] - local_cart[,1]
            # check cyclic boundary
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
        derivative_locbafu_x_2D <- t(t(derivative_stdbafu_x_2D) %*% jacobian2D_inv)
        bafux_2d[,i] <- derivative_locbafu_x_2D[1,]
        bafuy_2d[,i] <- derivative_locbafu_x_2D[2,]

        # area in unit of `Rearth`^2
        # earth global sum = 361e6 km2 = 361e12 m2 = 3.61e14 m2
        voltriangle[i] <- 1/2*abs(det(jacobian2D)) # elem-space
        cluster_area_2d[node] <- cluster_area_2d[node] + 1/3*voltriangle[i] # node-space

        # resolution based on area in unit of `Rearth`
        # fesom1:
        # from oce_rhs_tra.F90:
        #   res2=voltriangle(elem2)*1.73e-6 # factor 1.73 = sqrt(3); factor 1e-6 from km2 --> m2
        #   res=sqrt(res2)  !in km 
        #   --> res=sqrt(sqrt(3)*vol)
        # from cwekerle: 
        #   h(i)=sqrt(voltriangle(i)*sqrt(3))/1000; 
        # from pscholz:
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
        # fesom2:
        # from oce_mesh.F90:
        #   do n=1,myDim_nod2d+eDim_nod2D
        #     mesh%mesh_resolution(n)=sqrt(mesh%areasvol(mesh%ulevels_nod2d(n),n)/pi)*2._WP
        #   --> res_node = 2*sqrt(areavol/pi)
        # from tripyview:
        #   self.n_resol = np.sqrt(self.n_area[0,:]/np.pi)*2.0
        #   --> res_node = 2*sqrt(area_node/pi) 
        #   self.e_resol = jacobian.mean(axis=1)
        #   --> res_elem = mean_over_vertices_of_element(sqrt(dx^2+dy^2))
        # from sein et al. 2017:
        #   "The mesh resolution, defined as the square root of twice the area of the triangles ..."
        #   --> res=sqrt(2*vol)
        # from danilov 2022:
        #   "A rather good estimate is provided by the square root of the area of unit
        #    cell (twice the triangle area or area of the dual cell) which is only 9% coarser than the real resolution."
        #   --> res=sqrt(area_node)
        #   --> res=sqrt(2*area_elem)
        
        #resolution[i] <- sqrt(voltriangle[i]*sqrt(3)) # sqrt(3)=1.73
        resolution[i] <- sqrt(2*voltriangle[i])
        
        # update progress bar
        setTxtProgressBar(pb, i)

    } # end i in 1:elem2d_n

    # close progress bar
    close(pb)

    ## output
    message("todo: save elem2d")
    node_per_elem_dim <- ncdf4::ncdim_def("nodes_per_element", "", 1:3, create_dimvar=F)
    node_dim <- ncdf4::ncdim_def("nod2d", "", seq_along(xcsur), create_dimvar=F)
    elem_dim <- ncdf4::ncdim_def("elem2d", "", seq_along(resolution), create_dimvar=F)

    bafux_2d_var <- ncdf4::ncvar_def("bafux_2d", paste0(mesh_dist_unit, "-1"), list(node_per_elem_dim, elem_dim), mv)
    bafuy_2d_var <- ncdf4::ncvar_def("bafuy_2d", paste0(mesh_dist_unit, "-1"), list(node_per_elem_dim, elem_dim), mv)
    voltriangle_var <- ncdf4::ncvar_def("voltriangle", paste0(mesh_dist_unit, 2), list(elem_dim), mv)
    cluster_area_2d_var <- ncdf4::ncvar_def("cluster_area_2d", paste0(mesh_dist_unit, 2), node_dim, mv)
    resolution_var <- ncdf4::ncvar_def("resolution", mesh_dist_unit, elem_dim, mv)

    nc <- ncdf4::nc_create(deriv_2d_fname,
                    list(bafux_2d_var, bafuy_2d_var, voltriangle_var,
                         cluster_area_2d_var, resolution_var),
                    force_v4=force_v4)
    ncdf4::ncvar_put(nc, bafux_2d_var, bafux_2d)
    ncdf4::ncvar_put(nc, bafuy_2d_var, bafuy_2d)
    ncdf4::ncvar_put(nc, voltriangle_var, voltriangle)
    ncdf4::ncvar_put(nc, cluster_area_2d_var, cluster_area_2d)
    ncdf4::ncvar_put(nc, resolution_var, resolution)
    ncdf4::nc_close(nc)

    #return(list(bafux_2d=bafux_2d, 
    #            bafuy_2d=bafuy_2d, 
    #            voltriangle=voltriangle,
    #            cluster_area_2d=cluster_area_2d,
    #            resolution=resolution))

} # deriv_2d_function

