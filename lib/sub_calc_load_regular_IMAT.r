sub_calc_load_regular_IMAT <- function(regular_dx, regular_dy, 
                                       xp, yp, global_mesh,
                                       interppath, interpfname,
                                       mv) {
    
    ## R-function for calculating IMAT
    success <- load_package("splancs")
    if (!success) stop(helppage)
    success <- load_package("ncdf4")
    if (!success) stop(helppage)
   
    # how to define the regular grid correctly?
    # https://github.com/chrisdane/rfesom/issues/1
    # -> workaround-solution: treat all global meshes equally from -180,180 and -90,90
    if (global_mesh) {
        xlim <- c(-180, 180)
        ylim <- c(-90, 90)
    } else {
        xlim <- range(xp)
        ylim <- range(yp)
    }
    x_len <- diff(xlim)
    y_len <- diff(ylim)
    xi = seq(xlim[1]+regular_dx/2, xlim[2]-regular_dx/2, l=x_len/regular_dx)
    yi = seq(ylim[1]+regular_dy/2, ylim[2]-regular_dy/2, l=y_len/regular_dy)
    #xi = seq(xlim[1]+regular_dx/2, xlim[2]-regular_dx/2, b=regular_dx)
    #yi = seq(ylim[1]+regular_dy/2, ylim[2]-regular_dy/2, b=regular_dy)
    nxi <- length(xi)
    nyi <- length(yi)
    message("   --> regular longitudes will have ", nxi, " points with dlon = ", diff(xi)[1])
    ht(xi)
    message("   --> regular latitudes will have ", nyi, " points with dlat = ", diff(yi)[1])
    ht(yi)
    
    # save interpolation weights
    XI <- array(rep(xi, e=nyi), c(nyi,nxi))
    YI <- array(rep(yi, t=nxi), c(nyi,nxi))
    IMAT <- array(0, c(nyi, nxi))
    
    pb <- mytxtProgressBar(min=0, max=dim(xp)[2], style=pb_style,
                           char=pb_char, width=pb_width,
                           indent=paste0("   ", indent)) # 5 " " for default message()

    # for every element
    for (ii in 1:(dim(xp)[2])) {
        
        #if (ii %% 1e5 == 0) message(paste0(ii, "/", dim(xp)[2]))

        xmin   = min(xp[,ii]) 
        xmax   = max(xp[,ii]) 
        ymin   = min(yp[,ii])
        ymax   = max(yp[,ii]) 
        
        kk_xs  = which(xi <= xmin)[length(which(xi <= xmin))] #find(xi <= xmin,1,'last') 
        if (is.na(kk_xs) || length(kk_xs) == 0) kk_xs=1 #if (length(kk_xs) == 0) kk_xs=1
        kk_ys  = which(yi <= ymin)[length(which(yi <= ymin))] 
        if (is.na(kk_ys) || length(kk_ys) == 0) kk_ys=1 #if (length(kk_ys) == 0) kk_ys=1
        
        kk_xe  = which(xi >= xmax)[1] #find(xi>=xmax,1,'first')
        if (is.na(kk_xe) || length(kk_xe) == 0) kk_xe= length(xi) #if (length(kk_xe) == 0) kk_xe=length(xi)
        kk_ye  = which(yi >= ymax)[1] 
        if (is.na(kk_ye) || length(kk_ye) == 0) kk_ye= length(yi) #if (length(kk_ye) == 0) kk_ye=length(yi)

        AUX_XI = XI[kk_ys:kk_ye,kk_xs:kk_xe]
        AUX_YI = YI[kk_ys:kk_ye,kk_xs:kk_xe]
    
        IN  = splancs::inpip(cbind(as.vector(AUX_XI), as.vector(AUX_YI)), 
                             cbind(xp[,ii], yp[,ii]))
                    
        kk_x   = array(rep(kk_xs:kk_xe, e=kk_ye-kk_ys+1), 
                       c(kk_ye-kk_ys+1, length(kk_xs:kk_xe)))
        kk_y   = array(rep(kk_ys:kk_ye, t=kk_xe-kk_xs+1),
                       c(length(kk_ys:kk_ye), kk_xe-kk_xs+1))
        
        IMAT[kk_y[IN],kk_x[IN]] = ii    
    
        # update progress bar
        setTxtProgressBar(pb, ii)

    } # for element ii
    
    # close progress bar
    close(pb)
    
    #return(list(IMAT=IMAT, XI=XI, YI=YI, xi=xi, yi=yi, regular_dx=regular_dx, regular_dy=regular_dy)) 
    
    ## nc Output
    xi_dim <- ncdim_def("nxi", "", xi, create_dimvar=T)
    yi_dim <- ncdim_def("nyi", "", yi, create_dimvar=T)
    
    xi_var <- ncvar_def("xi", "degrees_east", xi_dim, mv, prec="double")
    yi_var <- ncvar_def("yi", "degrees_north", yi_dim, mv, prec="double")
    XI_var <- ncvar_def("XI", "degrees_east", list(yi_dim, xi_dim), mv, prec="double")
    YI_var <- ncvar_def("YI", "degrees_north", list(yi_dim, xi_dim), mv, prec="double")
    IMAT_var <- ncvar_def("IMAT", "interp_value", list(yi_dim, xi_dim), mv, prec="double")
    
    imatnc <- nc_create(paste0(interppath, "/", interpfname), 
                        list(xi_var, yi_var, XI_var, YI_var, IMAT_var), 
                        force_v4=force_v4)
    
    ncvar_put(imatnc, xi_var, xi)
    ncvar_put(imatnc, yi_var, yi)
    ncvar_put(imatnc, XI_var, XI)
    ncvar_put(imatnc, YI_var, YI)
    ncvar_put(imatnc, IMAT_var, IMAT)
    ncatt_put(imatnc, 0, "regular_dx", sprintf("%.3f", regular_dx))
    ncatt_put(imatnc, 0, "regular_dy", sprintf("%.3f", regular_dy))

    nc_close(imatnc)

} # end function
