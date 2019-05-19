sub_calc_regular_2d_interp <- function(I_MAT,XI,YI,
                                       xp,yp,datamat) {
    ## R-function for interpolating FESOM irregular triangle data to
    ## regular long,lat data with interpolation matrix I_MAT derived by
    ## P. Scholz's matlab script OK_SCRIPTS/main_CONVERT_triangular2regular_grid_v2.m

    AUX_I_MAT <- I_MAT
    AUX_I_MAT[I_MAT == 0] <- 1

    if (F) {
        message("XI")
        print(str(XI))
        message("xp")
        print(str(xp))
        message("AUX_I_MAT")
        print(str(AUX_I_MAT))
        message("range(AUX_IMAT)")
        print(range(AUX_I_MAT))
        message("drop(xp[1,AUX_I_MAT])")
        print(str(drop(xp[1,AUX_I_MAT])))
    }
    X <- XI - drop(xp[1,AUX_I_MAT])
    # same: X <- XI - array(xp[1,as.vector(AUX_I_MAT)], dim(AUX_I_MAT))
    Y <- YI - drop(yp[1,AUX_I_MAT])

    #X1 <- xp[2,AUX_I_MAT] - xp[1,AUX_I_MAT]
    X1 <- array(drop(xp[2,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT)) - 
          array(drop(xp[1,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT))
    #Y1 <- yc[2,AUX_I_MAT] - yc[1,AUX_I_MAT]
    Y1 <- array(drop(yp[2,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT)) -
          array(drop(yp[1,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT))

    #X2 <- xc[3,AUX_I_MAT] - xc[1,AUX_I_MAT]
    X2 <- array(drop(xp[3,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT)) -
          array(drop(xp[1,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT))
    #Y2 <- yc[3,AUX_I_MAT] - yc[1,AUX_I_MAT]
    Y2 <- array(drop(yp[3,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT)) -
          array(drop(yp[1,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT))

    D <- (Y2*X1) - (X2*Y1)

    #MAT <- datamat[1,AUX_I_MAT] +
    MAT <- array(drop(datamat[1,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT)) + 
           #[ [datamat[2,AUX_I_MAT] - datamat[1,AUX_I_MAT]] * [Y2*X - X2*Y] / D ] - 
           ((array(drop(datamat[2,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT)) - 
             array(drop(datamat[1,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT))) * (Y2*X - X2*Y) / D) - 
           #[ [datamat[3,AUX_I_MAT] - datamat[1,AUX_I_MAT]] * [Y1*X - X1*Y] / D ]
           ((array(drop(datamat[3,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT)) -
             array(drop(datamat[1,as.vector(AUX_I_MAT)]), dim(AUX_I_MAT))) * (Y1*X - X1*Y) / D)

    MAT[I_MAT == 0] <- NA

    return(MAT)
}
