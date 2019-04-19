## R function for transformation from geographical 
## to rotated coordinates.
## Rotation around euler angles (alpha, beta, gamma).
## Input: geographical coordinates

grid_rotate_g2r <- function(alpha, beta, gamma, glon, glat) {

rad=pi/180
al=alpha*rad
be=beta*rad
ga=gamma*rad

rotate_matrix <- array(NA, c(3,3))

rotate_matrix[1,1] <- cos(ga)*cos(al)-sin(ga)*cos(be)*sin(al)
rotate_matrix[1,2] <- cos(ga)*sin(al)+sin(ga)*cos(be)*cos(al)
rotate_matrix[1,3] <- sin(ga)*sin(be)
rotate_matrix[2,1] <- -sin(ga)*cos(al)-cos(ga)*cos(be)*sin(al)
rotate_matrix[2,2] <- -sin(ga)*sin(al)+cos(ga)*cos(be)*cos(al)
rotate_matrix[2,3] <- cos(ga)*sin(be)
rotate_matrix[3,1] <- sin(be)*sin(al)
rotate_matrix[3,2] <- -sin(be)*cos(al)
rotate_matrix[3,3] <- cos(be)

glat <- glat*rad
glon <- glon*rad

# Geographical Cartesian coordinates:
xr <- cos(glat)*cos(glon)
yr <- cos(glat)*sin(glon)
zr <- sin(glat)

# Rotated Cartesian coordinates:
xg <- drop(rotate_matrix[1,1])*xr + drop(rotate_matrix[1,2])*yr + drop(rotate_matrix[1,3])*zr
yg <- drop(rotate_matrix[2,1])*xr + drop(rotate_matrix[2,2])*yr + drop(rotate_matrix[2,3])*zr
zg <- drop(rotate_matrix[3,1])*xr + drop(rotate_matrix[3,2])*yr + drop(rotate_matrix[3,3])*zr

# Rotated coordinates:
rlat <- asin(zg)
rlon <- atan2(yg,xg)
a <- which(yg==0 && xg==0)
rlon[a] <- 0

rlat <- rlat/rad
rlon <- rlon/rad

return(list(rlon=rlon, rlat=rlat, rot_mat=rotate_matrix))

}
