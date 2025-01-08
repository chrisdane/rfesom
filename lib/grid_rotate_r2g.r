## R function for transformation from rotated
## to geographical coordinates.
## Rotation around euler angles (alpha, beta, gamma).
## Input: rotated coordinates

grid_rotate_r2g <- function(alpha, beta, gamma, rlon, rlat) {

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

rotate_matrix <- solve(rotate_matrix)

rlat <- rlat*rad
rlon <- rlon*rad

# Rotated Cartesian coordinates:
xr <- cos(rlat)*cos(rlon)
yr <- cos(rlat)*sin(rlon)
zr <- sin(rlat)

# Geographical Cartesian coordinates:
xg <- drop(rotate_matrix[1,1])*xr + drop(rotate_matrix[1,2])*yr + drop(rotate_matrix[1,3])*zr
yg <- drop(rotate_matrix[2,1])*xr + drop(rotate_matrix[2,2])*yr + drop(rotate_matrix[2,3])*zr
zg <- drop(rotate_matrix[3,1])*xr + drop(rotate_matrix[3,2])*yr + drop(rotate_matrix[3,3])*zr

# Geographical coordinates:
glat <- asin(zg)
glon <- atan2(yg,xg)
a <- which(yg==0 & xg==0)
glon[a] <- 0

glat <- glat/rad
glon <- glon/rad

return(list(glon=glon, glat=glat, rot_mat=rotate_matrix))

rm(list=ls())

}
