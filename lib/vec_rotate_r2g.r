vec_rotate_r2g <- function(alpha, beta, gamma, lon, lat, tlon, tlat, flag) {
## R function for rotating horizontal velocity field
## input:   lon = xc (longitudes)
##          lat = yc (latitutes)
##          tlon = vx (u component of horizontal velocity)
##          tlat = vy (v component of horizontal velocity)
##          flag =
##              1 (lon and lat are geographical coordinates)
##              0 (lon and lat are rotated coordinates)

if (flag == 1) {
    glon <- lon
    glat <- lat
    rotated_coords <- grid_rotate_g2r(alpha, beta, gamma, lon, lat)
    rlon <- rotated_coords$rlon
    rlat <- rotated_coords$rlat
} else {
    rlon <- lon
    rlat <- lat
    rotated_coords <- grid_rotate_r2g(alpha, beta, gamma, lon, lat)
    glon <- rotated_coords$glon
    glat <- rotated_coords$glat
}

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
glat <- glat*rad
glon <- glon*rad

# Rotated Cartesian coordinates:
txg <- -tlat*sin(rlat)*cos(rlon) - tlon*sin(rlon)
tyg <- -tlat*sin(rlat)*sin(rlon) + tlon*cos(rlon)
tzg <- tlat*cos(rlat)

# Geographical Cartesian coordinates:
txr <- drop(rotate_matrix[1,1])*txg + drop(rotate_matrix[1,2])*tyg + drop(rotate_matrix[1,3])*tzg 
tyr <- drop(rotate_matrix[2,1])*txg + drop(rotate_matrix[2,2])*tyg + drop(rotate_matrix[2,3])*tzg
tzr <- drop(rotate_matrix[3,1])*txg + drop(rotate_matrix[3,2])*tyg + drop(rotate_matrix[3,3])*tzg

# Geographical coordinates:
u <- -sin(glon)*txr + cos(glon)*tyr
v <- -sin(glat)*cos(glon)*txr - sin(glat)*sin(glon)*tyr + cos(glat)*tzr

if (F) {
    ws <- sort(sapply(ls(), function(x) object.size(get(x))), decreasing=T)/1024^2 # Mb
    print(paste0(indent, "   10 biggest objects in vec_rotate_r2g() [Mb]:"))
    print(round(ws[1:10], 3))
}

return(list(u=u, v=v, rot_mat=rotate_matrix))

rm(list=ls())

}
