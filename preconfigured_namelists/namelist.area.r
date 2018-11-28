## R

## Define plot limits for the chosen projection and area ...

#if (rotate_mesh) { # geographic coordinates
#if (rotate_mesh || !rotate_mesh) {

    projection <<- "rectangular" ## "rectangular", "stereographic", "orthographic"
    geogr_lims <<- T # define plot borders in geographical coordinates, i.e. make normal plot 
                     # in xy-plane in case of e.g. stereographic projection
    proj_lims  <<- F # define plot borders in projected coordinates, i.e. make "pizza piece" 
                     # plot in case of stereographic projection (does not have an effect 
                     # in case of rectangular or orthograhic projection)

    # default area selection
    if (all(transient_mode != c("csec_mean", "csec_depth", 
                           "moc_mean", "moc_depth"))) {

        # default ("flat earth") projection
        if (projection == "rectangular") {
            geogr_lims <<- T
            proj_lims <<- F
            if (area == "global") {
                map_geogr_lim_lon <<- c(-180, 180) # degree east 
                map_geogr_lim_lat <<- c(-90, 90) # degree north
            } else if (area == "atlantic") {
                map_geogr_lim_lon <<- c(-90, 10)
                map_geogr_lim_lat <<- c(-40, 80)
            } else if (area == "northatlantic") {
                map_geogr_lim_lon <<- c(-80, 0)
                map_geogr_lim_lat <<- c(30, 75)
            } else if (area == "northatlantic2") {
                map_geogr_lim_lon <<- c(-85, -10)
                map_geogr_lim_lat <<- c(30, 60)
            } else if (area == "NAeq") {
                map_geogr_lim_lon <<- c(-100, 15)
                map_geogr_lim_lat <<- c(0, 85)
            } else if (area == "NAseq") {
                map_geogr_lim_lon <<- c(-90, -20)
                map_geogr_lim_lat <<- c(5, 55)
            } else if (area == "northernNA") {
                map_geogr_lim_lon <<- c(-80, 30)
                map_geogr_lim_lat <<- c(30, 85)
            } else if (area == "easternNA") {
                map_geogr_lim_lon <<- c(-45, 30)
                map_geogr_lim_lat <<- c(60, 75)
            } else if (area == "bigna") {
                map_geogr_lim_lon <<- c(-100, 20)
                map_geogr_lim_lat <<- c(-10, 85)
            } else if (area == "bigna2") {
                map_geogr_lim_lon <<- c(-82, 20)
                map_geogr_lim_lat <<- c(10, 82.5)
            } else if (area == "kiekeNA") { # NA kieke diss fig. 2.1
                map_geogr_lim_lon <<- c(-65, 5)
                map_geogr_lim_lat <<- c(-35, 70)
            } else if (area == "NWC") { # North West Corner, Mertens et al. 2014, Fig. 17
                map_geogr_lim_lon <<- c(-55, -30)
                map_geogr_lim_lat <<- c(40, 55)
            } else if (area == "marzocchiFig4") { # marzocchi et al 2015, fig 4
                map_geogr_lim_lon <<- c(-90, 10)
                map_geogr_lim_lat <<- c(25, 75)
            } else if (area == "caa") {
                map_geogr_lim_lon <<- c(-127, -52)
                map_geogr_lim_lat <<- c(64, 85)
            } else if (area == "fram") {
                map_geogr_lim_lon <<- c(-25, 15)
                map_geogr_lim_lat <<- c(50, 85)
            } else if (area == "fram2") {
                map_geogr_lim_lon <<- c(-10, 15)
                map_geogr_lim_lat <<- c(76.5, 81)
            } else if (area == "southfram") {
                map_geogr_lim_lon <<- c(-15, 5)
                map_geogr_lim_lat <<- c(72.5, 77.5)
            } else if (area == "lsea") {
                map_geogr_lim_lon <<- c(-65, -41)
                map_geogr_lim_lat <<- c(53, 65)
            } else if (area == "lseaS14") { # Scholz et al. 2014 index area
                if (!any(search() == "package:R.matlab")) library(R.matlab)
                if (!any(search() == "package:splancs")) library(splancs)
                CbSCL_cont <<- readMat("CbSCL_polygon_LS_deselectshelf_2500m.mat")
                CbSCL_cont <<- cbind(t(CbSCL_cont$index.cont.x),
                                    t(CbSCL_cont$index.cont.y))
                CbSCL_cont <<- CbSCL_cont[-which(is.nan(CbSCL_cont))[1],]
                map_geogr_lim_lon <<- CbSCL_cont[,1]
                map_geogr_lim_lat <<- CbSCL_cont[,2]
            } else if (area == "lsea2") {
                map_geogr_lim_lon <<- c(-65, -45)
                map_geogr_lim_lat <<- c(55, 65)
            } else if (area == "lsea3") {
                map_geogr_lim_lon <<- c(-80, -10)
                map_geogr_lim_lat <<- c(45, 80)
            } else if (area == "lsea4") {
                map_geogr_lim_lon <<- c(-70, -30)
                map_geogr_lim_lat <<- c(50, 70)
            } else if (area == "lsea5") {
                map_geogr_lim_lon <<- c(-65, -5)
                map_geogr_lim_lat <<- c(54, 67.5)
            } else if (area == "lseawNA") {
                map_geogr_lim_lon <<- c(-60, -47)
                map_geogr_lim_lat <<- c(56.5, 60.5)
            } else if (area == "lseawSI") {
                map_geogr_lim_lon <<- c(-57, -47)
                map_geogr_lim_lat <<- c(56.5, 60.5)
            } else if (area == "lseaMLD") {
                map_geogr_lim_lon <<- c(-60, -45)
                map_geogr_lim_lat <<- c(55, 62.5)
            } else if (area == "MLDNA") {
                map_geogr_lim_lon <<- c(-65, 15)
                map_geogr_lim_lat <<- c(45, 82.5)
            } else if (area == "lseancep") {
                map_geogr_lim_lon <<- c(-58.125, -54.375)
                map_geogr_lim_lat <<- c(58.07594, 63.78832)
            } else if (area == "LSkarspeck") { # lab sea at Karspeck et al. 2015 coordinates
                map_geogr_lim_lon <<- c(-58, -50)
                map_geogr_lim_lat <<- c(56, 61)
            } else if (area == "LSboening") { # lab sea at Boening et al. 2016 supplementary red (left) box
                map_geogr_lim_lon <<- c(-65, -45)
                map_geogr_lim_lat <<- c(50, 65)
            } else if (area == "LSboening2") { # irm sea at Boening et al. 2016 supplementary blue (right) box
                map_geogr_lim_lon <<- c(-45, -30)
                map_geogr_lim_lat <<- c(50, 65)
            } else if (area == "LSboening3") { # irm sea at Boening et al. 2016 supplementary both regions
                map_geogr_lim_lon <<- c(-65, -30)
                map_geogr_lim_lat <<- c(50, 65)
            } else if (area == "LSmldl") {
                map_geogr_lim_lon <<- c(-58, -47)
                map_geogr_lim_lat <<- c(55.5, 62)
            } else if (area == "LSmldh") {
                map_geogr_lim_lon <<- c(-58, -45)
                map_geogr_lim_lat <<- c(55.5, 61)
            } else if (area == "LSmldl1") {
                polycoords <<- read.table(#"~/scripts/r/Low01_s1_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/Low01_s1_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt", 
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldl2") {
                polycoords <<- read.table(#"~/scripts/r/Low01_s2_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/Low01_s2_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldl3") {
                polycoords <<- read.table(#"~/scripts/r/Low01_s3_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/Low01_s3_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt", 
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldl4") {
                polycoords <<- read.table(#"~/scripts/r/Low01_s4_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/Low01_s4_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt", 
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldl5") {
                polycoords <<- read.table(#"~/scripts/r/Low01_s5_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/Low01_s5_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt", 
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldh1") {
                polycoords <<- read.table(#"~/scripts/r/LSea5_s1_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/LSea5_s1_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt", 
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldh2") {
                polycoords <<- read.table(#"~/scripts/r/LSea5_s2_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/LSea5_s2_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldh3") {
                polycoords <<- read.table(#"~/scripts/r/LSea5_s3_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/LSea5_s3_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt", 
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldh4") {
                polycoords <<- read.table(#"~/scripts/r/LSea5_s4_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/LSea5_s4_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSmldh5") {
                polycoords <<- read.table(#"~/scripts/r/LSea5_s5_mixlay_lsea_gt_1800m_Mar_1961-2009_ahull_alpha0.1.txt",
                                   "~/scripts/r/LSea5_s5_mixlay_lsea_gt_1800m_Mar_1961-2009_coords.txt", 
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LS31l") {
                polycoords <<- read.table("~/scripts/r/CbSCL_mesh_LS_ge_3100m_chull.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LS31h") {
                polycoords <<- read.table("~/scripts/r/LSea2_mesh_LS_ge_3100m_chull.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LS30l") {
                polycoords <<- read.table("~/scripts/r/CbSCL_mesh_LS_ge_3000m_chull.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LS30l2") {
                polycoords <<- read.table("~/scripts/r/CbSCL_mesh_LS_ge_3000m.and.hvel_lt_6.1_cms-1_chull.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LS35h") {
                polycoords <<- read.table("~/scripts/r/LSea2_mesh_LS_ge_3500m_chull.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "LSkieke06") {
                polycoords <<- read.table("~/scripts/r/LSkieke06_chull.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "lseawNAtilt") {
                polycoords <<- read.table("~/scripts/r/lseawNAtilt.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "icebox1") { # box for ice extend sum east of southern greenland
                map_geogr_lim_lon <<- c(-45, 10)
                map_geogr_lim_lat <<- c(57.5, 80)
            } else if (area == "icebox2") { # box for ice extend sum west of southern greenland
                map_geogr_lim_lon <<- c(-67.5, -45)
                map_geogr_lim_lat <<- c(45, 80)
            } else if (area == "icebox3") { # box for ice extend sum west of southtern greenland from davis
                map_geogr_lim_lon <<- c(-67.5, -45)
                map_geogr_lim_lat <<- c(45, 66.59)
            } else if (area == "wgc") {
                area <<- "wgc"
            } else if (area == "CLS") {
                map_geogr_lim_lon <<- c(-55, -48)
                map_geogr_lim_lat <<- c(56, 59.5)
            } else if (area == "gulfmex") {
                map_geogr_lim_lon <<- c(-100, -80)
                map_geogr_lim_lat <<- c(18, 32)
            } else if (area == "karib1") {
                map_geogr_lim_lon <<- c(-100, -55)
                map_geogr_lim_lat <<- c(5, 35)
            } else if (area == "CLS2") {
                map_geogr_lim_lon <<- c(-65, -50)
                map_geogr_lim_lat <<- c(50, 67.5)
            } else if (area == "GL") {
                map_geogr_lim_lon <<- c(-75, -10)
                map_geogr_lim_lat <<- c(55, 87.5)
            } else if (area == "sEGC") {
                map_geogr_lim_lon <<- c(-45, -35)
                map_geogr_lim_lat <<- c(57.5, 66)
            } else if (area == "sWGC") {
                map_geogr_lim_lon <<- c(-55, -45)
                map_geogr_lim_lat <<- c(57.5, 66)
            } else if (area == "IceSea1") {
                map_geogr_lim_lon <<- c(-20, -10)
                map_geogr_lim_lat <<- c(57.5, 61.5)
            } else if (area == " IceSea2") {
                map_geogr_lim_lon <<- c(-30, -20)
                map_geogr_lim_lat <<- c(50, 60)
            } else if (area == "IrmSea1") {
                map_geogr_lim_lon <<- c(-40, -32.5)
                map_geogr_lim_lat <<- c(60, 63)
            } else if (area == "IrmSea2") {
                map_geogr_lim_lon <<- c(-40, -32.5)
                map_geogr_lim_lat <<- c(55.5, 63)
            } else if (area == "GIN") {
                map_geogr_lim_lon <<- c(-7.5, 2.5)
                map_geogr_lim_lat <<- c(65, 77.5)
            } else if (area == "GIN2") {
                map_geogr_lim_lon <<- c(-20, 15)
                map_geogr_lim_lat <<- c(65, 80)
            } else if (area == "GINmldl") {
                map_geogr_lim_lon <<- c(-10, 0)
                map_geogr_lim_lat <<- c(67.5, 77.5)
            } else if (area == "GINmldh") {
                map_geogr_lim_lon <<- c(-10, 0)
                map_geogr_lim_lat <<- c(72.5, 76)
            } else if (area == "arctic") {
                map_geogr_lim_lon <<- c(-180, 180)
                map_geogr_lim_lat <<- c(0, 90)
            } else if (area == "arc08") { # claudis Arc08
                map_geogr_lim_lon <<- c(-39.99900, 39.99623)
                map_geogr_lim_lat <<- c(72.00031, 86.99952)
            } else if (area == "arc08fram") { # claudis Arc08 high-res zoom
                map_geogr_lim_lon <<- c(-20, 20)
                map_geogr_lim_lat <<- c(76, 82)
            } else if (area == "arc08framS") {
                map_geogr_lim_lon <<- c(-20, 10)
                map_geogr_lim_lat <<- c(72, 74)
            } else if (area == "arc08eke") {
                map_geogr_lim_lon <<- c(-20, 20)
                map_geogr_lim_lat <<- c(76, 80)
            } else if (area == "pacific") {
                map_geogr_lim_lon <<- c(120, -160) # in [-180 <= x <= 180] degree notation
                map_geogr_lim_lat <<- c(30, 60)
                grid_coord_lons <<- c(120)
                grid_coord_lats <<- c(35)
                grid_coord_labs <<- c(35)
                grid_coord_hemi <<- c("N")
            } else if (area == "atlantic1") {
                map_geogr_lim_lon <<- c(-85, -5)
                map_geogr_lim_lat <<- c(30, 80) #c(-90, 90)
            } else if (area == "atlantic2") {
                map_geogr_lim_lon <<- c(-85, -5)
                map_geogr_lim_lat <<- c(10, 80) #c(-90, 90)
            } else if (area == "stg") {
                map_geogr_lim_lon <<- c(-80, -20)
                map_geogr_lim_lat <<- c(20, 40) #c(-90, 90)
            } else if (area == "stghu") {
                map_geogr_lim_lon <<- c(-82.5, 0)
                map_geogr_lim_lat <<- c(8.4, 54.5) #c(-90, 90)
            } else if (area == "spg") {
                map_geogr_lim_lon <<- c(-65, -15)
                map_geogr_lim_lat <<- c(48, 60) #c(-90, 90)
            } else if (area == "GS") {
                map_geogr_lim_lon <<- c(-65, -15) 
                map_geogr_lim_lat <<- c(35, 62) 
            } else if (area == "GSeddy") {
                map_geogr_lim_lon <<- c(-77.5, -67.5)
                map_geogr_lim_lat <<- c(32.5, 40)
            } else if (area == "GSbox1") {
                map_geogr_lim_lon <<- c(-81, -45)
                map_geogr_lim_lat <<- c(26.5, 40)
            } else if (area == "GSbox2") {
                map_geogr_lim_lon <<- c(-83, -40)
                map_geogr_lim_lat <<- c(26.5, 50)
            } else if (area == "GSbox3") {
                map_geogr_lim_lon <<- c(-80, -40)
                map_geogr_lim_lat <<- c(35, 45)
            } else if (area == "GSbox4") {
                map_geogr_lim_lon <<- c(-85, -50)
                map_geogr_lim_lat <<- c(30, 42.5)
            } else if (area == "GSNAC") {
                map_geogr_lim_lon <<- c(-82, -10)
                map_geogr_lim_lat <<- c(25, 65)
            } else if (area == "CGS") {
                map_geogr_lim_lon <<- c(-50, -30)
                map_geogr_lim_lat <<- c(45, 50)
            } else if (area == "SGS") {
                map_geogr_lim_lon <<- c(-85, -10)
                map_geogr_lim_lat <<- c(25, 60)
            } else if (area == "gibraltar") {
                map_geogr_lim_lon <<- c(-10, -1)
                map_geogr_lim_lat <<- c(30, 40)
            } else if (area == "mow_center") { # street of gibraltar narrowest position
                map_geogr_lim_lon <<- c(-6.5, -4.5)
                map_geogr_lim_lat <<- c(34, 37)
            } else if (area == "mow_reservoir") { # mediterranean outflow water reservoir
                map_geogr_lim_lon <<- c(-25, -10)
                map_geogr_lim_lat <<- c(32, 42)
            } else if (area == "mow_reservoir2") {
                map_geogr_lim_lon <<- c(-40, 0)
                map_geogr_lim_lat <<- c(20, 50)
            } else if (area == "NH") {
                map_geogr_lim_lon <<- c(-180, 180) #c(-61, -49) #c(-62, 0) # c(6,10)
                map_geogr_lim_lat <<- c(0, 90) #c(37, 41) #c(38, 62) # c(52, 53)
            } else if (area == "SH") {
                map_geogr_lim_lon <<- c(-180, 180) 
                map_geogr_lim_lat <<- c(-90, 0) 
            } else if (area == "mldWeddel") {
                map_geogr_lim_lon <<- c(-62.05, -6.45)
                map_geogr_lim_lat <<- c(-80, -63)
            } else if (area == "ACCatl") {
                map_geogr_lim_lon <<- c(-80, 80)
                map_geogr_lim_lat <<- c(-80, -50)
            } else if (view_nadja1) {
                view <- "nadja1"
                map_geogr_lim_lon <- c(-4, 8)
                map_geogr_lim_lat <- c(51, 62)
            }
            if (area != "arctic") {
                poly_geogr_lim_lon <<- map_geogr_lim_lon
                poly_geogr_lim_lat <<- map_geogr_lim_lat
            }
            
        # stereographic ("polar") projection
        } else if (projection == "stereographic") {
            projection_par <<- c()
            if (area == "atlantic") {
                map_geogr_lim_lon <<- c(-80, 10)
                map_geogr_lim_lat <<- c(-30, 90)
                poly_geogr_lim_lon <<- c(-90, 20)
                poly_geogr_lim_lat <<- c(-35, 90)
            } else if (area == "northatlantic") {
                map_geogr_lim_lon <<- c(-90, -9)
                map_geogr_lim_lat <<- c(45, 70)
                poly_geogr_lim_lon <<- c(-80, 32)
                poly_geogr_lim_lat <<- c(30, 67)
                if (proj_lims) {
                    map_geogr_lim_lon <<- c(-90, 0)
                    map_geogr_lim_lat <<- c(45, 85)
                    poly_geogr_lim_lon <<- c(-75, 0)
                    poly_geogr_lim_lat <<- c(35, 90)
                }
            } else if (area == "northatlantic2") {
                map_geogr_lim_lon <<- c(-59, -10)
                map_geogr_lim_lat <<- c(40, 60)
                poly_geogr_lim_lon <<- c(-57, 3)
                poly_geogr_lim_lat <<- c(40, 62)
                grid_coord_lons <<- c(-63.4, -71.9, -60, -40, -20, 0, 2.5, -6, -20, -40)
                grid_coord_lats <<- c(50, 60, 64.1, 66.1, 65.6, 62.2, 60, 50, 41.4, 42.2)
                grid_coord_labs <<- c(50, 60, 60, 40, 20, 0, 60, 50, 20, 40)
                grid_coord_hemi <<- c("N", "N", "W", "W", "W", "E", "N", "N", "W", "W")
                if (proj_lims) {
                    map_geogr_lim_lon <<- c(-60, -5)
                    map_geogr_lim_lat <<- c(45, 60)
                    poly_geogr_lim_lon <<- c(-60, -5)
                    poly_geogr_lim_lat <<- c(40, 65)
                }
            } else if (area == "lsea") {
                map_geogr_lim_lon <<- c(-64, -40)
                map_geogr_lim_lat <<- c(55, 66)
                poly_geogr_lim_lon <<- c(-64, -40)
                poly_geogr_lim_lat <<- c(55, 66)
                grid_coord_lons <<- c(1)
                grid_coord_lats <<- c(1)
                grid_coord_labs <<- c(1)
                grid_coord_hemi <<- c("N")
                if (proj_lims) {
                    map_geogr_lim_lon <<- c(-62, -24)
                    map_geogr_lim_lat <<- c(50, 66)
                    poly_geogr_lim_lon <<- c(-67, -28)
                    poly_geogr_lim_lat <<- c(49, 70)
                }
            } else if (area == "lsea2") {
                map_geogr_lim_lon <<- c(-60, -22.5)
                map_geogr_lim_lat <<- c(50, 64)
                poly_geogr_lim_lon <<- c(-59, -19)
                poly_geogr_lim_lat <<- c(46, 67)
                grid_coord_lons <<- c(-61.3, -67.4, -60, -40, -20, -22.7, -27.1, -40)
                grid_coord_lats <<- c(50, 60, 67.9, 68.9, 67.6, 60, 50, 47.1)
                grid_coord_labs <<- c(50, 60, 60, 40, 20, 60, 50, 40)
                grid_coord_hemi <<- c("N", "N", "W", "W", "W", "N", "N", "W")
                if (proj_lims) {
                    map_geogr_lim_lon <<- c(-60, -22)
                    map_geogr_lim_lat <<- c(50, 66)
                    poly_geogr_lim_lon <<- c(-65, -26)
                    poly_geogr_lim_lat <<- c(49, 70)
                }
            } else if (area == "caa") {
                map_geogr_lim_lon <<- c(-120, -60)
                map_geogr_lim_lat <<- c(60, 85)
                poly_geogr_lim_lon <<- c(-100, -80)
                poly_geogr_lim_lat <<- c(65, 80)
            } else if (area == "arctic") {
                #proj_lims <<- T  # arctic area and stereographic projection does not
                #geogr_lims <<- F # work with geographic lims... dont know why...
                map_geogr_lim_lon <<- c(-180, 180)
                map_geogr_lim_lat <<- c(70, 90) #c(40, 90)
                poly_geogr_lim_lon <<- c(-180, 180)
                poly_geogr_lim_lat <<- c(70, 90) #c(40, 90)
            } else if (area == "NH") {
                map_geogr_lim_lon <<- c(-180, 180) #c(-61, -49) #c(-62, 0) # c(6,10)
                map_geogr_lim_lat <<- c(0, 90) #c(37, 41) #c(38, 62) # c(52, 53)
                poly_geogr_lim_lon <<- map_geogr_lim_lon
                poly_geogr_lim_lat <<- map_geogr_lim_lat
            }
            orient <<- c(mean(map_geogr_lim_lat), mean(map_geogr_lim_lon), 0)
            if (area == "arctic" || area == "nh") orient <<- c(90, -35, 0)
        
        # orthographic ("from space") projection
        } else if (projection == "orthographic") {
            projection_par <<- c()
            proj_lims <<- F
            geogr_lims <<- T
            if (area == "atlantic") {
                map_geogr_lim_lon <<- c(-180, 180)
                map_geogr_lim_lat <<- c(-90, 90)
                poly_geogr_lim_lon <<- c(-180, 180)
                poly_geogr_lim_lat <<- c(-90, 90)
                orient <<- c(45, -35, 0)
            } else {
                stop(paste0("Error: orthographic projection only implemented for 'area'='atlantic'."))
            }
        
        } # which projection

    # cross section or moc ...
    } else {

        # cross section area selection
        if (any(transient_mode == c("csec_mean", "csec_depth"))) {
            geogr_lims <<- T
            proj_lims <<- F
            projection <<- "rectangular"
            if (area == "csec_DS") {
                map_geogr_lim_lon <<- c(-33.3, -25)
                map_geogr_lim_lat <<- c(67.2, 65.5)
            } else if (area == "csec_DSe") {
                area <<- "csec_DSe"
                map_geogr_lim_lon <<- c(-29, -25)
                map_geogr_lim_lat <<- c(66.31783, 65.5)
            } else if (area == "csec_DSw") {
                map_geogr_lim_lon <<- c(-33.3, -29)
                map_geogr_lim_lat <<- c(67.2, 66.31783)
            } else if (area == "csec_CF") {
                map_geogr_lim_lon <<- c(-43.92017, -43.92017)
                map_geogr_lim_lat <<- c(57, 59.8) #c(59.8, 57)
            } else if (area == "csec_N53") {
                map_geogr_lim_lon <<- c(-52, -49)
                map_geogr_lim_lat <<- c(52.51, 54.16667)
            } else if (area == "csec_FBC") {
                map_geogr_lim_lon <<- c(-6.9, -8.4)
                map_geogr_lim_lat <<- c(61.9, 61) 
            } else if (area == "csec_BP1215") {
                map_geogr_lim_lon <<- c(-36.85739, -31.15646)  
                map_geogr_lim_lat <<- c(52.50856, 47.66858)
            } else if (area == "csec_IceShet") {
                map_geogr_lim_lon <<- c(-14.6, -5)
                map_geogr_lim_lat <<- c(64.5, 58.5)
            } else if (area == "csec_IFR") {
                map_geogr_lim_lon <<- c(-14.6, -7.037389)
                map_geogr_lim_lat <<- c(64.5, 62.073192)
            } else if (area == "csec_FSC") {
                map_geogr_lim_lon <<- c(-7.037389, -1.2)
                map_geogr_lim_lat <<- c(62.073192, 60.2)
            } else if (area == "csec_N41") {
                map_geogr_lim_lon <<- c(-46, -2)
                map_geogr_lim_lat <<- c(41.1, 41.1)
            } else if (area == "csec_ar7w") {
                map_geogr_lim_lon <<- c(-55.45, -48.26)
                map_geogr_lim_lat <<- c(53.66, 60.54)
            } else if (area == "csec_FP") {
                map_geogr_lim_lon <<- c(-50, -46)
                map_geogr_lim_lat <<- c(47.1, 47.1)
            } else if (area == "csec_irm") {
                map_geogr_lim_lon <<- c(-43.5, -32.5)
                map_geogr_lim_lat <<- c(60.5, 58)
            } else if (area == "csec_davis") {
                map_geogr_lim_lon <<- c(-53.25, -61.75)
                map_geogr_lim_lat <<- c(66.59, 66.59)
            } else if (area == "csec_N61") {
                map_geogr_lim_lon <<- c(-43.5, -4)
                map_geogr_lim_lat <<- c(61, 60)
            } else if (areaa == "csec_fram") {
                map_geogr_lim_lon <<- c(-12.83, 10.7)
                map_geogr_lim_lat <<- c(81.48, 78.99)
            } else if (area == "csec_bering") {
                map_geogr_lim_lon <<- c(-172, -166.5)
                map_geogr_lim_lat <<- c(65.75, 65.75)
            } else if (area == "csec_barents") {
                map_geogr_lim_lon <<- c(20, 20)
                map_geogr_lim_lat <<- c(69.5, 78)
            } else if (area == "csec_lseawNA") {
                map_geogr_lim_lon <<- c(-47, -47, -60, -60, -47) # box
                map_geogr_lim_lat <<- c(56.5, 60.5, 60.5, 56.5, 56.5)
            } else if (area == "LS30l") {
                polycoords <<- read.table("~/scripts/r/CbSCL_mesh_LS_ge_3000m_chull.txt",
                                   header=F, col.names=c("x", "y"))
                map_geogr_lim_lon <<- polycoords[,1]$x
                map_geogr_lim_lat <<- polycoords[,2]$y
            } else if (area == "csec_S30") {
                map_geogr_lim_lon <<- c(-50, 25)
                map_geogr_lim_lat <<- c(-30, -30)
            }
            poly_geogr_lim_lon <<- map_geogr_lim_lon
            poly_geogr_lim_lat <<- map_geogr_lim_lat
        
        # moc area selection
        } else if (any(transient_mode == c("moc_mean", "moc_depth"))) {
            geogr_lims <<- T
            proj_lims <<- F
            projection <<- "rectangular"
            if (area == "moc_NA") {

                # either provide mesh-specific (!) mask file a la Dima or a 
                # closed polygon of area of MOC calculation
                #moc_mask_file <<- "/work/ba0941/a270073/mesh/LSea1/mask_NA_LSea1.dat"
                #moc_mask_file <<- paste0(meshpath, "mask_NA_CORE2.dat")
                if (meshid == "core" || meshid == "CORE2_final") {
                    moc_mask_file <<- "/work/ba0941/a270073/mesh/core/mask_NA_CORE2.dat"
                } else {
                    stop(paste0("no moc mask file given for mesh '", meshid, "'"))
                }
            }
        
        } # which non-default area selection
        
    } # default or special area selection

#} else if (!rotate_mesh) {
#} else {
 
    if (F) {
        print("schingschangschong")

        if (proj_rectangular) {
            geogr_lims <<- T
            proj_lims <<- F
            projection <<- "rectangular"

            if (area_global) {
                area <<- "global"
                print("find global rotated coordinates later ...")
            }

        }

        if (proj_stereographic) {
            stop("not")
        }

        if (proj_orthographic) {
            stop("nott")
        }
    } # F 

#} # rotate_mesh or not 

