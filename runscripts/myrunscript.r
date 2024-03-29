# myrunscript.r

rfesompath <- "~/scripts/r/rfesom" 
hostname <- Sys.info()["nodename"]

############################################################

fesom_version <- "fesom" # default: fesom (=fesom1)

if (F) { # awi-esm-1-1-lr deck
    model <- "fesom"
    runid <- "awi-esm-1-1-lr"
    #datainpaths <- paste0("/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/", setting, "/outdata/fesom")
    if (F) {
        setting <- "piControl"
        datainpaths <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata/fesom"
        #years <- 1842:1941
        #years <- (1941-29):1941
        #years <- 1940:1941
        years <- 1941
    } else if (F) {
        setting <- "hist"
        datainpaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist/outdata/fesom"
        years <- 1850:2014
        #years <- 1985:2014
    } else if (F) {
        setting <- "1percCO2"
        datainpaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/1percCO2/outdata/fesom"
        #years <- 1850:1851
        years <- 1850:2099
        #years <- 2070:2099
    } else if (F) {
        setting <- "4CO2"
        datainpaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/4CO2/outdata/fesom"
        years <- 1850:2099
        #years <- 2070:2099
    }
    fpatterns <- "<setting>_fesom_<varname_nc>_<YYYY>0101.nc"
    meshid <- "core"
    meshpath <- paste0("/work/ab0995/a270046/meshes_default/", meshid)
    postpath <- "/work/ab0246/a270073/post/fesom"
    plotpath <- "/work/ab0246/a270073/plots/fesom"
    derivpath <- paste0("/work/ba0941/a270073/mesh/", model, "/", meshid, "/derivatives")
    interppath <- paste0("/work/ba0941/a270073/mesh/", model, "/", meshid, "/interp")
    if (F) {
        varname <- "MOCw"
        depths <- c(0, "max")
        area <- "NA"
        out_mode <- "moc_depth"
    } else if (F) {
        #varname <- "mlotst"
        #varname <- "tos"
        #varname <- "thetao"
        #varname <- "so"
        varname <- "potdens"
        depths <- c(0, "max")
        transient_out <- T
        regular_ltm_out <- F
        regular_transient_out <- F
        frequency_post <- "monthly"
        #out_mode <- "area"
        #out_mode <- "fldmean"
        out_mode <- "depth"
        #area <- "LSstolpe18"
        #area <- "GIN2"
        area <- "northernNA2"
    }

} else if (F) {
    runid <- "awicm-CMIP6_dsein"
    # /work/ab0995/a270067/fesom_echam/core/cpl_output_16 
    # --> echam 1901:2198 echam MM 419M per file
    # /work/uo0119/a270067/fesom_echam/core/cpl_output_16 
    # --> echam 1901:2199 post-processed 100G per file
    # --> fesom 1901:2199 thetao,so,tso as .nc 
    # --> fesom 1901:2199 all other as .tar for each variable

} else if (F) {
    model <- "fesom"
    runid <- "awicm-CMIP6_hu" # 2020:2700
    meshid <- "CORE2_final"
    setting <- "hu_svn471_ollie" # 2000:2363 (model years 1:364), 2803:2859 (model years 804:860)
    #setting <- "pi" # 2020:2700 (model years 862:1542)
    if (setting == "hu_svn471_ollie") {
        if (grepl("ollie", hostname)) {
            datainpaths <- "/work/ollie/cdanek/awicm-CMIP6/hu_svn471_ollie/" # 2000:2363 (model years 1:364), 2803:2859 (model years 804:860) 
        } else if (grepl("mistral", hostname)) {
            stop("not")
        }
    } else if (setting == "hu_svn477_ollie") {
        if (grepl("ollie", hostname)) {
            datainpaths <- "/work/ollie/ukrebska/AWI_CM/pi477_u/cpl_output/" # 2020:2700 (model years 862:1542)
        } else if (grepl("mistral", hostname)) {
            datainpaths <- "/work/ab0246/a270073/awicm-CMIP6_hu/hu_svn477_ollie/" # 2700 only
        }
    }

} else if (F) { # xiaoxu
    model <- "fesom"
    datainpaths <- "/pf/a/a270064/work/esm-experiments/mh_cmip/outdata/fesom"
    fpatterns <- "mh_cmip_fesom_<varname>_<YYYY>0101.nc"
    meshpath <- "/work/ab0995/a270046/meshes_default/core"
    rotate_mesh <- T
    postprefix <- "awi-esm-1-1-lr_mh_cmip"
    varname <- "tos"
    years <- 2105

} else if (F) {
    runid <- "awicm-CMIP6" 
    meshid <- "core"
    setting <- "restart_from_hu_oceanonly" ## 2701:2702 (PI-CTRL)
    #setting <- "restart_from_restart_from_hu_oceanonly" ## 2703:2710 (PI-CTRL)
    #setting <- "PI-CTRL" ## 2701:2999
    #setting <- "PI-CTRL" ## 2711:2869 (2701:2999)
    #setting <- "PI-CTRL2" ## 2870:2899
    #setting <- "PI-CTRL3" ## 2900:2910
    #setting <- "PI-CTRL4" ## 2911:2999
    #setting <- "PI-CTRL_nodynveg" ## 2800:2999
    #setting <- "restart_from_hu_oceanonly_1850" ## 2701 (1850)
    #setting <- "1850_dynveg" ## 2702:2729 (1850)

} else if (F) {
    model <- "fesom"
    runid <- "awicm-CMIP6_lars" # 1850:1980
    meshid <- "core"
    setting <- "htrans02"
    if (grepl("ollie", hostname)) {
        #datainpaths <- "/work/ollie/lackerma/awicm_tests/htrans02/outdata/fesom/"
        datainpaths <- paste0("/work/ollie/cdanek/", runid, "/", setting, "/")
    }

} else if (F) {
    model <- "fesom"
    runid <- "awicm-test" 
    meshid <- "core"
    setting <- "hist"
    datainpaths <- paste0("/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/", setting, "/outdata/fesom/")
    postpath <- paste0("/work/ab0246/a270073/post/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/", setting, "/")
    #fnames_user <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_test3/outdata/fesom/tos_fesom_18500101.nc"
    #fnames_user <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_test3/outdata/fesom/tossq_fesom_18500101.nc"
    #fnames_user <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_test3/outdata/fesom/zossq_fesom_18500101.nc"
    #fnames_user <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_test3/outdata/fesom/tob_fesom_18500101.nc"
    #fnames_user <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_test3/outdata/fesom/sob_fesom_18500101.nc"
    #fnames_user <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_test3/outdata/fesom/opottempmint_fesom_18500101.nc"
    #fnames_user <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_test3/outdata/fesom/somint_fesom_18500101.nc"
    
} else if (T) { # my phd stuff
    model <- "fesom"
    
    #runid <- "CbSCL"
    #meshid <- "CbSCL"
    #setting <- "spinup5"
    #setting <- "spinup6"

    #runid <- "test3"
    #meshid <- "swang"
    #setting <- ""

    #runid <- "Low01"
    #meshid <- "CbSCL"
    #setting <- "s1"
    #setting <- "s2"
    #setting <- "s3"
    #setting <- "s4"
    #setting <- "s5"
    #setting <- "s52" # where are ice.nc and ice.diag.nc from 1948 to 2007?!
    #setting <- "s6"

    runid <- "Low01_sub_lsea"
    meshid <- "CbSCL_sub_lsea"
    #setting <- "s1"
    #setting <- "s2"
    #setting <- "s3"
    #setting <- "s4"
    #setting <- "s5"
    setting <- "s52"

    #runid <- "Low01_sub_lseawNA"
    #meshid <- "CbSCL_sub_lseawNA"
    #setting <- "s1"
    #setting <- "s52"

    #runid <- "Low04" ## 1948-2007
    #meshid <- "CbSCL"
    #setting <- "s1"

    #runid <- "Low06" ## 1948-1998
    #meshid <- "CbSCL" 
    #setting <- "s1"

    #runid <- "Low07" ## 1948-1966
    #meshid <- "CbSCL" 
    #setting <- "s1"

    #runid <- "Low08" ## 1948-1966
    #meshid <- "CbSCL" 
    #setting <- "s1"

    #runid <- "LSea1" ## 48-00
    #meshid <- "LSea1"
    #setting <- "s1.new"
    #setting <- "96steps.spinup6"

    #runid <- "LSea2" ## Low+NCEP, 48-16
    #meshid <- "CbSCL"
    #setting <- "s1"
    #setting <- "s2"

    #runid <- "LSea3"
    #meshid <- "CbSCL"
    #setting <- "48steps.spinup1"
    #setting <- "48steps.new"

    #runid <- "LSea4"
    #meshid <- "LSea1"
    #setting <- "s1"
    #setting <- "s2"
    #setting <- "s3"
    #setting <- "s4"
    #setting <- "s5" # 1948-1984

    #runid <- "LSea5" 
    #meshid <- "LSea2"
    #setting <- "s1"
    #setting <- "s2"
    #setting <- "s3"
    #setting <- "s4"
    #setting <- "s5" 

    #runid <- "LSea5_sub_lsea" 
    #meshid <- "LSea2_sub_lsea"
    #setting <- "s1"
    #setting <- "s2"
    #setting <- "s3"
    #setting <- "s4"
    #setting <- "s5"

    #runid <- "LSea5_sub_GSNAC" 
    #meshid <- "LSea2_sub_GSNAC"
    #setting <- "s5"

    #runid <- "LSea5_sub_lseawNA" 
    #meshid <- "LSea2_sub_lseawNA"
    #setting <- "s1"
    #setting <- "s4"
    #setting <- "s5"

    #runid <- "LSea6"
    #meshid <- "LSea2"
    #setting <- "s1"

    #runid <- "LSea6_sub_lsea" 
    #meshid <- "LSea2_sub_lsea"
    #setting <- "s1"

    #runid <- "LSea6_sub_GSNAC" 
    #meshid <- "LSea2_sub_GSNAC"
    #setting <- "s1"

    #runid <- "LSea7"
    #meshid <- "LSea2"
    #setting <- "s1"

    #runid <- "LSea7_sub_lsea"
    #meshid <- "LSea2_sub_lsea"
    #setting <- "s1"

    #runid <- "K_GM1"
    #meshid <- "LSea2"
    #setting <- "s1"

    #runid <- "Kh001"
    #meshid <- "CbSCL"
    #setting <- "s1"

    #runid <- "Kh002"
    #meshid <- "LSea2"
    #setting <- "s1"

    #runid <- "GLr05"
    #meshid <- "LSea2"
    #setting <- "s1"
    #setting <- "s1.global.runoff"
    #setting <- "s1.global.runoff_clim"
    #setting <- "s1.onlyGL"

    #runid <- "CORE2" ## with GIS 1948-2009
    #meshid <- "CORE2_final" 
    #setting <- "s1"

    #runid <- "CORE2_ctl" ## without GIS 1948-2009
    #meshid <- "CORE2_final"
    #setting <- "s1"

    #runid <- "GIS02"
    #meshid <- "CORE2_final"
    #setting <- "s1"

    #runid <- "GIS03" ## salt restoring flux of exp "CORE2_ctl"
    #meshid <- "CORE2_final"
    #setting <- "s1"

    #runid <- "weak1" ## 2000-2009
    #meshid <- "LSea1"
    #setting <- ""

    #runid <- "weak2" ## 1948-1976
    #meshid <- "LSea2"
    #setting <- "s1"

    #runid <- "weak3" ## 1948-1952
    #meshid <- "CbSCL"
    #setting <- "s1"

    #runid <- "PP001" ## 1948-1974
    #meshid <- "LSea1"
    #setting <- "s1"

    #runid <- "Low02" ## 1948-2009
    #meshid <- "CbSCL"
    #setting <- "s1"

    #runid <- "ddiff" 1948-1951
    #meshid <- "LSea2"
    #setting <- "s1"

    #runid <- "ddif2" ## 1948-2009
    #meshid <- "CbSCL"
    #setting <- "s1"

    #runid <- "diff3" ## 1948-1969
    #meshid <- "LSea1"
    #setting <- "s1"

    #runid <- "ddif4" ## 1948-2016
    #meshid <- "CbSCL"
    #setting <- "s1"
    #setting <- "s2"

    #runid <- "ddif5" ## 1948-2016
    #meshid <- "LSea1"
    #setting <- "s1"

    #runid <- "ddif6"
    #meshid <- "CbSCL"
    #setting <- "s1"

    #runid <- "visc1"
    #meshid <- "LSea1"
    #setting <- ""

    #runid <- "LS4GS"
    #meshid <- "LSeaGS"
    #setting <- ""

    #runid <- "Sice1"
    #meshid <- "LSea1"
    #setting <- ""

    #runid <- "drag1" # 93-02
    #runid <- "drag1" # with cd; jan 2003
    #meshid <- "LSea2"
    #setting <- ""

    #runid <- "bol01" # mean: 1950-57, 80-92; diag: 1950-57  
    #meshid <- "agulhas" # DONT ROTATE!
    #setting <- ""

    #runid <- "bold"
    #meshid <- "agulhas" # DONT ROTATE!
    #setting <- "cpl_output_01" # tido?
    #setting <- "cpl_output_470" # thetao for nadja: 1900-2100, 1950-2100

    #runid <- "core_hist"
    #meshid <- "CORE2_final" # == Tidos /work/bm0944/a270111/meshes/core/mesh_Agulhas
    #setting <- ""

    #runid <- "Arc22_sub" # nod2d: 570732, monthly 2001-2009
    #runid <- "Arc22_sub_small" # nod2d: 222469, monthly 2001-2009
    #runid <- "Arc22_sub_daily" # nod2d: 570732, daily 2001-2009
    #runid <- "Arc22_daily" # nod2d: 1234322, monthly 2003
    #setting <- ""
    #meshid <- "Arc08" # nod2d: 1234322 
    #meshid <- "Arc08_sub" # nod2d: 570732
    #meshid <- "Arc08_sub_small" # nod2d: 222469 

    #runid <- "fro03" # 5-daily output from 1989:2007 !! some files have more than 73 weeks
    #meshid <- "Agulhas"
    #setting <- ""

    #runid <- "bfsq1"
    #runid <- "CAAc6"
    #meshid <- "CAAc6"
    #setting <- "xxsteps.1y"

    rotate_mesh <- T # my old meshes need to be rotated
    if (grepl("sub", meshid)) { # my subsets from e.g. LabSea
        message("meshid = \"", meshid, "\" --> set global_mesh and cycl to false ...")
        global_mesh <- F 
        cycl <- F
    }

    workpath <- "/work/ba1103/a270073"
    datainpaths <- paste0(workpath, "/out/fesom-1.4/", runid, "/", setting)
    workpath <- "/work/ab0246/a270073"
    meshpath   <- paste0(workpath, "/mesh/", model, "/", meshid) # path of fesom mesh
    derivpath  <- paste0(workpath, "/mesh/", model, "/", meshid, "/derivatives") # path where to save derivative file if wanted
    interppath <- paste0(workpath, "/mesh/", model, "/", meshid, "/interp") # path where to save regular interpolateion matrix if needed
    plotpath   <- paste0(workpath, "/plots/fesom") # path where to save plots if wanted
    workpath <- "/work/ba1103/a270073"
    
    if (meshid == "core") {
        rotate_mesh <- F
        if (grepl("ollie", hostname)) {
            meshpath <- "/work/ollie/pool/FESOM/meshes_default/core"
        } else if (grepl("mistral", hostname)) {
            meshpath <- "/work/ab0995/a270046/meshes_default/core"
        } else if (grepl("levante", hostname)) {
            meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
        }
    }
    if (runid == "bold" && setting == "cpl_output_01") {
        datainpaths <- paste0("/work/ab0995/a270067/fesom_echam/bold/", setting)
    }
    if (runid == "bold" && setting == "cpl_output_470") {
        datainpaths <- paste0("/work/bm0944/a270062/fesom_echam/bold/", setting)
        #meshpath <- "/work/bm0944/a270111/mesh_Agulhas/" ==
        #            "/work/ba0941/a270073/mesh/agulhas"
    }
    if (runid == "fro03") {
        datainpaths <- "/work/bm0944/a270067/fron/fro03/"
        meshpath <- "/work/ab0995/a270067/fesom/fron/mesh_Agulhas/"
    }
    if (runid == "bol01") {
        #datainpaths <- "/work/ab0995/a270067/fesom/bold/bol02/"
        datainpaths <- "/work/ab0995/a270067/fesom/bold/bol01/"
    }
    if ((runid == "CORE2" && setting == "s1") || 
        (runid == "CORE2_ctl" && setting == "s1")) { ## 1st CORE2 spinups in xuezhus directory
            datainpaths <- paste0("/work/ab0246/a270055/output_fesom_alone/", runid)
    }
    if (any(runid == c("Arc22_daily", "Arc22_sub_daily", 
                           "Arc22_sub", "Arc22_sub_small"))) {
        if (grepl("ollie", hostname)) {
            datainpaths <- paste0("/work/ollie/cwekerle/result/", runid)
            meshpath <- paste0("/work/ollie/cwekerle/mesh/", meshid)
        }
    }

    #varname <- "resolutionkm"
    #varname <- "bathy"
    #fpatterns <- paste0(runid, ".<YYYY>.*.nc")
    #varname <- "Ftemp"
    #varname <- "tos"
    #varname <- "tossq"
    #varname <- "temp"
    #fpatterns <- paste0(runid, ".<YYYY>.oce.mean.nc")
    #varname <- "potdens"
    #fpatterns <- paste0(runid, ".<YYYY>.", c("oce", "oce"), ".mean.nc") 
    #varname <- "zossq"
    #varname <- "vertvel"
    #varname <- "tke"
    #varname <- "mke"
    #varname <- "eke"
    #varname <- "mixlay"
    #varname <- "Nsquared"
    #varname <- "gradB"
    varname <- "FeKe"
    #varname <- "HRS"
    #varname <- "VRS"
    #varname <- "PmPe"
    #varname <- "PmPe_wN2"
    #varname <- "wbeddy"
    #varname <- "divuv"
    #varname <- "divuvt"
    #varname <- "divuvteddy"
    #varname <- "uv_bott_force_mean"
    #varname <- "uv_bott_force_eddy"
    #varname <- "tob"
    #varname <- "sob"
    #varname <- "opottempmint"
    #varname <- "somint"
    #varname <- "transport"
    #varname <- "sic"
    #varname <- "iceextent"
    
    #area <- "global"
    #area <- "lsea"
    area <- "LS30l2"
    #area <- "LS30l"
    #area <- "LS20to30l"
    #area <- "LS20to30h"
    #area <- "LShrswbeddy10h"
    #area <- "IceSea3"
    #area <- "csec_ar7w"
    #area <- "csec_S30"
    #area <- "csec_N74"

    depths <- 0
    #depths <- 113
    #depths <- c(0, 100)
    #depths <- c(0, 1400)
    #depths <- c(0, "MLD")
    #depths <- c(0, "max")
    #depths <- "bottom"

    integrate_depth <- F
    #integrate_depth <- T

    years <- 1948
    #years <- 1948:1949
    #years <- 1948:2009
    #years <- 1961:2009
    #years <- 1993:2009

    #season <- "Mar"
    recs <- 1:12
    #recs <- 3
    #recs <- 7
    #recs <- 8
    #recs <- 10
    #recs <- 12
    #recs <- 1:365
    #recs <- 1

    regular_ltm_out <- F
    #regular_ltm_out <- T
    regular_transient_out <- F
    #regular_transient_out <- T
    regular_dx <- regular_dy <- 1/4
    #regular_dx <- regular_dy <- 1/10
    transient_out <- F
    transient_out <- T
    #out_mode <- "select"
    #out_mode <- "fldmean"
    #out_mode <- "meanint"
    out_mode <- "fldint"
    #out_mode <- "depth"
    #out_mode <- "csec_depth"
    #out_mode <- "csec_mean"
    #out_mode <- "area"
    #rms_out <- T
    #sd_out <- T
   
    verbose <- 2
    #verbose <- 3

    # for new rfresom version 
    postprefix <- paste0(runid, "_", setting)
    if (F) { # old directory structure /mode/area/var/ instead of new /mode/var/
        postpath <- paste0(workpath, "/post/old/", runid, "/", setting) # my old phd folder structure
        transientpath <- paste0(postpath, "/", out_mode, "/", area, "/", varname)
        ltmpath <- paste0(postpath, "/ltm/", area, "/", varname)
        reg_transient_outpath <- paste0(postpath, "/regular_grid/", out_mode, "/", area, "/", varname)
        reg_ltm_outpath <- paste0(postpath, "/regular_grid/ltm/", out_mode, "/", area, "/", varname)
    } else if (T) { # new
        postpath   <- paste0(workpath, "/post/fesom")
    }

} # my phd stuff

## do not change below this line
if (interactive()) {
    user_runscript_filename <- normalizePath(sys.frames()[[1]]$ofile)
} else {
    args <- commandArgs(trailingOnly=F)
    user_runscript_filename <- normalizePath(sub("--file=", "", args[grep("--file=", args)]))
}
source(paste0(rfesompath, "/lib/main_rfesom.r")) 

