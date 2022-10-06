#########################################
## runscript for rfesom                ##
## https://github.com/chrisdane/rfesom ##
#########################################

rfesompath <- "~/scripts/r/rfesom" 
fesom_version <- "fesom" # default: fesom (=fesom1)

# any variable already defined in rfesom/namelists/namelist.config.r can be overwritten here

if (F) { # lackermann
    workpath <- "/work/ab0246/a270073"
    model <- "fesom"
    datainpaths <- "/work/ab0246/a270124/esm-experiments/awicm_pism/LIG01/outdata/fesom"
    fpatterns <- "LIG01_fesom_<varname_nc>_<YYYY>0101.nc"
    postprefix <- "LIG01"
    meshpath <- "/work/ab0246/a270064/meshes/CORE2_final"
    meshid <- "CORE2_final"
    rotate_mesh <- T 
    years <- 1935
    #varname <- "resolutionkm"
    varname <- "tos"
    recs <- 1:2
    regular_ltm_out <- T

} else if (F) { # awi-esm-1-1-lr piControl-1855:1954 = deck-1750:1849
    workpath <- "/work/ab0246/a270073"
    model <- "fesom"
    datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/piControl/r1i1p1f1/Omon/tos/gn/v20200212"
    fpatterns <- "<varname>_Omon_AWI-ESM-1-1-LR_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    postprefix <- "awi-esm-1-1-lr_piControl"
    meshpath <- "/work/ab0995/a270046/meshes_default/core"
    meshid <- "core"
    #years <- 1855:1856
    years <- 1855:1954
    #season <- "SON" # "FMA"
    varname <- "tos"
    #varname <- "mlotst"
    regular_ltm_out <- T
    regular_transient_out <- F
    transient_out <- F
    #out_mode <- "fldmean"
    #area <- "LSboening"
    #area <- "mldWeddel"

} else if (F) { # awi-esm-1-1-lr piControl-1955:2104 = deck-1850:1999
    workpath <- "/work/ab0246/a270073"
    model <- "fesom"
    #datainpaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/outdata/fesom"
    #fpatterns <- "PI-CTRL6_fesom_<varname>_<YYYY>0101.nc"
    datainpaths <- rep("/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/outdata/fesom", t=2)
    fpatterns <- c("PI-CTRL6_fesom_uo_<YYYY>0101.nc", "PI-CTRL6_fesom_vo_<YYYY>0101.nc")
    postprefix <- "awi-esm-1-1-lr_piControl"
    #postprefix <- "awi-esm-1-1-lr_piControl_monmax"
    meshpath <- "/work/ab0995/a270046/meshes_default/core"
    meshid <- "core"
    #years <- 2015:2016
    years <- 2015:2035 # awi-esm-1-1-lr piControl-2015:2035 = deck-1910:1930
    #years <- 1955:2104
    #season <- "SON" # "FMA" #"SON"
    #varname <- "mlotst"
    #frequency_post <- "monmean" # calc monthly means before any other stuff
    #varname <- "omldamax"
    #frequency_post <- "monmax"
    #varname <- "siextentn"
    varname <- "hvel"
    depths <- c(0, 200)
    regular_ltm_out <- T
    regular_transient_out <- F
    transient_out <- F
    #out_mode <- "select" # fldmean select
    #area <- "LSboening"
    #area <- "mldWeddel"

} else if (F) { # awi-esm-1-1-lr 1pctCO2
    workpath <- "/work/ab0246/a270073"
    model <- "fesom"
    #datainpaths <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/1percCO2/outdata/fesom"
    #fpatterns <- "1percCO2_fesom_<varname>_<YYYY>0101.nc"
    #datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/1pctCO2/r1i1p1f1/Omon/thetao/gn/v20200212"
    datainpaths <- c("/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/1pctCO2/r1i1p1f1/Omon/uo/gn/v20200212",
                     "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-ESM-1-1-LR/1pctCO2/r1i1p1f1/Omon/vo/gn/v20200212")
    #fpatterns <- "thetao_Omon_AWI-ESM-1-1-LR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc"
    fpatterns <- c("uo_Omon_AWI-ESM-1-1-LR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc",
                   "vo_Omon_AWI-ESM-1-1-LR_1pctCO2_r1i1p1f1_gn_<YYYY_from><MM_from>-<YYYY_to><MM_to>.nc")
    postprefix <- "awi-esm-1-1-lr_1percCO2"
    #postprefix <- "awi-esm-1-1-lr_1percCO2_monmax"
    meshpath <- "/work/ab0995/a270046/meshes_default/core"
    meshid <- "core"
    #years <- 1910
    #years <- 1910:1912
    years <- 1910:1930
    #season <- "SON" # "FMA"
    #varname <- "mlotst"
    #frequency_post <- "monmean" # calc monthly means before any other stuff
    #varname <- "omldamax"
    #frequency_post <- "monmax"
    #varname <- "thetao"
    #varname <- "uo"
    varname <- "hvel"
    #depths <- 0
    depths <- c(0, 200)
    regular_ltm_out <- T

} else if (F) { # awi-esm-1-1-lr lgm
    workpath <- "/work/ab0246/a270073"
    model <- "fesom"
    datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/PMIP/AWI/AWI-ESM-1-1-LR/lgm/r1i1p1f1/Omon/tob/gn/v20200212"
    fpatterns <- "<varname>_Omon_AWI-ESM-1-1-LR_lgm_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    postprefix <- "awi-esm-1-1-lr_lgm"
    meshpath <- "/work/ab0246/a270064/meshes/CORE2_lgmf"
    meshid <- "core"
    rotate_mesh <- T
    years <- 3901
    #years <- 3901:4000
    varname <- "mlotst"
    regular_ltm_out <- T

} else if (F) { # awi-cm-1-1-mr
    workpath <- "/work/ab0246/a270073"
    model <- "fesom"
    #datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Oday/omldamax/gn/v20181218"
    #fpatterns <- "omldamax_Oday_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Omon/mlotst/gn/v20181218"
    #fpatterns <- "mlotst_Omon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    datainpaths <- c("/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Omon/uo/gn/v20181218",
                     "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Omon/vo/gn/v20181218")
    fpatterns <- c("uo_Omon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc",
                   "vo_Omon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc")
    #datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/historical/r1i1p1f1/Omon/mlotst/gn/v20181218"
    #fpatterns <- "mlotst_Omon_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    #datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Oday/omldamax/gn/v20181218"
    #fpatterns <- "omldamax_Oday_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    #datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Omon/mlotst/gn/v20181218"
    #fpatterns <- "mlotst_Omon_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    #datainpaths <- c("/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Omon/uo/gn/v20181218",
    #                 "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Omon/vo/gn/v20181218")
    #fpatterns <- c("uo_Omon_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc",
    #               "vo_Omon_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc")
    postprefix <- "awi-cm-1-1-mr_piControl"
    #postprefix <- "awi-cm-1-1-mr_piControl_monmax"
    #postprefix <- "awi-cm-1-1-mr_historical"
    #postprefix <- "awi-cm-1-1-mr_1pctCO2_monmax"
    #postprefix <- "awi-cm-1-1-mr_1pctCO2"
    meshid <- "glob"
    meshpath <- paste0("/pool/data/AWICM/FESOM1/MESHES/", meshid)
    #years <- 2710:2712
    years <- 2710:2730 # awi-cm-1-1-mr piControl-2710:2730 = deck-1910:1930
    #years <- 1910:1912
    #years <- 1910:1930 # tcr mean
    #season <- "FMA" # "SON" "FMA"
    #varname <- "mlotst" # MLD by sigma_theta
    #varname <- "omldamax" # MLD by mixing scheme
    #frequency_post <- "monmax" # calc monthly means before any other stuff
    #varnme <- "siextentn"
    #varname <- "uo"
    varname <- "hvel"
    depths <- c(0, 200)
    regular_ltm_out <- T

} else if (F) { # awi-esm-1-1-lr_kh800 piControl
    workpath <- "/work/ba1103/a270073"
    #model <- "fesom"
    model <- "recom"
    #datainpaths <- "/mnt/lustre02/work/ab1095/a270094/AWIESM/SR_output/outdata/fesom" # chunk 1
    #datainpaths <- "/work/ba1103/a270094/AWIESM/test/outdata/fesom" # chunk 2
    #fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom" # chunk 3
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    #datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/restart/oasis3mct"
    #fpatterns <- "o2a_flux_<YYYY>0101-<YYYY>231"
    postprefix <- "awi-esm-1-1-lr_kh800_piControl"
    meshid <- "core"
    #meshpath <- "/work/ab0995/a270046/meshes_default/core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #years <- 1950:1951
    #years <- 1950:2029 # chunk 1: 1950:2029
    #years <- 2030:2685 # chunk 2: 2030:2685
    #years <- 2586:2685 # last 100 years chunk 2
    #years <- 2676:2685 # last 10 years chunk 2
    #years <- 2686
    years <- 2901:3000
    #years <- 2686:3000 # chunk 3: 2686:3000
    #years <- 3000
    #varname <- "tos"
    #varname <- "thetao"
    #varname <- "bgc02" # dic
    #varname <- "bgc03" # alkalinity
    #varname <- "bgc05" # phyc intracellular carbon concentration in small phytoplankton
    #varname <- "bgc08" # detc carbon concentration in detritus
    varname <- "bgc12" # doc
    #varname <- "bgc14" # diac intracellular carbon concentration in diatoms
    #varname <- "bgc20" # phycal calcite associated with nanophytoplankton
    #varname <- "bgc21" # detcal calcite associated with detritus
    #varname <- "CO2f"
    #varname <- "NPPd"
    #varname <- "NPPn"
    #varname <- "dpCO2s"
    #varname <- "benC"
    #varname <- "benCalc"
    # oasis:
    #varname <- "heat_oce"
    #varname <- "co2c_oce" # co2 concentration from the atmosphere
    #varname <- "sst_feom"
    #varname <- "co2_feom"
    #frequency_post <- "monmen"
    transient_out <- T
    regular_ltm_out <- F
    regular_transient_out <- F
    #out_mode <- "select"
    #out_mode <- "fldmean"
    out_mode <- "fldint"
    #out_mode <- "depth"
    #out_mode <- "areadepth"
    #depths <- 0
    depths <- c(0, "max")
    integrate_depth <- T

} else if (T) { # awi-esm-1-1-lr_kh800 historical historical2
    workpath <- "/work/ba1103/a270073"
    model <- "fesom"
    #model <- "recom"
    #datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical/outdata/", model)
    datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom"
    #datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/recom"
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    #postprefix <- "awi-esm-1-1-lr_kh800_historical_day"
    #postprefix <- "awi-esm-1-1-lr_kh800_historical"
    postprefix <- "awi-esm-1-1-lr_kh800_historical2"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #cycl <- F
    #years <- 2013:2014
    #years <- 1995:2014
    #years <- 1850:2014
    years <- 1982:2014
    #years <- 1995:2014 # last 20 years
    #years <- 2014
    varname <- "tos"
    #varname <- "thetao"
    #varname <- "mlotst"
    #varname <- "omldamax"
    #varname <- "CO2f"
    #varname <- "pCO2s"
    #varname <- "dpCO2s"
    #varname <- "bgc02" # dic
    #varname <- "bgc03" # talk
    #varname <- "bgc12" # doc
    #varname <- "bgc22" # oxygen
    #varname <- "diags3d01" # npp by nanophytoplankton
    #varname <- "diags3d02" # npp by diatoms
    #frequency <- "monthly"
    #frequency_post <- "yearmean"
    frequency_post <- "monmean"
    regular_ltm_out <- F
    transient_out <- T
    regular_transient_out <- F
    #out_mode <- "select"
    #out_mode <- "fldint"
    out_mode <- "fldmean"
    #out_mode <- "depth"
    #out_mode <- "areadepth"
    #integrate_depth <- T
    #depths <- c(0, "max")
    #area <- "g19_NH-HL"
    #area <- "g19_NH-ST"
    area <- "g19_EQU"
    #area <- "g19_SH-ST"
    #area <- "g19_SH-HL"

} else if (F) { # awi-esm-1-1-lr_kh800 ssp126
    workpath <- "/work/ba1103/a270073"
    model <- "fesom"
    datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/fesom"
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    postprefix <- "awi-esm-1-1-lr_kh800_ssp126"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    years <- 2015:2100
    varname <- "tos"
    #varname <- "mlotst"
    #varname <- "omldamax"
    #varname <- "pCO2s"
    #varname <- "dpCO2s"
    #varname <- "bgc02" # dic
    #varname <- "bgc03" # talk
    #frequency_post <- "monmean"
    regular_ltm_out <- F
    transient_out <- F
    out_mode <- "select"
    #out_mode <- "fldmean"

} else if (F) { # awi-esm-1-1-lr_kh800 ssp585
    workpath <- "/work/ba1103/a270073"
    model <- "fesom"
    #datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585b/outdata/fesom"
    #postprefix <- "awi-esm-1-1-lr_kh800_ssp585b"
    datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585/outdata/fesom"
    postprefix <- "awi-esm-1-1-lr_kh800_ssp585"
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    years <- 2015:2016
    #years <- 2015:2019
    #years <- 2081:2100
    #varname <- "tos"
    varname <- "thetao"
    #varname <- "diags3d01" # npp by nanophytoplankton
    #varname <- "diags3d02" # npp by diatoms
    #frequency_post <- "monmean"
    transient_out <- T
    regular_ltm_out <- F
    regular_transient_out <- F
    #out_mode <- "select"
    #out_mode <- "fldmean"
    #out_mode <- "fldint"
    out_mode <- "depth"
    #out_mode <- "areadepth"
    integrate_depth <- F
    depths <- c(0, "max")

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl
    workpath <- "/work/ba1103/a270073"
    model <- "fesom"
    #model <- "recom"
    if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_2685_1m/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_2685_1m"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_2685/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_2685"
    } else if (F) {
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_2percatm/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_2percatm"
    } else if (F) {
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_co2fsign/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_co2fsign"
    } else if (F) {
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_restartall/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_restartall"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl2/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl2"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest"
    } else if (T) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2"
        #datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/neg_co2_3879_monthly_restart/outdata/fesom"
        #fpatterns <- "<varname>_fesom_<YYYY><MM>01.nc"
    }
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #cycl <- F # test
    #years <- 2686
    #years <- 2686:2688
    #years <- 2686:2760
    #years <- 3151:3156
    #years <- 3151:3168
    #years <- 3151:3227
    #years <- 3208
    #years <- 3208:3590
    #years <- 3496:3497
    years <- 3498:3878
    #years <- 3591:3800
    #years <- 3801:3850
    #frequency <- "monthly"
    varname <- "tos"
    #varname <- "sos"
    #varname <- "thetao"
    #varname <- "siarean"
    #varname <- "CO2f"
    #varname <- "pCO2s"
    #varname <- "dpCO2s"
    #varname <- "pCO2a"
    #varname <- "bgc02" # dic
    #varname <- "bgc03" # talk
    #varname <- "bgc05" # phyc intracellular carbon concentration in small phytoplankton
    #varname <- "bgc08" # detc carbon concentration in detritus
    #varname <- "bgc12" # doc
    #varname <- "bgc14" # diac intracellular carbon concentration in diatoms
    #varname <- "bgc20" # phycal calcite associated with nanophytoplankton
    #varname <- "bgc21" # detcal calcite associated with detritus
    #varname <- "benC"
    #varname <- "benCalc"
    #frequency <- "annual"
    #frequency_post <- "yearmean"
    transient_out <- T
    regular_ltm_out <- F
    regular_transient_out <- F
    #regular_dx <- regular_dy <- 1/4
    #out_mode <- "select"
    #out_mode <- "fldint"
    out_mode <- "fldmean"
    #out_mode <- "depth"
    #depths <- 0
    #depths <- c(0, "max")
    #integrate_depth <- T
    #area <- "NH"
    #area <- "SH"
    #area <- "g19_NH-HL"
    #area <- "g19_NH-ST"
    #area <- "g19_EQU"
    #area <- "g19_SH-ST"
    #area <- "g19_SH-HL"
    exclude_sic <- F
    sic_varname <- "sic"
    sic_datainpath <- paste0(datainpaths[1], "/../fesom")
    sic_fpattern <- "sic_fesom_<YYYY>0101.nc"

} else if (F) { # mhw composite results
    workpath <- "/work/ba1103/a270073"
    model <- "fesom"
    if (T) { # piControl    
        if (T) { # mlotst/omldamax
            #varname <- "mlotst"
            varname <- "omldamax"
            datainpaths <- paste0("/work/ba1103/a270073/post/heatwaveR/composite/", varname)
            if (T) { # data
                fpatterns <- paste0("awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_", varname, "_data.nc")
                postprefix <- "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_data"
            } else if (F) { # seas
                fpatterns <- paste0("awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_", varname, "_seas.nc")
                postprefix <- "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_seas"
            }
        } else if (F) { # tau/curltau/ekmanP
            #varname <- "tau"
            #varname <- "curltau"
            #varname <- "ekmanP"
            varname <- "ekmanP_ms"
            datainpaths <- c("/work/ba1103/a270073/post/heatwaveR/composite/tauuo",
                             "/work/ba1103/a270073/post/heatwaveR/composite/tauvo")
            if (F) { # data
                fpatterns <- c("awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_tauuo_data.nc",
                               "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_tauvo_data.nc")
                postprefix <- "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_data"
            } else if (T) { # seas
                fpatterns <- c("awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_tauuo_seas.nc",
                               "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_tauvo_seas.nc")
                postprefix <- "awi-esm-1-1-lr_kh800_piControl_mhw_tos_ts_26860101-30001231_clim_26860101-30001231_pctile_90_minDuration_5_withTrend_composite_seas"
            }
        }
    }
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    years <- 2842 # average over 2686-1-1 to 3000-12-31
    regular_ltm_out <- T
    out_mode <- "select"

} else if (F) { # claudia hinrichs talk 
    workpath <- "/work/ba1103/a270073"
    model <- "fesom"
    # old ini:
    datainpaths <- "/work/ba1103/a270109/runs/fesom-recom_jra_clim_ctl/outdata/fesom"
    postprefix <- "fesom-recom_jra_clim_ctl"
    # new ini:
    #datainpaths <- "/work/ba1103/a270109/runs/fesom-recom_jra_clim_new_ini/outdata/fesom"
    #postprefix <- "fesom-recom_jra_clim_ctl_new_ini"
    #fpatterns <- "bgc03_fesom_<YYYY>0101.nc"
    fpatterns <- c("thetao_fesom_<YYYY>0101.nc", "so_fesom_<YYYY>0101.nc")
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    years <- 1850
    #recs <- 1:2
    #varname <- "bgc03"
    varname <- "potdens"
    frequency_post <- "monmean"
    regular_ltm_out <- T
    transient_out <- F
    out_mode <- "select"
    #out_mode <- "fldint"
    #out_mode <- "fldmean"
    #out_mode <- "depth"
    depths <- 0
    #depths <- c(3000, 3100)
    #depths <- c(0, "max")

} else if (F) { # reccap
    workpath <- "/work/ollie/cdanek"
    model <- "fesom"
    #datainpaths <- "/work/ollie/frbunsen/model_runs/reccap_A_varCO2_varclim_1958_2019"
    #postprefix <- "reccap_A"
    #datainpaths <- "/work/ollie/frbunsen/model_runs/reccap_B_cstCO2_cstclim_1958_2019"
    #postprefix <- "reccap_B"
    #datainpaths <- "/work/ollie/frbunsen/model_runs/reccap_C_varCO2_cstclim_1958_2019"
    #postprefix <- "reccap_C"
    datainpaths <- "/work/ollie/frbunsen/model_runs/reccap_D_cstCO2_varclim_1958_2019"
    postprefix <- "reccap_D"
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    meshid <- "core"
    meshpath <- "/work/ollie/jhauck/input/meshes/core_new_384"
    years <- 1958:2019
    varname <- "CO2f"
    regular_ltm_out <- F
    transient_out <- T
    #out_mode <- "select"
    out_mode <- "fldint"
    #out_mode <- "fldmean"
    #out_mode <- "depth"

} else if (F) { # ying
    fesom_version <- "fesom2"
    model <- "fesom2"
    #model <- "recom"
    if (F) {
        workpath <- "/work/ollie/cdanek"
        datainpaths <- "/work/ollie/yye/AWIESM_runs/PI_echamcold_test/outdata/fesom"
        postprefix <- "awi-esm-2.0-lr_piControl"
        fpatterns <- "<varname_nc>.fesom.<YYYY>12.01.nc"
        meshid <- "core2"
        meshpath <- "/home/ollie/lniu/workollie/AWIESM/pool/mesh_core2"
    } else if (T) {
        workpath <- "/work/ba1103/a270073"
        datainpaths <- "/work/bm1030/a270105/awiesm-output/pi_ciso_mpi/outdata/fesom"
        fpatterns <- "<varname_nc>.fesom.<YYYY>01.01.nc"
        postprefix <- "awi-esm-2.1-recom-par-tracers_piControl"
        meshid <- "core2"
        meshpath <- "/work/k20200/k202138/bb1029/AWIESM2.1-EXP/mesh_core2"
    }
    rotate_mesh <- F 
    #years <- 2001:3505
    #years <- 3110
    #years <- 3110:3479
    years <- 3440:3500
    #years <- 3400:3505
    #years <- 3505
    varname <- "sst"
    #varname <- "sss"
    #varname <- "temp"
    #varname <- "CO2f"
    #varname <- "DOC"
    frequency_post <- "yearmean"
    transient_out <- F
    regular_ltm_out <- T
    out_mode <- "select"
    #out_mode <- "fldmean"
    #out_mode <- "fldint"
    #depths <- 2.5

} else if (F) { # jstreffing
    workpath <- "/work/ab0246/a270073"
    fesom_version <- "fesom2"
    datainpaths <- "/mnt/lustre01/work/ab0246/a270092/download"
    fpatterns <- "so_first_timestep_lvl1000.nc"
    fnames_user <- "/mnt/lustre01/work/ab0246/a270092/download/so_first_timestep_lvl1000.nc"
    postprefix <- "awi-cm3"
    meshid <- "core"
    meshpath <- "/mnt/lustre01/work/ab0246/a270092/input/fesom2/core2"
    years <- 2049
    varname <- "so"
    depths <- 1000
    #varname <- "sss"
    regular_ltm_out <- T
    transient_out <- F
    out_mode <- "select"
    #out_mode <- "fldmean"

} # which setting

postpath <- paste0(workpath, "/post/", model)
plotpath <- paste0(workpath, "/plots/", model)
derivpath <- paste0(workpath, "/mesh/", fesom_version, "/", meshid, "/derivatives")
interppath <- paste0(workpath, "/mesh/", fesom_version, "/", meshid, "/interp")

### do not change below this line
if (interactive()) {
    user_runscript_filename <- normalizePath(sys.frames()[[1]]$ofile)
} else {
    args <- commandArgs(trailingOnly=F)
    user_runscript_filename <- normalizePath(sub("--file=", "", args[grep("--file=", args)]))
}
source(paste0(rfesompath, "/lib/main_rfesom.r"))

