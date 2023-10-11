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
    correct_dates <- F # cmorized data does not need date correction
    if (F) {
        datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Oday/omldamax/gn/v20181218"
        fpatterns <- "omldamax_Oday_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    } else if (F) {
        datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Omon/mlotst/gn/v20181218"
        fpatterns <- "mlotst_Omon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    } else if (F) {
        datainpaths <- c("/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Omon/uo/gn/v20181218",
                         "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/piControl/r1i1p1f1/Omon/vo/gn/v20181218")
        fpatterns <- c("uo_Omon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc",
                       "vo_Omon_AWI-CM-1-1-MR_piControl_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc")
    } else if (F) {
        datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/historical/r1i1p1f1/Omon/mlotst/gn/v20181218"
        fpatterns <- "mlotst_Omon_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    } else if (F) {
        datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Oday/omldamax/gn/v20181218"
        fpatterns <- "omldamax_Oday_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY_from>0101-<YYYY_to>1231.nc"
    } else if (F) {
        datainpaths <- "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Omon/mlotst/gn/v20181218"
        fpatterns <- "mlotst_Omon_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    } else if (F) {
        datainpaths <- c("/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Omon/uo/gn/v20181218",
                         "/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/1pctCO2/r1i1p1f1/Omon/vo/gn/v20181218")
        fpatterns <- c("uo_Omon_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc",
                       "vo_Omon_AWI-CM-1-1-MR_1pctCO2_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc")
    } else if (T) {
        datainpaths <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/AWI/AWI-CM-1-1-MR/historical/r1i1p1f1/Omon/tos/gn/v20181218"
        fpatterns <- "tos_Omon_AWI-CM-1-1-MR_historical_r1i1p1f1_gn_<YYYY_from>01-<YYYY_to>12.nc"
    }
    #postprefix <- "awi-cm-1-1-mr_piControl"
    #postprefix <- "awi-cm-1-1-mr_piControl_monmax"
    postprefix <- "awi-cm-1-1-mr_historical"
    #postprefix <- "awi-cm-1-1-mr_1pctCO2_monmax"
    #postprefix <- "awi-cm-1-1-mr_1pctCO2"
    meshid <- "glob"
    meshpath <- paste0("/pool/data/AWICM/FESOM1/MESHES/", meshid)
    #years <- 2710:2712
    #years <- 2710:2730 # awi-cm-1-1-mr piControl-2710:2730 = deck-1910:1930
    years <- 1850
    #years <- 1910:1912
    #years <- 1910:1930 # tcr mean
    #season <- "FMA" # "SON" "FMA"
    #frequency_post <- "monmax" # calc monthly means before any other stuff
    varname <- "tos"
    #varname <- "mlotst" # MLD by sigma_theta
    #varname <- "omldamax" # MLD by mixing scheme
    #varnme <- "siextentn"
    #varname <- "uo"
    #varname <- "hvel"
    #depths <- c(0, 200)
    regular_transient_out <- F
    regular_ltm_out <- F
    transient_out <- T
    out_mode <- "fldmean"

} else if (F) { # awi-esm-1-1-lr_kh800 piControl
    workpath <- "/work/ba1103/a270073"
    #model <- "fesom"
    model <- "recom"
    datainpaths <- "/work/ab1095/a270094/AWIESM/SR_output/outdata/fesom" # chunk 1
    #datainpaths <- "/work/ba1103/a270094/AWIESM/test/outdata/fesom" # chunk 2
    #datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom" # chunk 3
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    #datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/restart/oasis3mct"
    #fpatterns <- "o2a_flux_<YYYY>0101-<YYYY>231"
    postprefix <- "awi-esm-1-1-lr_kh800_piControl"
    meshid <- "core"
    #meshpath <- "/work/ab0995/a270046/meshes_default/core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #years <- 1950:1951
    years <- 1950:2029 # chunk 1: 1950:2029
    #years <- 2030:2685 # chunk 2: 2030:2685
    #years <- 2586:2685 # last 100 years chunk 2
    #years <- 2676:2685 # last 10 years chunk 2
    #years <- 2686
    #years <- 2901:3000
    #years <- 2686:3000 # chunk 3: 2686:3000
    #years <- 3000
    #varname <- "tos"
    #varname <- "thetao"
    #varname <- "bgc02" # dic
    #varname <- "bgc03" # alkalinity
    #varname <- "bgc05" # phyc intracellular carbon concentration in small phytoplankton
    #varname <- "bgc08" # detc carbon concentration in detritus
    #varname <- "bgc12" # doc
    #varname <- "bgc14" # diac intracellular carbon concentration in diatoms
    #varname <- "bgc16" # silica concentration in diatoms
    #varname <- "bgc17" # silicate in detritus
    #varname <- "bgc18" # dissolved silicic acid
    varname <- "benSi" # benthic silicate
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
    #depths <- c(0, "max")
    #integrate_depth <- T

} else if (F) { # awi-esm-1-1-lr_kh800 historical historical2 historical3
    workpath <- "/work/ba1103/a270073"
    #model <- "fesom"
    model <- "recom"
    datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/", model)
    #datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical3/outdata/", model)
    #fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    fpatterns <- "<varname_nc>_fesom_<YYYY>0101.nc"
    #postprefix <- "awi-esm-1-1-lr_kh800_historical_day"
    #postprefix <- "awi-esm-1-1-lr_kh800_historical"
    postprefix <- "awi-esm-1-1-lr_kh800_historical2"
    #postprefix <- "awi-esm-1-1-lr_kh800_historical3"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #cycl <- F
    #years <- 1850:1851
    #years <- 1850:2014
    years <- 1970:2014
    #years <- 1981:2014
    #years <- 1982:2014
    #years <- 1995:2014
    #years <- 1995:2014 # last 20 years
    #years <- 2014
    #varname <- "tos"
    #varname <- "thetao"
    #varname <- "mlotst"
    #varname <- "omldamax"
    #varname <- "CO2f"
    #varname <- "pCO2s"
    #varname <- "dpCO2s"
    varname <- "bgc02" # dic
    #varname <- "bgc03" # talk
    #varname <- "bgc06" # phytoplankton chlorophyll
    #varname <- "bgc12" # doc
    #varname <- "bgc15" # diatom chlorophyll
    #varname <- "bgc16" # silicate in diatoms
    #varname <- "bgc17" # silicate in detritus
    #varname <- "bgc18" # dissolved silicic acid
    #varname <- "benSi" # benthic silicate
    #varname <- "bgc22" # oxygen
    #varname <- "diags3d01" # npp by nanophytoplankton
    #varname <- "diags3d02" # npp by diatoms
    #varname <- "export_detC_100m"
    #frequency <- "monthly"
    #frequency_post <- "yearmean"
    #frequency_post <- "monmean"
    regular_ltm_out <- F
    transient_out <- T
    regular_transient_out <- F
    out_mode <- "select"
    #out_mode <- "fldint"
    #out_mode <- "fldmean"
    #out_mode <- "depth"
    #out_mode <- "areadepth"
    integrate_depth <- T
    #depths <- 0
    #depths <- 100
    depths <- c(0, "max")
    #area <- "g19_NH-HL"
    #area <- "g19_NH-ST"
    #area <- "g19_EQU"
    #area <- "g19_SH-ST"
    #area <- "g19_SH-HL"
    #area <- "nino34"
    #area <- "NH_66"

} else if (F) { # awi-esm-1-1-lr_kh800 ssp126 ssp245 ssp534-over ssp585
    workpath <- "/work/ba1103/a270073"
    #model <- "fesom"
    model <- "recom"
    if (T) { # ssp126
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_ssp126"
    } else if (F) { # ssp245
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp245/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_ssp245"
    } else if (F) { # ssp534-over
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp534-over/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_ssp534-over"
    } else if (F) { # ssp585
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_ssp585"
    }
    fpatterns <- "<varname_nc>_fesom_<YYYY>0101.nc"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    years <- 2015:2019
    #years <- 2015:2100
    #varname <- "tos"
    #varname <- "mlotst"
    #varname <- "omldamax"
    #varname <- "pCO2s"
    #varname <- "dpCO2s"
    varname <- "bgc02" # dic
    #varname <- "bgc03" # talk
    #varname <- "bgc06"
    #varname <- "bgc15"
    #varname <- "NPPtot" # total npp = diags3d01 + diags3d02
    #varname <- "export_detC_100m"
    #frequency_post <- "monmean"
    regular_ltm_out <- F
    transient_out <- T
    regular_transient_out <- F
    out_mode <- "select"
    #out_mode <- "fldmean"
    #out_mode <- "fldint"
    integreate_depth <- T
    #depths <- 0
    #depths <- 100
    depths <- c(0, "max")
    #area <- "g19_NH-HL"
    #area <- "g19_NH-ST"
    #area <- "g19_EQU"
    #area <- "g19_SH-ST"
    #area <- "g19_SH-HL"
    #area <- "nino34"
    #area <- "NH_66"

} else if (F) { # awi-esm-1-1-lr_kh800 piControl_LUtrans1850
    workpath <- "/work/ba1103/a270073"
    model <- "fesom"
    #model <- "recom"
    if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_LUtrans1850/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_piControl_LUtrans1850"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_LUtrans1850_levante/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_levante"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_LUtrans1850_new_r7/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_new_r7"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_LUtrans1850_r8/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_r8"
    } else if (T) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_LUtrans1850_new/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_new"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_LUtrans1850_newb/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_newb"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl_LUtrans1850_no_kmp/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_piControl_LUtrans1850_no_kmp"
    }
    #frequency_post <- "monmean"
    fpatterns <- "<varname>_fesom_<YYYY><MM>01.nc"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #years <- 2951:3062
    years <- 3001:3005
    #years <- 3001:3010
    varname <- "tos"
    #varname <- "bgc16"
    #varname <- "bgc17"
    #varname <- "bgc18"
    #varname <- "benSi"
    #integrate_depth <- T
    #depths <- c(0, "max")
    transient_out <- F
    regular_ltm_out <- T
    regular_transient_out <- F
    out_mode <- "select"
    #out_mode <- "fldmean"
    #out_mode <- "fldint"

} else if (F) { # awi-esm-1-1-lr_kh800 esm-piControl
    workpath <- "/work/ba1103/a270073"
    #model <- "fesom"
    model <- "recom"
    #fpatterns <- "<varname>_fesom_<YYYY><MM>01.nc"
    fpatterns <- "<varname_nc>_fesom_<YYYY><MM>01.nc"
    #fpatterns <- rep("<varname_nc>_fesom_<YYYY><MM>01.nc", t=2)
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
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest"
        fpatterns <- rep("<varname_nc>_fesom_<YYYY><MM>01.nc", t=2)
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_wout_talk_rest2"
        #datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/neg_co2_3879_monthly_restart/outdata/fesom"
        #datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_start3870/outdata/", model)
        #postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_start3870"
    } else if (F) {
        datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl2/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl2"
    } else if (T) {
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_nobio_spinup/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-piControl_nobio_spinup"
    }
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #cycl <- F # test
    #years <- 2686
    #years <- 2686:2688
    #years <- 2686:2760
    #years <- 3001:3150
    #years <- 3151:3156
    #years <- 3151:3168
    #years <- 3151:3207
    #years <- 3151:3227
    #years <- 3208
    #years <- 3208:3210
    #years <- 3208:3264 # part 1/2 nobio stupid monthly runs
    #years <- 3208:3265 
    #years <- 3208:3309
    #years <- 3208:3323
    #years <- 3208:3377
    #years <- 3208:3393
    #years <- 3208:3590
    #years <- 3208:3878 # part 1/2 esm-piControl_wout_talk_rest2 stupid monthly runs of year 3879
    #years <- 3208:3945
    #years <- 3222:3265
    years <- 3266:3945 # part 2/2 nobio stupid monthly runs
    #years <- 3310:3323
    #years <- 3496:3497
    #years <- 3498:3878
    #years <- 3517:3536
    #years <- 3537:3945
    #years <- 3591:3800
    #years <- 3591:3945
    #years <- 3801:3850
    #years <- 3851:3945
    #years <- 3871:3878
    #years <- 3877:3881 # test stupid annual (3877, 3878), monthly (3879), annual (3880, 3881) chunks
    #years <- 3879:3945
    #years <- 3880:3945 # part 2/2 stupid monthly runs of year 3879
    #years <- 3926:3945
    #varname <- "tos"
    #varname <- "sos"
    #varname <- "thetao"
    #varname <- "siarean"
    #varname <- "MOCw"
    #varname <- "CO2f"
    #varname <- "pCO2s"
    #varname <- "dpCO2s"
    #varname <- "pCO2a"
    varname <- "bgc02" # dic
    #varname <- "bgc03" # talk
    #varname <- "bgc05" # phyc intracellular carbon concentration in small phytoplankton
    #varname <- "bgc08" # detc carbon concentration in detritus
    #varname <- "bgc10" # carbon concentration in heterotrophs
    #varname <- "bgc12" # doc
    #varname <- "bgc14" # diac intracellular carbon concentration in diatoms
    #varname <- "bgc16" # silicate diatom
    #varname <- "bgc17" # silicate det
    #varname <- "bgc18" # dissolved silicic acid
    #varname <- "benSi" # benthic silicate
    #varname <- "bgc20" # phycal calcite associated with nanophytoplankton
    #varname <- "bgc21" # detcal calcite associated with detritus
    #varname <- "benC"
    #varname <- "benCalc"
    #varname <- "diags3d01" # npp by nanophytoplankton
    #varname <- "diags3d02" # npp by diatoms
    #varname <- "NPPtot" # total npp = diags3d01 + diags3d02
    #frequency <- "monthly"
    frequency <- "annual"
    #frequency_post <- "yearmean"
    transient_out <- T
    regular_ltm_out <- F
    regular_transient_out <- F
    #regular_dx <- regular_dy <- 1/4
    #out_mode <- "select"
    #out_mode <- "fldint"
    #out_mode <- "fldmean"
    out_mode <- "depth"
    #out_mode <- "depthint"
    #out_mode <- "moc_depth"
    #moc_mask_file <- "/work/ba1103/a270073/mesh/fesom/core/moc_mask_area_mocNA_mesh_core.txt"
    #depths <- 0
    depths <- c(0, "max")
    #integrate_depth <- T
    #area <- "NH"
    #area <- "SH"
    #area <- "g19_NH-HL"
    #area <- "g19_NH-ST"
    #area <- "g19_EQU"
    #area <- "g19_SH-ST"
    #area <- "g19_SH-HL"
    #area <- "mocNA"
    exclude_sic <- F
    sic_varname <- "sic"
    sic_datainpath <- paste0(datainpaths[1], "/../fesom")
    sic_fpattern <- "sic_fesom_<YYYY>0101.nc"

} else if (F) { # awi-esm-1-1-lr_kh800 esm-hist
    workpath <- "/work/ba1103/a270073"
    #model <- "fesom"
    model <- "recom"
    #fpatterns <- "<varname>_fesom_<YYYY><MM>01.nc"
    fpatterns <- "<varname_nc>_fesom_<YYYY><MM>01.nc"
    #fpatterns <- rep("<varname_nc>_fesom_<YYYY><MM>01.nc", t=2)
    datainpaths <- paste0("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/", model)
    postprefix <- "awi-esm-1-1-lr_kh800_esm-hist"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #years <- 2014
    years <- 1970:2014
    #varname <- "NPPtot" # total npp = diags3d01 + diags3d02
    varname <- "export_detC_100m"
    transient_out <- T
    regular_ltm_out <- F
    #out_mode <- "select"
    out_mode <- "fldint"
    #depths <- 0
    depths <- 100
    #depths <- c(0, "max")
    #integrate_depth <- T
    area <- "NH_66"

} else if (F) { # awi-esm-1-1-lr_kh800 esm-ssp126 esm-ssp245 esm-ssp370 esm-ssp534os esm-ssp585
    workpath <- "/work/ba1103/a270073"
    #model <- "fesom"
    model <- "recom"
    if (F) { # esm-ssp126
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp126/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-ssp126"
    } else if (F) { # esm-ssp245
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp245/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-ssp245"
    } else if (F) { # esm-ssp370
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp370/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-ssp370"
    } else if (F) { # esm-ssp534os
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp534os/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-ssp534os"
    } else if (T) { # esm-ssp585
        datainpaths <- paste0("/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp585/outdata/", model)
        postprefix <- "awi-esm-1-1-lr_kh800_esm-ssp585"
    }
    #fpatterns <- "<varname>_fesom_<YYYY><MM>01.nc"
    fpatterns <- "<varname_nc>_fesom_<YYYY><MM>01.nc"
    #fpatterns <- rep("<varname_nc>_fesom_<YYYY><MM>01.nc", t=2)
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    years <- 2015:2100
    #varname <- "NPPtot" # total npp = diags3d01 + diags3d02
    varname <- "export_detC_100m"
    transient_out <- T
    regular_ltm_out <- F
    #out_mode <- "select"
    out_mode <- "fldint"
    #depths <- 0
    depths <- 100
    #depths <- c(0, "max")
    #integrate_depth <- T
    area <- "NH_66"

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

} else if (F) { # mseifert
    workpath <- "/work/ba1103/a270073"
    model <- "fesom"
    if (F) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/piControl_original_bionotzero/outdata/fesom"
        postprefix <- "coccos_piControl_original_bionotzero"
    } else if (F) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/piControl_chris_code/outdata/fesom"
        postprefix <- "coccos_piControl_chris_code"
    } else if (T) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/piControl_original_bugfix/outdata/fesom"
        postprefix <- "coccos_piControl_original_bugfix"
    } else if (F) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/esm-piControl_original/outdata/fesom"
        postprefix <- "coccos_esm-piControl_original"
    } else if (F) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/esm-piControl_original_bionotzero/outdata/fesom"
        postprefix <- "coccos_esm-piControl_original_bionotzero"
    } else if (F) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/esm-piControl_original_start3870/outdata/fesom"
        postprefix <- "coccos_esm-piControl_original_start3870"
    } else if (F) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/esm-piControl_chris_code/outdata/fesom"
        postprefix <- "coccos_esm-piControl_chris_code"
    } else if (F) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/esm-piControl_chris_code_bugfix/outdata/fesom"
        postprefix <- "coccos_esm-piControl_chris_code_bugfix"
    } else if (F) {
        datainpaths <- "/work/ba1103/a270120/out/awicm-1.0-recom-coccos/oceannets/esm-piControl_chris_code_bugfix_orig_fesom_restart/outdata/fesom"
        postprefix <- "coccos_esm-piControl_chris_code_bugfix_orig_fesom_restart"
    }
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    years <- 3001:3009
    varname <- "tos"
    regular_ltm_out <- F
    transient_out <- T
    regular_transient_out <- F
    #out_mode <- "select"
    out_mode <- "fldmean"

} else if (F) { # chinrichs talk 
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

} else if (T) { # anne moree
    workpath <- "/work/ab1095/a270073"
    if (T) { # piControl2
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl2/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_piControl2"
        #years <- 1850:1851
        #years <- 1870:1879
        #years <- 1870:1945
        #years <- 1870:1970
        #years <- 1880:1945
        #years <- 1880:1970
        #years <- 1946:1970
        #years <- 1948:1967
        years <- 1951:1970
        #years <- 1961:1970
    } else if (F) { # historical3
        datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical3/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_historical3"
        years <- 1995:2014
    } else if (F) { # ssp585_2
        datainpaths <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585_2/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_ssp585_2"
        years <- 2080:2099
    }
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #varname <- "tos"
    varname <- "thetao"
    #varname <- "so"
    #varname <- "sos"
    #varname <- "virtual_salt"
    #varname <- "wnet"
    #varname <- "runoff"
    #depths <- 100
    #depths <- 580
    depths <- 2000
    frequency_post <- "monmean"
    #frequency_post <- "yearmean"
    regular_ltm_out <- T
    transient_out <- F
    regular_transient_out <- F
    out_mode <- "select"
    #out_mode <- "fldmean"
    #out_mode <- "fldint"
    #regular_dx <- regular_dy <- 0.1
    #area <- "S60"

} else if (F) { # sofia
    workpath <- "/work/ab1095/a270073"
    if (F) { # 0.1sv = landice
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/wrong_restart/ant_01sv/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_ant_01sv"
    } else if (F) { # 0.1sv no corr = landice no corr
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/wrong_restart/ant_01sv_nocorr/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_ant_01sv_nocorr"
    } else if (F) { # 0.1sv paleo 
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/wrong_restart/antwater/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_antwater"
    } else if (F) { # 1sv paleo
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/wrong_restart/antwater_1sv/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_antwater_1sv"
    } else if (F) { # 10sv paleo
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/wrong_restart/antwater_10sv/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_antwater_10sv"
    } else if (F) { # ptr
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/wrong_restart/ptr_01/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_ptr_01"
    } else if (T) { # fwf
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/fwf_01/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_fwf_01"
    } else if (F) { # runoff
        datainpaths <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/runoff_01/outdata/fesom"
        postprefix <- "awi-esm-1-1-lr_kh800_runoff_01"
    }
    fpatterns <- "<varname>_fesom_<YYYY>0101.nc"
    meshid <- "core"
    meshpath <- "/pool/data/AWICM/FESOM1/MESHES/core"
    #years <- 1870:1879
    #years <- 1870:1899
    #years <- 1870:1917
    #years <- 1870:1967
    #years <- 1870:1970
    #years <- 1880:1917
    #years <- 1880:1967
    #years <- 1900:1967
    #years <- 1880:1970
    #years <- 1918:1970
    #years <- 1948:1967
    years <- 1951:1970
    #years <- 1961:1970
    #years <- 1968:1970
    varname <- "thetao"
    #varname <- "so"
    #varname <- "sos"
    #varname <- "runoff"
    #varname <- "virtual_salt"
    #varname <- "virtual_salt_fwf"
    #varname <- "wnet"
    depths <- 2000
    regular_ltm_out <- T
    transient_out <- F
    frequency_post <- "monmean" # calc monthly means before any other stuff
    out_mode <- "select"
    #out_mode <- "fldmean"
    #out_mode <- "fldint"
    #area <- "S60"

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

### do not change below this line
if (interactive()) {
    user_runscript_filename <- normalizePath(sys.frames()[[1]]$ofile)
} else {
    args <- commandArgs(trailingOnly=F)
    user_runscript_filename <- normalizePath(sub("--file=", "", args[grep("--file=", args)]))
}
source(paste0(rfesompath, "/lib/main_rfesom.r"))

