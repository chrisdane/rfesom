# fesom

# fesom2

```bash
cd /mnt/lustre02/work/ba0989/a270077/PalModII/mis3/boundary_conditions/glac1d21_classic/fesom/mesh_CORE2_GLAC1D21
head nod2d.out
92416
1 -170.812870990052005 -73.894844390784101 1
2 -169.497296685393991 -74.040643526602807 1
3 -169.389569973685013 -73.682652348679198 0
4 -169.040338894926009 -73.329850389021203 0
5 -170.613428261319996 -73.383230093511898 0

head elem2d.out
180757
1 7 8
1 2 3
1 5 7
1 3 5
2 323 3
3 322 4

head aux3d.out
48 # = nz --> nz1 = 47
0.0
-5.0
-10.0
-20.0
-30.0
-40.0
-50.0
```

```r
path <- "/pf/a/a270077/mis3/experiments_production/LIG125/outdata/fesom"
files <- list.files(path, pattern="200001")
dim_list <- list("nod2"=c(), "nz_nod2"=c(), "nz_elem"=c(), "nz1_elem"=c(), "nz1_nod2"=c(), "elem"=c())
for (f in files) { 
    nc <- nc_open(paste0(path, "/", f))
    variable <- names(nc$var)[1]
    dims=names(nc$dim)
    dims=dims[-which(dims=="time")]
    message("file ", f, " variable ", variable, ": ", paste(dims, collapse=","))
    if (all(dims==c("nod2"))) {
        dim_list[["nod2"]] <- c(dim_list[["nod2"]], variable)
    } else if (all(dims==c("nz", "nod2"))) {
        dim_list[["nz_nod2"]] <- c(dim_list[["nz_nod2"]], variable)
    } else if (all(dims==c("nz", "elem"))) {
        dim_list[["nz_elem"]] <- c(dim_list[["nz_elem"]], variable)
    } else if (all(dims==c("nz1", "elem"))) {
        dim_list[["nz1_elem"]] <- c(dim_list[["nz1_elem"]], variable)
    } else if (all(dims==c("nz1", "nod2"))) {
        dim_list[["nz1_nod2"]] <- c(dim_list[["nz1_nod2"]], variable)
    } else if (all(dims=="elem")) {
        dim_list[["elem"]] <- c(dim_list[["elem"]], variable)
    } else {
        stop("dims ", paste(dims, collapse=", "), " not defined")
    }
}

dim_list
#$nod2
# [1] "a_ice"    "alpha"    "atmice_x" "atmice_y" "atmoce_x" "atmoce_y"
# [7] "beta"     "Bo"       "evap"     "fer_C"    "fh"       "fw"      
#[13] "hbl"      "iceoce_x" "iceoce_y" "m_ice"    "m_snow"   "MLD1"    
#[19] "MLD2"     "prec"     "reso"     "runoff"   "snow"     "ssh"     
#[25] "sss"      "sst"      "uice"     "vice"     "vve"     

#$nz_nod2
#[1] "bolus_w" "fer_K"   "Kv"      "N2"      "w"      

#$nz_elem
#[1] "Av"

#$nz1_elem
#[1] "bolus_u" "bolus_v" "u"       "v"      

#$nz1_nod2
#[1] "Redi_K"  "salt"    "slope_x" "slope_y" "slope_z" "temp"   

#$elem
#[1] "tx_sur" "ty_sur"
```


