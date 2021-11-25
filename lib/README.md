# fesom

## mesh resolution in km

1. from [oce_rhs_tra.F90#L261](https://gitlab.dkrz.de/modular_esm/fesom-1.4/-/blob/master/fesom_cpl/oce_rhs_tra.F90#L261)
```
res2=voltriangle(elem2)*1.73e-6
res=sqrt(res2)  !in km 
```
with 1.73 ~ sqrt(3), 1e-6 for km² instead of m² and `voltriangle` the volume of the surface elements (i.e. their area) defined in [oce_mesh_setup.F90#L333](https://gitlab.dkrz.de/modular_esm/fesom-1.4/-/blob/master/fesom_cpl/oce_mesh_setup.F90#L333)
```
do elem=1,myDim_elem2d
   call local_element_def_2D(elem, DET2D, derivative_locbafu_x_2D)
   voltriangle(elem) = abs(DET2D) * Vol2D
enddo
```
with `Vol2d` = 1/2 and `DET2D` calculated in [oce_mesh_setup.F90#L410](https://gitlab.dkrz.de/modular_esm/fesom-1.4/-/blob/master/fesom_cpl/oce_mesh_setup.F90#L410)
```
meancos=cos_elem2D(element)
do i=1,3
   node=elem2D_nodes(i,element)
   local_cart(1,i)=coord_nod2D(1,node) 
   local_cart(2,i)=r_earth * coord_nod2D(2,node) ! scaled cartesian coordinates
end do
  
! Jacobian
do i=1,2
   jacobian2D(:,i)= local_cart(:,i+1)-local_cart(:,1)
   if (jacobian2D(1,i)> domain_length/2.0) jacobian2D(1,i)=jacobian2D(1,i)-domain_length
   if (jacobian2D(1,i)<-domain_length/2.0) jacobian2D(1,i)=jacobian2D(1,i)+domain_length
end do
jacobian2D(1,:)=jacobian2D(1,:)*meancos *r_earth
  
! inverse of jacobian
call matrix_inverse_2x2(jacobian2D, jacobian2D_inv, DET)

! horizontal derivative
der_transp=matmul(transpose(derivative_stdbafu_x_2D), jacobian2D_inv)
derivative_locbafu_x_2D=transpose(der_transp)
```

To get the resolution on nodes and not on elements, calc
```
tmp <- inds <- rep(0, t=nod2d_n)
for (i in 1:elem2d_n) {
    elnodes <- elem2d[,i]
    tmp[elnodes] <- tmp[elnodes] + rep(resolution[i], t=3) # `resolution` is defined on elems
    inds[elnodes] <- inds[elnodes] + 1
}
tmp <- tmp/inds
```

Another form of this method would be (from matlab script of p. scholz)
```
x_kart=Rearth*1000.*cosd(aux_yc).*cosd(aux_xc);
y_kart=Rearth*1000.*cosd(aux_yc).*sind(aux_xc);
z_kart=Rearth*1000.*sind(aux_yc);
vek1=[x_kart(2,:)-x_kart(1,:);y_kart(2,:)-y_kart(1,:);z_kart(2,:)-z_kart(1,:)];
vek2=[x_kart(1,:)-x_kart(3,:);y_kart(1,:)-y_kart(3,:);z_kart(1,:)-z_kart(3,:)];
vek3=[x_kart(3,:)-x_kart(2,:);y_kart(3,:)-y_kart(2,:);z_kart(3,:)-z_kart(2,:)];
%_______________________________________________________________
%Calculate Area of triangles over vectorproduct: A=1/2*|axb|
resol=[sqrt(vek1(1,:).^2 + vek1(2,:).^2 + vek1(3,:).^2);...
       sqrt(vek2(1,:).^2 + vek2(2,:).^2 + vek2(3,:).^2);...  
       sqrt(vek3(1,:).^2 + vek3(2,:).^2 + vek3(3,:).^2)];
%___INTERPOLATE TRIANGLE AREA OF ELEMENTS TO NODE POINTS________
[interp_e2n_mat,interp_e2n_corector] = fesom_e2n_interp(obj);
obj.nodes_2d_resol = sum(resol(interp_e2n_mat).*interp_e2n_corector,2)./sum(interp_e2n_corector,2);
```

2. from [Sein et al. 2017](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017MS001099)
```
The mesh resolution, defined as the square root of twice the area of the triangles ..."
```
, i.e. `res = sqrt(2*vol)`.

The difference between method 1 versus 2 is the factor 1.73 ~ sqrt(3) versus 2, i.e. a 100 km2 surface element would yield a resolution of 13.16 versus 14.14 km.


from `<mesh>.initial.mesh.diag.nc`:
```
        float cluster_area(nodes_2d) ;
                cluster_area:description = "2D mesh cluster area" ;
                cluster_area:units = "m^2" ;
```

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


