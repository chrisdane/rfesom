__rfesom__<br/>

With this [R](https://cran.r-project.org/) tool you can read/post-process/plot FESOM version 1.x ([Danilov et al. 2004](https://www.sciencedirect.com/science/article/pii/S146350030200063X?via%3Dihub); [Wang et al. 2014](https://www.geosci-model-dev.net/7/663/2014/)) data.

Please note that the tool was only tested on linux so far and bugs do exist!
<br/><br/>

__Table of Contents__<br/>
<!--ts-->
   * [Install](#install)
   * [Get R](#get-r)
   * [Demo](#demo)
      * [Demo1](#demo1)
      * [Demo2](#demo2)
      * [Demo3](#demo3)
      * [Demo4](#demo4)
   * [Modify the runscript](#modify-the-runscript)
   * [Help](#help)
      * [R syntax basics](#r-syntax-basics)
      * [Installing new R packages (= libraries)](#installing-new-r-packages--libraries)
         * [Missing header or config files](#missing-header-or-config-files)
         * [Unable to load shared library](#unable-to-load-shared-library)
   * [Contribute](#contribute)
   * [References](#references)
   * [Appendix: Available variables](#appendix-available-variables)

<!-- Added by: a270073, at: Wed Aug 28 22:16:48 CEST 2019 -->

<!--te-->

# Install
Clone this repo with
```
$ git clone --recurse-submodules https://github.com/chrisdane/rfesom.git
```

# Get R
[Download and install R](https://cran.r-project.org/) or load it via `module`:
```
$ module load r # you can check the available modules with 'module avail' 
```

# Demo
Go to `cd rfesom` and run either in an active R session with
```
$ R # from terminal
source("runscripts/demo.run.r") # from within R
```
or from terminal with 
```
$ Rcript runscripts/demo.run.r
``` 
or 
```
$ nohup Rscript runscripts/demo.run.r > demo.run.log 2>&1 &
```
so that the program runs in background and you can close the connection when running a long job.

## Demo1
Saves fesoms mesh resolution in km on regular coordinates as netcdf and saves a spatial plot:
```
$ ncdump -h demo__resolutionkm_ltm_area_lsea_rectangular_regular_dx0.250_dy0.250.nc 
dimensions:
        lon = 96 ;
        lat = 48 ;
variables:
        double resolutionkm(lat, lon) ;
               resolutionkm:units = "km" ;
               resolutionkm:_FillValue = NaN ;
```

## Demo2
Saves 1) 1948 mean, 2) standard deviation (sd) and 3) 12 (monthly) records of ssh on regular coordinates as netcdf and saves a spatial plot of the temporal mean and sd fields.
```
$ ncdump -h demo__monthly_ssh_ltm_area_Jan-Dec_1948_mean_lsea_rectangular_regular_dx0.250_dy0.250.nc
dimensions:
        lon = 96 ;
        lat = 48 ;
        double ssh(lat, lon) ;
               ssh:units = "m" ;
               ssh:_FillValue = NaN ;
        double ssh_sd(lat, lon) ;
               ssh_sd:units = "m" ;
               ssh_sd:_FillValue = NaN ;
$ ncdump -h demo__monthly_ssh_transient_area_Jan-Dec_1948_mean_lsea_rectangular_regular_dx0.250_dy0.250.nc 
dimensions:
        time = 12 ;
        lon = 96 ;
        lat = 48 ;
variables:
        double ssh(time, lat, lon) ;
               ssh:units = "m" ;
               ssh:_FillValue = NaN ;
               ssh:long_name = "Sea Surface Height" ;
```

## Demo3
Saves potential temperature field mean in area "lsea" averaged between 0 and 100 meters depth as netcdf and saves a spatial plot of the temporal and depth mean.
```
$ ncdump -h demo__monthly_temp_transient_mean_Jan-Dec_1948_mean_0-100m_lsea_rectangular.nc
dimensions:
        time = 12 ;
variables:
        double temp_mean(time) ;
               temp_mean:units = "degC" ;
               temp_mean:_FillValue = NaN ;
               temp_mean:long_name = "Potential Temperature" ;
```

## Demo4
Saves potential temperature field mean in area "lsea" as a function of depth as netcdf and saves a spatial plot of the temporal and depth mean.
```
$ ncdump -h demo__monthly_temp_transient_depth_Jan-Dec_1948_mean_0-3600m_lsea_rectangular.nc
dimensions:
        time = 12 ;
        depth = 31 ;
variables:
        double temp_depth(depth, time) ;
               temp_depth:units = "degC" ;
               temp_depth:_FillValue = NaN ;
               temp_depth:long_name = "Potential Temperature" ;
```

# Modify the runscript
Different postprocessing options for different variables (see appendix here and `namelist.var.r`) are available, see `namelist.config.r`:
todo: insert table

Using this tool with your own modified runscript works best if 
* you make a copy of `runscripts/myrunscript.r` into e.g. `rfesom` or whereever you want
* by that, any future `git pull`s will not change your individualized runscript

# Help

## R syntax basics
* any help text for a function can be obtained within R by `?<functionname>`, e.g. `?sum`
* R counts from 1, not zero
* index syntax is `[]`, not `()`. So `mat2[1,2]` yields the 1st row and 2nd column element of the 2d-array `mat2` and `mat3[1:2,,c(4,6,8)]` all entries of the 2nd dimension of the 3d-array `mat3` in the 1st and 2nd row and the 4th, 6th and 8th entries of the 3rd dimension. 
* booleans `TRUE` and `FALSE` can be abbreviated with `T` and `F`
* variable assignment symbol is `<-`, e.g. `a <- 1` (`a = 1` works as well)
* 'not equal' condition is `!=`
* By default, subsetting in R removes redundant dimensions, i.e. the effect of matlabs `squeeze()` is applied automatically. For example
```
a <- array(1:6, c(3,2)); message("a:"); a; message("dim of 2d-array:"); dim(a); message("dim of 1d-subset of 2d-array:"); dim(a[1,]); message("length of 2d-array:"); length(a); message("length of 1d-subset of 2d-array:"); length(a[1,])
a:
     [,1] [,2]
[1,]    1    4
[2,]    2    5
[3,]    3    6
dim of 2d-array:
[1] 3 2
dim of 1d-subset of 2d-array:
NULL
length of 2d-array:
[1] 6
length of 1d-subset of 2d-array:
[1] 2
```
However, `rfesom` uses the 'matlab' way of subsetting:
```
a <- array(1:6, c(3,2)); message("a:"); a; message("dim of 2d-array:"); dim(a); message("dim of 1d-subset of 2d-array:"); dim(a[1,]); message("length of 2d-array:"); length(a); message("length of 1d-subset of 2d-array:"); length(a[1,])
a:
     [,1] [,2]
[1,]    1    4
[2,]    2    5
[3,]    3    6
dim of 2d-array:
[1] 3 2
dim of 1d-subset of 2d-array:
[1] 1 2
length of 2d-array:
[1] 6
length of 1d-subset of 2d-array:
[1] 2
```
For further information check the `drop` argument of the the `[` function: <code>?&grave;[&grave;</code>.

* elements from `list` or `data.frame` objects can be accessed via the `$` notation:
```
list1 <- list()
list1$arr1 <- array(1:6, c(2, 3))
list1$char1 <- c("a", "b", "c")
list1$[tabtab]
st1$arr1   list1$char1
```

## Installing new R packages (= libraries)
Within R, you can install the `ncdf4` package with
```
install.packages("ncdf4")
```
or with
```
install.packages("ncdf4", lib="/my/own/package/directory")
```
if you want to set a path where the package should be installed. The default package installation path is the first entry of `.libPaths()`, i.e. by default the argument `lib=.libPaths()[1]`.

### Missing header or config files
Some packages may need special header files or libraries that can be provided in an active R session as follows:
* package: rgdal 
```
gdal_path <- "/sw/rhel6-x64/gdal-2.1.3-gcc48"
proj4_path <- "/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48"
pkg_libs <- paste0("-Wl,-rpath,", gdal_path, "/lib:", proj4_path, "/lib")
install.packages("rgdal", 
                 configure.args=paste0("--with-gdal-config=", gdal_path, "/bin/gdal-config 
                                        --with-proj-include=", proj4_path, "/include 
                                        --with-proj-lib=", proj4_path, "/lib 
                                        PKG_LIBS=", pkg_libs))
```

* package: proj4
```
gdal_path <- "/sw/rhel6-x64/gdal-2.1.3-gcc48"
proj4_path <- "/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48"
pkg_libs <- paste0("-Wl,-rpath,", gdal_path, "/lib")
install.packages("proj4", 
                 configure.args=paste0("--with-proj-include=", proj4_path, "/include 
                                        --with-proj-lib=", proj4_path, "/lib 
                                        PKG_LIBS=", pkg_libs))
```

* pakage: rgeos
```
export LD_LIBRARY_PATH=/sw/rhel6-x64/geos-3.6.1-gcc48/lib/:$LD_LIBRARY_PATH
```

* package: units
```
export LD_LIBRARY_PATH=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/lib:$LD_LIBRARY_PATH
```

### Unable to load shared library
Package installation from source requires the same compiler that was used for building R. On a supercomputer, this sometimes raises a problem if compilers and/or R were loaded via the default `module load gcc r` command, which loads the current default version numbers which are not neccesarily compatible. Then, running the command given above for installing a package, a typical error looks like
```
libstdc++.so.6: version `CXXABI_1.3.8' not found
```

A solution to this is as follows. First, identify the R executable. Within R, run
```
file.path(R.home(), "bin", "exec", "R")
[1] "/sw/rhel6-x64/r/r-3.5.3-gcc48/lib64/R/bin/exec/R"
```
Then, in the shell, type
```
$ ldd /sw/rhel6-x64/r/r-3.5.3-gcc48/lib64/R/bin/exec/R
        linux-vdso.so.1 =>  (0x00007ffd249ee000)
        libR.so => not found
        libRblas.so => not found
        libgomp.so.1 => /sw/rhel6-x64/gcc/gcc-4.8.2/lib64/libgomp.so.1 (0x00002b5263631000)
        libpthread.so.0 => /lib64/libpthread.so.0 (0x00002b5263840000)
        libc.so.6 => /lib64/libc.so.6 (0x00002b5263a5d000)
        librt.so.1 => /lib64/librt.so.1 (0x00002b5263df1000)
        /lib64/ld-linux-x86-64.so.2 (0x000055c633768000)
```
Apparently, this R binary was build with `gcc-4.8.2`. Now, load the correct `gcc` version with
```
module purge
module load gcc/4.8.2 r 
```
and rerun the package installation given above.

In order to not to run into this problem again and again, I defined the following alias in my `.bashrc`:
```
alias R='echo "module purge ..."; module purge; echo "module load gcc/4.8.2 r ..."; module load gcc/4.8.2 r; echo "module list ..."; module list; R --quiet'
```
Then, running R includes the following:
```
$ R
module purge ...
module load gcc/4.8.2 r ...
module list ...
Currently Loaded Modulefiles:
  1) gcc/4.8.2              2) r/3.5.3
```

# Contribute
Run `$ lib/./set_links.sh` to set git hook links for automatic table of content updates in the `README.md` on `git commit` using the `nobackup` branch of the `github-markdown-toc` repo https://github.com/jordantrizz/github-markdown-toc forked from https://github.com/ekalinin/github-markdown-toc:
```
$ cat lib/set_links.sh
#!/bin/bash

echo
echo "******** run set_links.sh **********"
home=$(git rev-parse --show-toplevel)
echo "ln -s $home/lib/.pre-commit $home/.git/hooks/pre-commit"
ln -s $home/lib/.pre-commit $home/.git/hooks/pre-commit
echo "ln -s $home/lib/.post-commit $home/.git/hooks/post-commit"
ln -s $home/lib/.post-commit $home/.git/hooks/post-commit
echo "******** finish set_links.sh **********"
echo
```

# References  
Danilov, S., G. Kivman, and J. Schröter, 2004: A finite-element ocean model: principles and evaluation. Ocean Modelling, 6 (2), 125–150, doi:10.1016/S1463-5003(02)00063-X.

Wang, Q., S. Danilov, D. Sidorenko, R. Timmermann, C. Wekerle, X. Wang, T. Jung, and J. Schröter, 2014b: The Finite Element Sea Ice-Ocean Model (FESOM) v.1.4: formulation of an ocean general circulation model. Geoscientific Model Development, 7 (2), 663–693, doi:10.5194/gmd-7-663-2014.

# Appendix: Available variables  
This is an automated chapter generated by `rfesom_varlist2readme.r` based on the `longname` definitions in `namelist.var.r`.

| Variable name                                     |
|---------------------------------------------------|
| Sea Surface Temperature |
| Potential Temperature |
| Salinity |
| In Situ Density |
| Potential Density |
| In situ Buoyancy |
| Potential Buoyancy |
| Zonal Velocity |
| Meridional Velocity |
| Horizontal Velocity |
| Zonal Velocity Squared |
| Mean Zonal Velocity Squared |
| Meridional Velocity Squared |
| Mean Meridional Velocity Squared |
| Zonal Geostrophic Velocity |
| Meridional Geostrophic Velocity |
| Horizontal Geostrophic Velocity |
| Horizontal Eddy Momentum Flux |
| SGS Zonal Velocity |
| SGS Meridional Velocity |
| Horizontal SGS Velocity |
| Mean Zonal Advective Temperature Flux |
| Mean Meridional Advective Flux of Temperature |
| Mean Horizontal Advective Flux Temperature Flux |
| Total Horizontal Advective Temperature Flux |
| Eddy Zonal Temperature Flux |
| Eddy Meridional Temperature Flux |
| Eddy Horizontal Temperature Flux |
| Total SGS Zonal Temperature Flux |
| Total SGS Meridional Temperature Flux |
| Total Horizontal SGS Temperature Flux |
| Zonal Advective Flux of Salinity |
| Meridional Advective Flux of Salinity |
| Horizontal Advective Flux of Salinity |
| Zonal Eddy Salinity Flux |
| Meridional Eddy Salinity Flux |
| Norm of Horizontal Eddy Salinity Flux |
| SGS Zonal Salinity Flux |
| SGS Meridional Salinity Flux |
| Total Horizontal SGS Salinity Flux |
| Zonal in situ Density Flux |
| Meridional in situ Density Flux |
| Norm of Horizontal in situ Density Flux |
| Zonal Eddy in situ Density Flux |
| Meridional Eddy in situ Density Flux |
| Norm of Horizontal Eddy in situ Density Flux |
| Relative Vorticity |
| Relative Vorticity / f |
| Relative Vorticity / <code>\|</code>f<code>\|</code> |
| Horizontal Strain (normal part) |
| Horizontal Strain (shear part) |
| Horizontal Strain |
| Squared Relative Vorticity |
| Okubo-Weiss Parameter |
| Ertel Potential Vorticity |
| PV_bc |
| PV_vert |
| Gradient Richardson Number |
| Mean Kinetic Energy |
| Total Kinetic Energy |
| Eddy Kinetic Energy |
| Horizontal Reynolds Stress |
| Vertical Reynolds Stress |
| Kinetic Mean -> Kinetic Eddy Conversion |
| wb (Potential Mean -> Kinetic Mean Conversion) |
| Norm of horizontal mean buoyancy flux |
| Potential Mean -> Potential Eddy Conversion |
| wb (Potential Eddy -> Kinetic Eddy Conversion) |
| Norm of Horizontal Eddy Buoyancy Flux |
| Vertical Velocity |
| grad_h T |
| grad_h B |
| grad_h MLD |
| Sea Surface Height |
| Mixed Layer Depth |
| Buoyancy Frequency Squared |
| Barotropic wavespeed |
| Mode-m baroclinic gravity-wave speed |
| Mode-m baroclinic long rossby-wave speed |
| Horizontal velocity baroclinic m-mode |
| Vertical velocity baroclinic m-mode |
| Vertical Diffusivity |
| Horizontal Diffusivity |
| GM Thickness Diffusivity |
| Passive Tracer |
| Mean Wind Stress Energy |
| Eddy Wind Stress Energy |
| div_h(u_h rho) |
| div_h(u_hrho) |
| div_h(u_h b) |
| div_h(u_hb) |
| div_h(u_sgs_h b) |
| Divergence of mean horizontal temperature flux |
| Divergence of eddy horizontal temperature flux |
| div_h(u_h t) |
| Divergence of total horizontal SGS temperature flux |
| Divergence of mean horizontal SGS temperature flux |
| Divergence of eddy horizontal SGS temperature flux |
| grad_laplace_inv_div_h(u_h t) |
| Divergent part of depth-integrated eddy temperature flux (eq 4 JM02) |
| Zonal divergent part of depth-integrated eddy temperature flux (eq 5 JM02) |
| Meridional divergent part of depth-integrated eddy temperature flux (eq 5 JM02) |
| Divergent part of depth-integrated eddy temperature flux (eq 5 JM02) |
| Divergence of mean horizontal salt flux |
| Divergence of eddy horizontal salt flux |
| Divergence of total horizontal SGS salt flux |
| Divergence of mean horizontal SGS salt flux |
| Divergence of eddy horizontal SGS salt flux |
| dz(wrho) |
| dz(wb) |
| dz(wT) |
| dz(wS) |
| dz(wb) |
| Horizontal Buoyancy Diffusion |
| Vertical Density Diffusion |
| Vertical Buoyancy Diffusion |
| Vertical Temperature Diffusion |
| Vertical Salinity Diffusion |
| Isoneutral Slope x |
| Isoneutral Slope y |
| Isoneutral Slope |
| Isoneutral Slope Squared |
| Mean Bottom Stress Energy |
| Eddy Bottom Stress Energy |
| MOCw |
| MOCv |
| Air Temperature 2m |
| Runoff |
| Freshwater Flux |
| Air Specific Humidity |
| Atmosphere Shortwave Radiation |
| Atmosphere Longwave Radiation |
| Latent Heat Flux To Ocean |
| Sensible Heat Flux To Ocean |
| Net heat flux to ocean |
| Net freshwater flux to ocean |
| Wind Speed |
| Curl of Wind Speed |
| Meridional Wind Stress |
| Zonal Wind Stress |
| Norm of Wind Stress |
| Wind Stress Curl |
| Ekman Pumping |
| Total Wind Energy |
| Mean Wind Energy |
| Eddy Wind Energy |
| Virtual Salt |
| Saltinity Relaxation |
| Drag Coefficient |
| Temperature flux to ocean |
| Salt flux to ocean |
| Salt flux to ocean |
| Thermal density flux to ocean |
| Thermal density flux to ocean |
| Haline density flux to ocean |
| Haline density flux to ocean |
| Density flux to ocean |
| Density flux to ocean |
| Density flux to ocean |
| Thermal buoyancy flux to ocean |
| Thermal buoyancy flux to ocean |
| Haline buoyancy flux to ocean |
| Haline buoyancy flux to ocean |
| Buoyancy flux to ocean |
| Buoyancy flux to ocean |
| Buoyancy flux to ocean |
| Sea Ice Zonal Velocity |
| Sea Ice Meridional Velocity |
| Horizontal Ice Velocity |
| Sea Ice Concentration |
| Sea Ice Thickness |
| Sea Ice Extent |
| Sea Ice Volume |
| Snow Thickness |
| Growth Rate of eff. Ice Thickness |
| Transport |
| Bathymetry |
| Norm of horizontal bathymetry gradient |
| Scalar product of horizontal velocity and horizontal bathymetry gradient times -1 |
| coriolis parameter over depth |
| Resolution |
| Resolution |
| Mesh Area |
| First Barolinic Rossby Radius of Deformation |

