# rfesom

Hi

With this [R](https://cran.r-project.org/) tool you can read/post-process/plot FESOM version 1.x ([Danilov et al. 2004](https://www.sciencedirect.com/science/article/pii/S146350030200063X?via%3Dihub); [Wang et al. 2014](https://www.geosci-model-dev.net/7/663/2014/)) data.

Please note that the tool was only tested on linux so far and bugs do exist.

## 1 How to run

Clone this repo with
```
$ git clone --recurse-submodules https://github.com/chrisdane/rfesom.git
```

Install R or load via module:
```
$ module load r
```
Start R via
```
$ R
```
and install the `ncdf4` package:
```R
install.packages("ncdf4")
# or
install.packages("ncdf4", lib="/my/own/package/directory")
```
The default package installation path is 
```
.libPaths()
```
Quit R via
```
q()
```

### 1.1 Example dataset

Run `rfesom` either in an active R session with
```R
source("rfesom.r")
```
or via 
```bash
$ Rcript rfesom.r
``` 
or 
```bash
$ nohup Rscript rfesom.r > rfesom.log 2>&1 &
```
in background.

Three files are produced:
1. \*transient\*.nc
2. \*ltm\*.nc
3. *.png

The 1st file contains the sea surface height (SSH) on a regular (longitude, latitude) grid with a cell size of 1/4° (see `regular_dx` and `regular_dy` in `namelists/rfesom.namelist.r`) and with 12 time records (12 months of the year 2009). This file was produced because `varname="ssh"` (check `namelists/rfesom.vardef.r`), `regular_anim_output=T` (T/F for TRUE/FALSE) and `anim_mode="area"`. The "anim" implies that the the post processed FESOM data shall have a time dimension ("transient"). `recs=1:12` and `years=2009` select the time range and the spatial region `area="lsea"` is defined in `namelists/refesom.areadef.r`. Since SSH is a 2D variable, the `depths` argument is ignored.

The 2nd file contains the same as the 1st but the temporal average over the 12 months of the year 2009, i.e. there is no time dimension in the output. This file was produced because `regular_ltm_output=T` ("ltm" for time mean).

The 3rd file is a .png plot (`plot_file=".png"`) of the data of the 2nd file and was produced because `plot_map=T`. Note that `rfesom` only plots temporal averaged data if the selected time period is longer than one 1.

### 1.2 Your own data

Modify the three files `rfesom.namelist.r`, `rfesom.vardef.r` and `rfesom.areadef.r` according to your needs.
If you save your own namelist you need to change the line 
```R
...
source("myown.rfesom.namelist.r")
...
```
in `rfesom.r`. Note that the two files `rfesom.vardef.r` and `rfesom.areadef.r` are already called in `rfesom.namelist.r`.

## References  

Danilov, S., G. Kivman, and J. Schröter, 2004: A finite-element ocean model: principles and evaluation. Ocean Modelling, 6 (2), 125–150, doi:10.1016/S1463-5003(02)00063-X.

Wang, Q., S. Danilov, D. Sidorenko, R. Timmermann, C. Wekerle, X. Wang, T. Jung, and J. Schröter, 2014b: The Finite Element Sea Ice-Ocean Model (FESOM) v.1.4: formulation of an ocean general circulation model. Geoscientific Model Development, 7 (2), 663–693, doi:10.5194/gmd-7-663-2014.

## Available variables

(automated list based on namelist.var.r)

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
| Relative Vorticity / |f| |
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

