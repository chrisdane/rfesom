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
