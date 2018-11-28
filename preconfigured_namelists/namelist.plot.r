#############################
## User input for rfesom.r ##
#############################

# plot device options
plot_map <<- T # plot a horizontal map of time-averaged 2D or depth-averaged/integrated 3D data?
plot_csec <<- F # make a plot of the location of the cross secion if transient_mode == "csec_mean" or "csec_depth"?
plot_type <<- "interp" #"const" # "const" or "interp" 
                        ## "interp" for spatial interpolation using akima::interp (can be very slow if mesh is huge)
                        ## "const" for constant colors in each data polygon (fastest)
                        ## "interp2" testing
                        ## note: "interp" plots landmasses which do not represent the land masses
                        ##       associated with the respective mesh.
                        ##       For seeing the actual land masses, use "const".
plot_file <<- "png" # "png" or "pdf"
plot_size <<- c(2666, 2666)
dpi <<- 400

# if plot_type == "interp"
interp_method    <<- "bilinear" ## "bilineaar" or "bicubic_spline"
                                ## note: "bicubic_spline" can be veeeery slow if mesh is huge
interp_dlon_plot <<- "auto" ## dlon and dlat in degrees or "auto" 
interp_dlat_plot <<- "auto" ## if "auto", the median resolution of 
                            ## the irregular fesom mesh in 'area' is used

# if plot_type == "const"
elem_border_col <<- NA ## "color" or "#fffff" or NA for no border (default); only applies if plot_type="const"

# what to add to plot
continentborders <<- F # only applies if plot_type="const"
                ## If TRUE, the built-in dataset "world" is used for drawing coast lines 
                ## If FALSE, the plot plane is colored completely in 'land_col' prior to
                ## to plotting the data polygons. 
fill_continent <<- T # fill countries with 'land_col' color? only applies if plot_type="const"
land_col <<- "gray65"
landborder_col <<- land_col # "black"
fill_ocean <<- F # fill ocean with 'ocean_col' color?
ocean_col <<- land_col #"white"
bg_col <<- "white" #"transparent" ## bg color. if "transparent", choose "png" as 'plot_file'
grid_plot <<- T # add a grid to plot?
grid_labels <<- F # plot grid coordinate labels derived by built-in function map.grid()?
                  # (default is false because this is pretty ugly...)
plot_title <<- T # plot title?
xyaxis_labels <<- T # "Longitude" and "Latitude" as axis labels?
quiver_tag  <<- F # plot u- and v- quivers in case of a vector variable
                  # note: if TRUE, 'uv_out' needs to set to TRUE as well
quiver_mode <<- 3 # 1: a quiver for every node (may result in a plot full of quivers
                  #    if the mesh resolution is high)
                  # 2: a quiver for every element (mean over 3 nodes)
                  # 3: a quiver for every degree interval 'quiver_degree_intervall'
quiver_degree_intervall <<- 1.5 #0.5 # [degree] in case of quiver_mode == 3
quiver_thr <<- 0 #80 #0.04 # [m s^(-1)]; set to 0 to plot all quivers
quiver_scale_fac <<- 0.05 #25 # play around with scaling arrow length
quiver_arrow_edge_angle <<- 10 # [degree]
quiver_arrow_edge_length <<- 0.05
quiver_legend_velocity  <<- 0.04 # [unit of variable]

# which font to use
font_family <<- "Droid Sans Mono"
if (any(search() == "package:extrafont")) {
    if (!any(fonts() == font_family)) {
        font_family <<- "sans" # the default 
    }
}

## Colorbar Options
horizontal <<- F
ncolors <<- 11 #15 #18 #300 #16 #300 #300 #15 #500 # number of colors that make up the color bar
yaxis_label_roundfac <<- 2 # how many decimal digits in colorbar labels 
max_legend_labs <<- 13
#pal <<- colorRampPalette(c("white", "blue", "darkgreen", "yellow", "orange"))
#pal <<- colorRampPalette(c("white", "yellow", "orange", "red", "darkred"))
#pal <<- colorRampPalette(c("lightblue1", "blue", "red", "darkred"))
#pal <<- colorRampPalette(c("blue", "cyan", "darkgreen", "green", "yellow", "orange", "red", "darkred"))
pal <<- colorRampPalette(c("white", "#ccede1", "#99dbc4", "#3ebb75", "#4cbc38",
                           "#c1df13", "#feef00", "#fabc09", "#f14b1c", "#f47486"))
#pal <<- colorRampPalette(c("plum1", "mediumorchid4", "slateblue4", "darkgreen", "lightskyblue1", 
#                          "darkseagreen2", "chartreuse2", "yellow", "orange", "darkred", "black"))
#pal <<- colorRampPalette(c(ocean_col, "plum1", "mediumorchid4", "slateblue4", "darkgreen", "lightskyblue1",
#                          "darkseagreen2", "chartreuse2", "yellow", "orange", "darkred", "black"))
anom_color_bar <<- T # if data has both positive and negative values, use a bicolor colorbar
omit_white <<- F # omit (T) or include (F) white in the anomaly color bar
zero_col <<- "white" # colors for positive and negative color ranges
pos_cols <<- c(zero_col, "red")
neg_cols <<- c("blue", zero_col)

## Colorbar levels
# default: automatic color levels
levels_abs <<- "zlim" # colorbar is defined later by min/max of the data (= variable 'zlim')
levels_sd <<- "zlim"

# user defined levels overwrite default color levels
if (T) {
    if (varname == "resolutionkm") {
        levels_abs <<- c(5:10, seq(10, 25, b=5), seq(30, 100, b=10), 150, 200, 250)
    }
    if (varname == "ssh") {
        levels_sd <<- c(seq(0, 0.20, b=0.05), 0.5)
    }
}

