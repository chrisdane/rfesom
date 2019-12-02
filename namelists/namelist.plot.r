###########################
## User input for rfesom ##
###########################

# plot device options
plot_map <- T # plot a horizontal map of time-averaged 2D or depth-averaged/integrated 3D data?
plot_type <- "const" #"const" # "const" or "interp" 
                     # "interp":  spatial interpolation using akima::interp (can be very slow if mesh is huge)
                     # "const":   constant colors in each data polygon (fastest)
                     # "interp2": testing
                     # note: "interp" plots landmasses which do not represent the land masses
                     #       of the mesh. For the actual land masses, use "const".
plot_moc_mask <- T # plot moc mask area
plot_csec <- T # make a plot of the location of the cross secion if out_mode == "csec_mean" or "csec_depth"?

plot_file <- "png" # "png" or "pdf"
plot_size <- c(2666, 2666)
dpi <- 400

# if plot_type == "interp"
interp_method    <- "bilinear" ## "bilineaar" or "bicubic_spline"
                                ## note: "bicubic_spline" can be veeeery slow if mesh is huge
interp_dlon_plot <- "auto" ## dlon and dlat in degrees or "auto" 
interp_dlat_plot <- "auto" ## if "auto", the median resolution of 
                            ## the irregular fesom mesh in 'area' is used

# if plot_type="const"
elem_border_col <- NA ## "color" or "#fffff" or NA for no border (default)

# what to add to plot
continentborders <- F # only applies if plot_type="const"
                ## If TRUE, the built-in dataset "world" is used for drawing coast lines 
                ## If FALSE, the plot plane is colored completely in 'land_col' prior to
                ## to plotting the data polygons. 
fill_continent <- T # fill countries with 'land_col' color? only applies if plot_type="const"
land_col <- "gray65"
landborder_col <- land_col # "black"
fill_ocean <- F # fill ocean with 'ocean_col' color?
ocean_col <- land_col #"white"
bg_col <- "white" #"transparent" ## bg color. if "transparent", choose "png" as 'plot_file'
plot_grid <- T # add a grid to plot?
grid_labels <- F # plot grid coordinate labels derived by built-in function map.grid()?
                  # (default is false because this is pretty ugly...)
plot_title <- T # plot title?
xyaxis_labels <- T # "Longitude [°]" and "Latitude [°]" as axis labels?
quiver_tag  <- F # plot u- and v- quivers in case of a vector variable
                  # note: if TRUE, 'uv_out' needs to set to TRUE as well
quiver_mode <- 3 # 1: a quiver for every node (may result in a plot full of quivers
                  #    if the mesh resolution is high)
                  # 2: a quiver for every element (mean over 3 nodes)
                  # 3: a quiver for every degree interval 'quiver_degree_intervall'
quiver_degree_intervall <- 1.5 #0.5 # [degree] in case of quiver_mode == 3
quiver_thr <- 0 #80 #0.04 # [m s^(-1)]; set to 0 to plot all quivers
quiver_scale_fac <- 0.05 #25 # play around with scaling arrow length
quiver_arrow_edge_angle <- 10 # [degree]
quiver_arrow_edge_length <- 0.05
quiver_legend_velocity  <- 0.04 # [unit of variable]

# which font to use
font_family <- "Droid Sans Mono"
if (any(search() == "package:extrafont")) {
    if (!any(fonts() == font_family)) {
        font_family <- "sans" # the default 
    }
}

## Colorbar Options
# defaults for image.plot.pre()
nlevels <- 11 
max_labels <- 11
zlevels <- NULL
cols <- NULL
palname <- NULL
method <- "pretty"
power_min <- NULL
axis.labels <- NULL
axis.round <- NULL
axis.zoom <- F
axis.addzlims <- T
anom_colorbar <- NULL
center_col <- "white"
center_include <- T
horizontal <- F

# old: 
#pal <- colorRampPalette(c("white", "#ccede1", "#99dbc4", "#3ebb75", "#4cbc38",
#                           "#c1df13", "#feef00", "#fabc09", "#f14b1c", "#f47486"))
#pal <- colorRampPalette(c("plum1", "mediumorchid4", "slateblue4", "darkgreen", "lightskyblue1", 
#                          "darkseagreen2", "chartreuse2", "yellow", "orange", "darkred", "black"))
#pal <- colorRampPalette(c(ocean_col, "plum1", "mediumorchid4", "slateblue4", "darkgreen", "lightskyblue1",
#                          "darkseagreen2", "chartreuse2", "yellow", "orange", "darkred", "black"))
