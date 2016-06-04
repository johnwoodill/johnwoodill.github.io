########################################################################################################
#-------------------------------------------------------------------------------------------------------
# Author    : A. John Woodill
# Date      : 1/26/2015
# Filename  : California.R
# Code      : Precipitation Spatial Post for Website
# Specifics : Merges mean precipitation for California plotted with Spatial object in ggplot2
#-------------------------------------------------------------------------------------------------------
########################################################################################################

library(plyr)
library(dplyr)
library(ggplot2)
library(automap)   # For Spacial Data
library(akima)     # For interpolation

#------------------------------------------------------
# Data wrangling of PRISM data and Spatial Objects
#------------------------------------------------------
  CA_2000_appt <- read.csv("CA_2000_appt.csv")
  sub_data = as.data.frame(CA_2000_appt)
  coord_vars = c("latitude","longitude")
  data_vars = setdiff(colnames(sub_data), coord_vars)         # All colnames except for Lat/Long
  sp_points = SpatialPoints(sub_data[,coord_vars])            # All lat/long in Spatial Point format
  sp_df = SpatialPointsDataFrame(sp_points,           # Spatial Data Frame of lat/long and remaining cols
                                 sub_data[,data_vars,drop=FALSE])    
  regions <- c("california")
  map_base_data <- filter(map_data("state"), region == "california")   # Get California lat/long coord.
  map_base_data <- rename(map_base_data, longitude = long)             # Rename columns
  map_base_data <- rename(map_base_data, latitude = lat)
  
  # Createsa Spatial Polygon for each state with lat/long
  # ---> Used for plotting the map
  
  state_regions = function(x) {        # Gets lat/long for each state to build Spatial Polygon
    state = unique(x$region)
    print(state)
    Polygons(list(Polygon(x[,c("latitude","longitude")])),ID=state)
  }
  
  state_pg = SpatialPolygons(dlply(map_base_data,     # Builds a Spatial Polygon of all state/regions
                                   .(region), 
                                   state_regions))  

  #------------------------------------------------------
  # Spline interpolation with akima package
  #------------------------------------------------------
  
  fld = with(sub_data, interp(x = longitude, y = latitude, z = APPT, duplicate="median",
                              xo=seq(min(map_base_data$longitude), 
                                     max(map_base_data$longitude), length = 100),
                              yo=seq(min(map_base_data$latitude), 
                                     max(map_base_data$latitude), length = 100),
                              extrap=TRUE, linear=FALSE))
  melt_x = rep(fld$x, times=length(fld$y))                            # Pull out longitude values from list
  melt_y = rep(fld$y, each=length(fld$x))                             # Pull out latitude values from list
  melt_z = as.vector(fld$z)                                           # Pull out appt values from list
  level_data = data.frame(longitude=melt_x,                           # Build data.frame
                          latitude=melt_y, 
                          APPT=melt_z)                                        
  interp_data = na.omit(level_data)                                   # Remove all NA values
  grid_points = SpatialPoints(interp_data[,2:1])                      # Build Spatial Points into object
  in_points = !is.na(over(grid_points,state_pg))  # Logical determining points inside all regions and not NA
  inside_points = interp_data[in_points, ]        # Removes all points outside of Spatial Polygons
  
  #------------------------------------------------------
  # Plot Spatial Objects with ggplot2
  #------------------------------------------------------
  
  map_base_aesthetics = aes(x=longitude, y=latitude, group=group)    # Aesthetics for ggplot2
  map_base = geom_polygon(data=map_base_data, map_base_aesthetics)   # Map Base
  borders = geom_polygon(data=map_base_data,                         # Draws boundaries
                         map_base_aesthetics, 
                         color="black", fill=NA)    
  
  ggplot(data=inside_points, aes(x=longitude, y=latitude))  +      # Setup ggplot2
    geom_tile(aes(fill=APPT)) +                                    # Initial overlay of appt
    stat_contour(aes(z=APPT)) +                                    # Create contours for concentrations
    coord_equal() +                                                # Equalize plots
    scale_fill_gradient2(low="blue", mid="white",high="darkgreen", # Set colors for low, mid, high
                         midpoint=mean(inside_points$APPT)) +
    ggtitle("California Precipitation - 2000") +                  # Plot title
    borders                                                       # Draw California border


