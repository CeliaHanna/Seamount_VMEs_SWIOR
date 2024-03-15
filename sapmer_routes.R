library(ggplot2)
library(ggmap)
library(raster)
library(magrittr)
library(ggmap)
library(viridis)
library(mapscale)
library(grid)
library(grDevices)
library(prettymapr)
library(colorspace)

setwd("/Users/user/Desktop")

#load the raster map

bathymap<-raster("~/Desktop/rasterfiles/Sapmer_25m_interp.tif")

# plot the map using terrain colours 

myplot<- plot(bathymap, main="", col=my_terrain_scale)


# read in coordinate csv files for dive 14

dive14 <-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive14meta2.csv")

#convert the lat long coordinates in each csv file to UTM to match raster file

#dive 14 
coordinates(dive14)<-c("lon","lat")
coordinates(dive14)
proj4string(dive14)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=39 +south +datum=WGS84 +units=m +no_defs")
dive14UTM<- spTransform(dive14, utm_crs)
dive14UTM
dive14UTMcoords<-coordinates(dive14UTM)
dive14UTMcoords
colnames(dive14UTMcoords) <- c("UTM northing","UTM easting")

## create a line object from the UTM coordinates for each dive and plot these on map
#dive 14
line <- Line(dive14UTMcoords)
lines <- Lines(list(line), ID = "1")
lines_sp <- SpatialLines(list(lines), CRS(proj4string(bathymap)))
lines_sp
line_coords<- coordinates(lines_sp)

plot(lines_sp, add=TRUE, col="black",lwd=2)
text(x =600000 , y =5927800, labels = "Dive 14", col = "red", cex=0.9,font=2)

#add north arrow 
addnortharrow(
  pos = "topright",
  padin = c(0.15, 0.15),
  scale = 0.5,
  lwd = 1,
  border = "black",
  cols = c("white", "black"),
  text.col = "black"
)

# Add 1 km scale bar at the bottom left


# Calculate position for the scale bar
# Assuming the lower left corner for the start of the scale bar
x_start <- 590000 # Adjust as per your map
y_start <- 5915000 # Position for the scale bar, adjust as needed
x_end <- x_start + 1000 # 1 km scale bar, adjust the length as per your requirement


# Draw the scale bar
rect(xleft = x_start, ybottom = y_start - 100, xright = x_end, ytop = y_start, col = "black")

# Add labels for the scale bar
text(x = x_start + 500, y = y_start - 600, labels = "1km", cex = 0.7) # Start label



myplot<-recordPlot()
myplot

png("~/Desktop/PLOTS/SapmerROVroutes.png", width = 2500, height = 2000, res = 300)
replayPlot(myplot)
dev.off()
