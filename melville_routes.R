library(ggplot2)
library(ggmap)
library(raster)
library(magrittr)
library(viridis)
library(mapscale)
library(grid)
library(grDevices)
library(prettymapr)
library(colorspace)

setwd("/Users/user/Desktop")

#load the raster map

old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)

mar = c(5.1, 4.1, 4.1, 2.1) 
map<-raster("/Users/user/Desktop/rasterfiles/Melville_DEM_38S.tif")

# Define colors
my_terrain_scale <- terrain.colors(20) # Example, replace with your color scale


# plot map 
plot(map,col=my_terrain_scale,main="",xlim = c(645000, 660000),ylim = c(5732500, 5745000))


# Calculate position for the scale bar
# Assuming the lower left corner for the start of the scale bar
x_start <- 646000 # Adjust as per your map
y_start <- 5734000 # Position for the scale bar, adjust as needed
x_end <- x_start + 1000 # 1 km scale bar, adjust the length as per your requirement

# Draw the scale bar
rect(xleft = x_start, ybottom = y_start - 100, xright = x_end, ytop = y_start, col = "black")

# Add labels for the scale bar
text(x = x_start + 500, y = y_start - 400, labels = "1km", cex = 0.7) # Start label



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


#read in coordinate csv files for dives 1,2,4,5###
getwd()
dive7<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive7meta2.csv")
dive8<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive8meta2.csv")
dive9<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive9meta2.csv")
dive10<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive10meta2.csv")

route<-read.csv("/Users/user/Desktop/metadata_flow/4.+TSPD/Melville_metadata/merged8.csv")

#convert the lat long coordinates in each csv file to UTM to match raster file
crs(bathymap)

#plot route 
coordinates(route)<-c("lon","lat")
proj4string(route)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
routeUTM<- spTransform(route, utm_crs)
routeUTM
routeUTMcoords<-coordinates(routeUTM)
routeUTMcoords
colnames(routeUTMcoords) <- c("UTM northing","UTM easting")


#dive 7 
coordinates(dive7)<-c("lon","lat")
proj4string(dive7)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive7UTM<- spTransform(dive7, utm_crs)
dive7UTM
dive7UTMcoords<-coordinates(dive7UTM)
dive7UTMcoords
colnames(dive7UTMcoords) <- c("UTM northing","UTM easting")

#dive 8
coordinates(dive8)<-c("lon","lat")
coordinates(dive8)
proj4string(dive8)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive8UTM<- spTransform(dive8, utm_crs)
dive8UTM<- spTransform(dive8UTM, utm_crs)
dive8UTM
dive8UTMcoords<-coordinates(dive8UTM)
colnames(dive8UTMcoords) <- c("UTM northing","UTM easting")

#dive 9
coordinates(dive9)<-c("lon","lat")
coordinates(dive9)
proj4string(dive9)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive9UTM<- spTransform(dive9, utm_crs)
dive9UTM
dive9UTMcoords<-coordinates(dive9UTM)
colnames(dive9UTMcoords) <- c("UTM northing","UTM easting")

#dive 10 
coordinates(dive10)<-c("lon","lat")
proj4string(dive10)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive10UTM<- spTransform(dive10, utm_crs)
dive10UTM
dive10UTMcoords<-coordinates(dive10UTM)
colnames(dive10UTMcoords) <- c("UTM northing","UTM easting")


##create a line object from the UTM coordinates for each dive and plot these on map
# line <- Line(routeUTMcoords)
# lines <- Lines(list(line), ID = "1")
# lines_sp <- SpatialLines(list(lines), CRS(proj4string(map)))
# lines_sp
# line_coords<- coordinates(lines_sp)
#  
# plot(lines_sp, add=TRUE, col="black",lwd=2)
# text(x =650000 , y =5737000, labels = "Dive 7", col = "black", cex=0.7)


#dive 7
line <- Line(dive7UTMcoords)
lines <- Lines(list(line), ID = "1")
lines_sp <- SpatialLines(list(lines), CRS(proj4string(map)))
lines_sp
line_coords<- coordinates(lines_sp)

plot(lines_sp, add=TRUE, col="black",lwd=2)
text(x =650500 , y =5737900, labels = "Dive 7", col = "red", cex=0.9,font=2)

#dive8
line8 <- Line(dive8UTMcoords)
lines8 <- Lines(list(line8), ID = "1")
lines_sp8 <- SpatialLines(list(lines8), CRS(proj4string(map)))
lines_sp8

plot(lines_sp8, add=TRUE, col='black',lwd=2)
text(x =652700, y =5741900, labels = "Dive 8", col = "red", cex=0.9, font=2)

#dive9
line9 <- Line(dive9UTMcoords)
lines9 <- Lines(list(line9), ID = "1")
lines_sp9 <- SpatialLines(list(lines9), CRS(proj4string(map)))
lines_sp9
plot(lines_sp9, add=TRUE, col="black",lwd=2)
text(x =653100 , y =5740000, labels = "Dive 9", col = "red", cex=0.9, font=2)

# #dive 4
# 
line10 <- Line(dive10UTMcoords)
lines10 <- Lines(list(line10), ID = "1")
lines_sp10 <- SpatialLines(list(lines10), CRS(proj4string(map)))
lines_sp10
plot(lines_sp10, add=TRUE, col="black",lwd=2)
text(x =654400 , y =5737500, labels = "Dive 10", col = "red", cex=0.9, font=2)


myplot<-recordPlot()
myplot

png("~/Desktop/PLOTS/melvilleROVroutes_plot.png", width = 2000, height = 2000, res = 300)
replayPlot(myplot)
dev.off()


# add points on the map showing where the fishing litter is 

fishing_litter <- data.frame(
  lat = c(-38.47142, -38.46826, -38.46798, -38.47678, -38.46623, -38.47576, -38.47635),
  lon = c(46.76227, 46.76155, 46.76149, 46.74496, 46.75907, 46.74537, 46.74513)
)


coordinates(fishing_litter) <- c("lon", "lat")
proj4string(fishing_litter) <- CRS("+proj=longlat +datum=WGS84")

# need to convert to a spatial object 
litterUTM <- spTransform(fishing_litter, utm_crs)
litterUTMcoords <- coordinates(litterUTM)

points(litterUTMcoords, pch = 18, col = "black", cex = 1.8)
points(litterUTMcoords, pch = 18, cex = 1.25, col = "red", bg = "red")  
