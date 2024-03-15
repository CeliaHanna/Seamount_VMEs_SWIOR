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
library(maptools)

setwd("/Users/user/Desktop")

#load the raster map

bathymap<-raster("~/Desktop/rasterfiles/Atlantis_25m_v3_interp.tif")


# Plot the map using terrain colours 

my_terrain_scale <- terrain.colors(n = 20)

myplot<- plot(bathymap, main="", col=my_terrain_scale,xlim=c(510000, 550000), ylim=c(6360000, 6390000))


# Calculate position for the scale bar
# Assuming the lower left corner for the start of the scale bar
x_start <- 520000 # Adjust as per your map
y_start <- 6370000 # Position for the scale bar, adjust as needed
x_end <- x_start + 1000 # 1 km scale bar, adjust the length as per your requirement

# Draw the scale bar
rect(xleft = x_start, ybottom = y_start - 100, xright = x_end, ytop = y_start, col = "black")

# Add labels for the scale bar
text(x = x_start + 500, y = y_start - 600, labels = "1km", cex = 0.7) # Start label


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



#read in coordinate csv files for dives 15,16,17###
getwd()
dive15<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive15meta2.csv")
dive16<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive16meta2.csv")
dive17<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive17meta2.csv")


#convert the lat long coordinates in each csv file to UTM to match raster file

#dive 15 
coordinates(dive15)<-c("lon","lat")
proj4string(dive15)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=40 +south +datum=WGS84 +units=m +no_defs")
dive15UTM<- spTransform(dive15, utm_crs)
dive15UTM
dive15UTMcoords<-coordinates(dive15UTM)
dive15UTMcoords
colnames(dive15UTMcoords) <- c("UTM northing","UTM easting")

#dive 16
coordinates(dive16)<-c("lon","lat")
coordinates(dive16)
proj4string(dive16)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=40 +south +datum=WGS84 +units=m +no_defs")
dive16UTM<- spTransform(dive16, utm_crs)
dive16UTM
dive16UTMcoords<-coordinates(dive16UTM)
colnames(dive16UTMcoords) <- c("UTM northing","UTM easting")

#dive 17 
coordinates(dive17)<-c("lon","lat")
proj4string(dive17)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=40 +south +datum=WGS84 +units=m +no_defs")
dive17UTM<- spTransform(dive17, utm_crs)
dive17UTM
dive17UTMcoords<-coordinates(dive17UTM)
colnames(dive17UTMcoords) <- c("UTM northing","UTM easting")


##create a line object from the UTM coordinates for each dive and plot these on map
#dive 15
line <- Line(dive15UTMcoords)
lines <- Lines(list(line), ID = "1")
lines_sp <- SpatialLines(list(lines), CRS(proj4string(bathymap)))
lines_sp
line_coords<- coordinates(lines_sp)

plot(lines_sp, add=TRUE, col="black",lwd=2)
text(x =529500 , y =6380624, labels = "Dive 15", col = "red", cex=0.9,font=2)

#dive 16
line2 <- Line(dive16UTMcoords)
lines2 <- Lines(list(line2), ID = "1")
lines_sp2 <- SpatialLines(list(lines2), CRS(proj4string(bathymap)))
lines_sp2

plot(lines_sp2, add=TRUE, col='black',lwd=2)
text(x =521100, y =6380000, labels = "Dive 16", col = "red", cex=0.9,font=2)

#dive 17
line3 <- Line(dive17UTMcoords)
lines3 <- Lines(list(line3), ID = "1")
lines_sp3 <- SpatialLines(list(lines3), CRS(proj4string(bathymap)))
lines_sp3
plot(lines_sp3, add=TRUE, col="white",lwd=2)
text(x =529400 , y =6382500, labels = "Dive 17", col = "red", cex=0.9, font=2)

myplot<-recordPlot()
myplot

png("~/Desktop/atlantisROVroutes.png", width = 2500, height = 2000, res = 300)
replayPlot(myplot)
dev.off()
