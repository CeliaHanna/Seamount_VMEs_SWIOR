library(ggplot2)
library(ggmap)
library(raster)
library(magrittr)
library(ggsn)
library(maptools)
library(maps)
library(rasterVis)

setwd("/Users/user/Desktop")

#load the raster map
bathymap<-raster("/Users/user/Desktop/rasterfiles/Coral_25m_v4_interp.tif")

#plot the map

myplot<- plot(bathymap, main="", col=my_terrain_scale)

#read in coordinate csv files for dives 1,2,4,5###
getwd()
dive1<-read.csv("/Users/user/Desktop/metadata_flow/original_CORAL/dive1.csv")
dive2<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive2meta2.csv")
dive3<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive3meta2.csv")
dive4<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive4meta2.csv")
dive5<-read.csv("/Users/user/Desktop/trimmed_metafiles2/dive5meta2.csv") 

#convert the lat long coordinates in each csv file to UTM to match raster file

#dive 1 
coordinates(dive1)<-c("lon","lat")
proj4string(dive1)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive1UTM<- spTransform(dive1, utm_crs)
dive1UTM
dive1UTMcoords<-coordinates(dive1UTM)
dive1UTMcoords
colnames(dive1UTMcoords) <- c("UTM northing","UTM easting")

#dive 2
coordinates(dive2)<-c("lon","lat")
coordinates(dive2)
proj4string(dive2)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive2UTM<- spTransform(dive2, utm_crs)
dive2UTM
dive2UTMcoords<-coordinates(dive2UTM)
colnames(dive2UTMcoords) <- c("UTM northing","UTM easting")

#dive 3
coordinates(dive3)<-c("lon","lat")
coordinates(dive3)
proj4string(dive3)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive3UTM<- spTransform(dive3, utm_crs)
dive3UTM
dive3UTMcoords<-coordinates(dive3UTM)
colnames(dive3UTMcoords) <- c("UTM northing","UTM easting")

#dive 4 
coordinates(dive4)<-c("lon","lat")
proj4string(dive4)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive4UTM<- spTransform(dive4, utm_crs)
dive4UTM
dive4UTMcoords<-coordinates(dive4UTM)
colnames(dive4UTMcoords) <- c("UTM northing","UTM easting")

#dive 5
coordinates(dive5)<-c("lon","lat")
proj4string(dive5)<-CRS("+proj=longlat +datum=WGS84") 
utm_crs <- CRS("+proj=utm +zone=38 +south +datum=WGS84 +units=m +no_defs")
dive5UTM<- spTransform(dive5, utm_crs)
dive5UTM
dive5UTMcoords<-coordinates(dive5UTM)
colnames(dive5UTMcoords) <- c("UTM northing","UTM easting")


##create a line object from the UTM coordinates for each dive and plot these on map
#dive 1
line <- Line(dive1UTMcoords)
lines <- Lines(list(line), ID = "1")
lines_sp <- SpatialLines(list(lines), CRS(proj4string(bathymap)))
lines_sp
line_coords<- coordinates(lines_sp)

plot(lines_sp, add=TRUE, col="black",lwd=2)
text(x =327900 , y =5420500, labels = "Dive 1", col = "red", cex=0.9, font = 2)

#dive 2
line2 <- Line(dive2UTMcoords)
lines2 <- Lines(list(line2), ID = "1")
lines_sp2 <- SpatialLines(list(lines2), CRS(proj4string(bathymap)))
lines_sp2

plot(lines_sp2, add=TRUE, col='black',lwd=2)
text(x =320000, y =5417500, labels = "Dive 2", col = "red", cex=0.9, font = 2)

#dive 3
line3 <- Line(dive3UTMcoords)
lines3 <- Lines(list(line3), ID = "1")
lines_sp3 <- SpatialLines(list(lines3), CRS(proj4string(bathymap)))
lines_sp3
plot(lines_sp3, add=TRUE, col="black",lwd=2)
text(x =327800 , y =5419300, labels = "Dive 3", col = "red", cex=0.9, font = 2)

#dive 4
line4 <- Line(dive4UTMcoords)
lines4 <- Lines(list(line4), ID = "1")
lines_sp4 <- SpatialLines(list(lines4), CRS(proj4string(bathymap)))
lines_sp4
plot(lines_sp4, add=TRUE, col="black",lwd=2)
text(x =327000 , y =5417000, labels = "Dive 4", col = "red", cex=0.9, font = 2)

#dive 5
line5 <- Line(dive5UTMcoords)
lines5 <- Lines(list(line5), ID = "1")
lines_sp5 <- SpatialLines(list(lines5), CRS(proj4string(bathymap)))
lines_sp5
plot(lines_sp5, add=TRUE, col="black",lwd=2)
text(x =327500, y =5418500, labels = "Dive 5", col = "red", cex=0.9, font = 2)


# Adding scale bar 

# Calculate position for the scale bar
# Assuming the lower left corner for the start of the scale bar
x_start <- 310000 # Adjust as per your map
y_start <- 5400000 # Position for the scale bar, adjust as needed
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
myplot<-recordPlot()
myplot

png("~/Desktop/coral_ROVroutes.png", width = 2000, height = 2000, res = 300)
replayPlot(myplot)
dev.off()

