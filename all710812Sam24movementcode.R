
library(adehabitatHR)
library(rgdal)
library(raster)

#set working directory
setwd("C:/Users/Mannipetti/Documents/thesisbootcamp/rstudio/movementdata")
#Read in csv file
ALL710812SAM24 = read.csv("all710812Sam24.csv")
#check over file to ensure proper formatting/colums
#ncol(ALL710812SAM24)
#names(ALL710812SAM24)
#remove unescessary files (study name, etc)#
#ALL710812SAM24 = ALL710812SAM24[, c("Year","day", "hours", "individual.local.identifier", "location.long", "location.lat")]
#checkpoint
head(ALL710812SAM24)

#as.POSIXlt(ALL710812SAM24$timestamp)

## ALL710812SAM24$timestamp = ALL710812SAM24[, c("year", "day", "hour")] ##
#save as new formatting - this will eventually be w/out the NA values
ALL710812SAM24.spdf = ALL710812SAM24
summary(ALL710812SAM24.spdf)
#check for categories with NA values for removal#
#which(is.na(ALL710812SAM24.spdf$location.lat))
#which(is.na(ALL710812SAM24.spdf$location.long))
#Remove NA valued rows
#technically not necessary to remove lat and long as one requires the other#
#ALL710812SAM24.spdf = ALL710812SAM24.spdf[!is.na(ALL710812SAM24.spdf$location.long), ]
#ALL710812SAM24.spdf = ALL710812SAM24.spdf[!is.na(ALL710812SAM24.spdf$location.lat), ]

#Convert the data into a spatialpointsdataframe 
ALL710812SAM24.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(ALL710812SAM24.spdf$location.long,
                                                                 ALL710812SAM24.spdf$location.lat)), data=ALL710812SAM24.spdf, proj4string =
                                        CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#transform the data from a degree-based coordinate system into meter based
ALL710812SAM24.spdf <- spTransform(ALL710812SAM24.spdf,
                           CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0
                              +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#plot spatial object
plot(ALL710812SAM24.spdf)

##HOME RANGE ESTIMATION - MINIMUM CONVEX POLYGON

#100 percent MCP based on multiple individuals
ALL710812SAM24.mcp = mcp(ALL710812SAM24.spdf[, "individual.local.identifier"], percent = 95, unin = "m", unout = "km2")
#plot the home ranges using color to differentiate individuals
plot(ALL710812SAM24.mcp, col = c(1:5))
#better plot w axes
plot(ALL710812SAM24.mcp, col = c("blue","red", "green", "yellow",
                         "magenta"), axes = TRUE)
#review the area for each home range (this is 100 percent of points). stored in table
ALL710812SAM24.mcp@data
as.data.frame(ALL710812SAM24.mcp)
#output a shapefile (maybe for qgis?)
writeOGR(ALL710812SAM24.mcp, dsn = "realALL710812SAM24MCP100", layer = "ALL710812SAM24MCP100", driver = "ESRI Shapefile")

#HOME RANGE ESTIMATION - KERNEL DENSITY ESTIMATE 

#adhoc distribution for h value (least squares cross later = LCSV)
ALL710812SAM24.Khref = kernelUD(ALL710812SAM24.spdf[, "individual.local.identifier"], h = "href", grid = 150)
#ALL710812SAM24.lcsv = kernelUD(ALL710812SAM24.spdf[, "individual.local.identifier"], h = "LSCV", grid = 150)

#plot individually
image(ALL710812SAM24.Khref)
#Charlie specific example of generating a raster object for each 
CharlieKDE.rast <- (raster(as(ALL710812SAM24.Khref$Charlie, "SpatialPixelsDataFrame")))
#output that file in wd
writeRaster(CharlieKDE.rast, "CharlieKDERast.tif")
#plot pretty KDE raster, oh so nice 
plot(CharlieKDE.rast)
#plot the points for that specific individual on top of aforementioned raster
Charlie <- ALL710812SAM24.spdf[ALL710812SAM24.spdf$individual.local.identifier == "Charlie", ]
plot(Charlie, add = T, cex = 0.1)

#EXAMPLES OF THIS FOR ALL
#SamKDE.rast <- (raster(as(ALL710812SAM24.Khref$Sam, "SpatialPixelsDataFrame")))
#plot(SamKDE.rast)
#Sam <- ALL710812SAM24.spdf[ALL710812SAM24.spdf$individual.local.identifier == "Sam", ]
#plot(Sam, add = T, cex = 0.1)

#IoKDE.rast <- (raster(as(ALL710812SAM24.Khref$Io, "SpatialPixelsDataFrame")))
#plot(IoKDE.rast)
#Io <- ALL710812SAM24.spdf[ALL710812SAM24.spdf$individual.local.identifier == "Io", ]
#plot(Io, add = T, cex = 0.1)

#KrisKDE.rast <- (raster(as(ALL710812SAM24.Khref$Kris, "SpatialPixelsDataFrame")))
#plot(KrisKDE.rast)
#Kris <- ALL710812SAM24.spdf[ALL710812SAM24.spdf$individual.local.identifier == "Kris", ]
#plot(Kris, add = T, cex = 0.1)

#OllieKDE.rast <- (raster(as(ALL710812SAM24.Khref$Ollie, "SpatialPixelsDataFrame")))
#plot(OllieKDE.rast)
#Ollie <- ALL710812SAM24.spdf[ALL710812SAM24.spdf$individual.local.identifier == "Ollie", ]
#plot(Ollie, add = T, cex = 0.1)


#Currently broken, intended to plot all KDE (.95) on one frame
ALL710812SAM24.KDE95 <- getverticeshr(ALL710812SAM24.Khref, percent = 95)
plot(ALL710812SAM24.KDE95, col = c("blue", "red", "green", "yellow",
                           "magenta"), axes = TRUE)

#Considering Paths of Animal Movement and Dealing with Temporal Autocorrelation

turt.traj <- as.ltraj(coordinates(ALL710812SAM24.spdf), date=ALL710812SAM24.spdf$timestamp
                      ,id=ALL710812SAM24.spdf$individual.local.identifier)

#Compare Kernel to getvolume
par(mfrow=c(2,1))
par(mar=c(0,0,2,0))
image(ALL710812SAM24.Khref[[1]])
title("Output of KernelUD")
xyz = as.image.SpatialGridDataFrame(ALL710812SAM24.Khref[[1]])
contour(xyz,add=TRUE)
par(mar=c(0,0,2,0))
image(vud[[1]])
title("Output of getvolumeUD")
xyzv = as.image.SpatialGridDataFrame(vud[[1]])
contour(xyzv,add=TRUE)

##TRY AGAIN HOME RANGE
homerange <- getverticeshr(ALL710812SAM24.Khref, percent = 95, unout = "km2")
as.data.frame(homerange)
hh = kernel.area(ALL710812SAM24.Khref, percent = seq(50,95,by=5), unout = "km2")
hh 


ALL710812SAM24.spdf$Timestamp =as.POSIXct(strptime(ALL710812SAM24.spdf$Timestamp, "%Y-%M-%d %H"))

ALL710812SAM24.traj = as.ltraj(coordinates(ALL710812SAM24.spdf), date = ALL710812SAM24.spdf$Timestamp,
                       id = ALL710812SAM24.spdf$individual.local.identifier)
