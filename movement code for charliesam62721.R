#set working directory
setwd("C:/Users/Mannipetti/Documents/thesisbootcamp/rstudio/movementdata")
#Read in csv file
CharlieSam62721 = read.csv("CharlieSamOllie627-21.csv")
#check over file to ensure proper formatting/colums
#ncol(CharlieSam62721)
#names(CharlieSam62721)
#remove unescessary files (study name, etc)#
#CharlieSam62721 = CharlieSam62721[, c("Year","day", "hours", "individual.local.identifier", "location.long", "location.lat")]
#checkpoint
head(CharlieSam62721)

#as.POSIXlt(CharlieSam62721$timestamp)

## CharlieSam62721$timestamp = CharlieSam62721[, c("year", "day", "hour")] ##
#save as new formatting - this will eventually be w/out the NA values
CharlieSam62721.spdf = CharlieSam62721
summary(CharlieSam62721.spdf)
#check for categories with NA values for removal#
#which(is.na(CharlieSam62721.spdf$location.lat))
#which(is.na(CharlieSam62721.spdf$location.long))
#Remove NA valued rows
#technically not necessary to remove lat and long as one requires the other#
#CharlieSam62721.spdf = CharlieSam62721.spdf[!is.na(CharlieSam62721.spdf$location.long), ]
#CharlieSam62721.spdf = CharlieSam62721.spdf[!is.na(CharlieSam62721.spdf$location.lat), ]

#Convert the data into a spatialpointsdataframe 
CharlieSam62721.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(CharlieSam62721.spdf$location.long,
                                                                         CharlieSam62721.spdf$location.lat)), data=CharlieSam62721.spdf, proj4string =
                                                CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#transform the data from a degree-based coordinate system into meter based
CharlieSam62721.spdf <- spTransform(CharlieSam62721.spdf,
                                   CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0
                              +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#plot spatial object
plot(CharlieSam62721.spdf)

##HOME RANGE ESTIMATION - MINIMUM CONVEX POLYGON

#100 percent MCP based on multiple individuals
CharlieSam62721.mcp = mcp(CharlieSam62721.spdf[, "individual.local.identifier"], percent = 100, unin = "m", unout = "km2")
#plot the home ranges using color to differentiate individuals
plot(CharlieSam62721.mcp, col = c(1:5))
#better plot w axes
plot(CharlieSam62721.mcp, col = c("blue","red", "green", "yellow",
                                 "magenta"), axes = TRUE)
#review the area for each home range (this is 100 percent of points). stored in table
CharlieSam62721.mcp@data
as.data.frame(CharlieSam62721.mcp)
#output a shapefile (maybe for qgis?)
writeOGR(CharlieSam62721.mcp, dsn = "realCharlieSam62721MCP100", layer = "CharlieSam62721MCP100", driver = "ESRI Shapefile")

#HOME RANGE ESTIMATION - KERNEL DENSITY ESTIMATE 

#adhoc distribution for h value (least squares cross later = LCSV)
CharlieSam62721.Khref = kernelUD(CharlieSam62721.spdf[, "individual.local.identifier"], h = "href", grid = 60, extent = .2)
#CharlieSam62721.lcsv = kernelUD(CharlieSam62721.spdf[, "individual.local.identifier"], h = "LSCV", grid = 150)

#plot individually
image(CharlieSam62721.Khref)
#Charlie specific example of generating a raster object for each 
CharlieKDE.rast <- (raster(as(CharlieSam62721.Khref$Charlie, "SpatialPixelsDataFrame")))
#output that file in wd
writeRaster(CharlieKDE.rast, "CharlieKDERast.tif")
#plot pretty KDE raster, oh so nice 
plot(CharlieKDE.rast)
#plot the points for that specific individual on top of aforementioned raster
Charlie <- CharlieSam62721.spdf[CharlieSam62721.spdf$individual.local.identifier == "Charlie", ]
plot(Charlie, add = T, cex = 0.1)

#EXAMPLES OF THIS FOR ALL
#SamKDE.rast <- (raster(as(CharlieSam62721.Khref$Sam, "SpatialPixelsDataFrame")))
#plot(SamKDE.rast)
#Sam <- CharlieSam62721.spdf[CharlieSam62721.spdf$individual.local.identifier == "Sam", ]
#plot(Sam, add = T, cex = 0.1)

#IoKDE.rast <- (raster(as(CharlieSam62721.Khref$Io, "SpatialPixelsDataFrame")))
#plot(IoKDE.rast)
#Io <- CharlieSam62721.spdf[CharlieSam62721.spdf$individual.local.identifier == "Io", ]
#plot(Io, add = T, cex = 0.1)

#KrisKDE.rast <- (raster(as(CharlieSam62721.Khref$Kris, "SpatialPixelsDataFrame")))
#plot(KrisKDE.rast)
#Kris <- CharlieSam62721.spdf[CharlieSam62721.spdf$individual.local.identifier == "Kris", ]
#plot(Kris, add = T, cex = 0.1)

#OllieKDE.rast <- (raster(as(CharlieSam62721.Khref$Ollie, "SpatialPixelsDataFrame")))
#plot(OllieKDE.rast)
#Ollie <- CharlieSam62721.spdf[CharlieSam62721.spdf$individual.local.identifier == "Ollie", ]
#plot(Ollie, add = T, cex = 0.1)


#Currently broken, intended to plot all KDE (.95) on one frame
CharlieSam62721.KDE95 <- getverticeshr(CharlieSam62721.Khref, percent = 95)
plot(CharlieSam62721.KDE95, col = c("blue", "red", "green", "yellow",
                                   "magenta"), axes = TRUE)

#Considering Paths of Animal Movement and Dealing with Temporal Autocorrelation

turt.traj <- as.ltraj(coordinates(CharlieSam62721.spdf), date=CharlieSam62721.spdf$timestamp
                      ,id=CharlieSam62721.spdf$individual.local.identifier)

#Compare Kernel to getvolume
par(mfrow=c(2,1))
par(mar=c(0,0,2,0))
image(CharlieSam62721.Khref[[1]])
title("Output of KernelUD")
xyz = as.image.SpatialGridDataFrame(CharlieSam62721.Khref[[1]])
contour(xyz,add=TRUE)
par(mar=c(0,0,2,0))
image(vud[[1]])
title("Output of getvolumeUD")
xyzv = as.image.SpatialGridDataFrame(vud[[1]])
contour(xyzv,add=TRUE)

##TRY AGAIN HOME RANGE
homerange <- getverticeshr(CharlieSam62721.Khref, percent = 90)
as.data.frame(homerange)
ii = kernel.area(CharlieSam62721.Khref, percent = seq(50,95,by=5))
ii 


CharlieSam62721.spdf$Timestamp =as.POSIXct(strptime(CharlieSam62721.spdf$Timestamp, "%Y-%M-%d %H"))

CharlieSam62721.traj = as.ltraj(coordinates(CharlieSam62721.spdf), date = CharlieSam62721.spdf$Timestamp,
                               id = CharlieSam62721.spdf$individual.local.identifier)
