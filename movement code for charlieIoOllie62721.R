#set working directory
setwd("C:/Users/Mannipetti/Documents/thesisbootcamp/rstudio/movementdata")
#Read in csv file
CharlieIoOllie62721 = read.csv("CharlieIoOllie627-21.csv")
#check over file to ensure proper formatting/colums
#ncol(CharlieIoOllie62721)
#names(CharlieIoOllie62721)
#remove unescessary files (study name, etc)#
#CharlieIoOllie62721 = CharlieIoOllie62721[, c("Year","day", "hours", "individual.local.identifier", "location.long", "location.lat")]
#checkpoint
head(CharlieIoOllie62721)

#as.POSIXlt(CharlieIoOllie62721$timestamp)

## CharlieIoOllie62721$timestamp = CharlieIoOllie62721[, c("year", "day", "hour")] ##
#save as new formatting - this will eventually be w/out the NA values
CharlieIoOllie62721.spdf = CharlieIoOllie62721
summary(CharlieIoOllie62721.spdf)
#check for categories with NA values for removal#
#which(is.na(CharlieIoOllie62721.spdf$location.lat))
#which(is.na(CharlieIoOllie62721.spdf$location.long))
#Remove NA valued rows
#technically not necessary to remove lat and long as one requires the other#
#CharlieIoOllie62721.spdf = CharlieIoOllie62721.spdf[!is.na(CharlieIoOllie62721.spdf$location.long), ]
#CharlieIoOllie62721.spdf = CharlieIoOllie62721.spdf[!is.na(CharlieIoOllie62721.spdf$location.lat), ]

#Convert the data into a spatialpointsdataframe 
CharlieIoOllie62721.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(CharlieIoOllie62721.spdf$location.long,
                                                                         CharlieIoOllie62721.spdf$location.lat)), data=CharlieIoOllie62721.spdf, proj4string =
                                                CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#transform the data from a degree-based coordinate system into meter based
CharlieIoOllie62721.spdf <- spTransform(CharlieIoOllie62721.spdf,
                                   CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0
                              +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#plot spatial object
plot(CharlieIoOllie62721.spdf)

##HOME RANGE ESTIMATION - MINIMUM CONVEX POLYGON

#100 percent MCP based on multiple individuals
CharlieIoOllie62721.mcp = mcp(CharlieIoOllie62721.spdf[, "individual.local.identifier"], percent = 95, unin = "m", unout = "km2")
#plot the home ranges using color to differentiate individuals
plot(CharlieIoOllie62721.mcp, col = c(1:5))
#better plot w axes
plot(CharlieIoOllie62721.mcp, col = c("blue","red", "green", "yellow",
                                 "magenta"), axes = TRUE)
#review the area for each home range (this is 100 percent of points). stored in table
CharlieIoOllie62721.mcp@data
as.data.frame(CharlieIoOllie62721.mcp)
#output a shapefile (maybe for qgis?)
writeOGR(CharlieIoOllie62721.mcp, dsn = "realCharlieIoOllie62721MCP100", layer = "CharlieIoOllie62721MCP100", driver = "ESRI Shapefile")

#HOME RANGE ESTIMATION - KERNEL DENSITY ESTIMATE 

#adhoc distribution for h value (least squares cross later = LCSV)
CharlieIoOllie62721.Khref = kernelUD(CharlieIoOllie62721.spdf[, "individual.local.identifier"], h = "href", grid = 150)
#CharlieIoOllie62721.lcsv = kernelUD(CharlieIoOllie62721.spdf[, "individual.local.identifier"], h = "LSCV", grid = 150)

#plot individually
image(CharlieIoOllie62721.Khref)
#Charlie specific example of generating a raster object for each 
CharlieKDE.rast <- (raster(as(CharlieIoOllie62721.Khref$Charlie, "SpatialPixelsDataFrame")))
#output that file in wd
writeRaster(CharlieKDE.rast, "CharlieKDERast.tif")
#plot pretty KDE raster, oh so nice 
plot(CharlieKDE.rast)
#plot the points for that specific individual on top of aforementioned raster
Charlie <- CharlieIoOllie62721.spdf[CharlieIoOllie62721.spdf$individual.local.identifier == "Charlie", ]
plot(Charlie, add = T, cex = 0.1)

#EXAMPLES OF THIS FOR ALL
#SamKDE.rast <- (raster(as(CharlieIoOllie62721.Khref$Sam, "SpatialPixelsDataFrame")))
#plot(SamKDE.rast)
#Sam <- CharlieIoOllie62721.spdf[CharlieIoOllie62721.spdf$individual.local.identifier == "Sam", ]
#plot(Sam, add = T, cex = 0.1)

#IoKDE.rast <- (raster(as(CharlieIoOllie62721.Khref$Io, "SpatialPixelsDataFrame")))
#plot(IoKDE.rast)
#Io <- CharlieIoOllie62721.spdf[CharlieIoOllie62721.spdf$individual.local.identifier == "Io", ]
#plot(Io, add = T, cex = 0.1)

#KrisKDE.rast <- (raster(as(CharlieIoOllie62721.Khref$Kris, "SpatialPixelsDataFrame")))
#plot(KrisKDE.rast)
#Kris <- CharlieIoOllie62721.spdf[CharlieIoOllie62721.spdf$individual.local.identifier == "Kris", ]
#plot(Kris, add = T, cex = 0.1)

#OllieKDE.rast <- (raster(as(CharlieIoOllie62721.Khref$Ollie, "SpatialPixelsDataFrame")))
#plot(OllieKDE.rast)
#Ollie <- CharlieIoOllie62721.spdf[CharlieIoOllie62721.spdf$individual.local.identifier == "Ollie", ]
#plot(Ollie, add = T, cex = 0.1)


#Currently broken, intended to plot all KDE (.95) on one frame
CharlieIoOllie62721.KDE95 <- getverticeshr(CharlieIoOllie62721.Khref, percent = 95)
plot(CharlieIoOllie62721.KDE95, col = c("blue", "red", "green", "yellow",
                                   "magenta"), axes = TRUE)

#Considering Paths of Animal Movement and Dealing with Temporal Autocorrelation

turt.traj <- as.ltraj(coordinates(CharlieIoOllie62721.spdf), date=CharlieIoOllie62721.spdf$timestamp
                      ,id=CharlieIoOllie62721.spdf$individual.local.identifier)

#Compare Kernel to getvolume
par(mfrow=c(2,1))
par(mar=c(0,0,2,0))
image(CharlieIoOllie62721.Khref[[1]])
title("Output of KernelUD")
xyz = as.image.SpatialGridDataFrame(CharlieIoOllie62721.Khref[[1]])
contour(xyz,add=TRUE)
par(mar=c(0,0,2,0))
image(vud[[1]])
title("Output of getvolumeUD")
xyzv = as.image.SpatialGridDataFrame(vud[[1]])
contour(xyzv,add=TRUE)

##TRY AGAIN HOME RANGE
homerange <- getverticeshr(CharlieIoOllie62721.Khref, percent = 95, unout = "km2")
as.data.frame(homerange)
ii = kernel.area(CharlieIoOllie62721.Khref, percent = seq(50,95,by=5),unout = "km2")
ii 


CharlieIoOllie62721.spdf$Timestamp =as.POSIXct(strptime(CharlieIoOllie62721.spdf$Timestamp, "%Y-%M-%d %H"))

CharlieIoOllie62721.traj = as.ltraj(coordinates(CharlieIoOllie62721.spdf), date = CharlieIoOllie62721.spdf$Timestamp,
                               id = CharlieIoOllie62721.spdf$individual.local.identifier)


install.packages("ggmap")
Charlie= read.csv("charlieGPSmap.csv")
Io=read.csv("IoGPSmap.csv")
Ollie=read.csv("OllieGPSplot.csv")
png <- get_map(c(36.88, 0.3685), maptype='satellite', scale=2, zoom=12)
ggmap(png)+
  geom_polygon(data=Charlie, aes(x=Charlie$location.lat, y =Charlie$location.long, fill = "Charlie"), size=3)+
geom_polygon(data=Io, aes(x=Io$location.lat, y =Io$location.long, fill = "Io"), size=3)+
geom_polygon(data=Ollie, aes(x=Ollie$location.lat, y =Ollie$location.long, fill = "Ollie"), size=3)

