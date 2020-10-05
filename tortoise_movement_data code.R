install.packages("adehabitatHR")
install.packages("rgdal")
install.packages("raster")


library(adehabitatHR)
library(rgdal)
library(raster)



# IMPORT DATA AND RUN ANALYSES

#set working directory
setwd("C:/Users/Mannipetti/Documents/thesisbootcamp/rstudio/movementdata")
#Read in csv file
turtle = read.csv("masterTortMove_JLA.csv")
#check over file to ensure proper formatting/colums
ncol(turtle)
names(turtle)
#remove unescessary files (study name, etc)#
turtle = turtle[, c("Year","day", "hours", "individual.local.identifier", "location.long", "location.lat")]
#checkpoint
head(turtle)

as.POSIXlt(turtle$timestamp)

## turtle$timestamp = turtle[, c("year", "day", "hour")] ##
#save as new formatting - this will eventually be w/out the NA values
turtle.spdf = turtle
summary(turtle.spdf)
#check for categories with NA values for removal#
which(is.na(turtle.spdf$location.lat))
which(is.na(turtle.spdf$location.long))
#Remove NA valued rows
#technically not necessary to remove lat and long as one requires the other#
turtle.spdf = turtle.spdf[!is.na(turtle.spdf$location.long), ]
turtle.spdf = turtle.spdf[!is.na(turtle.spdf$location.lat), ]

#Convert the data into a spatialpointsdataframe 
turtle.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(turtle.spdf$location.long,
                                                                turtle.spdf$location.lat)), data=turtle.spdf, proj4string =
                                       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#transform the data from a degree-based coordinate system into meter based
turtle.spdf <- spTransform(turtle.spdf,
                          CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0
                              +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#plot spatial object
plot(turtle.spdf)

##HOME RANGE ESTIMATION - MINIMUM CONVEX POLYGON

#100 percent MCP based on multiple individuals
turtle.mcp = mcp(turtle.spdf[, "individual.local.identifier"], percent = 95, unin = "m", unout = "km2")
#plot the home ranges using color to differentiate individuals
plot(turtle.mcp, col = c(1:5))
#better plot w axes
plot(turtle.mcp, col = c("blue","red", "green", "yellow",
                        "magenta"), axes = TRUE)
#review the area for each home range (this is 100 percent of points). stored in table
turtle.mcp@data
as.data.frame(turtle.mcp)

#output a shapefile (maybe for qgis?)
writeOGR(turtle.mcp, dsn = "realTurtleMCP100", layer = "turtleMCP100", driver = "ESRI Shapefile")

#HOME RANGE ESTIMATION - KERNEL DENSITY ESTIMATE 

#adhoc distribution for h value (least squares cross later = LCSV)
turtle.Khref = kernelUD(turtle.spdf[, "individual.local.identifier"], h = "href", grid = 1000, extent = .5)
#turtle.lcsv = kernelUD(turtle.spdf[, "individual.local.identifier"], h = "LSCV", grid = 150)

#plot individually
image(turtle.Khref)
#Charlie specific example of generating a raster object for each 
CharlieKDE.rast <- (raster(as(turtle.Khref$Charlie, "SpatialPixelsDataFrame")))
#output that file in wd
writeRaster(CharlieKDE.rast, "CharlieKDERast.tif")
#plot pretty KDE raster, oh so nice 
plot(CharlieKDE.rast)
#plot the points for that specific individual on top of aforementioned raster
Charlie <- turtle.spdf[turtle.spdf$individual.local.identifier == "Charlie", ]
plot(Charlie, add = T, cex = 0.1)

#EXAMPLES OF THIS FOR ALL
#SamKDE.rast <- (raster(as(turtle.Khref$Sam, "SpatialPixelsDataFrame")))
#plot(SamKDE.rast)
#Sam <- turtle.spdf[turtle.spdf$individual.local.identifier == "Sam", ]
#plot(Sam, add = T, cex = 0.1)

#IoKDE.rast <- (raster(as(turtle.Khref$Io, "SpatialPixelsDataFrame")))
#plot(IoKDE.rast)
#Io <- turtle.spdf[turtle.spdf$individual.local.identifier == "Io", ]
#plot(Io, add = T, cex = 0.1)

#KrisKDE.rast <- (raster(as(turtle.Khref$Kris, "SpatialPixelsDataFrame")))
#plot(KrisKDE.rast)
#Kris <- turtle.spdf[turtle.spdf$individual.local.identifier == "Kris", ]
#plot(Kris, add = T, cex = 0.1)

#OllieKDE.rast <- (raster(as(turtle.Khref$Ollie, "SpatialPixelsDataFrame")))
#plot(OllieKDE.rast)
#Ollie <- turtle.spdf[turtle.spdf$individual.local.identifier == "Ollie", ]
#plot(Ollie, add = T, cex = 0.1)







#Currently broken, intended to plot all KDE (.95) on one frame
turtle.KDE95 <- getverticeshr(turtle.Khref, percent = 95)
plot(turtle.KDE95, col = c("blue", "red", "green", "yellow",
                          "magenta"), axes = TRUE)

#Considering Paths of Animal Movement and Dealing with Temporal Autocorrelation

turtle.spdf$timestamp =paste(turtle.spdf$Date, turtle.spdf$Time) 

turtle.spdf$timestamp = as.POSIXct(turtle.spdf$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")

turtle.spdf$timestamp

turt.traj <- as.ltraj(coordinates(turtle.spdf), date=turtle.spdf$timestamp
                     ,id=turtle.spdf$individual.local.identifier)

turt.traj


plot(turt.traj[1])

likturt <- liker(turt.traj, sig2 = 10, rangesig1 = c(.01, 1))

turt.bb <- kernelbb(turt.traj, sig1 = c(.1081, .0249, .1616, .0268, .6383),
                   sig2 = 10)

image(turt.bb)

turtbb1 = (raster(as(turt.bb$Charlie, "SpatialPixelsDataFrame")))
turtbb95 =getverticeshr(turt.bb, 95, unout = "km2")
plot(turtbb95[1,], border = "red", lwd=2, add = TRUE)

turtbb95@data
#Compare Kernel to getvolume
par(mfrow=c(2,1))
 par(mar=c(0,0,2,0))
 image(turtle.Khref[[1]])
 title("Output of KernelUD")
 xyz = as.image.SpatialGridDataFrame(turtle.Khref[[1]])
 contour(xyz,add=TRUE)
 par(mar=c(0,0,2,0))
 image(vud[[1]])
 title("Output of getvolumeUD")
 xyzv = as.image.SpatialGridDataFrame(vud[[1]])
 contour(xyzv,add=TRUE)

##TRY AGAIN HOME RANGE
 homerange <- getverticeshr(turtle.Khref, percent = 90)
 as.data.frame(homerange)
 ii = kernel.area(turtle.Khref, percent = seq(50,95,by=5))
ii 


turtle.spdf$Timestamp =as.POSIXct(strptime(turtle.spdf$Timestamp, "%Y-%M-%d %H"))

turtle.traj = as.ltraj(coordinates(turtle.spdf), date = turtle.spdf$Timestamp,
                       id = turtle.spdf$individual.local.identifier)
