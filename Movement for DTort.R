setwd("C:/Users/Mannipetti/Documents/thesisbootcamp/rstudio/movementdata")


DTort = read.csv("dtort2.csv")
head(DTort)

summary(DTort)

DTort.spdf<- SpatialPointsDataFrame(coords=as.data.frame(cbID(DTort$LAT,
                                                     DTort$LONG)), data=DTort, proj4string =
                            CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#transform the data from a degree-based coordinate system into meter based
DTort.spdf <- spTransform(DTort.spdf,
                           CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0
                              +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

DTort.spdf$ï..DATE = as.POSIXct(strptime(DTort.spdf$ï..DATE, "%Y-%m-%d"))

DTort$ï..DATE


plot(DTort.spdf)


head(DTort.spdf)

tort.mcp = mcp(DTort.spdf[,"ID"], percent = 90, unin = "m", unout = "km2")

plot(tort.mcp, col = c(1:6))

tort.mcp@data
as.data.frame(tort.mcp)

  tortKhref = kernelUD(DTort.spdf[,"ID"], h = "href", grid = 150)

image(tortKhref)

#Alex Raster/Track Plot and KernelUD homerange
Alex.rast = (raster(as(tortKhref$Alex, "SpatialPixelsDataFrame")))
writeRaster(Alex.rast, "AlexRast.tif")
plot(Alex.rast)
Alex = DTort.spdf[DTort.spdf$ID == "Alex",]
plot(Alex, add = T, cex = 0.1)

Hadley.rast = (raster(as(tortKhref$Hadley, "SpatialPixelsDataFrame")))
writeRaster(Hadley.rast, "HadleyRast.tif")
plot(Hadley.rast)
Hadley = DTort.spdf[DTort.spdf$ID == "Hadley",]
plot(Hadley, add = T, cex = 0.1)

Morgan.rast = (raster(as(tortKhref$Morgan, "SpatialPixelsDataFrame")))
writeRaster(Morgan.rast, "MorganRast.tif")
plot(Morgan.rast)
Morgan = DTort.spdf[DTort.spdf$ID == "Morgan",]
plot(Morgan, add = T, cex = 0.1)

Quin.rast = (raster(as(tortKhref$Quin, "SpatialPixelsDataFrame")))
writeRaster(Quin.rast, "QuinRast.tif")
plot(Quin.rast)
Quin = DTort.spdf[DTort.spdf$ID == "Quin",]
plot(Quin, add = T, cex = 0.1)

Riley.rast = (raster(as(tortKhref$Riley, "SpatialPixelsDataFrame")))
writeRaster(Riley.rast, "RileyRast.tif")
plot(Riley.rast)
Riley = DTort.spdf[DTort.spdf$ID == "Riley",]
plot(Riley, add = T, cex = 0.1)

Toni.rast = (raster(as(tortKhref$Toni, "SpatialPixelsDataFrame")))
writeRaster(Toni.rast, "ToniRast.tif")
plot(Toni.rast)
Toni = DTort.spdf[DTort.spdf$ID == "Toni",]
plot(Toni, add = T, cex = 0.1)

TortKDE95 = getverticeshr(tortKhref, percent=95)

as.data.frame(TortKDE95)

plot(TortKDE95, col = c("blue","red","yellow","Orange","Pink"), axes = TRUE)


Tort.traj = as.ltraj(coordinates(DTort.spdf), date = DTort.spdf$ï..DATE, id = DTort$ID)



#AMT style 

DTort = read.csv("dtort2.csv")


head(DTort)
head(DTort.spdf)

dt = prepData(DTort, type = "LL", coordNames=c("LAT", "LONG"))

head(dt)

summary(dt)

write.csv(dt, file = "steps.csv")
