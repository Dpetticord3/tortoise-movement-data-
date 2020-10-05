#generic skeleton to produce mcp kernel ud home range in matrices

data = read.csv("data.csv")
data.spdf = data
data.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(data.spdf$location.long,
                                                                         data.spdf$location.lat)), data=data.spdf, proj4string =
                                                CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
data.spdf <- spTransform(data.spdf,
                                   CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0
                              +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#MCP
data.mcp = mcp(data.spdf[, "individual.local.identifier"], percent = 100, unin = "m", unout = "km2")
data.mcp@data
as.data.frame(data.mcp)
#Kernel
data.Khref = kernelUD(data.spdf[, "individual.local.identifier"], h = "href", grid = 100)
data.KDE95 <- getverticeshr(data.Khref, percent = 95)
homerange <- getverticeshr(data.Khref, percent = 90)
as.data.frame(homerange)
#Vud
vud = getvolumeUD(data.Khref)
Vhomerange = getverticeshr(vud, percent = 90)
as.data.frame(Vhomerange)

