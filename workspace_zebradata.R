install.packages("adehabitatHR")
install.packages("rgdal")
install.packages("raster")


library(adehabitatHR)
library(rgdal)
library(raster)

getwd()

setwd("C:/Users/Mannipetti/Documents/thesisbootcamp/rstudio/movementdata")

zebra = read.csv("MigratoryZebra.csv")

zebra = subset(zebra, manually.marked.outlier !="true")

zebra = zebra[, c("event.id", "visible", "individual.local.identifier", "timestamp", "location.long", "location.lat", "utm.easting", "utm.northing", "utm.zone")]

head(zebra)

zebra$timestamp = as.POSIXct(strptime(zebra$timestamp, "%Y-%m-%d %H:%M:%S"))

zebra.spdf = zebra

summary(zebra.spdf)


which(is.na(zebra.spdf$location.lat))
which(is.na(zebra.spdf$location.long))

zebra.spdf = zebra.spdf[!is.na(zebra.spdf$location.long), ]

zebra.spdf = zebra.spdf[!is.na(zebra.spdf$timestamp), ]

zebra.spdf <- SpatialPointsDataFrame(coords=as.data.frame(cbind(zebra.spdf$location.long,
                                                                zebra.spdf$location.lat)), data=zebra.spdf, proj4string =
                                       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

zebra.spdf <- spTransform(zebra.spdf,
                          CRS("+proj=lcc +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0
                              +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
plot(zebra.spdf)


zebra.mcp = mcp(zebra.spdf[, "individual.local.identifier"], percent = 100, unin = "m", unout = "km2")

plot(zebra.mcp, col = c(1:7))

plot(zebra.mcp, col = c("blue", "gray", "red", "orange", "green", "yellow",
                        "magenta"), axes = TRUE)

zebra.mcp@data
as.data.frame(zebra.mcp)

zebra.Khref = kernelUD(zebra.spdf[, "individual.local.identifier"], h = "href")

image(zebra.Khref)

z3864KDE.rast <- (raster(as(zebra.Khref$Z3864, "SpatialPixelsDataFrame")))

writeRaster(z3864KDE.rast, "Z3864KDERast.tif")

plot(z3864KDE.rast)

z3864 <- zebra.spdf[zebra.spdf$individual.local.identifier == "Z3864", ]

plot(z3864, add = T, cex = 0.1)

zebra.KDE95 <- getverticeshr(zebra.Khref, percent = 95)

plot(zebra.KDE95, col = c("blue", "gray", "red", "orange", "green", "yellow",
                          "magenta"), axes = TRUE)
