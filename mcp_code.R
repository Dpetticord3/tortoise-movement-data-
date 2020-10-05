#set working directory
setwd("C:/Users/Mannipetti/Documents/thesisbootcamp/rstudio/movementdata")
#Read in csv file
turtle = read.csv("masterTortMove.csv")

#save as new formatting - this will eventually be w/out the NA values
turtle.spdf = turtle
summary(turtle.spdf)
head(turtle.spdf)
#check for categories with NA values for removal#
#which(is.na(turtle.spdf$location.lat))
#which(is.na(turtle.spdf$location.long))
#Remove NA valued rows
#technically not necessary to remove lat and long as one requires the other#
#turtle.spdf = turtle.spdf[!is.na(turtle.spdf$location.long), ]
#turtle.spdf = turtle.spdf[!is.na(turtle.spdf$location.lat), ]

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
cp = mcp(turtle.spdf[, "individual.local.identifier"], percent = 95)

class(cp)

plot(cp)
plot(turtle.spdf, add = TRUE)

as.data.frame(cp)
hrs = mcp.area(turtle.spdf[, "individual.local.identifier"], percent = seq(50,100,by =5))

##HOME RANGE ESTIMATION - MINIMUM CONVEX POLYGON

#100 percent MCP based on multiple individuals
turtle.mcp = mcp(turtle.spdf[, "individual.local.identifier"], percent = 100, unin = "m", unout = "km2")
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


data(turtle.spdf)
