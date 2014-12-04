# Setup ------------------------------------------------------------------------
setwd("S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject")

# packages needed
library(rgdal)
library(grid)
library(gstat)
library(maptools)



# Load Data and Plot------------------------------------------------------------
points <- readOGR("../GIS","Kayak_UM_MSU_2014")

# make lake depth (m) using elevation of shoreline
points$lakedepth <- 351.733 - points$Bot_Ele

# histogram of bottom elevation
hist(points$lakedepth, main='Histogram of Lake Depth (m)', 
  xlab = 'Lake Depth (m)')

# look at qq plots for untransformed and transformed data
par(mfrow=c(1,3))
qqnorm(points$lakedepth, main = 'qq-plot-untransformed')
qqline(points$lakedepth)

# sqrt root
qqnorm(sqrt(points$lakedepth), main = 'qq-plot-sqrt')
qqline(sqrt(points$lakedepth))
# log
qqnorm(log(points$lakedepth), main = 'qq-plot-log')
qqline(log(points$lakedepth))



# make a map

# uncomment the png and dev.off lines to write out the figure
#png('figures/LakeDepth_points.png', type = "cairo", units = "in", width = 4.5, height = 4, res = 300)

lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
spplot(points, 'lakedepth', col.regions=rev(lakePal(200)), colorkey=T,
       main='Higgins Lake Point Depth Data', cex=.5)
# scale bar legend title
grid.text('Lake Depth (m)',x=unit(0.965, "npc"),y=unit(0.5, 'npc'), rot=-90)

#dev.off()


#Add shoreline points-------------------------------------------------------------
shorepoints <- readOGR("../GIS","Shoreline_Points_GCS")
names(shorepoints)
names(points)
proj4string(shorepoints)
proj4string(points)

# match column names between shore points and data points
shorepoints <- shorepoints[,c('Lat','Lon','Bot_Ele','Id')]
newnames <- c('Lat','Lon','Bot_Ele','ID')
names(shorepoints) <- newnames

#add shore depth and combine shoreline to points
shorepoints$lakedepth <- 0
allpoints <- spRbind(points, shorepoints)

# reproject to meters
utmproj <- '+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs' 
allpoints <- spTransform(allpoints, CRS(utmproj))

# write out Shapefile
writeOGR(allpoints,"../GIS","All_PointsWithShoreline", driver= "ESRI Shapefile")



# variogram -----------------------------------------------------------------------
# try it on full dataset
vario <- variogram(lakedepth ~ 1, allpoints)
plot(vario, col='black', main="Omnidirectional Variogram for All Data")
vgm <- vgm(model="Sph", nugget=0, psill=220, range=3000)
vgm <- fit.variogram(vario,vgm)
plot(vario, model=vgm, main= "Omnidirectional Variogram for All Data")

# Randomly select points

