# Setup ------------------------------------------------------------------------
setwd("S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject")

# packages needed
library(rgdal)
library(grid)
library(gstat)
library(maptools)


# Load Data and Plot------------------------------------------------------------
# all depth points (sonar, shoal, kayak), cleaned and projected
points <- readOGR("../GIS/cleanData_duplicatesRemoved",
                  "Kayak_UM_MSU_2014_noDupes_utm")

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


#Load data with shoreline points------------------------------------------------
allpoints <- readOGR("../GIS/cleanData_duplicatesRemoved",
                       "AllPoints_withShoreline_utm")



# variogram -----------------------------------------------------------------------
# try it on full dataset
# fit variogram
vario <- variogram(lakedepth ~ 1, points)
plot(vario, col='black', main="Omnidirectional Variogram for Data No Shoreline")
vgm.no_shore <- vgm(model="Sph", nugget=0, psill=220, range=3000)
vgm.no_shore <- fit.variogram(vario,vgm.no_shore)
# make plot
png('figures/vario_points_no shoreline.png', type = "cairo", units = "in", width = 4.5, height = 4, res = 300)
plot(vario, model=vgm.no_shore, main= "Omnidirectional Variogram \n for Data No Shoreline")
dev.off()

# Randomly select 5000 points
Index_5000 <- round(runif(5000, min=1, max=564951))
points_5000 <- points[Index_5000,]
# fit variogram
vario_5000 <- variogram(lakedepth ~ 1, points_5000)
plot(vario_5000, col='black', main="Omnidirectional Variogram for 5000 points No Shoreline")
vgm.5000 <- vgm(model="Sph", nugget=0, psill=120, range=2)
vgm.5000 <- fit.variogram(vario_5000,vgm.5000)
# make plot
png('figures/variogram5000.png', type = "cairo", units = "in", width = 4.5, height = 4, res = 300)
plot(vario_5000, model=vgm.5000, main= "Omnidirectional Variogram for \n 5000 Points Without Shoreline")
dev.off()

# Randomly select 10000 points
Index_10000 <- round(runif(10000, min=1, max=564951))
points_10000 <- points[Index_10000,]
# fit variogram
vario_10000 <- variogram(lakedepth ~ 1, points_10000)
plot(vario_10000, col='black', main="Omnidirectional Variogram for 10000 points No Shoreline")
vgm.10000 <- vgm(model="Sph", nugget=0, psill=120, range=2)
vgm.10000 <- fit.variogram(vario_10000,vgm.10000)
# make plot
png('figures/variogram10000.png', type = "cairo", units = "in", width = 4.5, height = 4, res = 300)
plot(vario_10000, model=vgm.10000, main= "Omnidirectional Variogram for \n 10000 Points Without Shoreline")
dev.off()

# Randomly select 50000 points
Index_50000 <- round(runif(50000, min=1, max=564951))
points_50000 <- points[Index_50000,]
# fit variogram
vario_50000 <- variogram(lakedepth ~ 1, points_50000)
plot(vario_50000, col='black', main="Omnidirectional Variogram for 50000 points No Shoreline")
vgm.50000 <- vgm(model="Sph", nugget=0, psill=120, range=2)
vgm.50000 <- fit.variogram(vario_50000,vgm.50000)
# make plot
png('figures/variogram50000.png', type = "cairo", units = "in", width = 4.5, height = 4, res = 300)
plot(vario_50000, model=vgm.50000, main= "Omnidirectional Variogram for \n 50000 Points Without Shoreline")
dev.off()
