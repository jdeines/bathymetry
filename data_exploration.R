# Setup ------------------------------------------------------------------------
setwd("S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject")

# packages needed
library(rgdal)
library(RColorBrewer)
library(classInt)


# Load Data and Plot------------------------------------------------------------
points <- readOGR("../GIS","Kayak_UM_MSU_2014")

# histogram of bottom elevation
hist(points$Bot_Ele, main='Histogram of Bottom Elevation (m ASL)', 
  xlab = 'Bottom Elevation (m ASL)')

# make lake depth (m)
points$lakedepth <- 351.733 - points$Bot_Ele

# map
par(mar=c(.5,.5,.5,.5))
nclasses
class <- classIntervals(points$lakedepth, 

plot(points, col='blue')
spplot(points, 'lakedepth', col.regions=terrain.colors(5))


