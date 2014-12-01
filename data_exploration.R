# Setup ------------------------------------------------------------------------
setwd("S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject")

# packages needed
library(rgdal)
library(grid)


# Load Data and Plot------------------------------------------------------------
points <- readOGR("../GIS","Kayak_UM_MSU_2014")

# histogram of bottom elevation
hist(points$Bot_Ele, main='Histogram of Bottom Elevation (m ASL)', 
  xlab = 'Bottom Elevation (m ASL)')

# make lake depth (m) using elevation of shoreline
points$lakedepth <- 351.733 - points$Bot_Ele

# make a map

# uncomment the png and dev.off lines to write out the figure
#png('figures/LakeDepth_points.png', type = "cairo", units = "in", width = 4.5, height = 4, res = 300)

lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
spplot(points, 'lakedepth', col.regions=rev(lakePal(200)), colorkey=T,
       main='Higgins Lake Point Depth Data', cex=.5)
# scale bar legend title
grid.text('Lake Depth (m)',x=unit(0.965, "npc"),y=unit(0.5, 'npc'), rot=-90)

#dev.off()
