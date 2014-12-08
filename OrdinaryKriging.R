# Setup ------------------------------------------------------------------------
setwd("S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject")

# packages needed
library(rgdal)
library(grid)
library(gstat)
library(maptools)




#ORDINARY KRIGING-----------------------------------------------

gisCleanDir <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/GIS/cleanData_duplicatesRemoved'
allpointsClean <- readOGR(gisCleanDir,"AllPoints_withShoreline_utm")

# Jill's quick 10 m test ---------------------------------------
Rdatadirectory <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/'
load(paste0(Rdatadirectory, 'Grid10m.RData'))
load(paste0(Rdatadirectory, 'data_exploration.RData'))
ok.krige <- krige(lakedepth~1, allpointsClean, grid10m, model=vgm.10000, nmax=8)

# plot it!
lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
spplot(ok.krige, 'var1.pred', col.regions=rev(lakePal(200)))
#------------------------------------------------------------------

# try kriging with all points
Rdatadirectory <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/'
load(paste0(Rdatadirectory, 'Grid3m.RData'))
load(paste0(Rdatadirectory, 'data_exploration.RData'))
#proj4string(grid10m) <- proj4string(allpointsClean)
ok.krige <- krige(lakedepth~1, allpointsClean, grid3m, model=vgm.10000, nmax=8)






