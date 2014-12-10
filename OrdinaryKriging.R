# Setup ------------------------------------------------------------------------
setwd("S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject")

# packages needed
library(rgdal)
library(grid)
library(gstat)
library(maptools)
library(raster)



#ORDINARY KRIGING-----------------------------------------------

gisCleanDir <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/GIS/cleanData_duplicatesRemoved'
allpointsClean <- readOGR(gisCleanDir,"AllPoints_withShoreline_utm")

points <- readOGR("../GIS/cleanData_duplicatesRemoved",
                  "Kayak_UM_MSU_2014_noDupes_utm")

#Random Sampling------------------------------------------------------

#use "points" because that is the dataset without the shoreline
points$ID <- c(1:nrow(points)) #assigning a number to each row via the ID field to allow for comparing
random5000 <- points[sample(1:nrow(points), 5000, replace=FALSE),] #randomly sampled 5000 points from the "points" dataset
modelpoints <- points[!points$ID %in% random5000$ID, ] #points without the random 5000 points


#Transforms to Normal Scores-------------------------------------------

modelpoints.ns <- nscore(modelpoints$lakedepth)   # found normal score
modelpoints.ns.df <- modelpoints.ns[[2]]                 # extract normal score data frame (back-transform table)
Normalpoints <- cbind(as.data.frame(modelpoints),modelpoints.ns.df)   # combine nscores with lake depths/coordintes
coordinates(Normalpoints) <- ~x+y             # re-spatialize
utmproj <- '+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs' #give it the right projection
proj4string(Normalpoints) <- utmproj

#Krige Normal Data----------------

#Ordinary Krige
Rdatadirectory <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/'
load(paste0(Rdatadirectory, 'Grid5m.RData'))
load(paste0(Rdatadirectory, 'ns_vgm10000.RData'))
proj4string(grid5m) <- proj4string(Normalpoints) #making the coordinate systems match 
ok.krige <- krige(nscore~1, Normalpoints, grid5m, model=Normalvgm.10000, nmax=16)

#plot ordinary krige
lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
spplot(ok.krige, 'var1.pred', col.regions=rev(lakePal(200)), main ="Ordinary Kriging")
save(ok.krige, file=paste0(Rdatadirectory, 'ok.krige16.Rdata'))

#change to raster
okRaster <- raster(ok.krige[,'var1.pred'])
spplot(okRaster, col.regions=rev(lakePal(200)), main= "Ordinary Kriging")

#save raster
RdataOutputs <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/Outputs/'
save(okRaster, file=paste0(RdataOutputs,'okRaster.Rdata'))

