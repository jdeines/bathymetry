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
everypoint <- points
#y <- subset(everypoint, everypoint!="random5000")
yes <- random5000$ID
modelpoints <- points[!points$ID %in% yes, ] #points without the random 5000 points

#Transforms to Normal Scores-------------------------------------------

#modified function from AS #nscore_examples.R from Nov 18th lecture
#same file has function to go back
#NOTE: our's is not sorted and his was
nscore <- function(x) {
  # Takes a vector of values x and calculates their normal scores. Returns 
  # a list with the scores and an ordered table of original values and
  # scores, which is useful as a back-transform table. See backtr().
  nscore <- qqnorm(x, plot.it = FALSE)$x  # normal score 
  trn.table <- data.frame(x=x,nscore=nscore)
  
  return (list(nscore=nscore, trn.table=trn.table))
}

modelpoints.ns <- nscore(modelpoints$lakedepth)   # found normal score
modelpoints.ns.df <- modelpoints.ns[[2]]                 # extract normal score data frame (back-transform table)
Normalpoints <- cbind(as.data.frame(modelpoints),modelpoints.ns.df)   # combine nscores with lake depths/coordintes
coordinates(Normalpoints) <- ~coords.x1+coords.x2             # re-spatialize
utmproj <- '+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs' #give it the right projection
proj4string(Normalpoints) <- utmproj

#Krige Normal Data----------------

#Ordinary Krige
Rdatadirectory <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/'
load(paste0(Rdatadirectory, 'Grid5m.RData'))
load(paste0(Rdatadirectory, 'data_exploration.RData'))
load(paste0(Rdatadirectory, 'ns_vgm10000.RData'))
proj4string(grid5m) <- proj4string(Normalpoints) #making the coordinate systems match 
ok.krige <- krige(nscore~1, Normalpoints, grid5m, model=Normalvgm.10000, nmax=16)

#plot ordinary krige
lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
spplot(ok.krige, 'var1.pred', col.regions=rev(lakePal(200)), main ="Ordinary Kriging")
save(ok.krige, file=paste0(Rdatadirectory, 'ok.krige16.Rdata'))

#change to raster
okRaster <- raster(ok.krige[,'var1.pred'])

#save raster
Rdatadirectory <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/'
save(okRaster, file=paste0(Rdatadirectory,'okRaster.RData'))

#Simulation Krige
load(paste0(Rdatadirectory, 'Grid10m.RData'))
proj4string(grid10m) <- proj4string(Normalpoints) #making the coordinate systems match 
ok.sim <- krige(nscore~1, Normalpoints, grid10m, model=Normalvgm.10000, nmax=16, nsim=20)

#plot simulation krige
#spplot(ok.sim, 'var1.pred', col.regions=rev(lakePal(200)), main ="Simulation Kriging")

#Make the model output grid
ok.sim.sp <- grid5m
#slot(ok.sim.sp, "data") <- ok.sim

#Create an average map for simulation results
ok.sim.sp$avg <- rowMeans(ok.sim, dims=1)
spplot(ok.sim.sp, "avg", sp.layout = list(pts,border,jth), col.regions=ca.cols(20),
             main=paste(me,"Joshua Tree habitat from simulation average", sep=" - "))

# Make the model output spatial
#h.sims.sp <- ca.grid
#slot(h.sims.sp, "data") <- habitat.sims

# Create an average map for all simulation results
#h.sims.sp$avg <- rowMeans(habitat.sims, dims=1)
#spplot(h.sims.sp, "avg", col.regions=rev(lakePal(200)), main = "Simulation Kriging Average")
    

#USING OLD VARIABLES
# Jill's quick 10 m test ---------------------------------------
Rdatadirectory <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/'
load(paste0(Rdatadirectory, 'Grid10m.RData'))
#load(paste0(Rdatadirectory, 'data_exploration.RData'))
ok.krige <- krige(lakedepth~1, allpointsClean, grid10m, model=vgm.10000, nmax=8)

# plot it!
lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
spplot(ok.krige, 'var1.pred', col.regions=rev(lakePal(200)))
# Ordinary Kriging ------------------------------------------------------------------

# try kriging with all points
Rdatadirectory <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/'
load(paste0(Rdatadirectory, 'Grid3m.RData'))
load(paste0(Rdatadirectory, 'data_exploration.RData'))
#proj4string(grid10m) <- proj4string(allpointsClean)
ok.krige <- krige(lakedepth~1, allpointsClean, grid3m, model=vgm.10000, nmax=8)