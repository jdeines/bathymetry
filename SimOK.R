# Setup ------------------------------------------------------------------------

# packages needed
library(rgdal)
library(gstat)
library(maptools)
library(raster)

#ORDINARY KRIGING w/ simulations-----------------------------------------------

dataDir <- 'F:/users/deinesji/Geo866Kriging/data'
points <- readOGR(dataDir,"Kayak_UM_MSU_2014_noDupes_utm")

#Random Sampling------------------------------------------------------

#use "points" because that is the dataset without the shoreline
points$ID <- c(1:nrow(points)) #assigning a number to each row via the ID field to allow for comparing
random5000 <- points[sample(1:nrow(points), nrow(points)-25000, replace=FALSE),] #randomly sampled 5000 points from the "points" dataset
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
coordinates(Normalpoints) <- ~x+y             # re-spatialize
utmproj <- '+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs' #give it the right projection
proj4string(Normalpoints) <- utmproj

#Krige Normal Data----------------

#Simulation Krige
load(paste0(dataDir, '/Grid5m.RData'))
load(paste0(dataDir, '/Grid10m.RData'))
load(paste0(dataDir, '/ns_vgm10000.RData'))
proj4string(grid5m) <- proj4string(Normalpoints) #making the coordinate systems match 

ptm <- proc.time()
ok.sim <- krige(nscore~1, Normalpoints, grid5m, model=Normalvgm.10000, 
                nmax=8, nsim=20, maxdist=500)
runtime <- proc.time() - ptm

save(ok.sim, file=paste0(dataDir,'/ok.sim.25000.all.RData'))