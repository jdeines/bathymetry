# Setup ------------------------------------------------------------------------

# packages needed
library(rgdal)
library(gstat)
library(maptools)
library(raster)
source('S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/Functions.R')

#ORDINARY KRIGING w/ simulations-----------------------------------------------

dataDir <- 'F:/users/deinesji/Geo866Kriging/data'
points <- readOGR(dataDir,"Kayak_UM_MSU_2014_noDupes_utm")

#Random Sampling------------------------------------------------------

#use "points" because that is the dataset without the shoreline
points$ID <- c(1:nrow(points)) #assigning a number to each row via the ID field to allow for comparing
set.seed(866)
random5000 <- points[sample(1:nrow(points), nrow(points)-5000, replace=FALSE),] #randomly sampled 5000 points from the "points" dataset
modelpoints <- points[!points$ID %in% random5000$ID, ] #points without the random 5000 points
# save validation points
rdatadir <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/Outputs/'
save(random5000, file=paste0(rdatadir, 'validPoints_5000_seed866.Rdata'))


#Transforms to Normal Scores-------------------------------------------

modelpoints.ns <- nscore(modelpoints$lakedepth)   # found normal score
modelpoints.ns.df <- modelpoints.ns[[2]]                 # extract normal score data frame (back-transform table)
Normalpoints <- cbind(as.data.frame(modelpoints),modelpoints.ns.df)   # combine nscores with lake depths/coordintes
coordinates(Normalpoints) <- ~x+y             # re-spatialize
utmproj <- '+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs' #give it the right projection
proj4string(Normalpoints) <- utmproj

#tidy
rm(modelpoints.ns, modelpoints.ns.df, modelpoints, points, random5000)

#Break Up Grids-----------------------------------------------------------------
# break up grids into a list of grids, 'gridList'
# use gridList to break up points into a list of points, pointList

# Grid List
load(paste0(dataDir, '/Grid5m.RData'))
# our proj4 strings are out of order for some reason...force matching
proj4string(grid5m) <- proj4string(Normalpoints)

# break into subsets
bbox(grid5m)[1,2]-bbox(grid5m)[1,1]  # get ranges
bbox(grid5m)[2,2]-bbox(grid5m)[2,1]
# 8 complete
xcuts <- seq(bbox(grid5m)[1,1],bbox(grid5m)[1,2],length.out=4)
ycuts <- seq(bbox(grid5m)[2,1],bbox(grid5m)[2,2],length.out=4)

gridList <- list()
index <- 1
for(i in 2:length(xcuts)){
  for(m in 2:length(ycuts)){
    if (sum(grid5m$x < xcuts[i] & grid5m$x > xcuts[i-1] &
              grid5m$y < ycuts[m] & grid5m$y > ycuts[m-1]) > 0){
      gridList[[index]] <- subset(grid5m, 
                                grid5m$x < xcuts[i] & grid5m$x > xcuts[i-1] &
                                grid5m$y < ycuts[m] & grid5m$y > ycuts[m-1])
    } else {
      gridList[[index]] <- character(0)  # dummy thing to remove later
    }
    index <- index+1  
  }
}

# remove dummy lists
gridList <- Filter(length, gridList)

# Point List

pointList <- list()
for (m in 1:length(gridList)){
  pointList[[m]] <- Normalpoints[gridList[[m]],]
}

# tidy up
rm(i, m, index, xcuts, ycuts)

# krige a bunch of times!!
load(paste0(dataDir, '/ns_vgm10000.RData'))

sectionSims <- list()
for (i in 1:length(gridList)){  # going to split up on machines!
  sectionSims[[i]] <-  krige(nscore~1, pointList[[i]], gridList[[i]], 
                             model=Normalvgm.10000, nmax=8, nsim=10, maxdist=1500)
  #print(paste('Finished grid',i,'. Time:',Sys.time()))
}

# merge sections back together
total <- do.call(rbind, sectionSims)

#Simulation average
nscores <- Normalpoints@data[,6:7]
total$sim1back <- ns.backtr(total$sim1, nscores, tails='none')
total$sim2back <- ns.backtr(total$sim2, nscores, tails='none')
total$sim3back <- ns.backtr(total$sim3, nscores, tails='none')
total$sim4back <- ns.backtr(total$sim4, nscores, tails='none')
total$sim5back <- ns.backtr(total$sim5, nscores, tails='none')
total$sim6back <- ns.backtr(total$sim6, nscores, tails='none')
total$sim7back <- ns.backtr(total$sim7, nscores, tails='none')
total$sim8back <- ns.backtr(total$sim8, nscores, tails='none')
total$sim9back <- ns.backtr(total$sim9, nscores, tails='none')
total$sim10back <- ns.backtr(total$sim10, nscores, tails='none')

# stack lake values
totStack <- stack(total[,11:20])
totMean <- mean(totStack)
lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
spplot(totMean, col.regions=rev(lakePal(200)), main ="Simulation Kriging")

#Saving Simulation Average
RdataOutputs <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/Outputs/'
save(totMean, file=paste0(RdataOutputs, 'simAll_9chunks.Rdata'))


# 
# # recombines subsets
# total2 = rbind(m, m.subset)
# #but note that in this case total will have some pixels appear twice,
# #which you will not see in
# image(total2)
# #but for instance,
# image(rbind(m.subset, m))
# #will mask the larger values. Then,
# #as(total, "SpatialGridDataFrame")
# 
# 
# # old! --------------------------------------------
# 
# ptm <- proc.time()
# ok.sim <- krige(nscore~1, Normalpoints, grid5m, model=Normalvgm.10000, 
#                 nmax=8, nsim=20, maxdist=500)
# runtime <- proc.time() - ptm
# 
# #save(ok.sim, file=paste0(dataDir,'/ok.sim.25000.all.RData'))
# 
# ok.sim$sim11back <- ns.backtr(ok.sim$sim11, nscores, tails='none')
# ok.sim$sim12back <- ns.backtr(ok.sim$sim12, nscores, tails='none')
# ok.sim$sim13back <- ns.backtr(ok.sim$sim13, nscores, tails='none')
# ok.sim$sim14back <- ns.backtr(ok.sim$sim14, nscores, tails='none')
# ok.sim$sim15back <- ns.backtr(ok.sim$sim15, nscores, tails='none')
# ok.sim$sim16back <- ns.backtr(ok.sim$sim16, nscores, tails='none')
# ok.sim$sim17back <- ns.backtr(ok.sim$sim17, nscores, tails='none')
# ok.sim$sim18back <- ns.backtr(ok.sim$sim18, nscores, tails='none')
# ok.sim$sim19back <- ns.backtr(ok.sim$sim19, nscores, tails='none')
# ok.sim$sim20back <- ns.backtr(ok.sim$sim20, nscores, tails='none')
# 
# # stack backtransformed columns
# simRas <- stack(ok.sim[,21:40])
# 
# #Plotting Simulation Average
# simMean <- mean(simRas)
# lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
# spplot(simMean, col.regions=rev(lakePal(200)), main ="Simulation Kriging")
# 
# #Saving Simulation Average
# RdataOutputs <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/Outputs/'
# save(simMean, file=paste0(RdataOutputs, 'simAll_9chunks.Rdata'))
