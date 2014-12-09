# packages needed
library(rgdal)
library(grid)
library(gstat)
library(maptools)
library(latticeExtra)

# Load data----------------------------------------------------------------------------------------------
setwd("S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject")
load("./Higgins_Code/RData_Objects/Normalpoints.RData")
load("./Higgins_Code/RData_Objects/ok.krige16.RData")
FinalLakePoly <- readOGR("S:/Projects/2013/Higgins_Lake/Higgins_Bath/GIS/lakePolys", "FinalLakePoly")

# Transform back normal scores function from Ashton's lecture--------------------------------------------
ns.backtr <- function(scores, nscore, tails='none', draw=TRUE) {
  if(tails=='separate') { 
    mean.x <- mean(nscore$x)
    small.x <- nscore$x < mean.x
    large.x <- nscore$x > mean.x
    small.sd <- sqrt(sum((nscore$x[small.x]-mean.x)^2)/
                       (length(nscore$x[small.x])-1))
    large.sd <- sqrt(sum((nscore$x[large.x]-mean.x)^2)/
                       (length(nscore$x[large.x])-1))
    min.x <- mean(nscore$x) + (min(scores) * small.sd)
    max.x <- mean(nscore$x) + (max(scores) * large.sd)
    # check to see if these values are LESS extreme than the
    # initial data - if so, use the initial data.
    #print(paste('lg.sd is:',large.sd,'max.x is:',max.x,'max nsc.x is:',max(nscore$trn.table$x)))
    if(min.x > min(nscore$x)) {min.x <- min(nscore$x)}
    if(max.x < max(nscore$x)) {max.x <- max(nscore$x)}
  }
  min.sc <- min(scores)
  max.sc <- max(scores)
  x <- c(min.x, nscore$x, max.x)
  nsc <- c(min.sc, nscore$nscore, max.sc)
  
  if(draw) {plot(nsc,x, main='Transform Function')}
  back.xf <- approxfun(nsc,x) # Develop the back transform function
  val <- back.xf(scores)
  return(val)
}

# Tranform OK back-----------------------------------------------------------------------------------------------
ok.pred16 <- ns.backtr(ok.krige$var1.pred, Normalpoints, tails='separate')
ok.krige$bk_pred <- ok.pred16
ok.krige@data[ok.krige$bk_pred < 0,]$ bk_pred <- 0
ok.var16 <- ns.backtr(ok.krige$var1.var, Normalpoints, tails='separate')
ok.krige$bk_var <- ok.var16

# Plot Back_OK---------------------------------------------------------------------------------------------------
# Plot data points
lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
pts <- list('sp.points', Normalpoint, pch=1, cex=0.7, col='gray30')

spplot(ok.krige['bk_pred'], col.regions=rev(lakePal(200)), 
       scales=list(draw = TRUE), main="Higgins Lake Depth OK Predictions") +
layer(sp.polygons(FinalLakePoly, col='black'))



#dev.print(png, "tmin_okpred.png", height=520, width=460)

print(spplot(ok.krige['bk_var'], col.regions=heat.colors(20), sp.layout=list(pts, FinalLakePoly), 
             scales=list(draw = TRUE), formula=sqrt(bk_var)~x+y, 
             main="TMIN: OK Standard Errors", sep=" - "))
#dev.print(png, "tmin_okse.png", height=520, width=460)