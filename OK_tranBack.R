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

# Tranform OK back-----------------------------------------------------------------------------------------------
ok.pred16 <- ns.backtr(ok.krige$var1.pred, Normalpoints, tails='none')
ok.krige$bk_pred <- ok.pred16
#ok.var16 <- ns.backtr(ok.krige$var1.var, Normalpoints, tails='none')
#ok.krige$bk_var <- ok.var16

#ok.krige$var1.stan_error <- sqrt(ok.krige$var1.var)
#ok.stan_error <- ns.backtr(ok.krige$var1.stan_error, Normalpoints, tails='none')
#ok.krige$bk_var.stan_error <- ok.stan_error^2
#ok.krige$stan_error <- ok.stan_error
# Plot Back_OK---------------------------------------------------------------------------------------------------

lakePal <- colorRampPalette(c('midnightblue','turquoise1'))
# plot ok prediction
png('figures/LakeDepth_OK16.png', type = "cairo", units = "in", width = 4.5, height = 4, res = 300)
spplot(ok.krige['bk_pred'], col.regions=rev(lakePal(200)), 
       scales=list(draw = TRUE), main="Higgins Lake Depth OK Predictions") +
layer(sp.polygons(FinalLakePoly, col='black'))
dev.off()
save(ok.krige['bk_pred'], file=paste0(RdataOutputs,'okRasterbt.Rdata'))

# plot ok variance
png('figures/LakeDepth_OK_var16.png', type = "cairo", units = "in", width = 4.5, height = 4, res = 300)
spplot(ok.krige['var1.var'], col.regions=heat.colors(20), 
       scales=list(draw = TRUE), main="Higgins Lake Depth OK Variance") +
layer(sp.polygons(FinalLakePoly, col='black'))
dev.off()
save(ok.krige, file="./Higgins_Code/RData_Objects/ok.tran_bk.Rdata")

#Note: under "names(ok.krige)" bk_var is transformed back
#change ok.krige to raster
ok.bt.Raster <- raster(ok.krige[,'bk_pred'])
spplot(ok.bt.Raster, col.regions=rev(lakePal(200)), main= "Ordinary Kriging")
Rdatadirectory <- 'S:/Projects/2013/Higgins_Lake/Higgins_Bath/KrigingProject/Higgins_Code/RData_Objects/'
save(ok.bt.Raster, file=paste0(Rdatadirectory,'ok.bt.Raster.Rdata'))

save(okRaster, file=paste0(RdataOutputs,'okRaster.Rdata'))