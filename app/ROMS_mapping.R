#https://rstudio-pubs-static.s3.amazonaws.com/241543_1ab89f843e844434a927c8b5ca0b4d5a.html

#.libPaths("C:/Users/CJM/OneDrive - NIVA/R/R-3.5.0/library")


library(angstroms)   
library(ncdump)       
library(tabularaster) 
library(rworldmap)
roms_path<-"fram/MARTINI800/martini800_v2_Chl_surf.nc"
roms_path<-"fram/MARTINI800/martini800_v2_TotP_s41_uv.nc"

ncd <- NetCDF(roms_path)
ncd$variable$name

vname <- "light_Chl"
vname <- "TotP"
dd <- romsdata(roms_path, varname = vname, slice = c(1, 1), transpose = TRUE)
plot(dd)  ## this is pure 0-dimX, 0-dimY index space
longlat <- romscoords(roms_path, transpose = TRUE)
contour(longlat[[1]], add = TRUE, lty = 2)
