# get rotated raster to interpolate with MARTINI results
library(leaflet)
library(raster)
library(dplyr)
r <- raster("./raster/zero_rotate.tif") # test_rotate.tif
crs(r) <- CRS('+init=epsg:3035')

paramlist<-c("Chl","DO_bot","NH4_summer","NH4_winter",
             "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
             "TN_summer","TN_winter","TP_summer","TP_winter")

param<- paramlist[1]

df_points<-read.table(file="rda/points_epsg3035.txt",header=T,stringsAsFactors=F,sep=";") %>%
  dplyr::select(X=XCoord,Y=YCoord,ID)

for(param in paramlist){
  load(paste0("fram/rda/",param,".Rda"))
  df <- df %>%
    select(ID=id,value)
  #names(df)[names(df)=="value"]<-param
  df <- df_points %>%
    left_join(df,by="ID")
  x <- rasterize(df[, 1:2], r, df[,4], fun=mean)
  
  rf <- writeRaster(x, filename=paste0("raster/",param,".grd",overwrite=TRUE))
  plot(x, main=param)

  }
  


waterbodies <- shapefile("nve/CoastalWBs_WGS84_simple.shp")

# Sets the projection to British National Grid
#proj4string(dat.pp) <- CRS("+init=EPSG:27700")

#waterbodies2 <- shapefile("shp/martini_shapes.shp")
#waterbody1 <- shapefile("shp/martini_shape_select.shp")

waterbodies@data$highlight<-0

match<-"0101000032-4-C"
waterbodies@data[waterbodies@data$Vannforeko==match,"highlight"]<-1

wb<-as.vector(waterbodies@data$Vannforeko)
highlight<-as.vector(waterbodies@data$highlight)
df<-data.frame(wb,highlight)
