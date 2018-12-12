# Save raster data from thredds.met.no 

library(ncdf4)
library(RCurl)
library(raster)
library(tidyverse)
library(sf)

library(rgdal)

val <- as.matrix(read.table('chl.txt',sep=" "))
lon <- as.matrix(read.table('chl_lon.txt',sep=" "))
lat <- as.matrix(read.table('chl_lat.txt',sep=" "))
  
  dfval<-as.data.frame.table(val, responseName = "value")
  dflon<-as.data.frame.table(lon, responseName = "lon")
  dflat<-as.data.frame.table(lat, responseName = "lat")
  
  df <- dfval %>%
    left_join(dflat) %>% 
    left_join(dflon) %>%
    select(lat,lon,value)
  
  
  #df <- df %>% filter(value>-32000)
  df <- df %>% filter(lon>0,lon<30,lat>40,lat<70)
  df <- df %>% mutate(value=ifelse(value<0,-999,value))
  
  write.table(df,file="chl3.txt",row.names=F)
  
  coords<-select(lon,lat)
  value<-df %>% select(value)
  df2 <- SpatialPointsDataFrame(coords, value)
  
  plot(df2)
  
  r<-rasterFromXYZ(df)
  
  
  r <- raster("C:/Data/GitHub/MARTINI/raster/chl")
  
r <- raster(ncol=609,nrow=881)
r <- setValues(r,val)

ggplot(df) +  
  geom_tile(aes(fill=factor(value),alpha=0.8))
  coord_equal()
  
  plot(r)

  
   crs()
  topazproj = ccrs.Stereographic(central_latitude=90, central_longitude=-45, false_easting=0, false_northing=0)  
  
  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  # # Read in netcdf file
  # url<-'http://thredds.met.no/thredds/ncss/aromearcticlatest/arome_arctic_extracted_2_5km_latest.nc'
  # url<-'https://thredds.met.no/thredds/dodsC/topaz/dataset-topaz4-bio-arc-myoceanv2-be'
  # nc = nc_open(url)
  # 
  # url<-'https://thredds.met.no/thredds/dodsC/topaz/dataset-topaz4-bio-arc-myoceanv2-be'
  # 
  # dat = getURL(url)
  
  
  