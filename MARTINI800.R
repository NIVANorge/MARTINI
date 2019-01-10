
library(ncdf4)
#library(RCurl)
#library(raster)
library(tidyverse)
library(chron)
library(RColorBrewer)
library(lattice)

# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm
dname <- "Chl"  # note: tmp means temperature (not temporary)

# open NetCDF file
ncfname<-"MARTINI800/Combined_martini800_avg_Chl_surf.nc"
ncin <- nc_open(ncfname)
#print(ncin)

# Get the longtiudes and latitudes, using the ncvar_get() function in ncdf4.
lon <- ncvar_get(ncin, "lon_rho")
#lon <- lon[,1]
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "lat_rho", verbose = F)
#lat <- lat[1,]
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))

# Get the time variable and its attributes using the ncvar_get() and ncatt_get() functions
# also get the number of times using the dim() function.
t <- ncvar_get(ncin, "ocean_time")
tunits <- ncatt_get(ncin, "ocean_time", "units")
nt <- dim(t)

# Get the variable and its attributes, and verify the size of the array.
tmp.array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(tmp.array)

# Get the global attributes.
title <- ncatt_get(ncin, 0, "title")
institution <- ncatt_get(ncin, 0, "institution")
datasource <- ncatt_get(ncin, 0, "source")
references <- ncatt_get(ncin, 0, "references")
history <- ncatt_get(ncin, 0, "history")
Conventions <- ncatt_get(ncin, 0, "Conventions")

# Close the NetCDF file using the nc_close() function
nc_close(ncin)

# Convert the time variable
# The time variable, in “time-since” units can be converted into “real” (or more easily readable) time values by splitting the time tunits$value string into its component parts, and then using the chron() function to determine the absolute value of each time value from the time origin.

# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(t, origin = c(tmonth, tday, tyear))

# Replace NetCDF fillvalues with R NAs
tmp.array[tmp.array == fillvalue$value] <- NA

# Get the total number of non-missing grid cells
length(na.omit(as.vector(tmp.array[, , 1])))

# Get a single time slice of the data, create an R data frame, and write a .csv file
m <- 1
tmp.slice <- tmp.array[, , m]

image(lon,lat,tmp.slice, col = rev(brewer.pal(10, "RdBu")))
dim(tmp.slice)
dim(lon)
dim(lat)
image(tmp.slice, col = rev(brewer.pal(10, "RdBu")))

lat[1,2]-lat[1,1]
lat[480,2]-lat[480,1]

lat[1,426]-lat[1,425]
lat[480,426]-lat[480,425]

res[,,1]<-lon
res[,,2]<-lat
res[,,3]<-tmp.slice


r <- raster(ncfname, varname = dname)
plot(r)

library(rasterVis)
# Notice `g`plot (from rasterVis) and not `gg`plot
gplot(r) + 
  geom_tile(aes(fill = value))

ggplot(aes(x=lat, y=lon, fill=tmp.slice), data=mpr) + geom_raster() + coord_equal()

raster(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA, template=NULL)