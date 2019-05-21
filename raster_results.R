# get rotated raster to interpolate with MARTINI results

r <- raster("./raster/test_rotate.tif")


waterbodies <- shapefile("nve/CoastalWBs.shp")
waterbodies2 <- shapefile("shp/martini_shapes.shp")
waterbody1 <- shapefile("shp/martini_shape_select.shp")
