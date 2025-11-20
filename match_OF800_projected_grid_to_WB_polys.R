

# match projected OF800 grid cells to Oslofjord WB polygons

shp <- sf::st_read("app/shp/oslofjord/oslofjord_waterbodies.shp", quiet=T)
shp <- shp %>%
  select(Vannforeko)

polys_geo <- readRDS("processing/grid_OF800_geo_vector_incl_NAs.Rds")

polys_geo <- polys_geo %>%
  mutate(id=row_number()) %>%
  relocate(id)

shp_wb <- sf::st_read("app/shp/oslofjord/oslofjord_waterbodies.shp",
                      quiet=T, check_ring_dir=T, promote_to_multi=F)


sf::st_crs(polys_geo)
sf::st_crs(shp)

intersect <- sf::st_intersection(polys_geo, shp)

intersect2 <- intersect %>%
  mutate(area_km2 = 0.001*as.numeric(sf::st_area(.)))

intersect2 <- intersect2 %>%
  sf::st_drop_geometry() %>%
  select(Vannforeko, id, area_km2) %>%
  group_by(Vannforeko) %>%
  mutate(f = area_km2/sum(area_km2, na.rm=T)) %>%
  ungroup()

saveRDS(intersect, file="processing/intersection_OF800_poly_with_WBs.Rds")

saveRDS(intersect2, file="processing/raster_cellid_to_WB.Rds")


rfile <- "OF800/res_v10f/DINredRA80-J10_v2/of800_v10f_DINredRA80-J10_v2_N3_n_bc_0_5_10m_unweighted_av_monmean_summer_mean.nc"

rfile <- paste0("OF800/res_v10f/MSMDI_modelling/DINredRA80-J10/MSMDIpred_unproj_",2019,"_BRTselect_martini_by_arendal_season_simp.tif")

r <- terra::rast(rfile)
r
vals <- values(r)

coords <- sf::st_coordinates(pts)

grid <- pts %>%
  sf::st_drop_geometry() %>%
  select(lon_rho, lat_rho) %>%
  bind_cols(coords)

ggplot() +
  #geom_sf(data=shp) +
  geom_sf(data=pts) +
  theme_minimal()



ggplot() +
  #geom_sf(data=shp) +
  geom_sf(data=polys_geo) +
  theme_minimal()

shp <- shp %>%
  sf::st_transform(crs=sf::st_crs(r))

dfr <- r %>%
  terra::intersect(shp)

shp
r
sf::st_crs(shp)
sf::st_crs(r)

