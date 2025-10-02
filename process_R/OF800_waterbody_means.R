library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
#library(patchwork)

source("process_R/utils.R")

polys_geo <- readRDS("processing/grid_OF800_geo_vector_incl_NAs.Rds")
#ind_data <- readRDS("processing/res_v10f.Rds")
ind_data <- readRDS("processing/res_v10aa.Rds")

r_grid <- terra::rast("OF800/of800_grid_polar_stereo.tif")
wkt_OF800 <- readLines("wkt_OF800", warn=F) %>% 
  paste0(collapse="")
terra::crs(r_grid) <- wkt_OF800

# without saving converted raster (tif) or figures (png)

#ind_data <- readRDS("processing/res_v10f.Rds")
#ind_data <- readRDS("processing/res_v10f_20241016.Rds")

ind_data <- readRDS("processing/res_v10aa.Rds")


rs <- purrr::map(ind_data, convert_nc, r0=r_grid, .progress=T, project=F)

saveRDS(rs, file="processing/res_v10aa_rasters.Rds")
# get the MSMDI rasters by from MSMDI_results.R 

# rs <- readRDS("processing/res_v10aa_rasters.Rds")
rs1 <- rs

# rs_MSMDIx <- readRDS("app/raster_OF800/rs_MSMDI.Rds")
rs <- c(rs1, rs_MSMDI)

names <- names(rs)


r_limits <- purrr::map2(rs, names, r_min_max, .progress=T) %>%
  bind_rows()

param_lims <- r_limits %>%
  group_by(param) %>%
  summarise(min=min(min,na.rm=T),
            max=max(max,na.rm=T),
            .groups = "drop")

write.table(param_lims, file="app/param_limits.csv", sep=";", row.names=F, col.names=T, quote=T, fileEncoding="UTF-8")

grid <- readRDS("processing/raster_cellid_to_WB.Rds")
grid <- readRDS("processing/raster_cellid_to_WB_OM3.Rds")

means <- purrr::map2(rs, names, wb_means, grid=grid, .progress=T) %>%
  bind_rows()


#saveRDS(means, file="OF800/res_v10f/res_v10f_WB_means_20241016.Rds")
#means <- readRDS("OF800/res_v10f/res_v10f_WB_means_20241016.Rds")
saveRDS(means, file="OF800/res_v10aa/res_v10aa_WB_means.Rds")
means <- readRDS("OF800/res_v10aa/res_v10aa_WB_means.Rds")
means0 <- readRDS("OF800/res_v10aa/res_v10aa_WB_means.v1.Rds")


# get modelled salinities


sal_summer <- list(parameter="psu_summer",
                   file="OF800/res_v10f/of800_v10f_salt_bc_0_5_10m_unweighted_av_monmean_summer_mean.nc")

sal_winter <- list(parameter="psu_winter",
                   file="OF800/res_v10f/of800_v10f_salt_bc_0_5_10m_unweighted_av_monmean_winter_mean.nc")

sal_data <- list(sal_summer=sal_summer, sal_winter=sal_winter)

rs_psu <- purrr::map(sal_data, convert_nc, r0=r_grid, .progress=T, project=F)

names_psu <- names(rs_psu)
means_psu <- purrr::map2(rs_psu, names_psu, wb_means, grid=grid, .progress=T) %>%
  bind_rows()

means_psu <- means_psu %>%
  mutate(season=stringr::str_remove(param,"psu_")) %>%
  select(Vannforeko, psu=mean, season) %>%
  filter(!is.na(psu))
  

means <- means %>%
  mutate(season=ifelse(stringr::str_detect(name,"summer"),"summer",NA_character_)) %>%
  mutate(season=ifelse(stringr::str_detect(name,"winter"),"winter",season)) %>%
  mutate(season=ifelse(stringr::str_detect(name,"Chl"),"summer",season))


means <- means %>%
  left_join(means_psu, by=c("Vannforeko", "season"))

saveRDS(means, file="OF800/res_v10aa/res_v10aa_WB_means_incl_psu.Rds")






# ------------------------------- plots of grids and WBs ---------------------

df_grid <- readRDS("OF800/wb_grid.Rds")



# fix the CRS of the raster
wkt_OF800 <- readLines("wkt_OF800", warn=F) %>% 
  paste0(collapse="")
terra::crs(r_grid) <- wkt_OF800
# wkt_OF800

crs_OF800 <- sf::st_crs(9354)
crs_OF800$wkt <- wkt_OF800


nwb <- length(unique(df_grid$Vannforeko))

mycols <-RColorBrewer::brewer.pal(12, "Paired")
mycols <- rep(mycols, 6)


ggplot() +
  geom_tile(data=df_grid, aes(x=x, y=y, fill=Vannforeko)) +
  geom_sf(data=shp_wb, fill=NA) +
  coord_sf(default_crs=crs_OF800, datum=crs_OF800) +
  scale_fill_manual(values=mycols, guide="none") +
  theme_minimal()

ggplot() +
  geom_point(data=df_grid, aes(x=lon, y=lat, colour=Vannforeko), size=0.3) +
  geom_sf(data=shp_wb, fill=NA) +
  coord_sf() +
  scale_colour_manual(values=mycols, guide="none") +
  theme_minimal()


dfx <- df_grid %>%
  filter(lon>10.8, lat < 59.4)

ggplot() +
  geom_tile(data=dfx, aes(x=lon, y=lat, fill=Vannforeko),colour="black", alpha=1) +
  geom_point(data=dfx, aes(x=lon, y=lat, colour=Vannforeko), size=0.6) +
  geom_sf(data=shp_wb, fill=NA) +
  coord_sf(xlim=c(11,11.5), ylim=c(58.9, 59.2)) + #, datum=4326, default_crs=crs_OF800) +
  #coord_sf(datum=4326, default_crs=crs_OF800) +
  scale_colour_manual(values=mycols, guide="none") +
  scale_fill_manual(values=mycols, guide="none") +
  theme_minimal()

sf::st_crs(4326)




