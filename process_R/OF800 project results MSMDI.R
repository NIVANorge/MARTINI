library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(terra)

source("processing/utils.R")

polys_geo <- readRDS("processing/grid_OF800_geo_vector_incl_NAs.Rds")


r_grid <- terra::rast("OF800/of800_grid_polar_stereo.tif")

# fix the CRS of the raster
wkt_OF800 <- readLines("wkt_OF800", warn=F) %>% 
  paste0(collapse="")
terra::crs(r_grid) <- wkt_OF800
plot(r_grid)


#wb_grid <- readRDS("processing/grid_OF800_geo_intersect_WBs.Rds")
df_grid <- polys_geo %>%
  sf::st_drop_geometry() %>%
  select(x,y,lon,lat) %>%
  mutate(MSMDI=NA)


scenario_list <- c("baseline",
                   "DINPsources100pcred",
                   "DINsources100pcred",
                   "DINsources50pcred",
                   "DINTSMsources100pcred",
                   "DINTSMsources50pcred",
                   "NTSMsources100pcred",
                   "v3_Scenario_A",
                   "v3_Scenario_B")    

scenario_names <- c("baseline",
                    "DIN100pc",
                    "DIN50pc",
                    "DINTSM100pc",
                    "DINTSM50pc",
                    "DINP100pc",
                    "NTSM100pc",
                    "Scenario A",
                    "Scenario B")

year_list <- c("2017","2018","2019")


msmdi_period_avg <- function(scenario, years, polys_geo){
  
  #  files <- paste0("OF800/res_v10f/MSMDI_modelling/",scenario,"/MSMDIpred_unproj_",years,"_BRTselect_martini_by_arendal_season_simp.tif")
 # files <- paste0("C:/Users/CJM/NIVA/230195 - Modellering av Oslofjorden - Documents/AP4_BioKval/MSMDI/Predictions/of800_v10aa/",scenario,"/MSMDIpred_unproj_",years,"_BRT_martini_season_simp.tif")
  
  files <- paste0("C:/Users/CJM/NIVA/230195 - Modellering av Oslofjorden - Documents/AP4_BioKval/MSMDI/Predictions/of800_v10aa/",scenario,"/MSMDIpred_unproj_",years,"_BRT_martini_by_arendal_filledmonth_noChl_season.tif")
  
  r_list <- lapply(files, terra::rast)
  
  # r_list <- lapply(r_list, terra::flip)
  
  val_list <- lapply(r_list, values)
  val_list <- lapply(val_list, function(x) x[,1])
  val_list <- lapply(val_list, function(x) ifelse(is.na(x),NA,x))
  
  df_list <- rep(polys_geo, length(val_list))
  
  df_year <- function(year, values, df){
    df$MSMDI <- values
    df$Year <- year
    return(df)
  }
  
  df <- polys_geo %>%
    sf::st_drop_geometry()
  
  df_list <- purrr::map2(years, val_list, df_year, df)
  
  df <- bind_rows(df_list)
  
  dfavg <- df %>%
    group_by(x,y) %>%
    summarise(MSMDI=mean(MSMDI,na.rm=T), .groups="drop") %>%
    mutate(MSMDI=ifelse(is.na(MSMDI),NA,MSMDI))
  
  
  dfavg <- polys_geo %>%
    left_join(dfavg, by=c("x","y"))
  
  return(dfavg)
}


read_msmdi <- function(df, scenario, r, outfolder="app/raster_OF800", proj="EPSG:3857", parameter="MSMDI", pngfolder=""){
  # save the projected raster result
  # browser()
  scen_res <- stringr::str_remove(scenario, "sources")
  scen_res <- stringr::str_remove(scen_res, "red")
  scen_res <- stringr::str_replace(scen_res, "v3_Scenario_", "Scenario ")
  
  file_r <- paste0(outfolder, "/", scen_res, "_", parameter, ".tif")
  
  df <- df %>%
    sf::st_drop_geometry()
  
  vals <- df[[parameter]] 
  
  names(r)[[1]] <- parameter
  values(r) <- vals
  
  r_proj <- terra::project(r, terra::crs(proj), mask=T)
  
  terra::writeRaster(r_proj, filename=file_r, overwrite=T)
  
  if(pngfolder!=""){
    
    pheight <- 12
    pwidth <- 15
    pdpi <- 300
    
    p <- plot_nc(r, proj=3857) # 
    file_png <- paste0(pngfolder, "/", scen_res, "_", parameter, ".png")
    ggsave(p, filename=file_png, height=pheight, width=pwidth, units="cm", dpi=pdpi, bg="white")
    
  }
  
  return(r)
}


list_df_msmdi <- purrr::map(scenario_list, msmdi_period_avg, year_list, polys_geo)

names(list_df_msmdi) <- scenario_list %>%
  stringr::str_replace("v3_Scenario_", "Scenario ")

#names_r <- stringr::str_remove(scenario_list, "sources")
#names_r <- stringr::str_remove(names_r, "red")
names_r <- paste0(scenario_names, "_MSMDI")

# res <- purrr::map2(list_df_msmdi, scenario_list, read_msmdi, r_grid, pngfolder="OF800/png")
res <- purrr::map2(list_df_msmdi, scenario_list, read_msmdi, r_grid, .progress=T)
names(res) <- names_r

null_res <- lapply(res, is.null) %>% unlist()
null_res <- c(1:length(res))[!null_res]
rs_MSMDI <- res[null_res]

saveRDS(rs_MSMDI, file="app/raster_OF800/res_v10aa_MSMDI.Rds")

# ------------------ check values baseline ------------------ 



dfx <- polys_geo %>%
  select(x,y)
rx <- rs_MSMDI[[1]]

dfx$MSMDI <- terra::values(rx)

plot(rx)

ggplot() +
  geom_sf(data=dfx, colour=NA, aes(fill=MSMDI)) +
  #coord_sf(xlim=c(10.2,11.0), ylim=c(59.2, 60)) +
  scale_fill_continuous(na.value = NA, name= "MSMDI", type="viridis") +
  theme_minimal()


ggplot() +
  geom_histogram(data=dfx, aes(x=MSMDI), binwidth = 0.01) +
  theme_minimal()

# ------------- ind_vals -------------------

# rs_MSMDI <- readRDS("app/raster_OF800/res_v10aa_MSMDI.Rds")


ind_vals <- readRDS("OF800/res_v10aa/res_v10aa_raster_vals.Rds")

msmdi_vals <- function(df){
  df <- df %>%
    sf::st_drop_geometry() %>%
    pull("MSMDI")
}

x <- rs_MSMDI[[1]]
x <- terra::values(x, dataframe=T) %>% pull(1)

for(i in 1:length(rs_MSMDI)){
  x <- rs_MSMDI[[i]]
  x <- terra::values(x, dataframe=T) %>% 
    pull(1)
  ind_vals <- ind_vals %>%
    append(list(x))
  name_msmdi <- names(rs_MSMDI)[i]
  names(ind_vals)[length(ind_vals)] <- name_msmdi
}


names(ind_vals)

saveRDS(ind_vals, "OF800/res_v10aa/res_v10aa_raster_vals.Rds")


