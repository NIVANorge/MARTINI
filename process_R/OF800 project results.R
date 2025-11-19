library(tidyr)
library(dplyr)

source("process_R/utils.R")

r_grid <- terra::rast("OF800/of800_grid_polar_stereo.tif")

# fix the CRS of the raster
wkt_OF800 <- readLines("wkt_OF800", warn=F) %>% 
  paste0(collapse="")
terra::crs(r_grid) <- wkt_OF800

basefolder <- "./OF800/"

result_set <- "v10ad"

scenario_ids <- c("",
                  "pristine",
                  "ScenarioA",
                  "ScenarioB")

scenario_names <- c("baseline",
                    "Pristine",
                    "Scenario A",
                    "Scenario B")


folders <- c("OF800/res_v10ad/OF800_v10ad/",
             "OF800/res_v10ad/OF800_v10ad_v3_pristine/",
             "OF800/res_v10ad/OF800_v10ad_v3_Scenario_A/",
             "OF800/res_v10ad/OF800_v10ad_v3_Scenario_B/")

# files also have prefixes depending on scenario
scenario_prefix <- c("of800_v10ad",
                     "of800_v10ad_v3_pristine",
                     "of800_v10ad_v3_Scenario_A",
                     "of800_v10ad_v3_Scenario_B")

files <- folders %>% 
  lapply(list.files, pattern="*.nc", full.names=T) %>%
  unlist()

filenames <- basename(files)


param_ids <- c( "light_Chl_vbc_0_5_10m_unweighted_av_monmean_summer_mean",
                "light_Chl_bc_0_5_10m_unweighted_av_timpctl90",
                "N3_n_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "N3_n_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                "N4_n_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "N4_n_0_5_10m_unweighted_av_monmean_winter_mean",
                "O2_o_vbc3_s0_timpctl10",
                "TotN_bc_0_5_10m_unweighted_av_monmean_summer_mean",      
                "TotN_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                "TotP_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "TotP_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                "TRP_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "TRP_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                "zsd_vbc_monmean_summer_mean")

params <- c("Chl_summer",
            "Chl",
            "NO3_summer", "NO3_winter",
            "NH4_summer", "NH4_winter",
            "DO_bot",
            "TN_summer", "TN_winter",
            "TP_summer", "TP_winter",
            "PO4_summer", "PO4_winter",   
            "Secchi")



# sid <- 1:length(param_ids)
# pid <- 1:length(param_ids)


ind_data <- list()

for(sid in 1:length(scenario_ids)){
  for(pid in 1:length(param_ids)){
    
    file <- paste0(folders[sid], scenario_prefix[sid],"_",  param_ids[pid], ".nc")
    
    if(!file.exists(file)){
      cat(paste0("file not found!    ", file, "\n"))
    }
    
    dat <- list(list(parameter=params[pid], 
                scenario=scenario_names[sid], 
                file=file))
    
    ind_name <- paste0(scenario_names[sid],"_", params[pid])
    ind_data <- append(ind_data, dat)
    names(ind_data)[length(ind_data)] <- ind_name
  }
}

saveRDS(ind_data, file=paste0("processing/res_", result_set, ".Rds"))

#saveRDS(r_grid, file="processing/grid_OF800_PolarSterographic.Rds")


ind_data <- readRDS(paste0("processing/res_", result_set, ".Rds"))

do_replace <- T

rs <- purrr::map(ind_data, convert_nc, r0=r_grid, outfolder=paste0("app/raster_OF800"), overwrite=do_replace, .progress=T)

rs <- purrr::map(ind_data, convert_nc, r0=r_grid, outfolder=paste0("OF800/res_", result_set, "/tif"), overwrite=do_replace, .progress=T)
rs <- purrr::map(ind_data, convert_nc, r0=r_grid, pngfolder=paste0("OF800/res_", result_set, "/png"), overwrite=do_replace,.progress=T)

# without saving converted raster (tif) or figures (png)
# rs <- purrr::map(ind_data, convert_nc, r0=r_grid, .progress=T)


# redo DO
ids <- (1:length(ind_data))[stringr::str_detect(names(ind_data),"DO_bot")]
ind_data_DO <- ind_data[ids]
rsDO <- purrr::map(ind_data_DO, convert_nc, r0=r_grid, outfolder="OF800/tif", pngfolder="OF800/png", .progress=T)

r_grid <- readRDS("processing/grid_OF800_PolarSterographic.Rds")


