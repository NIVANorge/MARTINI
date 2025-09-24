library(tidyr)
library(dplyr)

source("process_R/utils.R")

r_grid <- terra::rast("OF800/of800_grid_polar_stereo.tif")

# fix the CRS of the raster
wkt_OF800 <- readLines("wkt_OF800", warn=F) %>% 
  paste0(collapse="")
terra::crs(r_grid) <- wkt_OF800

basefolder <- "./OF800/"

result_set <- "v10f"
result_set <- "v10aa"

scenario_ids <- c("",
                  "DINsources100pcred",
                  "DINTSMsources100pcred",
                  "DINPsources100pcred",
                  "NTSMsources100pcred")

scenario_names <- c("baseline",
                    "DIN100pc",
                    "DINTSM100pc",
                    "DINP100pc",
                    "NTSM100pc")

# DINsources100pcred 100% reduction of DIN loads
# DINTSMsources100pcred 100% reduction of DIN loads and TSM loads
# DINPsources100pcred 100% reduction of DIN and DIP loads
# NTSMsources100pcred 100% reduction of N loads and TSM loads

folders <- c("OF800/res_v10aa/OF800_v10aa/",
             "OF800/res_v10aa/OF800_v10aa_DINsources100pcred/",
             "OF800/res_v10aa/OF800_v10aa_DINTSMsources100pcred/",
             "OF800/res_v10aa/OF800_v10aa_DINPsources100pcred/",
             "OF800/res_v10aa/OF800_v10aa_NTSMsources100pcred/")

# files also have prefixes depending on scenario
scenario_prefix <- c("of800_v10aa",
                     "of800_v10aa_DINsources100pcred",
                     "of800_v10aa_DINTSMsources100pcred",
                     "of800_v10aa_DINPsources100pcred",
                     "of800_v10aa_NTSMsources100pcred")

files <- folders %>% 
  lapply(list.files, pattern="*.nc", full.names=T) %>%
  unlist()

filenames <- basename(files)


param_ids <- c(# "light_Chl_0_5_10m_unweighted_av_monmean_summer_mean",
                #"light_Chl_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "light_Chl_sbc_0_5_10m_unweighted_av_monmean_summer_mean",
                "light_Chl_bc_0_5_10m_unweighted_av_timpctl90",
                "N3_n_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "N3_n_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                #"N4_n_0_5_10m_unweighted_av_monmean_summer_mean",
                #"N4_n_0_5_10m_unweighted_av_monmean_winter_mean",
                "N4_n_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "N4_n_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                #  "O2_o_bc_bottom_hraw_timpctl10",
                "O2_o_sbc_s0_timpctl10",
                "TotN_bc_0_5_10m_unweighted_av_monmean_summer_mean",      
                "TotN_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                "TotP_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "TotP_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                "TRP_bc_0_5_10m_unweighted_av_monmean_summer_mean",
                "TRP_bc_0_5_10m_unweighted_av_monmean_winter_mean",
                "zsd_bc_monmean_summer_mean")

params <- c("Chl_summer",
            "Chl",
            "NO3_summer", "NO3_winter",
            "NH4_summer", "NH4_winter",
            "DO_bot",
            "TN_summer", "TN_winter",
            "TP_summer", "TP_winter",
            "PO4_summer", "PO4_winter",   
            "Secchi")


sid <- 1:length(param_ids)
pid <- 1:length(param_ids)


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

rs <- purrr::map(ind_data, convert_nc, r0=r_grid, outfolder=paste0("OF800/res_", result_set, "/tif"), .progress=T)
rs <- purrr::map(ind_data, convert_nc, r0=r_grid, pngfolder=paste0("OF800/res_", result_set, "/png"), .progress=T)

# without saving converted raster (tif) or figures (png)
# rs <- purrr::map(ind_data, convert_nc, r0=r_grid, .progress=T)


# redo DO
ids <- (1:length(ind_data))[stringr::str_detect(names(ind_data),"DO_bot")]
ind_data_DO <- ind_data[ids]
rsDO <- purrr::map(ind_data_DO, convert_nc, r0=r_grid, outfolder="OF800/tif", pngfolder="OF800/png", .progress=T)

r_grid <- readRDS("processing/grid_OF800_PolarSterographic.Rds")


