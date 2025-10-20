

adj_EQR<- function(bio,sup){
  sup <- ifelse(is.na(sup),1,sup)
  if(!is.na(bio)){
    if(bio>=0.6 & bio < 0.8){
      if(sup<0.6){
        # reduce bio by 1 class
        bio <- bio - 0.2
        # but new overall EQR should not be lower than supporting
        bio <- ifelse(bio < sup, sup, bio)
      }
    }
    else if(bio>=0.8 & bio < 1){
      if(sup<0.8){
        # reduce bio by 1 class
        bio <- bio - 0.2
        # but new overall EQR should not be lower than supporting
        bio <- ifelse(bio < sup, sup, bio)
      }
    }
    else if(bio>=1){
      if(sup<0.8){
        # reduce bio by 1 class
        bio <- 0.799
      }
    }
  }
  return(bio)
}


aggregate_wb <- function(df){
  
  
  # supporting - first average by season
  
  res_wb_sup <- df  %>%
    filter(QEtype=="Sup") %>%
    rowwise() %>%
    mutate(season = indicator_info(Indicator, "season"),
           pressure = indicator_info(Indicator, "pres"),
           group_sup = indicator_info(Indicator, "group")) %>%
    ungroup()
  
  
  res_wb_sup_avg <- res_wb_sup %>%
    group_by(WB, Period,QEtype, Kvalitetselement, scenario, pressure, 
             season, group_sup) %>%
    summarise(EQR=mean(EQR, na.rm=T), .groups = "drop") 
  
  res_wb_sup_avg_info <- res_wb_sup_avg %>%
    select(WB, Period, scenario,group_sup, EQR) %>%
    pivot_wider(names_from = "group_sup", values_from = "EQR")

  
  res_wb_sup_avg <- res_wb_sup_avg %>%
    group_by(WB, Period, QEtype, Kvalitetselement, scenario, pressure) %>%
    summarise(EQR=mean(EQR, na.rm=T), .groups = "drop") 

  
  res_wb_sup <- res_wb_sup_avg %>%
    filter(QEtype=="Sup") %>%
    select(Period, WB, QEtype, Kvalitetselement, scenario, pressure, EQR) %>%
    #select(Period, WB, scenario, Kvalitetselement, Indikator,EQR, Status, QEtype) %>%
    group_by(WB, Period, QEtype, scenario) %>%
    arrange(EQR) %>%
    slice(1) %>%
    ungroup()
  
  res_wb_sup <- res_wb_sup %>%
    select(Period, WB, scenario, 
           Worst_Supporting=pressure, Supporting=EQR) %>%
    left_join(res_wb_sup_avg_info, by=c("WB", "Period", "scenario"))
  
  # biological
  res_wb_bio_QE <- df %>%
    filter(QEtype=="Bio") %>%
    select(Period, WB, scenario, Kvalitetselement, Indikator,EQR, Status, QEtype) %>%
    group_by(WB, Period, QEtype, Kvalitetselement, scenario) %>%
    summarise(EQR = mean(EQR, na.rm=T), .groups = "drop")
  
  res_wb_bio_QE <- res_wb_bio_QE %>%
    select(Period, WB, scenario, Kvalitetselement, EQR, QEtype) %>%
    group_by(WB, Period, QEtype, scenario) %>%
    arrange(EQR) %>%
    slice(1) %>%
    ungroup()
  
  
  res_wb_bio <- res_wb_bio_QE %>%
    select(Period, WB, scenario, 
           Worst_Biological=Kvalitetselement,
           Biological=EQR)
  
  res_wb <- merge(res_wb_bio, res_wb_sup, 
                  by=c("Period", "WB", "scenario"),
                  all=T)
  
  res_wb <- res_wb %>%
    rowwise() %>%
    mutate(EQR=adj_EQR(Biological, Supporting)) %>%
    mutate(Status=ifelse(is.na(EQR),NA,
                         ifelse(EQR<0.2,"Bad",
                                ifelse(EQR<0.4,"Poor",
                                       ifelse(EQR<0.6,"Mod",
                                              ifelse(EQR<0.8,"Good","High")))))) %>%
    ungroup()
  
  res_wb <- res_wb %>%
    relocate(WB,Period, scenario, Biological, Supporting, EQR, Status, Worst_Biological, Worst_Supporting)
  
  return(res_wb)
  
}



aggregate_wb_0 <- function(df){
  
  
  # supporting
  
  res_wb_sup_avg <- df %>%
    filter(QEtype=="Sup") %>%
    group_by(WB, Period, QEtype, scenario) %>%
    summarise(EQRavg=mean(EQR, na.rm=T), .groups = "drop") %>%
    filter(QEtype=="Sup")
  
  res_wb_sup <- df %>%
    filter(QEtype=="Sup") %>%
    select(Period, WB, scenario, Kvalitetselement, Indikator,EQR, Status, QEtype) %>%
    group_by(WB, Period, QEtype, scenario) %>%
    arrange(EQR) %>%
    slice(1) %>%
    ungroup()
  
  res_wb_sup <- res_wb_sup %>%
    select(Period, WB, scenario, 
           Worst_Supporting=Indikator) %>%
    #Supporting=EQR)
    left_join(res_wb_sup_avg, by=c("Period", "WB", "scenario")) %>%
    select(Period, WB, scenario, 
           Worst_Supporting, Supporting=EQRavg)
  
  # biological
  res_wb_bio_QE <- df %>%
    filter(QEtype=="Bio") %>%
    select(Period, WB, scenario, Kvalitetselement, Indikator,EQR, Status, QEtype) %>%
    group_by(WB, Period, QEtype, Kvalitetselement, scenario) %>%
    summarise(EQR = mean(EQR, na.rm=T), .groups = "drop")
  
  res_wb_bio_QE <- res_wb_bio_QE %>%
    select(Period, WB, scenario, Kvalitetselement, EQR, QEtype) %>%
    group_by(WB, Period, QEtype, scenario) %>%
    arrange(EQR) %>%
    slice(1) %>%
    ungroup()
  
  
  res_wb_bio <- res_wb_bio_QE %>%
    select(Period, WB, scenario, 
           Worst_Biological=Kvalitetselement,
           Biological=EQR)
  
  res_wb <- merge(res_wb_bio, res_wb_sup, 
                  by=c("Period", "WB", "scenario"),
                  all=T)
  
  res_wb <- res_wb %>%
    rowwise() %>%
    mutate(EQR=adj_EQR(Biological, Supporting)) %>%
    mutate(Status=ifelse(is.na(EQR),NA,
                         ifelse(EQR<0.2,"Bad",
                                ifelse(EQR<0.4,"Poor",
                                       ifelse(EQR<0.6,"Mod",
                                              ifelse(EQR<0.8,"Good","High")))))) %>%
    ungroup()
  
  res_wb <- res_wb %>%
    select(WB,Period, scenario, Biological, Supporting, EQR, Status, Worst_Biological, Worst_Supporting)
  
  return(res_wb)
  
}

indicator_info<- function(param, out=NA_character_){
  list_param <- c("Chl_summer", "Chl", "DO_bot", "MSMDI",
                  "NO3_summer", "NO3_winter", 
                  "NH4_summer", "NH4_winter", 
                  "TN_summer", "TN_winter", 
                  "TP_summer", "TP_winter", 
                  "PO4_summer", "PO4_winter", 
                  "Secchi")
  list_KE <- rep("Fysisk-kjemiske", length(list_param))
  list_QE <- rep("Physical-chemical", length(list_param))
  
  
  
  list_KE <- ifelse(list_param %in% c("Chl_summer","Chl"), "Planteplankton", list_KE)
  list_KE <- ifelse(list_param %in% c("MSMDI"), "Makroalger", list_KE)
  list_QE <- ifelse(list_param %in% c("Chl_summer","Chl"), "Phytoplankton", list_QE)
  list_QE <- ifelse(list_param %in% c("MSMDI"), "Macroalgae", list_QE)

  list_KE_type <- list_KE
  list_KE_type <- ifelse(list_KE_type=="Fysisk-kjemiske","Støtteparameter","Biologiske")
  list_QE_type <- list_QE
  list_QE_type <- ifelse(list_QE_type=="Physical-chemical","Sup","Bio")
  
  list_indikator <- c("Klorofyll a, sommer (µg/l)",
                      "Klorofyll a, 90. percentil (µg/l)",
                      "Oksygen (ml O2/l)", 
                      "MSMDI (Nedre voksegrense) - EQR", 
                      "Nitrat-nitrogen, sommer (µg N/l)", "Nitrat-nitrogen, vinter (µg N/l)", 
                      "Ammonium-nitrogen, sommer (µg N/l)", "Ammonium-nitrogen, vinter (µg N/l)", 
                      "Total nitrogen, sommer (µg N/l)", "Total nitrogen, vinter (µg N/l)", 
                      "Total fosfor, sommer (µg P/l)", "Total fosfor, vinter (µg P/l)",
                      "Fosfat-fosfor, sommer (µg P/l)", "Fosfat-fosfor, vinter (µg P/l)", 
                      "Siktdyp (m), sommer")
  list_indikator_short <- c("Klfa sommer", "Klfa 90pct",
                            "Oksygen", "MSMDI", 
                            "NO3-N sommer", "NO3-N vinter", 
                            "NH4-N sommer", "NH4-N vinter", 
                            "TN sommer", "TN vinter", 
                            "TP sommer", "TP vinter",
                            "PO4-P sommer", "PO4-P vinter", 
                            "Siktdyp")
  
  list_season <- c("", "",
                   "", "", 
                   "Sommer", "Vinter", 
                   "Sommer", "Vinter", 
                   "Sommer", "Vinter", 
                   "Sommer", "Vinter",
                   "Sommer", "Vinter", 
                   "Sommer")
  
  list_pressure <- c("", "",
                "Organisk", "", 
                "Eutrofiering", "Eutrofiering", 
                "Eutrofiering", "Eutrofiering", 
                "Eutrofiering", "Eutrofiering", 
                "Eutrofiering", "Eutrofiering",
                "Eutrofiering", "Eutrofiering", 
                "Eutrofiering")
  
  list_group_sup <- c("", "",
                     "sup_org", "", 
                     "sup_sommer", "sup_vinter", 
                     "sup_sommer", "sup_vinter", 
                     "sup_sommer", "sup_vinter", 
                     "sup_sommer", "sup_vinter",
                     "sup_sommer", "sup_vinter", 
                     "sup_sommer")
  
  
  
  out <- ifelse(is.na(out),"", tolower(out))
  if(out=="shortname"){
    var_out <- list_indikator_short
  }else if(out=="qe"){
    var_out <- list_QE
  }else if(out %in% c("qetype","qe_type")){
    var_out <- list_QE_type
  }else if(out=="ke"){
    var_out <- list_KE
  }else if(out %in% c("ketype","ke_type")){
    var_out <- list_KE_type
  }else if(out %in% c("season")){
    var_out <- list_season
  }else if(out %in% c("pressure","pres","presfaktor")){
    var_out <- list_pressure
  }else if(out %in% c("group","grp")){
    var_out <- list_group_sup
  }else{
    var_out <- list_indikator
  }
  
  ix <- (1:length(list_param))[tolower(list_param)==tolower(param)]
  
  
  return(var_out[ix]) 
  
}


grid_points_to_wb <- function(r, shp){
  require(terra)
  
  
  vars <- names(r)
  vars <- vars[!vars %in% c("lon_rho","lat_rho")]
  vars <- vars[1]
  
  ix <- match(vars, names(r))
  
  msg <- paste0(names(r)[ix], " [", paste0(names(r), collapse=", "), "]")
  cli::cli_inform(msg)
  
  # get the values from the results nc file
  vals <- values(r)[,ix]
  
  df <- data.frame(val=vals)
  coords<- terra::xyFromCell(r, 1:nrow(df)) %>%
    as.data.frame()
  
  df <- bind_cols(coords,df)
  names(df)[3] <- "val"
  
  
  sf <- df %>% 
    mutate(id=row_number()) %>%
    sf::st_as_sf(coords=c("x","y"), crs=terra::crs(r), remove=F)
  
  sf <- sf %>% 
    sf::st_transform(crs=sf::st_crs(shp))
  
  shp <- sf::st_make_valid(shp)
  
  # shp <- shp %>% 
  #   select(OBJECTID, Vannforeko, Vannfore_1)
  
  pts <- sf::st_intersection(sf, shp)
  
  df <- pts %>%
    sf::st_drop_geometry()
  
  coords <- sf::st_coordinates(pts) %>% 
    as.data.frame() 
  names(coords) <- c("lon","lat")
  
  pts <- pts %>%
    bind_cols(coords)
  
  return(pts)
  
}


r_min_max <- function(r, name){
  require(terra)
  
  param <- names(r)[1]
  vals <- values(r)[,1]
  
  min <- min(vals, na.rm=T)
  max <- max(vals, na.rm=T)
  
  df <- data.frame(min=min, max=max)
  
  df <- df %>%
    mutate(name=name) %>%
    mutate(param=param) %>%
    relocate(name, param)
  
  return(df)
}


wb_means <- function(r, name, grid){
  require(terra)
  
  rnames <- names(r)
  rnames <- rnames[!rnames %in% c("lon_rho", "lat_rho")]
  
  param <- rnames[1]
  vals <- values(r)[,param]
  
  df <- data.frame(val=vals)
  df <- df %>%
    mutate(id=row_number()) %>%
    relocate(id)

  df <- df %>%
    left_join(grid, by="id")
  
  df <- df %>%
    mutate(f=ifelse(is.na(val),0,f))

  df_mean <- df %>%
    mutate(OK=ifelse(is.na(val),0,1)) %>%
    group_by(Vannforeko) %>%
    summarise(n=n(),n_ok=sum(OK,na.rm=T), 
              mean=sum(val*f, na.rm=T) /sum(f,na.rm=T), 
              sd=sd(val, na.rm=T), .groups="drop")
  
  df_mean <- df_mean %>%
    filter(!is.na(Vannforeko)) %>%
    mutate(name=name) %>%
    mutate(param=param) %>%
    mutate(mean=ifelse(is.nan(mean),NA,mean))
  
  return(df_mean)
}

wb_means_old <- function(r, name, r0, grid){
  require(terra)
  
  param <- names(r)[1]
  vals <- values(r)[,1]
  
  grid <- grid %>%
    rename(bathy=val)
  
  df <- data.frame(val=vals)
  coords<- terra::xyFromCell(r_grid, 1:nrow(df)) %>%
    as.data.frame()
  
  df <- bind_cols(coords,df)
  
  df <- grid %>%
    left_join(df, by=c("x","y"))
  
  df <- df %>%
    sf::st_drop_geometry()


  df <- df %>%
    mutate(OK=ifelse(is.na(val),0,1)) %>%
    group_by(Vannforeko, Vannfore_1) %>%
    summarise(n=n(),n_ok=sum(OK,na.rm=T), mean=mean(val, na.rm=T), 
              sd=sd(val, na.rm=T), .groups="drop")
  
  df <- df %>%
    mutate(name=name) %>%
    mutate(param=param) %>%
    mutate(mean=ifelse(is.nan(mean),NA,mean))
  
  return(df)
}


plot_nc <- function(r, proj=NA, shp=NULL){
  require(terra)
  require(ggplot2)
  
  if(is.na(proj)){
    proj <- sf::st_crs(r) 
  }else{
    r <- terra::project(r, terra::crs(paste0("EPSG:",proj)), mask=T)
  }
  
  
  #df <- data.frame(r %>% as.data.frame()
  df <- data.frame(val=values(r))
  coords<- terra::xyFromCell(r, 1:nrow(df)) %>%
    as.data.frame()
  
  df <- bind_cols(coords,df)
  label <- names(r)[1]
  names(df)[3] <- "val"
  
  df <- df %>%
    filter(!is.na(val))
  
  p <- ggplot() +
    geom_tile(data=df, aes(x=x,y=y,fill=val))
  
  if(!is.null(shp)){
    p <- p + 
      geom_sf(data=shp) 
  }
  
  p <- p +
    theme_minimal() +
    scale_fill_distiller(label, palette = "Spectral", na.value="white") +
    coord_sf(default_crs=proj) +
    labs(x="",y="")
  return(p)
}






convert_nc <- function(ind_data, r0, proj="EPSG:3857", outfolder="", pngfolder="", 
                       overwrite=F, project=T){
  
  require(terra)
  #' there seems to be a problem with the coordinates in the average result
  #' files so we will take a known raster and replace the values
  #' with the values from our variable in question

  
  file_r <- paste0(outfolder, "/", ind_data$scenario, "_", ind_data$parameter, ".tif")
  if(file.exists(file_r) & overwrite==F){
    msg <- paste0("File exists: ", file_r , " (overwrite=F)")
    cli::cli_inform(msg)
    return(NULL)
  }
  file_png <- paste0(pngfolder, "/", ind_data$scenario, "_", ind_data$parameter, ".png")
  if(file.exists(file_png) & overwrite==F){
    msg <- paste0("File exists: ", file_png , " (overwrite=F)")
    cli::cli_inform(msg)
    return(NULL)
  }
  
  # read nc file
  if(!file.exists(ind_data$file)){
    msg <- paste0("Input file not found: ", ind_data$file)
    cli::cli_inform(msg)
    return(NULL)
  }
  r <- terra::rast(ind_data$file)
  
  vars <- names(r)
  vars <- vars[!vars %in% c("lon_rho","lat_rho")]
  vars <- vars[1]
  
  ix <- match(vars, names(r))
  
  msg <- paste0(names(r)[ix], " [", paste0(names(r), collapse=", "), "]")
  cli::cli_inform(msg)
  
  # get the values from the results nc file
  vals <- values(r)[,ix]
  
  if(length(vals)!=length(values(r0))){
    msg <- paste0("Number of points in file not equal to number of grid cells:\n",
                  basename(file))
    stop(msg)
  }
  
  if(ind_data$parameter=="DO_bot"){
    # convert from mmol/m3 to ml/L
    # https://www.aqua-calc.com/calculate/mole-to-volume-and-weight
    vals <- vals * 0.0223924
  }
  if(ind_data$parameter %in% c("NO3_summer","NO3_winter","NH4_summer","NH4_winter", "TN_summer", "TN_winter")){
    # mmol/L  to mg-N/L
    vals <- vals * 14.007
  }
  if(ind_data$parameter %in% c("TP_summer","TP_winter","PO4_summer","PO4_winter")){
    # mmol/L  to mg-P/L
    vals <- vals *  30.974
  }
  
  r <- r0
  names(r)[[1]] <- ind_data$parameter
  values(r) <- vals
  
  
  # project the raster to LAEA
  if(project==T){
    #use project=F to get raster values from the uprojected raster
    r_proj <- terra::project(r, terra::crs(proj), mask=T)
  }else{
    r_proj <- r
  }
  
  if(outfolder!=""){
    # save the projected raster result
    file_r <- paste0(outfolder, "/", ind_data$scenario, "_", ind_data$parameter, ".tif")
    terra::writeRaster(r_proj, filename=file_r, overwrite=T)
  }
  if(pngfolder!=""){
    # save plot
    pheight <- 12
    pwidth <- 15
    pdpi <- 300
    
    p <- plot_nc(r, proj=3857) # 
    file_png <- paste0(pngfolder, "/", ind_data$scenario, "_", ind_data$parameter, ".png")
    ggsave(p, filename=file_png, height=pheight, width=pwidth, units="cm", dpi=pdpi, bg="white")
  }
  return(r_proj)
}

