# help functions for app


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

adj_EQR<- function(bio,sup, reduce2classes=F){
  
  if(length(bio)==0){
    return(NA)
  }
  if(length(sup)==0){
    sup <- 1
  }
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
      if(sup<0.6){
        # reduce bio by 2 classes
        reduce_by <- ifelse(reduce2classes, 0.4,0.2)
        bio <- bio - reduce_by
        # but new overall EQR should not be lower than supporting
        bio <- ifelse(bio < sup, sup, bio)
      }else if(sup<0.8){
        # reduce bio by 1 class
        bio <- bio - 0.2
        # but new overall EQR should not be lower than supporting
        bio <- ifelse(bio < sup, sup, bio)
      }
    }
    else if(bio>=1){
      if(sup<0.6){
        # reduce bio by 1 or 2 classes
        bio <- ifelse(reduce2classes, 0.599, 0.799)
      }else if(sup<0.8){
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
           pressure = indicator_info(Indicator, "pres")) %>%
    ungroup()
  
  
  res_wb_sup_avg <- res_wb_sup %>%
    group_by(WB, Period,QEtype, Kvalitetselement, scenario, pressure, season) %>%
    summarise(EQR=mean(EQR, na.rm=T), .groups = "drop") 
  
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
           Worst_Supporting=pressure, Supporting=EQR)
  
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
  }else{
    var_out <- list_indikator
  }
  
  ix <- (1:length(list_param))[tolower(list_param)==tolower(param)]
  
  
  return(var_out[ix]) 
  
}

# 
# aggregate <- function(df, baseline="baseline", map_status=F){
#   if(!"Indicator" %in% names(df)){
#     cat("!!!!!!!!!!!! aggregation without indicator !!!!!!!!!!!\n")
#     cat(paste0("    map status = ", map_status, "\n"))
#   }
#   # results for map are different from those for the indicator tables
#   dfgrp <- param_group_df()
# 
#   scenario_selected <- df$scenario[1]
#   
#   if(map_status==T){
#     df <- df %>%
#       select(Indicator,Indikator, scenario, EQR)
#   }else{
#     df <- df %>%
#     select(Indicator,Indikator, scenario, EQR, EQR_comp)
#   if(scenario_selected==baseline){
#     df <- df %>%
#       select(Indicator,Indikator, scenario, EQR)
#   }else{
#     df0 <- df %>%
#       select(Indicator,Indikator, EQR=EQR_comp) %>%
#       mutate(scenario = baseline)
#     df <- df %>%
#       select(Indicator,Indikator, scenario, EQR) %>%
#       bind_rows(df0)
#   }
#   }
#   
#   
#     
#   df <- df %>%
#     left_join(dfgrp, by="Indicator")
#   
#   res_group0_min <- df %>%
#     group_by(scenario, group0) %>%
#     arrange(EQR) %>%
#     slice(1) %>%
#     ungroup()
#   
#   
#   res_group0_sup_avg <- df %>%
#     group_by(scenario, group0) %>%
#     summarise(EQRavg=mean(EQR, na.rm=T), .groups = "drop") %>%
#     filter(group0=="sup")
#   
#   
#   res_sup <- res_group0_min %>%
#     filter(group0=="sup") %>%
#     select(scenario, 
#            Worst_Supporting=Indikator) %>%
#     #Supporting=EQR)
#     left_join(res_group0_sup_avg, by=c("scenario")) %>%
#     select(scenario, 
#            Worst_Supporting, Supporting=EQRavg)
#   
#   res_bio <- res_group0_min %>%
#     filter(group0=="bio") %>%
#     select(scenario, 
#            Worst_Biological=Indikator,
#            Biological=EQR)
#   
#   if(nrow(res_bio)==0){
#     res_bio <- df %>%
#       distinct(scenario) %>%
#       mutate(Worst_Biological=NA_character_,
#              Biological=NA)
#   }
#   if(nrow(res_sup)==0){
#     res_sup <- df %>%
#       distinct(scenario) %>%
#       mutate(Worst_Supporting=NA_character_,
#              Supporting=NA)
#   }
#   
#   res <- merge(res_bio, res_sup, 
#                   by=c("scenario"),
#                   all=T)
#   
#   
#   # adjust overall EQR according to WFD
#   # a less than good supporting status can reduce overall status
#   res <- res %>%
#     rowwise() %>%
#     mutate(EQR=adj_EQR(Biological, Supporting)) %>%
#     mutate(Status=ifelse(is.na(EQR),NA,
#                          ifelse(EQR<0.2,"Bad",
#                                 ifelse(EQR<0.4,"Poor",
#                                        ifelse(EQR<0.6,"Mod",
#                                               ifelse(EQR<0.8,"Good","High")))))) %>%
#     ungroup()
#   browser()
#   return(res)
#   
#   
# }


plot_pal <- function(pal=NA_character_){
  
  pal_list <- c("AS","wes","rainbow","spectral","viridis")
  
  pal_id <- tryCatch({
    n <- as.numeric(pal)
  },
  warning=function(w){
    return(NA)
  })
  
  if(!is.na(pal_id)){
    pal_id <- min(length(pal_list), pal_id)
    pal_id <- max(1, pal_id)
    pal <- pal_list[pal_id]
  }else{
    pal <- ifelse(is.na(pal),pal_list[1], pal)
    pal <- pal_list[pal_list==pal]
    if(length(pal)==0){
      pal <- pal_list[1]
    }
  }
  
  if(pal=="AS"){
    cols<-c("#ffffff","#4ed1d1","#00ffff","#00e38c","#00c000",
            "#78de00","#ffff00","#ffa200","#ff0000","#ff1e78",
            "#ec3fff","#7c22ff","#4040ff","#20207e","#242424",
            "#7e7e7e","#e0e0e0","#eed3bb","#d8a476","#aa7647",
            "#663300")
    cols<-cols[2:21]
  }
  if(pal=="wes"){
    cols <- wes_palette("Zissou1", n=20, type = "continuous")
    cols <- as.character(cols)
  }
  if(pal=="rainbow"){
    cols<-rev(rainbow(20))
  }
  if(pal=="viridis"){
    cols<-viridis(20)
  }
  
  if(pal=="spectral"){
    cols<-rev(brewer.pal(11, "Spectral"))
  }
  return(cols)
}

# #plot_pal("wes")
# 
# param_group_df <- function(){
#   params<-c("Chl_summer",
#             "Chl",
#             "MSMDI",
#             "NQI1","H","Secchi","DO_bot",
#             "NH4_summer","NH4_winter",
#             "NO3_summer","NO3_winter",
#             "PO4_summer","PO4_winter",
#             "TN_summer","TN_winter",
#             "TP_summer","TP_winter")
#   group0<-c("bio",
#             "bio",
#             "bio","bio",
#             "bio","sup",
#             "sup",
#             "sup",
#             "sup",
#             "sup",
#             "sup",
#             "sup",
#             "sup",
#             "sup",
#             "sup",
#             "sup",
#             "sup")
#   group1<-c("Phytoplankton",
#             "Phytoplankton",
#             "Macroalgae","Benthic fauna",
#             "Benthic fauna","Secchi",
#             "Oxygen",
#             "Nutrients",
#             "Nutrients",
#             "Nutrients",
#             "Nutrients",
#             "Nutrients",
#             "Nutrients",
#             "Nutrients",
#             "Nutrients",
#             "Nutrients",
#             "Nutrients")
#   group2<-c("Phytoplankton",
#             "Phytoplankton",
#             "Macroalgae","Benthic fauna",
#             "Benthic fauna","Secchi",
#             "Oxygen",
#             "N","N",
#             "N","N",
#             "P","P",
#             "N","N",
#             "P","P")
#   
#   df <- data.frame(Indicator=params, group0, group1, group2)
# 
#   return(df)  
# }


param_group<-function(parameter){
  params<-c("Ecological Status",
            "Chl_summer",
            "Chl",
            "MSMDI",
            "NQI1","H","Secchi","DO_bot",
            "NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter",
            "PO4_summer","PO4_winter",
            "TN_summer","TN_winter",
            "TP_summer","TP_winter")
  group<-c("Økologisk tilstand",
            "Planteplankton",
            "Planteplankton",
            "Makroalger","Bentisk fauna",
            "Bentisk fauna","Siktdyp",
            "Oksygen",
            "Næringssalter",
            "Næringssalter",
            "Næringssalter",
            "Næringssalter",
            "Næringssalter",
            "Næringssalter",
            "Næringssalter",
            "Næringssalter",
            "Næringssalter",
            "Næringssalter")
  
  group<-c("Økologisk tilstand",
           "Planteplankton",
           "Planteplankton",
           "Makroalger","Bentisk fauna",
           "Bentisk fauna","Eutrof., sommer",
           "Organisk",
           "Eutrof., sommer",
           "Eutrof., vinter",
           "Eutrof., sommer",
           "Eutrof., vinter",
           "Eutrof., sommer",
           "Eutrof., vinter",
           "Eutrof., sommer",
           "Eutrof., vinter",
           "Eutrof., sommer",
           "Eutrof., vinter")
  
  group<-group[params==parameter]
  return(group)
}


plottitle<-function(parameter){
  params<-c("Ecological Status",
            "Chl_summer",
            "Chl",
            "MSMDI",
            "NQI1","H","Secchi","DO_bot",
            "NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter",
            "PO4_summer","PO4_winter",
            "TN_summer","TN_winter",
            "TP_summer","TP_winter")
  titles<-c("Ecological status","Chl a [µg/l]",
            "Chl a 90. pct [µg/l]",
            "MSMDI [EQR]","NQI1 [EQR]",
            "H [EQR]","Secchi [m]",
            "DO bottom [ml/l]",
            "NH4 summer [µg-N/l]",
            "NH4 winter [µg-N/l]",
            "NO3 summer [µg-N/l]",
            "NO3 winter [µg-N/l]",
            "PO4 summer [µg-P/l]",
            "PO4 winter [µg-P/l]",
            "TN summer [µg-N/l]",
            "TN winter [µg-N/l]",
            "TP summer [µg-P/l]",
            "TP winter [µg-P/l]")
  
  title<-titles[params==parameter]
  return(title)
  
}



plottitle_png<-function(parameter){
  params<-c("Ecological Status",
            "Chl_summer",
            "Chl",
            "MSMDI",
            "NQI1","H","Secchi","DO_bot",
            "NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter",
            "PO4_summer","PO4_winter",
            "TN_summer","TN_winter",
            "TP_summer","TP_winter")
  titles<-c("Ecological status",
            "Chl a, summer [µg/l]",
            "Chl a 90. pct [µg/l]",
            "MSMDI [EQR]","NQI1 [EQR]",
            "H [EQR]","Secchi [m]",
            "DO bottom [ml/l]",
            "NH4 summer [µg-N/l]",
            "NH4 winter [µg-N/l]",
            "NO3 summer [µg-N/l]",
            "NO3 winter [µg-N/l]",
            "PO4 summer [µg-P/l]",
            "PO4 winter [µg-P/l]",
            "TN summer [µg-N/l]",
            "TN winter [µg-N/l]",
            "TP summer [µg-P/l]",
            "TP winter [µg-P/l]")
  
  title<-titles[params==parameter]
  return(title)
  
}

# https://github.com/r-spatial/mapview/issues/258
labelFormatCustom = function (prefix = "", suffix = "", between = " &ndash; ", 
                              digits = 2, big.mark = ",", transform = identity, scientific=T) 
{
  
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = scientific,#TRUE, 
           big.mark = big.mark, digits=digits)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), 
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", 
             prefix, p[-n], between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}




static_map <- function(wb){
  # wbselected, parameter, param_lims
  ext <- wb %>%
    sf::st_transform(crs=sf::st_crs(3857)) %>%
    sf::st_bbox()
  
  p <- basemaps::basemap_ggplot(ext, map_service = "esri", map_res = 2,
                      map_type = "world_light_gray_base") 
  
  return(p)
  
}


scenario_desc <- function(scenario){
  list_scenario_sel <- c(
    "Baseline" = "baseline",
    "DIN  -50%" = "DIN50pc",
    "DIN -100%" = "DIN100pc",
    "DIP -100%" = "DIN100pc",
    "DIN -100% DIP -100%" = "DINP100pc",
    "optimistic-realistic" = "DINRA80-J10",
    "DIN  -50% TSM  -50%" = "DINTSM50pc",
    "DIN -100% TSM -100%" = "DINTSM100pc",
    "N -100% TSM -100%" ="NTSM100pc",
    "Scenario A" = "Scenario A",
    "Scenario B" = "Scenario B")
  
  res <- names(list_scenario_sel)[list_scenario_sel==scenario]
  
  return(res)
}

scenario_desc_figs <- function(scenario){
  list_scenario_sel <- c(
    "Baseline" = "baseline",
    "DIN -50%" = "DIN50pc",
    "DIN -100%" = "DIN100pc",
    "DIP -100%" = "DIN100pc",
    "DIN -100%, DIP -100%" = "DINP100pc",
    "optimistic-realistic" = "DINRA80-J10",
    "DIN -50%, TSM -50%" = "DINTSM50pc",
    "DIN -100%, TSM -100%" = "DINTSM100pc",
    "N -100%, TSM -100%" ="NTSM100pc",
    "Scenario A" = "Scenario A",
    "Scenario B" = "Scenario B")
  
  res <- names(list_scenario_sel)[list_scenario_sel==scenario]
  
  return(res)
}



legendtitle_figs<-function(parameter){
  params<-c("Ecological Status",
            "Chl_summer",
            "Chl",
            "MSMDI",
            "NQI1","H","Secchi","DO_bot",
            "NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter",
            "PO4_summer","PO4_winter",
            "TN_summer","TN_winter",
            "TP_summer","TP_winter")
  titles<-c("Ecological status","Chl a [µg/l]",
            "Chl a [µg/l]",
            "MSMDI [EQR]","NQI1 [EQR]",
            "H [EQR]","Secchi [m]",
            "DO [ml/l]",
            "NH4 [µg-N/l]",
            "NH4 [µg-N/l]",
            "NO3 [µg-N/l]",
            "NO3 [µg-N/l]",
            "PO4 [µg-P/l]",
            "PO4 [µg-P/l]",
            "TN [µg-N/l]",
            "TN [µg-N/l]",
            "TP [µg-P/l]",
            "TP [µg-P/l]")
  
  title<-titles[params==parameter]
  return(title)
  
}

legend_labs <- function(labelvals){
  interval <- floor(length(labelvals) / 4)
  labels <- c()
  sel <- seq(1,length(labelvals),interval)
  
  scales::label_number(labelvals,accuracy=3)
  
  
}

scale_values <- function(colorvals){
  
  
  diff <- colorvals$max - colorvals$min
  
  minsteps <- 15
  tens <- 6
  
  diff0 <- c(5,2, 1)
  diff0 <- diff0 * 10^tens
  
  nsteps <- ceiling(diff / diff0)
  
  while(length(nsteps[nsteps > minsteps]) == 0){
    tens <- tens - 1
    diff0 <- diff0 * 0.1
    nsteps <- ceiling(diff / diff0)
  }
  
  diff0 <- diff0[nsteps > minsteps] %>% max()
  nsteps <- nsteps[nsteps > minsteps] %>% min() 
  min_neat <- diff0 * floor(colorvals$min / diff0) 
  max_neat <- diff0 * ceiling(colorvals$max / diff0) 
  
  vals_neat <- seq(min_neat,max_neat, diff0)
  
  labs0 <-  vals_neat[1:(length(vals_neat)-2)]
  breaks <- vals_neat[2:(length(vals_neat)-1)]
  
  ndigits <- -1*tens
  if(ndigits>0){
    labs0 <- format(labs0, nsmall=ndigits)
    labs1 <- format(breaks, nsmall=ndigits)
    labs <- paste0(labs0, " - ", labs1)
  }else{
    labs <- paste0(labs0, " - ", breaks)
  }
  
  
  return(list(breaks=breaks, labels=labs, intervals=length(breaks)))
}


