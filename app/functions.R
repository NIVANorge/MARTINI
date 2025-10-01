# help functions for app


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

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

aggregate <- function(df, baseline="baseline", map_status=F){
  if(!"Indicator" %in% names(df)){
    cat("!!!!!!!!!!!! aggregation without indicator !!!!!!!!!!!\n")
    cat(paste0("    map status = ", map_status, "\n"))
  }
  # results for map are different from those for the indicator tables
  dfgrp <- param_group_df()

  scenario_selected <- df$scenario[1]
  
  if(map_status==T){
    df <- df %>%
      select(Indicator,Indikator, scenario, EQR)
  }else{
    df <- df %>%
    select(Indicator,Indikator, scenario, EQR, EQR_comp)
  if(scenario_selected==baseline){
    df <- df %>%
      select(Indicator,Indikator, scenario, EQR)
  }else{
    df0 <- df %>%
      select(Indicator,Indikator, EQR=EQR_comp) %>%
      mutate(scenario = baseline)
    df <- df %>%
      select(Indicator,Indikator, scenario, EQR) %>%
      bind_rows(df0)
  }
  }
  
  
    
  df <- df %>%
    left_join(dfgrp, by="Indicator")
  
  res_group0_min <- df %>%
    group_by(scenario, group0) %>%
    arrange(EQR) %>%
    slice(1) %>%
    ungroup()
  
  
  res_group0_sup_avg <- df %>%
    group_by(scenario, group0) %>%
    summarise(EQRavg=mean(EQR, na.rm=T), .groups = "drop") %>%
    filter(group0=="sup")
  
  
  res_sup <- res_group0_min %>%
    filter(group0=="sup") %>%
    select(scenario, 
           Worst_Supporting=Indikator) %>%
    #Supporting=EQR)
    left_join(res_group0_sup_avg, by=c("scenario")) %>%
    select(scenario, 
           Worst_Supporting, Supporting=EQRavg)
  
  res_bio <- res_group0_min %>%
    filter(group0=="bio") %>%
    select(scenario, 
           Worst_Biological=Indikator,
           Biological=EQR)
  
  if(nrow(res_bio)==0){
    res_bio <- df %>%
      distinct(scenario) %>%
      mutate(Worst_Biological=NA_character_,
             Biological=NA)
  }
  if(nrow(res_sup)==0){
    res_sup <- df %>%
      distinct(scenario) %>%
      mutate(Worst_Supporting=NA_character_,
             Supporting=NA)
  }
  
  res <- merge(res_bio, res_sup, 
                  by=c("scenario"),
                  all=T)
  
  
  # adjust overall EQR according to WFD
  # a less than good supporting status can reduce overall status
  res <- res %>%
    rowwise() %>%
    mutate(EQR=adj_EQR(Biological, Supporting)) %>%
    mutate(Status=ifelse(is.na(EQR),NA,
                         ifelse(EQR<0.2,"Bad",
                                ifelse(EQR<0.4,"Poor",
                                       ifelse(EQR<0.6,"Mod",
                                              ifelse(EQR<0.8,"Good","High")))))) %>%
    ungroup()
  
  return(res)
  
  
}


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

#plot_pal("wes")

param_group_df <- function(){
  params<-c("Chl_summer",
            "Chl",
            "MSMDI",
            "NQI1","H","Secchi","DO_bot",
            "NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter",
            "PO4_summer","PO4_winter",
            "TN_summer","TN_winter",
            "TP_summer","TP_winter")
  group0<-c("bio",
            "bio",
            "bio","bio",
            "bio","sup",
            "sup",
            "sup",
            "sup",
            "sup",
            "sup",
            "sup",
            "sup",
            "sup",
            "sup",
            "sup",
            "sup")
  group1<-c("Phytoplankton",
            "Phytoplankton",
            "Macroalgae","Benthic fauna",
            "Benthic fauna","Secchi",
            "Oxygen",
            "Nutrients",
            "Nutrients",
            "Nutrients",
            "Nutrients",
            "Nutrients",
            "Nutrients",
            "Nutrients",
            "Nutrients",
            "Nutrients",
            "Nutrients")
  group2<-c("Phytoplankton",
            "Phytoplankton",
            "Macroalgae","Benthic fauna",
            "Benthic fauna","Secchi",
            "Oxygen",
            "N","N",
            "N","N",
            "P","P",
            "N","N",
            "P","P")
  
  df <- data.frame(Indicator=params, group0, group1, group2)

  return(df)  
}


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
