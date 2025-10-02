library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
#library(patchwork)

source("process_R/utils.R")

use_S3_for_missing <- T

# join means to status class boundaries

# means <- readRDS(file="OF800/res_v10f/res_v10f_WB_means_incl_psu.Rds")
# means <- readRDS(file="OF800/res_v10f/res_v10f_WB_means_incl_psu_20241016.Rds")
means <- readRDS(file="OF800/res_v10aa/res_v10aa_WB_means_incl_psu.Rds")



means <- means %>%
  filter(!is.na(mean))

thresholds_sup<- readRDS("processing/Walday_et_al23_thresholds.Rds")

# thresholds$threshold <- factor(thresholds$threshold, 
#                                levels=c("Worst","PB","MP","GM","HG","Ref"))

thresholds_sup <- thresholds_sup %>%
  select(param, psu, EQR, value) %>%
  mutate(EQR=ifelse(EQR==1,"10",paste0("0",10*EQR))) %>%
  pivot_wider(names_from="EQR",names_prefix="EQR",names_sort=T, values_from="value")


find_bnds <- function(param, psu=NA, bnds){
  parameter<-param
  psu_mean <- psu
  bnds <- bnds %>%
    filter(param==parameter)
  
  n <- bnds %>%
    filter(!is.na(psu)) %>%
    nrow()
  
  if(is.na(psu) | n==0){
    if(nrow(bnds)>1){
      bnds <- bnds %>%
        slice(1)
    }
  # }else if(is.na(psu_min) | is.na(psu_max)){
  #   
  }else{
    psu_max <- max(bnds$psu, na.rm=T)
    psu_min <- min(bnds$psu, na.rm=T)
    if(psu_mean<=psu_min){
      bnds <- bnds %>%
        filter(psu==psu_min)
    }else if(psu_mean>=psu_max){
      bnds <- bnds %>%
        filter(psu==psu_max)
    }else{
      frac <- (psu_mean - psu_min)/(psu_max-psu_min)
      bnds_min <- bnds %>%
        filter(psu==psu_min) %>%
        mutate(across(c(EQR00,EQR02,EQR04,EQR06,EQR08,EQR10) ,~ .x * (1-frac)))
      bnds_max <- bnds %>%
        filter(psu==psu_max) %>%
        mutate(across(c(EQR00,EQR02,EQR04,EQR06,EQR08,EQR10) ,~ .x * frac))
      
      bnds <- bind_rows(bnds_min,bnds_max)
      
      bnds <- bnds %>%
        mutate(across(c(EQR00,EQR02,EQR04,EQR06,EQR08,EQR10) ,sum)) %>%
        slice(1)
    } 
  }
  
  return(c(EQR00=bnds$EQR00,
              EQR02=bnds$EQR02,
              EQR04=bnds$EQR04,
              EQR06=bnds$EQR06,
              EQR08=bnds$EQR08,
              EQR10=bnds$EQR10))
}

user<-Sys.getenv ("USERNAME")
folder<- paste0("C:/Users/",user,"/NIVA/230195 - Modellering av Oslofjorden - Documents/AP1_Dataanalyse/")

# waterbody types S1, S2, S3 (use this for Chl)
van_type <- read.table(paste0(folder,"gis/oslofjord/oslofjord_waterbodies.txt"),sep=";", header=T)
van_type <- van_type %>%
  select(Vannforeko, Type)


source("process_R/vannnet_type.R")
van_type <- vannnet_type 

# thresholds for DO_bot

threshold <- read.table(paste0(folder,"Threshold Values/IndicatorBoundaryLookup.txt"),header=T,quote="",sep=";")%>%
  rename("Code"="Indicator")%>%
  mutate(Match=ifelse(Match=="",NA,Match),
         MatchValue=ifelse(MatchValue=="",NA,MatchValue)) %>%
  rename(EQR00=Worst, EQR02=PB, EQR04=MP, EQR06=GM, EQR08=HG, EQR10=Ref)



if(use_S3_for_missing){
  threshold <- threshold %>%
    filter(MatchValue!="S5")
  
  thresholdS5 <- threshold %>%
    filter(MatchValue=="S3") %>%
    mutate(MatchValue = "S5")

  thresholdUK <- thresholdS5 %>%
    mutate(MatchValue = "unknown")
  
  
  threshold <- threshold %>%
    bind_rows(thresholdS5)%>%
    bind_rows(thresholdUK)
}





threshold_DO <- threshold %>%
  filter(Code=="DO_bot") 

threshold_DO <- threshold_DO %>%
  mutate(EQR00=0, EQR10=6)

# thresholds for Chl summer
threshold_Chl_mean <-read.table(file=paste0(folder,"Threshold Values/summer_klfa thresholds.txt"),sep=";", header = T) %>%
  rename(EQR00=Worst, EQR02=PB, EQR04=MP, EQR06=GM, EQR08=HG, EQR10=Ref) %>%
  select(Type=MatchValue, EQR00, EQR02, EQR04, EQR06, EQR08, EQR10)

# create values for EQR00
threshold_Chl_mean <- threshold_Chl_mean %>%
  mutate(EQR00=round(10^(2*log10(EQR02) -log10(EQR04)),2))

if(use_S3_for_missing){
  threshold_Chl_mean <- threshold_Chl_mean %>%
    filter(Type!="S5")
  
  threshold_Chl_mean_S5 <- threshold_Chl_mean %>%
    filter(Type=="S3") %>%
    mutate(Type = "S5")
  
  threshold_Chl_mean_UK <- threshold_Chl_mean_S5 %>%
    mutate(Type = "unknown")
    
  threshold_Chl_mean <- threshold_Chl_mean %>%
    bind_rows(threshold_Chl_mean_S5)%>%
    bind_rows(threshold_Chl_mean_UK)
}




# different indicators are assigned boundaries in different ways
# we will split the df into different categories of indicator, assign boundaries and then rejoin them


params_sup <- c("NO3_summer","NO3_winter","NH4_summer","NH4_winter", #"DO_bot",
                "TN_summer" ,"TN_winter","TP_summer","TP_winter",
                "PO4_summer","PO4_winter","Secchi")
params_bio <- c("Chl_summer","Chl","MSMDI")

 means <- means %>%
  left_join(van_type, by="Vannforeko")
 
 
 shp_wb <- sf::st_read("app/shp/oslomod3.shp", quiet=T)
 
 shp_wb <- shp_wb %>%
   left_join(van_type, by="Vannforeko")
 
 shp_wb <- shp_wb %>%
   mutate(Type=ifelse(is.na(Type), "unknown", Type))
 

 col_type <- c("S1"="#006600","S2"="#009966",
               "S3"="#006699",
               "S5" = "#dd8000",
               "unknown"="#ff0000")
ggplot() +
   geom_sf(data = shp_wb, 
           aes(fill=Type), 
           colour=NA, alpha=0.5) +
   theme_minimal() +
  scale_fill_manual(values = col_type) 
 

means_chl <- means %>%
  filter(param %in% c("Chl_summer")) %>%
  left_join(threshold_Chl_mean, by="Type")

means_chl90 <- means %>%
  filter(param %in% c("Chl")) %>%
  left_join(threshold %>% filter(Code=="Chl") %>% 
              select(Type=MatchValue, EQR10,EQR08,EQR06,EQR04,EQR02,EQR00),
            by="Type") %>%
  mutate(EQR00=2*EQR02)


means_do <- means %>%
  filter(param %in% c("DO_bot")) %>%
  mutate(EQR00=0, EQR02=1.5, EQR04=2.5, EQR06=3.5, EQR08=4.5, EQR10=6)

#     6;4.5;3.5;2.5;1.5;0;0.688139204184214

means_idx <- means %>% # indices already on 0-1 EQR scale
  filter(param %in% c("MSMDI")) %>%
  mutate(EQR00=0, EQR02=0.2, EQR04=0.4, EQR06=0.6, EQR08=0.8, EQR10=1)

means_sup <- means %>%
  filter(param %in% params_sup)

means_sup <- means_sup %>%
  rowwise() %>%
  mutate(bnds=list(find_bnds(param, psu, thresholds_sup))) %>%
  ungroup() %>%
  unnest_wider(bnds)

means_all <- bind_rows(means_chl, means_chl90, means_do, means_idx, means_sup)

ok <- means_all %>% 
  group_by(param) %>% 
  summarise(mean=max(mean,na.rm=T), .groups="drop") %>%
  purrr::pluck("param")

means_all <- means_all %>%
  filter(param %in% ok)

# if(nrow(means)!=nm){
#   cat("number of indicator results has changed\n")
#   }

res_ind <- means_all %>%
  #filter(!is.na(EQR06)) %>% # exclude indicators with no threshold value
  rowwise() %>%
  mutate(scenario=stringr::str_split_i(name,"_",1)) %>%
  ungroup()


res_ind <- res_ind %>%
  rowwise() %>%
  mutate(Indikator=indicator_info(param, out="shortname"),
         IndikatorDesc=indicator_info(param),
         Kvalitetselement=indicator_info(param, out="KE"),
         QEtype=indicator_info(param, out="QEtype")) %>%
  ungroup()

source("process_R/EQR_functions.R")

res_ind <- res_ind %>% 
  rename(value=mean, Ref=EQR10, HG=EQR08, GM=EQR06, MP=EQR04, PB=EQR02, Worst=EQR00) %>%
  CalcEQR()

res_ind <- res_ind %>%
  mutate(Period="2017-2019") %>%
  select(WB=Vannforeko, Indicator=param, Period, scenario, Value=value, Type, Salinity=psu, Kvalitetselement, 
         Indikator, IndikatorDesc, 
         Ref, HG, GM, MP, PB, Worst, EQR, Status=Class, QEtype)

res_ind_all <- res_ind

list_optChl <- c(NA, "chl90", "chl_summer")

# chl90 <- T
for(optChl in list_optChl){
  
  file_suffix <- ifelse(is.na(optChl),"",paste0("_", optChl))
  file_res_wb <- paste0("app/WB_results_OF", file_suffix, ".csv")
  file_res_ind <- paste0("app/indicator_results_OF", file_suffix, ".csv")
  file_rds_res_wb <- paste0("OF800/res_v10aa/WB_results_OF", file_suffix, ".Rds")
  file_rds_res_ind <- paste0("OF800/res_v10aa/indicator_results_OF", file_suffix, ".Rds")
  
  optChl <- ifelse(is.na(optChl),"",optChl)
  
  if(optChl == "chl90"){
    res_ind <- res_ind_all %>%
      filter(Indicator!="Chl_summer")
  }else if(optChl == "chl_summer"){
    res_ind <- res_ind_all %>%
      filter(Indicator!="Chl")
  }else{
    res_ind <- res_ind_all
  }
  
  res_wb <- aggregate_wb(res_ind)
  
  
  write.table(res_wb, file=file_res_wb, sep=";", row.names=F, col.names=T, quote=T, fileEncoding="UTF-8")
  write.table(res_ind, file=file_res_ind, sep=";", row.names=F, col.names=T, quote=T, fileEncoding="UTF-8")
  
  
  saveRDS(res_ind, file = file_rds_res_ind)
  saveRDS(res_wb, file = file_rds_res_wb)
  
} # for Chl90 in c(T,F)



# get quality element names
df <- res_ind

  mutate(Code=Indicator) %>%
  GetQEs(HEAT=F)


# do lookup for Kvalitetselement Indikator StatusVN EQR_VN ParameterVN


paste0(unique(res_ind$param), collapse="', '")


names(df_ind) %>% paste0(collapse=" ")
names(df_wb) %>% paste0(collapse=" ")




indicator_info("tn_summer", out="QE")

df_ind$Indikator %>% unique() %>% paste0(collapse="', '")



