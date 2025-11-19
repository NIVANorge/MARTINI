library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
#library(patchwork)

source("process_R/utils.R")


# join means to status class boundaries

# means <- readRDS(file="OF800/res_v10f/res_v10f_WB_means_incl_psu.Rds")
# means <- readRDS(file="OF800/res_v10f/res_v10f_WB_means_incl_psu_20241016.Rds")
means <- readRDS(file="OF800/res_v10ad/res_v10ad_WB_means_incl_psu.Rds")



means <- means %>%
  filter(!is.na(mean))

thresholds_sup <- thresholds_supporting()  %>%
  mutate(version=as.character(version))


find_bnds <- function(param, psu=NA, bnds, bnds_version=""){
  # browser()
  parameter<-param
  psu_mean <- psu
  bnds <- bnds %>%
    filter(param==parameter) %>%
    filter(version==bnds_version)
  
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


source("process_R/vannnet_type.R")
van_type <- vannnet_type 

van_type <- van_type %>%
  rename(Type2018=Type,Type2025=TypeKR) %>% 
  mutate(Type2023=Type2018) %>%
  pivot_longer(c("Type2018","Type2023","Type2025"),
               names_to = "version", values_to = "Type", 
               names_prefix="Type")


threshold <- thresholds_bio() %>%
  rename(param=Code) %>%
  mutate(version=as.character(version))






# different indicators are assigned boundaries in different ways
# we will split the df into different categories of indicator, assign boundaries and then rejoin them


params_sup <- c("NO3_summer","NO3_winter","NH4_summer","NH4_winter", #"DO_bot",
                "TN_summer" ,"TN_winter","TP_summer","TP_winter",
                "PO4_summer","PO4_winter","Secchi", "DO_bot")
params_bio <- c("Chl_summer","Chl","MSMDI")

means0 <-means

means <-means0

 means <- means %>%
  left_join(van_type, by=c("Vannforeko"), relationship = "many-to-many")
 
 
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
  filter(param %in% c("Chl_summer", "Chl")) %>%
  left_join(threshold, by=c("Type"="MatchValue","version","param"))



means_idx <- means %>% # indices already on 0-1 EQR scale
  filter(param %in% c("MSMDI")) %>%
  mutate(EQR00=0, EQR02=0.2, EQR04=0.4, EQR06=0.6, EQR08=0.8, EQR10=1)

means_sup <- means %>%
  filter(param %in% params_sup)

x <- means_sup %>%
  slice(1:100) %>%
  rowwise() %>%
  mutate(bnds=list(find_bnds(param, psu, thresholds_sup, version))) %>%
  ungroup() %>%
  unnest_wider(bnds)



means_sup <- means_sup %>%
  rowwise() %>%
  mutate(bnds=list(find_bnds(param, psu, thresholds_sup, version))) %>%
  ungroup() %>%
  unnest_wider(bnds)

means_all <- bind_rows(means_chl, means_idx, means_sup)

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


# ------- add model-based boundaries ------------------

res_ind_model <- res_ind %>%
  filter(version=="2018") %>%
  mutate(version="model") %>%
  select(-c(EQR10, EQR08, EQR06, EQR04, EQR02, EQR00))
  
res_ind_model_bnds <- res_ind_model %>%  
  filter(scenario=="Pristine") %>%
  select(Vannforeko, Indikator, EQR10=mean) %>%
  mutate(EQR08 = ifelse(Indikator %in% c("Siktdyp","Oksygen","MSMDI"),
                        EQR10*0.8, EQR10*1.5),
         EQR06 = ifelse(Indikator %in% c("Siktdyp","Oksygen","MSMDI"),
                        EQR10*0.6, EQR10*(1.5^2)),
         EQR04 = ifelse(Indikator %in% c("Siktdyp","Oksygen","MSMDI"),
                        EQR10*0.4, EQR10*(1.5^3)),
         EQR02 = ifelse(Indikator %in% c("Siktdyp","Oksygen","MSMDI"),
                        EQR10*0.2, EQR10*(1.5^4)),
         EQR00 = ifelse(Indikator %in% c("Siktdyp","Oksygen","MSMDI"),
                        EQR10*0.2, EQR10*(1.5^5)))

res_ind_model <- res_ind_model %>%
  left_join(res_ind_model_bnds, by=c("Vannforeko", "Indikator"))

res_ind <- res_ind %>% 
  bind_rows(res_ind_model)

# ------- END add model-based boundaries ------------------


res_ind <- res_ind %>% 
  rename(value=mean, Ref=EQR10, HG=EQR08, GM=EQR06, MP=EQR04, PB=EQR02, Worst=EQR00) %>%
  CalcEQR()

res_ind <- res_ind %>%
  mutate(Period="2017-2019") %>%
  select(version, WB=Vannforeko, Indicator=param, Period, scenario, Value=value, Type, Salinity=psu, Kvalitetselement, 
         Indikator, IndikatorDesc, 
         Ref, HG, GM, MP, PB, Worst, EQR, Status=Class, QEtype)



file_res_wb <- "app/WB_results_OF.csv"
file_res_ind <- "app/indicator_results_OF.csv"
file_rds_res_wb <- "OF800/res_v10ad/WB_results_OF.Rds"
file_rds_res_ind <- "OF800/res_v10ad/indicator_results_OF.Rds"






res_wb <- aggregate_wb(res_ind)

write.table(res_wb, file=file_res_wb, sep=";", row.names=F, col.names=T, quote=T, fileEncoding="UTF-8")
write.table(res_ind, file=file_res_ind, sep=";", row.names=F, col.names=T, quote=T, fileEncoding="UTF-8")




saveRDS(res_ind, file = file_rds_res_ind)
saveRDS(res_wb, file = file_rds_res_wb)




# -------------- checks --------------------
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



