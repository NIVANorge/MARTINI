library(dplyr)

source("CalcEQR.R")
# get waterbody average values
# read in saved rda values with seasonal / annual averages and find average for waterbodies

#file matching MARTINI grid results to WBs
df_WB<-read.table(file="nve/WB_match_MARTINI.txt",header=T,stringsAsFactors=F,sep=",") %>% 
  dplyr::select(WB=VANNFOREKO,ID)

WB_avg<-function(folder="fram/rda/",parameter,df_match){

  load(paste0(folder,parameter,".Rda"))
  df <- df_match %>%
    left_join(df,by=c("ID"="id"))

  unit <- switch(parameter,
                 Chl = "[µg/l]",
                 DO_bot = "[ml-O2/l]",
                 NH4_summer= "[µg-N/l]",
                 NH4_winter= "[µg-N/l]",
                 NO3_summer= "[µg-N/l]",
                 NO3_winter= "[µg-N/l]",
                 PO4_summer= "[µg-P/l]",
                 PO4_winter= "[µg-P/l]",
                 TN_summer= "[µg-N/l]",
                 TN_winter= "[µg-N/l]",
                 TP_summer= "[µg-P/l]",
                 TP_winter= "[µg-P/l]")
  
  dfavg <- df %>%
    group_by(WB) %>%
    filter(!is.nan(value)) %>%
    filter(!is.na(value)) %>%
    summarise(min=min(value,na.rm=T),
              max=max(value,na.rm=T),
              value=mean(value,na.rm=T),
              n=n()) %>%
    ungroup %>%
    mutate(Indicator=parameter,Unit=unit)
  
    
  return(dfavg)
}

paramlist<-c("Chl","DO_bot","NH4_summer","NH4_winter",
             "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
             "TN_summer","TN_winter","TP_summer","TP_winter")

df_ind <- data.frame(WB=NULL,val=NULL)
for(p in paramlist){
  print(p)
  df <- WB_avg(parameter=p,df_match=df_WB)
  df_ind<-bind_rows(df_ind,df)
}

# read waterbody type information and join to indicators
# then we can use this to get the boundary values
filename<-"nve/Vann-nett_Rapport 16.5.2019.csv"
dftype<-read.table(file=filename,sep=";",stringsAsFactors=F,header=T) %>%
  select(WB=VannforekomstID,type=Nasjonal.vanntype,Kysttype,Salinitet)

# dfx<-distinct(dftype,Nasjonal.vanntype,Økoregion.kyst,Kysttype,Salinitet) %>%
#filter(Økoregion.kyst %in% c("Nordsjøen Sør","Skagerak"))

df_ind <- df_ind %>% 
  left_join(dftype,by="WB")

Salinitet<-c("Skagerak (> 25)","Skagerak (5 - 25)","Mesohalin (5 - 18)","Polyhalin (18 - 30)","Euhalin (> 30)")
SalinityClass<-c(">18","18","5",">18",">18")
Salinity<-data.frame(Salinitet,SalinityClass,stringsAsFactors=F)

df_ind <- df_ind %>%
  left_join(Salinity,by="Salinitet")

# Get indicator Boundaries
filename<-"IndicatorBoundaryLookup.txt"
dfbounds<-read.table(file=filename,sep="\t",stringsAsFactors=F,header=T,quote="") 

dfmatch <- distinct(dfbounds,Indicator,Match) %>%
  filter(!is.na(Indicator),Indicator!="")

df_ind <- df_ind %>% left_join(dfmatch,by="Indicator")

df_ind <- df_ind %>% mutate(MatchValue=ifelse(Match=="type",type,
                                              ifelse(Match=="SalinityClass",SalinityClass,NA)))


df_ind <- df_ind %>% left_join(dfbounds,by=c("Indicator","Match","MatchValue"))


df_ind<-GetClass(df_ind)


#save(df_ind,file="indicators.Rda")
