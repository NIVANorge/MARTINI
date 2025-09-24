
# read the list giving QE for each indicator
.QElist <- read.table(file="processing/indicatorQE.csv",sep=";",header=T,comment.char="", quote='"')


.EQR <- function(val,Ref,HG,GM,MP,PB,Worst,NIVAklass=F){
  
  # this function calculates the EQR for a single set of
  # indicator value and corresponding boundary values
  
  # this function is called from CalcEQR() for each row in a dataframe

  eqrvals <- c(1,0.8,0.6,0.4,0.2,0)
  bndvals <- c(Ref,HG,GM,MP,PB,Worst)
  na_vals <- is.na(bndvals) 
  bndvals <- bndvals[!is.na(bndvals)]
  
  if(length(bndvals)<4){  
    # we didn't have the minimum of 4 boundary values
    eqr <- NA
  }else{ 
    n_neg_vals <- length(bndvals[bndvals<0])
    replace_neg <- ifelse(n_neg_vals>0,FALSE,TRUE)
    
    # in case of missing RefCon or Worst values...
    # if all the other values are > 0 (e.g. Chl boundaries)
    # then Ref should not be negative!
    
    if(is.na(Ref)){
      Ref <- ifelse(is.na(Ref), HG + (HG-GM), Ref)
      Ref <- ifelse(Ref<0 & replace_neg,0, Ref)
    }
    if(is.na(Worst)){
      Worst <- ifelse(is.na(Worst), PB + (PB-MP), Worst)
      Worst  <- ifelse(Worst <0 & replace_neg,0, Worst )
    }
    
    bndvals <- c(Ref,HG,GM,MP,PB,Worst)
    if(bndvals[1]>bndvals[6]){
      eqrvals <- rev(eqrvals)
      bndvals <- rev(bndvals)
      na_vals <- rev(na_vals)
    }
    
    ix0 <- length(bndvals[bndvals<=val])
    ix1 <- ifelse(ix0<6,ix0+1,6)
    ix0<- ifelse(ix0<1,1,ix0)
    #ix0 <- ifelse(ix1>1,ix1-1,1)
    if(ix1!=ix0){
      eqr <- eqrvals[ix0] + (eqrvals[ix1] - eqrvals[ix0]) * ((val - bndvals[ix0]) / (bndvals[ix1] - bndvals[ix0]))
    }else{
      eqr <- eqrvals[ix1]
    }
    if(NIVAklass==T){
      # using the rule that if no Ref value is given
      # and EQR > 0.8 then EQR should be 0.9 
      # if EQR < 0. 2 and no Worst value is given
      # then EQR should be 0.1
      if(val<bndvals[2] & na_vals[1]==T){
        eqr <- 0.5*(eqrvals[1]+eqrvals[2])
      }else if(val>bndvals[5] & na_vals[6]==T){
        eqr <- 0.5*(eqrvals[5]+eqrvals[6])
      }
    }
  }
  return(eqr)
  
}

.EQRclass <- function(EQR, classnames=c("Bad","Poor","Mod","Good","High")){
  
  # this function gives the status class name based on the EQR values
  if(is.na(EQR)){
    return(NA_character_)
  }else{
    EQR5 <- 1+floor(EQR*5)
    EQR5 <- ifelse(EQR5>5,5,EQR5)
    EQR5 <- ifelse(EQR5<1,1,EQR5)
    return(classnames[EQR5])
  }
}


CalcEQR <- function(df,
                    varnames=c("value","Ref","HG","GM","MP","PB","Worst"),
                    NIVAklass=F, getClass=T){
  
  # function takes a dataframe with value and boundary columns
  # adds a new column with EQR values and a column with status class
  
  # option NIVAklass (default is FALSE)
  # if TRUE, then the EQR function will NOT estimate missing Ref
  # or Worst values. Instead it will use EQR = 0.9 when the 
  # calculated EQR would be > 0.8
  
  if(length(varnames)!=7){
    stop("7 variable names expected: value, Ref, HG, GM, MP, PB, Worst")
  }
  df <- df %>%
    rowwise() %>%
    mutate(EQR=.EQR(get(varnames[1]),get(varnames[2]),get(varnames[3]),
                    get(varnames[4]),get(varnames[5]),get(varnames[6]),
                    get(varnames[7]),
                    NIVAklass=NIVAklass)) 
  if(getClass==T){
    df <- df %>%
      mutate(Class=.EQRclass(EQR))  
  }else{
    df <- df %>%
      mutate(Class=NA_character_)
  }
  df <- df %>%
    ungroup()
  
  return(df)
  
}


.QEtype <- function(QE){
  
  # this function gives the quality element type for a QE
  
  biological <- c("Seagrass","Phytoplankton","Bottom fauna","Macroalgae",
                  "Ålegræs","Planteplankton", "Bundfauna", "Makroalger")
  
  supporting <- c("Nutrients", "Oxygen", "Secchi", "Næringsstoffer")
  
  
  biological <- tolower(biological)
  supporting <- tolower(supporting)
  
  if(tolower(QE) %in% biological){
    return("Bio")
  }else if (tolower(QE) %in% supporting){
    return("Sup")
  }else{
    return(NA_character_)
  }
}


.QE <- function(ind, QElist, lang=""){
  
  # this function gives the quality element (type for a QE) for an indicator
  
  ind <- tolower(ind)
  if(lang!=""){
    QEcolname <- paste0("QE_", lang)
    QElist <- QElist %>%
      select(-QE) 
    names(QElist)[names(QElist)==QEcolname] <- "QE"
  }
  QElist <-  QElist %>%
    filter(tolower(indicator)==tolower(ind))
  
  QE <- QElist %>%
    pluck("QE")
  
  QE <- ifelse(length(QE)==0, NA_character_, QE)
  return(QE)
  
}

GetQEs<-function(df, lang="", HEAT=T){
  
  df <- df %>%
    mutate(Code=ifelse(is.na(Code),Parameter,Code)) %>%
    mutate(Code=ifelse(Code=="",Parameter,Code))
  
  
  df <- df %>%
    rowwise() %>%
    mutate(QE = .QE(Code, QElist=.QElist, lang=lang)) %>%
    mutate(QEtype = .QEtype(QE)) %>%
    ungroup()
  
  if(HEAT==T){
    df <- df %>%
      rowwise() %>%
      mutate(HEATCat = .HEATcategory(QE)) %>%
      ungroup()
    
  }
  
  return(df)
  
}


aggregate <- function(df, group_vars=c("Vannforeko","Vannfore_1","Year"), method="NO", lang="", level="overall"){
  
  # do aggregation from indicator to QE level
  
  group_vars_type <- c(group_vars, "QEtype")
  group_vars_QE <- c(group_vars_type, "QE")
  
  dfQE <- df %>%
    group_by(across(any_of(group_vars_QE))) %>% 
    summarise(EQR_QE=mean(EQR,na.rm=T),.groups="drop")
  
  if(level=="overall"){
    
    # do aggregation from QE level to overall
    
    dfType <- dfQE %>%
      group_by(across(any_of(group_vars_type))) %>% 
      arrange(EQR_QE) %>%
      slice(1) %>%
      ungroup()
    
    dfQEworst <- dfType %>%
      select(-EQR_QE) %>%
      pivot_wider(names_from=QEtype, values_from = QE, names_sort=T, names_prefix="Worst_")
    
    dfEQR <- dfType %>%
      select(-QE) %>%
      pivot_wider(names_from=QEtype, values_from = EQR_QE, names_sort=T, names_prefix="EQR_") 
    
    
    dfEQR <- dfEQR %>%
      rowwise() %>%
      mutate(EQR = .EQR_reduced(EQR_Bio,EQR_Sup)) %>%
      ungroup()
    
    dfEQR <- dfEQR %>%
      left_join(dfQEworst,by=group_vars)
    
    
    dfEQR <- dfEQR %>%
      rowwise() %>%
      mutate(Class=.EQRclass(EQR)) %>%
      ungroup()
    
    return(dfEQR)
  }else{
    return(dfQE)
  }
  
}


.EQR_reduced<-function(EQRb,EQRs){
  
  # the Biological EQR determines the overall status 
  # but can be reduced by one class if supporting is less than good 
  # we do this by subtracting 0.2 from the EQR
  # BUT the EQR cannot be reduced to less than the EQR for supporting QEs
  # this is why there is a max(EQR-b, EQRs) in the formula
  
  EQR <- ifelse(is.na(EQRb),0,EQRb)
  EQRs <- ifelse(is.na(EQRs),1,EQRs)
  EQR <- ifelse(EQRb < 0.6, 
                EQRb,
                ifelse(EQRs<0.6, 
                       ifelse(EQRb<1,
                              max(EQRb-0.2),
                              0.799),
                       EQRb))
  EQR <- ifelse(is.na(EQRb),NA,EQR)
  return(EQR)
}


# ------------------- HEAT functions --------------------------


CalcER <- function(df, value_var="value", threshold_var="GM", HG_var="HG", resp_var=NA_character_){
  
  # function takes a dataframe with value and threshold columns 
  # (and HG to determine direction of the indicator - see the .EUtRatio function)
  # adds a new column with ER values and a column with status class

  if(is.na(resp_var) & is.na(HG_var)){
    stop("Need to specify either a response variable column or a column with high/good values")
  }
  
  if(is.na(HG_var)){
    df <- df %>%
      rowwise() %>%
      mutate(ER=.EutRatio(val=get(value_var),threshold=get(threshold_var),resp=get(resp_var)))
  }else{
    df <- df %>%
      rowwise() %>%
      mutate(ER=.EutRatio(val=get(value_var),threshold=get(threshold_var),HG=get(HG_var)))
    
  }
  df <- df  %>%
    mutate(Class=.EQRclass(EQR)) %>%
    ungroup()
  
  return(df)
  
}


.EutRatio <- function(val, threshold, HG=NA, resp=NA, replaceinf=T){
  
  # function to calculate Eutrophication Ratio (ER) as ratio  of observed
  # and threshold values - this is the original HEAT method
  
  HG <- ifelse(is.na(HG),threshold-1,HG)
  
  resp <- ifelse(is.na(resp), 
                 ifelse(HG<threshold,1,-1),
                 resp)
  
  # we are only using the HG value to indicate the direction of the indicator 
  # i.e. chl - the HG value is lower than the GM
  # for Secchi the HG value is greater than GM
  
  # if no HG value is given, the default is that increasing values
  # correspond to worsening status (e.g. as for chl)
  
  ER <- ifelse(resp>0, val/threshold, threshold/val)
  if(replaceinf){
    replaceval<-100 #why is it 100??
  }else{
    replaceval<-NA
  }
  ER<-ifelse(is.infinite(ER),replaceval,ER)
  return(ER)  
  
}


.HEATcategory <- function(QE){
  
  # this function gives the indicator category for an indicator
  
  nutrients <- c("Nutrients","Næringsstoffer")
  
  primary <- c("Phytoplankton","Planteplankton",
               "Secchi")
  
  secondary <- c("Seagrass","Bottom fauna","Macroalgae","Oxygen","Ålegræs", "Bundfauna", "Makroalger")
  
  nutrients <- tolower(nutrients)
  primary <- tolower(primary)
  secondary <- tolower( secondary)
  
  if(tolower(QE) %in% nutrients){
    return("Nut")
  }else if (tolower(QE) %in% primary){
    return("Pri")
  }else if (tolower(QE) %in% secondary){
    return("Sec")
  }else{
    return(NA_character_)
  }
}

.HEATclass <- function(ER=NA, classnames=c("Bad","Poor","Mod","Good","High")){
  ER<- ifelse(is.numeric(ER),ER,NA)
  if(is.na(ER)){
    return(NA_character_)
  }else{
      classid <- ifelse(ER>=0.2,
                        ifelse(ER>=0.4,
                               ifelse(ER>=0.6,
                                    ifelse(ER>=0.8,5,4),
                                    3),
                               2),
                        1)
                               
  return(classnames[classid])
  }
  
}

aggregateHEAT <- function(df, group_vars=c("Vannforeko","Vannfore_1","Year"), level="overall", var_cat=NA_character_){
  
  # do aggregation from indicator to category level

  var_cat <- ifelse(is.na(var_cat), "cat", tolower(var_cat))
  var_cat <- names(df)[stringr::str_detect(tolower(names(df)),var_cat)==T]
  if(length(var_cat)>0){
    if(length(var_cat)>1){
      stop("A unique variable specifying the HEAT categories was not found")
    }
  }else{
    stop("Variable specifying the HEAT categories was not found")
  }
  
  group_vars_cat <- c(group_vars, var_cat)
  
  dfCat <- df %>%
    group_by(across(any_of(group_vars_cat))) %>% 
    summarise(ER_Cat=mean(ER,na.rm=T),.groups="drop")
  
  if(level=="overall"){
    
    # do aggregation from QE level to overall
    
    dfER <- dfCat %>%
      group_by(across(any_of(group_vars))) %>% 
      arrange(-desc(ER_Cat)) %>%
      slice(1) %>%
      ungroup() %>%
      rename(ER = ER_Cat, Worst=!!as.name(quo_name(var_cat)))
    
    
    dfCat <- dfCat  %>%
      pivot_wider(names_from=all_of(var_cat), values_from = ER_Cat, names_sort=T, names_prefix="ER_")
    
    
    dfER <- dfER %>%
      left_join(dfCat,by=group_vars)
    
    dfER <- dfER %>%
      rowwise() %>%
      mutate(HEATClass=.HEATclass(ER)) %>%
      ungroup()
    
    return(dfER)
  }else{
    return(dfCat)
  }
  
}

