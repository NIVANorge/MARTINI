#' GetClass
#' 
#' 
#' @param df A dataframe 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \item{value} 
#'   \item{Ref}{ } 
#'   \item{HG}{ } 
#'   \item{GM}{ } 
#'   \item{MP}{ } 
#'   \item{PB}{ } 
#'   \item{Worst}{ } 
#'   \item{Resp}{ } 
#'   #'   
#' 
#' @examples
GetClass<-function(df){
  Categories<-c("Bad","Poor","Mod","Good","High","Ref")
  names(df)[names(df)=="RefCond"]<-"Ref"
  names(df)[names(df)=="H.G"]<-"HG"
  names(df)[names(df)=="G.M"]<-"GM"
  names(df)[names(df)=="M.P"]<-"MP"
  names(df)[names(df)=="P.B"]<-"PB"
  #names(df)[names(df)==""]<-""
  df$Ref<-as.numeric(df$Ref)
  df$HG<-as.numeric(df$HG)
  df$GM<-as.numeric(df$GM)
  df$MP<-as.numeric(df$MP)
  df$PB<-as.numeric(df$PB)
  df$Worst<-as.numeric(df$Worst)
  
  df$Resp<-ifelse(df$HG > df$GM,-1,1)
  df$class1<-ifelse(df$Resp==1,
                    ifelse(is.na(df$Ref),F,df$value<df$Ref),
                    ifelse(is.na(df$Ref),F,df$value>df$Ref))
  df$class2<-ifelse(df$Resp==1,df$value<df$HG,df$value>df$HG)
  df$class3<-ifelse(df$Resp==1,df$value<df$GM,df$value>df$GM)
  df$class4<-ifelse(df$Resp==1,df$value<df$MP,df$value>df$MP)
  df$class5<-ifelse(df$Resp==1,df$value<df$PB,df$value>df$PB)
  df$ClassID<-df$class1+df$class2+df$class3+df$class4+df$class5+1
  df$Bnd1<-df$Worst
  df$Bnd2<-df$PB
  df$Bnd1<-ifelse(df$ClassID==2,df$PB,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==2,df$MP,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==3,df$MP,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==3,df$GM,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==4,df$GM,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==4,df$HG,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==5,df$HG,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==5,df$Ref,df$Bnd2)

  # if "RefCon" is not defined and value is better than High/Good, then the EQR is set to 0.9
  # if "Worst" is not defined and value is worse than Poor/Bad, then the EQR is set to 0.1
  
  df$EQR<-ifelse(df$ClassID==6,1,
                 ifelse(is.na(df$Bnd2),
                        0.2*(df$ClassID-1)+0.1,
                        ifelse(is.na(df$Bnd1),
                               0.2*(df$ClassID-1)+0.1,
                               0.2*((df$ClassID-1)+(df$value-df$Bnd1)/(df$Bnd2-df$Bnd1)))))
  
  # truncate EQR values <0 or >1
  df$EQR<-ifelse(df$EQR>1,1,df$EQR)
  df$EQR<-ifelse(df$EQR<0,0,df$EQR)
  #Class cannot be better than "High":
  df$ClassID<-ifelse(df$ClassID>5,5,df$ClassID)
  df$Class<-ifelse(is.na(df$ClassID),NA,Categories[df$ClassID])
  df<-select(df,-c(Resp,class1,class2,class3,class4,class5,Bnd1,Bnd2))
  return(df)
}
