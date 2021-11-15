library(shiny)
library(dplyr)
library(leaflet)
library(rgdal)
library(raster)
library(shinydashboard)
library(DT)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


plottitle<-function(parameter){
  params<-c("Ecological Status","Chl","MSMDI","NQI1","H","Secchi","DO_bot","NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
            "TN_summer","TN_winter","TP_summer","TP_winter")
  titles<-c("Ecological status","Chl a [µg/l]","MSMDI [EQR]","NQI1 [EQR]","H [EQR]","Secchi [m]","DO bottom [ml/l]","NH4 summer [µg-N/l]","NH4 winter [µg-N/l]",
            "NO3 summer [µg-N/l]","NO3 winter [µg-N/l]","PO4 summer [µg-P/l]","PO4 winter [µg-P/l]",
            "TN summer [µg-N/l]","TN _winter [µg-N/l]","TP summer [µg-P/l]","TP winter [µg-P/l]")
  
  title<-titles[params==parameter]
  return(title)
  
}

# https://github.com/r-spatial/mapview/issues/258
labelFormatCustom = function (prefix = "", suffix = "", between = " &ndash; ", 
                              digits = 3, big.mark = ",", transform = identity, scientific=T) 
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

waterbodies <- shapefile("nve/CoastalWBs_WGS84_no_holes_simple.shp")


# ----------------- UI -------------------------------------------------------- 

ui <- dashboardPage(skin = "black",title="MARTINI Status Assessment",
                    dashboardHeader(title = "MARTINI"),
                    dashboardSidebar(sidebarMenu(id="tabs",
                                                 menuItem("Map", tabName = "Map", icon = icon("map-marker")),
                                                 menuItem("Indicators", tabName = "indicators", icon=icon("chart-bar")),
                                                 #menuItem("Status", tabName = "status", icon = icon("bar-chart")),
                                                 menuItem("About", tabName = "about", icon = icon("book"))#,
                    )),
                    dashboardBody(tabItems(
                      # tab content
                      tabItem(tabName = "Map",
                              fluidRow(
                                
                                column(6,
                                       leafletOutput("mymap",height="600px"),""),
                                       
                                
                                column(3,selectInput("selParam",label="Display variable:",
                                                     c("Ecological Status","Chl","MSMDI","NQI1","H","Secchi","DO_bot","NH4_summer","NH4_winter",
                                                       "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
                                                       "TN_summer","TN_winter","TP_summer","TP_winter"
                                                       )),
                                       radioButtons("colors", "Scale colours:",
                                                    c("Viridis" = "virid",
                                                      "pyncview (continuous)" = "pync_c",
                                                      "pyncview (discrete)" = "pync_d")),
                                       checkboxInput("logscale","Use log scale",value=FALSE),
                                       selectInput("selPeriod",label="Period:",
                                                   c("2017-2019","2017","2018","2019"
                                                   )),
                                       
                                       checkboxInput("showStatus","Show WB status",value=TRUE),
                                       
                                       actionButton("resetzoom", "Reset zoom"),
                                       
                                       h3(htmlOutput("WBinfo")),
                                       uiOutput("WBbutton")        
                                       #actionButton("recalc", "New points")
                                       )
                              )),
                      tabItem(tabName = "indicators",
                              fluidRow( column(10,
                                               h3(htmlOutput("SelectedWB")),
                                               DT::dataTableOutput("dtind")
                                               
                                               #selectInput("selInd",label="",c("Chlorophyll a ","Total phosphorous","Phosphate P","Total nitrogen","Nitrate N","Ammonium N","Secchi depth","Oxygen","What else?"),multiple=T)
                                               )),
                              fluidRow( column(7,
                                               h3(htmlOutput("SelectedWBStatus")),
                                               DT::dataTableOutput("dtWB")
                                               
                              ))),
                      tabItem(tabName = "status",
                              fluidRow( column(6,""))),
                      tabItem(tabName = "about",
                              fluidRow( column(6,
                                               h3("About this app"),
                                               p("Description required...."),
                                               h3("Instructions"),
                                               p("How to use this app..."))))
                      
                      )))

# ----------------- Server -------------------------------------------------------- 

server <- function(input, output, session) {
  values <- reactiveValues()
  values$wbselected <- ""
  #values$parameter <- "Chl"
  values$parameter <- "Ecological Status"
  #values$parameter <- "DO_bot"
  values$period<-"2017-2019"
  values$run <- FALSE
  values$lng=9.208247
  values$lat=58.273135
  values$zoom=7
  
  revList<-c("DO_bot","Secchi")
  

  df_WB<-read.table(file="nve/WBlist.txt",header=T,stringsAsFactors=F,sep=";")
  #df_ind <- read.table(file="indicator_results.txt",sep="\t",header=T)
  df_ind <- read.table(file="indicator_results_v8.csv",sep=";",header=T)
  df_wb <- read.table(file="WB_results_v8.csv",sep=";",header=T)
  
  params<-c("Ecological Status","Chl","MSMDI","NQI1","H","Secchi","DO_bot","NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
            "TN_summer","TN_winter","TP_summer","TP_winter")
  
  
  output$WBinfo <- renderText({
    if (values$wbselected=="") {
      ""
    }else{
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
      #browser()
     paste0("<b>Selected Waterbody:</b><br>",
            WB_name,"<br>",
            values$wbselected)
      
    }
  })
  
  output$SelectedWB <- renderText({
    cat(file=stderr(),"output$SelectedWB=",values$wbselected,"\n")
    if (values$wbselected=="") {
      "No waterbody selected"
    }else{
      #load("indicators.Rda")
      
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
      df_ind <- df_ind %>% filter(WB==values$wbselected)
      type<-df_ind$type[1]
      Salinity<-df_ind$Salinity[1]
      CoastType<-df_ind$Type[1]
      cat(file=stderr(),values$wbselected," ",WB_name,", ",type," ",CoastType,", ",Salinity,"\n")
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
    
      paste0("<b>",values$wbselected," ",WB_name,"</b><br>",type," ",CoastType,", ",Salinity,"</b><br>Period: ",values$period)
            
    }
  })
  
  
  output$SelectedWBStatus <- renderText({
    if (values$wbselected=="") {
      ""
    }else{
      "Aggregated status"
    }
  })
  
  tagList(
    sliderInput("n", "N", 1, 1000, 500),
    textInput("label", "Label")
  )
  
  output$WBbutton <- renderUI({
    if (values$wbselected=="") {
      ""
    }else{
      buttontext <-paste0("Show ",values$wbselected)
      tagList(actionButton("goWB", buttontext))
    }
  })
  
  
  rs <- reactive({
    values$parameter<-input$selParam
    values$period<-input$selPeriod
    if(values$parameter=="Ecological Status"){
      return(NULL)
    }else{
      rfile<-values$period
      rfile<-ifelse(rfile=="2017-2019","",paste0("_",rfile))
      rfile<-paste0("raster/",values$parameter,rfile,".tif")
      return(raster(rfile))
    }
  })
  

  labs <- lapply(seq(nrow(waterbodies@data)), function(i) {
    paste0(waterbodies@data[i, "Vannfore_1"], '<br>', 
           waterbodies@data[i, "Vannforeko"] ) 
  })
  
  
  wbstatus <- reactive({
    if(values$parameter=="Ecological Status"){
      df<-df_wb %>% 
        filter(Period==values$period) %>%
        dplyr::select(WB,Status)
    }else{
    df<-df_ind %>% 
      filter(Indicator==values$parameter) %>%
      filter(Period==values$period) %>%
      dplyr::select(WB,Status)
    }
    df$Status <- factor(df$Status,levels=c("Bad","Poor","Moderate","Good","High"))
    dat <- waterbodies@data
    dat <- dat %>%
      left_join(df,by=c("Vannforeko"="WB")) %>%
      mutate(ShapeLabel = paste0(Vannfore_1,"<br>",Vannforeko)) %>%
      mutate(ShapeLabel=ifelse(is.na(Status),ShapeLabel,paste0(ShapeLabel,"<br>Status: ",Status)))
    
    waterbodies@data <- dat
    waterbodies
  }) 
  
  # create color pal
  colorpal <- reactive({
    mypal <- c("#ff0000","#ff8c2b","#ffff00","#00d600","#007eff")
      
     colorFactor(palette=mypal, domain=wbstatus()$Status)})

  colorpalrev <- reactive({
    mypal <- c("#ff0000","#ff8c2b","#ffff00","#00d600","#007eff")
    
    colorFactor(palette=mypal, domain=wbstatus()$Status)})
  
  
  output$mymap <- renderLeaflet({
    values$rezoom
    values$rezoom<-FALSE
    r <- rs()
    shape_wb <- wbstatus()
    palshp <- colorpal()
    statuslevels <- c("Bad","Poor","Moderate","Good","High")
    statuslevels <- factor(statuslevels,levels=rev(statuslevels))
    
    selected_wb <- isolate(values$wbselected)
    
    #statusopacity <- ifelse(isolate(input$showStatus),0.9,0)
    statusopacity <- ifelse(input$showStatus,0.9,0)
    
    lm <- leaflet() %>% 
      #addTiles() %>% 
      addProviderTiles(providers$Esri.WorldGrayCanvas)
    
    if(!is.null(r)){
      if(input$logscale==T){
        r <- calc(r, fun=function(x){log10(x)})
      }
      
      palAS<-c("#ffffff","#4ed1d1","#00ffff","#00e38c","#00c000",
               "#78de00","#ffff00","#ffa200","#ff0000","#ff1e78",
               "#ec3fff","#7c22ff","#4040ff","#20207e","#242424",
               "#7e7e7e","#e0e0e0","#eed3bb","#d8a476","#aa7647",
               "#663300")
      palAS<-palAS[2:21]
      
      colorscheme<-input$colors
      if(colorscheme=="pync_c"){
        palrev <- colorNumeric(palAS, values(r),na.color = "transparent", reverse=T)
        pal <- colorNumeric(palAS, values(r),na.color = "transparent")
      }else if(colorscheme=="pync_d"){
        palrev <- colorBin(palAS, values(r),bins=20,na.color = "transparent", reverse=T)
        pal <- colorBin(palAS, values(r),bins=20,na.color = "transparent")
      }else{
        palrev <- colorNumeric("viridis", values(r),na.color = "transparent", reverse=T)
        pal <- colorNumeric("viridis", values(r),na.color = "transparent")
      }

      if(input$logscale==T){
         scalefun = function(x){
           sort(10^x, decreasing = TRUE)
         }
         useSF<-T
      }else{
        scalefun = function(x){
          sort(x,decreasing = TRUE)
        }
        useSF<-F
      }
      
      if(values$parameter %in% revList){
        lm <- lm  %>%
          addRasterImage(r, colors = palrev, opacity=0.7) %>%
          addLegend(pal=pal,values=values(r),title=plottitle(values$parameter),  
                    labFormat = labelFormatCustom(transform = function(x) scalefun(x), digits=3,scientific=useSF)
                    )
                    
      }else{
        lm <- lm  %>%
          addRasterImage(r, colors = pal, opacity=0.7) %>%
          addLegend(pal=palrev,values=values(r),title=plottitle(values$parameter),  
                    labFormat = labelFormatCustom(transform = function(x) scalefun(x), digits=3,scientific=useSF)
          ) 
      }
    }
      

      lm <- lm %>%
      addPolygons(data = shape_wb, 
                  #fillColor = "transparent",
                  fillColor =  ~palshp(Status),
                color = "black", #~factpal(highlight),
               opacity = 0.5,
                fillOpacity = statusopacity,
                weight = 1,
                layerId = shape_wb@data$Vannforeko,
                
                # Highlight WBs upon mouseover
                highlight = highlightOptions(
                  weight = 3,
                  #fillColor = "#FF3300" ,#"transparent",
                  #fillOpacity = 0.3,
                  color = "red",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE),  
                
                # # Add label info when mouseover
               label = lapply(labs, htmltools::HTML),
               #label=HTML(ShapeLabel),
                #label =  HTML(paste0("<p>",waterbodies$Vannfore_1,"</p><p>",waterbodies$Vannforeko,"</p>")),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")#,

      )
    
    if(input$showStatus){
      lm <- lm %>%
        addLegend(pal=palshp,values=statuslevels,title="Status")
    }
 
     lm <- lm  %>%
        setView(lng=isolate(values$lng),
                lat=isolate(values$lat),
                zoom=isolate(values$zoom))

      
    if(selected_wb!=""){
      selected <- waterbodies[waterbodies$Vannforeko==selected_wb,]
      lm <- lm %>%
        addPolygons(data = selected, 
                    fillColor = "transparent",
                    fillOpacity = 1, 
                    color = "red",
                    weight = 3, 
                    opacity = 1.0,
                    stroke = T,
                    layerId = "Selected")
    }  
    lm
    
  })
  
  observeEvent(input$mymap_zoom, {
    values$zoom<- input$mymap_zoom
  })
  
  observeEvent(input$mymap_center$lng, {
    values$lng<-input$mymap_center$lng
  })
  observeEvent(input$mymap_center$lat, {
    values$lat<-input$mymap_center$lat
  })
  
  observeEvent(input$resetzoom,{
    values$lng=9.208247
    values$lat=58.273135
    values$zoom=7
    values$rezoom=TRUE
  }
  )
  
  observeEvent(input$mymap_shape_click, {
    #create object for clicked polygon
    click <- input$mymap_shape_click
    
    if(values$wbselected==click$id){
      values$wbselected <-""
      
    }else{
     values$wbselected <- click$id
     
    }
    print(click$id)
    
    #pulls lat and lon from shiny click event
    lat <- click$lat
    lon <- click$lng
    
    #puts lat and lon for click point into its own data frame
    coords <- as.data.frame(cbind(lon, lat))
    
    #converts click point coordinate data frame into SP object, sets CRS
    point <- SpatialPoints(coords)
    proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    #retrieves country in which the click point resides, set CRS for country
    selected <- waterbodies[point,]
    proj4string(selected) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    proxy <- leafletProxy("mymap")
    if(click$id == "Selected"){
      proxy %>% removeShape(layerId = "Selected")
      values$wbselected <-""
    } else {
      proxy %>% addPolygons(data = selected, 
                            fillColor = "transparent",
                            fillOpacity = 1, 
                            color = "red",
                            weight = 3, 
                            opacity = 1.0,
                            stroke = T,
                            layerId = "Selected")
    } 
    
    
    })
  
  observeEvent(input$goWB, {  
    updateTabItems(session, "tabs", "indicators")
  })
  

   
  
  # table of indicator results
  #dtind
  output$dtind <- DT::renderDataTable({

    df<-df_ind %>%
      dplyr::select(WB,Indicator,Period,Kvalitetselement,Value,EQR,
                    Ref,HG,GM,MP,PB,Worst,Status)
    if(values$wbselected==""){
      df<-data.frame()
    }else{
      cat(file=stderr(),"values$wbselected=",values$wbselected,"\n")

      df<-df %>% 
        filter(WB==values$wbselected) %>%
        filter(Period==values$period)
      
      df$Indicator <- factor(df$Indicator,levels=params)
      
      df<-df %>%
        arrange(Indicator) %>%
        mutate(Value=round(Value,3),EQR=round(EQR,3)) %>%
        dplyr::select(-c(WB,Period))
      
      
      
    }
    
    return(df)
    
  },options=list(dom='t',pageLength = 99,autoWidth=TRUE))
  
  output$dtWB <- DT::renderDataTable({
    ClassList<-c("Bad","Poor","Moderate","Good","High")
    df<-df_wb %>%
      dplyr::select(WB,Period,Worst_Biological,Biological,Worst_Supporting,Supporting,EQR,Status) 

    if(values$wbselected==""){
      df<-data.frame()
    }else{
      cat(file=stderr(),"values$wbselected=",values$wbselected,"\n")
      
      df<-df %>% 
        filter(WB==values$wbselected) %>%
        filter(Period==values$period) %>%
        mutate(EQR_Biological=round(Biological,3),
               EQR_Supporting=round(Supporting,3),
               EQR=round(EQR,3)) %>%
        dplyr::select(`Worst Biological QE`=Worst_Biological,`EQR Biological`=EQR_Biological,
                      `Worst Supporting QE`=Worst_Supporting,`EQR Supporting`=EQR_Supporting,
                      `EQR Overall`=EQR,Status)  
    }
    
    return(df)
    
  },options=list(dom='t',pageLength = 99,autoWidth=TRUE),rownames= FALSE)
  
  
   
}

shinyApp(ui, server)

