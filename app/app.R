library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(shinydashboard)
library(shinyjs)
library(terra)
library(reactable)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


plottitle<-function(parameter){
  params<-c("Ecological Status","Chl_summer","MSMDI","NQI1","H","Secchi","DO_bot","NH4_summer","NH4_winter",
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



# ----------------- UI -------------------------------------------------------- 
#shinyjs::

ui <- dashboardPage(skin = "black",title="MARTINI Status Assessment",
                    dashboardHeader(title = "MARTINI\nOslofjord"),
                    dashboardSidebar(sidebarMenu(id="tabs",
                                                 menuItem("Map", tabName = "Map", icon = icon("map-marker")),
                                                 menuItem("About", tabName = "about", icon = icon("book"))#,
                    )),
                    dashboardBody(useShinyjs(),
                                  tabItems(
                      # tab content
                      tabItem(tabName = "Map",
                              fluidRow(
                                column(1, HTML("<br>"),p(actionButton("resetzoom", "Reset zoom"))),
                                
                                column(2,
                                       # selectInput("selPeriod",label="Period:",
                                       #               c("2017-2019","2017","2018","2019"
                                       #               )),
                                       
                                       selectInput("selScenario", label="Scenario:",
                                                   c("Baseline",
                                                     "N -100%",
                                                     "P -100%",
                                                     "N -100% P -100%"
                                                   ))
                              ),
                              
                              # "NQI1","H" - currently no results for these parameters
                              
                              column(2,selectInput("selParam",label="Display variable:",
                                                            c("Ecological Status","Chl_summer","MSMDI","Secchi","DO_bot","NH4_summer","NH4_winter",
                                                              "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
                                                              "TN_summer","TN_winter","TP_summer","TP_winter"
                                                            ))), 
                              column(2,
                                     p(disabled(checkboxInput("scaleDiscrete","Discrete colours",value=F))),
                                     p(checkboxInput("showStatus","Show status",value=TRUE))
                              ),
                              ),
                              fluidRow(                                
                                column(6,
                                       leafletOutput("mymap",height="800px"),""),
                                column(5,
                                       h3(htmlOutput("WBinfo")),
                                       textOutput("titleTblInd"),
                                       reactableOutput("tblind"),
                                       HTML("<BR>"),
                                       textOutput("titleTblAgg"),
                                       reactableOutput("tblagg")  
                                       
                                       #DT::dataTableOutput("dtind")
                                       )
                              )
                              ),
                      tabItem(tabName = "status",
                              fluidRow( column(6,""))),
                      tabItem(tabName = "about",
                              fluidRow( column(6,
                                               h3("About this app"),
                                               p("The work to develop this app was part of the MARTINI project ('MARTINI - Understanding and predicting water quality for ecosystem-based management of Norwegian fjords, coastal waters and seas'), funded by the Norwegian Research Council (Forskningsrådet) under the MARINFORSK program (Marine Resources and Environent)."),
                                               p(a(href="https://prosjektbanken.forskningsradet.no/project/FORISS/280759", "https://prosjektbanken.forskningsradet.no/project/FORISS/280759",target="_blank")),
                                               p("This app is written in the R statistical programming language, using the 'Shiny' package. Results from the MARTINI biogeochemical model results (MARTINI800 Hindcast Archive 2017-2019) are used, showing spatial variation of ecologically relevant indicator parameters, representing both biological quality elements (phytoplankton, macroalgae and and benthic fauna) and supporting physico-chemical indicators (Secchi depth, bottom oxygen and nutrients)."),
                                               p("Following the Water Framework Directive principles, the app aggregates estimated values of individual indicators to arrive at an overall ecological status in a selected waterbody. The user can investigate the the spatial variation of the indicator parameters as well as the resulting ecological status in coastal water bodies, expressed as an ecological quality ratio (EQR) determined by comparing simulated values of indicator parameters with official threshold values used to classify status of water bodies."),
                                               p("Coming developments of the application will include results of simulations under different scenarios such as nutrient load reductions. This will allow users to examine resulting changes in estimated indicator parameters under these scenarios and in the predicted ecological status."))))
                      
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
  values$lng=10.7
  values$lat=59.4
  values$zoom=9
  
  revList<-c("DO_bot","Secchi")
  

 # waterbodies <- sf::st_read("nve/oslofjord_wbs.shp", quiet=T)
  waterbodies <- sf::st_read("shp/oslofjord/oslofjord_waterbodies.shp",
                        quiet=T, check_ring_dir=T, promote_to_multi=F)

  df_WB<-read.table(file="nve/WBlist.txt",header=T,stringsAsFactors=F,sep=";")
  df_ind <- read.table(file="indicator_results_OF.csv",sep=";",header=T)
  df_wb <- read.table(file="WB_results_OF.csv",sep=";",header=T)
  df_wb_obs <- read.table(file="EQR_status.txt",sep=";",header=T)

  
  # obs_stns <- sf::st_read("shp/obs_stns.shp", quiet=T)
  obs_stns <-  read.table("obs_stations.csv", sep=";", header=T)

  # "NQI1","H" - currently no results for these parameters
  params<-c("Ecological Status","Chl_summer","MSMDI","Secchi","DO_bot","NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
            "TN_summer","TN_winter","TP_summer","TP_winter")
  
  labs <- lapply(seq(nrow(waterbodies)), function(i) {
    paste0(waterbodies$Vannfore_1[i], '<br>', 
           waterbodies$Vannforeko[i] ) 
  })
  
  
  observeEvent(values$parameter, {
    if(values$parameter!="Ecological Status"){
      shinyjs::enable("scaleDiscrete")
    }else{
      shinyjs::disable("scaleDiscrete")
    }
  })
  
  output$WBinfo <- renderText({
    if (values$wbselected=="") {
      ""
    }else{
      scenario <- input$selScenario
      scenario <- ifelse(scenario=="", "", paste0(" [", scenario, "]"))
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
      #browser()
     paste0(WB_name," ",  values$wbselected, scenario)
      
    }
  })
  
  output$SelectedWB <- renderText({
    cat(file=stderr(),"output$SelectedWB=",values$wbselected,"\n")
    if (values$wbselected=="") {
      "No waterbody selected. Please select a waterbody using the map."
    }else{
      #load("indicators.Rda")
      
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
      df_ind <- df_ind %>% dplyr::filter(WB==values$wbselected)
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
    values$period<-  "2017-2019"  #input$selPeriod
    if(values$parameter=="Ecological Status"){
      return(NULL)
    }else{
      #browser()
      #"
      scenario <- switch(input$selScenario,
                         'Baseline' = 'baseline',
                         'N -100%' = 'DIN100pc',
                         'P -100%' = 'DIP100pc',
                         'N -100% P -100%' =  'DINP100pc'
                         )
      
      rfile<-values$period
      rfile<-ifelse(rfile=="2017-2019","",paste0("_",rfile))
      rfile<-paste0("raster/",values$parameter,rfile,".tif")
      
      rfile<- paste0("../OF800/tif/", scenario, "_", values$parameter,".tif")
      #cat(paste0(rfile,"\n"))
      return(terra::rast(rfile))
    }
  })
  

  scenario_sel <-  reactive({
    scenario <- switch(input$selScenario,
                         'Baseline' = 'baseline',
                         'N -100%' = 'DIN100pc',
                         'P -100%' = 'DIP100pc',
                         'N -100% P -100%' =  'DINP100pc'
  )
    return(scenario)
  }) 
  
  wbstatus <- reactive({
    
    if(values$parameter=="Ecological Status"){
      if(input$selScenario=="Observations"){
        df <-  df_wb_obs %>%
          dplyr::select(WB=Vannforeko, Status=Class)

      }else{
      df<-df_wb %>% 
        dplyr::filter(Period==values$period) %>%
        dplyr::filter(scenario==scenario_sel()) %>%
        dplyr::select(WB,Status)
      }
    }else{

    df<-df_ind %>% 
      dplyr::filter(Indicator==values$parameter) %>%
      dplyr::filter(Period==values$period) %>%
      dplyr::filter(scenario==scenario_sel()) %>%
      dplyr::select(WB,Status)
    }
    df$Status <- factor(df$Status,levels=c("Bad","Poor","Mod","Good","High"))

    dat <- waterbodies  %>%
      as.data.frame()
    

    dat <- dat %>%
      left_join(df,by=c("Vannforeko"="WB")) %>%
      mutate(ShapeLabel = paste0(Vannfore_1,"<br>",Vannforeko)) %>%
      mutate(ShapeLabel=ifelse(is.na(Status),ShapeLabel,paste0(ShapeLabel,"<br>Status: ",Status)))
    waterbodies$ShapeLabel <- dat$ShapeLabel
    waterbodies$Status <- dat$Status

    waterbodies
  }) 
  
  # create color pal
  colorpal <- reactive({
    mypal <- c("#ff0000","#ff8c2b","#ffff00","#00d600","#007eff")
      
     colorFactor(palette=mypal, domain=wbstatus()$Status, na.color="#CCCCCC")})

  colorpalrev <- reactive({
    mypal <- c("#ff0000","#ff8c2b","#ffff00","#00d600","#007eff")
    
    colorFactor(palette=mypal, domain=wbstatus()$Status, na.color="#CCCCCC")})
  
  
  output$mymap <- renderLeaflet({
   
    values$rezoom
    values$rezoom<-FALSE
    r <- rs()
    shape_wb <- wbstatus()
    palshp <- colorpal()
    statuslevels <- c("Bad","Poor","Mod","Good","High")
    statuslevels <- factor(statuslevels,levels=rev(statuslevels))
    
    selected_wb <- isolate(values$wbselected)
    
    #statusopacity <- ifelse(isolate(input$showStatus),0.9,0)
    statusopacity <- ifelse(input$showStatus,0.9,0)
    
    lm <- leaflet() %>% 
      #addTiles() %>% 
      addProviderTiles(providers$Esri.WorldGrayCanvas)
    
    if(!is.null(r)){
      
      palAS<-c("#ffffff","#4ed1d1","#00ffff","#00e38c","#00c000",
               "#78de00","#ffff00","#ffa200","#ff0000","#ff1e78",
               "#ec3fff","#7c22ff","#4040ff","#20207e","#242424",
               "#7e7e7e","#e0e0e0","#eed3bb","#d8a476","#aa7647",
               "#663300")
      palAS<-palAS[2:21]
      
      colorvals <- values(r)
      # if(values$parameter=="NO3_summer"){
      #   colorvals <- c(0,300)
      # }
    
      
      colorsdiscrete<-input$scaleDiscrete
      if(colorsdiscrete==F){
        palrev <- colorNumeric(palAS, colorvals,na.color = "transparent", reverse=T)
        pal <- colorNumeric(palAS,colorvals,na.color = "transparent")
      }else{
        palrev <- colorBin(palAS, colorvals,bins=20,na.color = "transparent", reverse=T)
        pal <- colorBin(palAS, colorvals,bins=20,na.color = "transparent")
      }
        #palrev <- colorNumeric("viridis", values(r),na.color = "transparent", reverse=T)
        #pal <- colorNumeric("viridis", values(r),na.color = "transparent")
      

        scalefun = function(x){
          sort(x,decreasing = TRUE)
        }
        useSF<-F
      
      
      if(values$parameter %in% revList){
        lm <- lm  %>%
          addRasterImage(r, colors = palrev, opacity=0.7) %>%
          addLegend(pal=pal,values=colorvals,title=plottitle(values$parameter),  
                    labFormat = labelFormatCustom(transform = function(x) scalefun(x), digits=3,scientific=useSF)
                    )
                    
      }else{
        lm <- lm  %>%
          addRasterImage(r, colors = pal, opacity=0.7) %>%
          addLegend(pal=palrev,values=colorvals,title=plottitle(values$parameter),  
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
                layerId = shape_wb$Vannforeko,
                  
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
     
     # show observations stations
     if(F){
       # obs_stns <- 
       
       lm <- lm %>%
         addMarkers(data = obs_stns,
                    lat = obs_stns$Lat,
                    lng = obs_stns$Lon)
         # addMarkers(data = obs_stns, 
         #             fillColor = "transparent",
         #             fillOpacity = 1, 
         #             color = "red",
         #             weight = 3, 
         #             opacity = 1.0,
         #             stroke = T,
         #             layerId = "Selected")
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
    #values$lng=9.208247
    #values$lat=58.273135
    #values$zoom=7
    values$lng=10.7
    values$lat=59.4
    values$zoom=9
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
    point <- sf::st_as_sf(coords, coords = c("lon","lat"), crs=4326)
    
    #retrieves country in which the click point resides, set CRS for country
    selected <- waterbodies[point,]
    

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
  


  
  output$tblind <- reactable::renderReactable({
    
    shiny::req(values$wbselected)
    
    df<-df_ind %>%
      dplyr::select(WB,Indicator,Indikator, IndikatorDesc, Period,scenario,Kvalitetselement,Value,EQR,
                    Ref,HG,GM,MP,PB,Worst,Status)
    if(values$wbselected==""){
      df<-data.frame()
    }else{
      cat(file=stderr(),"values$wbselected=",values$wbselected,"\n")
      
      df<-df %>% 

        dplyr::filter(WB==values$wbselected) %>%
        dplyr::filter(Period==values$period) %>%
        dplyr::filter(scenario==scenario_sel())

      
      df$Indicator <- factor(df$Indicator,levels=params)
      
      df<-df %>%
        arrange(Indicator) %>%
        mutate(Value=round(Value,2),EQR=round(EQR,2)) %>%
        dplyr::select(-c(WB,Period)) %>%
        relocate(Kvalitetselement)

      
    }
    
    colw=35
    mypal <- c("#ff000030","#ff8c2b30","#ffff0030","#00d60030","#007eff30")
    reactable(df, class = "noParenthesis",
              highlight =TRUE,
              compact=TRUE,
              resizable = TRUE,
              defaultPageSize = 20,
              style = "white-space: nowrap;",
              sortable = FALSE,
              #groupBy = "Kvalitetselement",
              columns = list(
                Indicator = colDef(show = F), #colDef(width=100), #show = F,name="WB [Period]"),
                IndikatorDesc = colDef(show=F), 
                Indikator= colDef(name="Indikator", width=180),
                Kvalitetselement=colDef(width=110),
                Value=colDef(width=40),
                scenario = colDef(show = F), 
                EQR=colDef(width=colw, aggregate = "min"),
                Ref=colDef(width=colw),
                HG=colDef(width=colw),
                GM=colDef(width=colw),
                MP=colDef(width=colw),
                PB=colDef(width=colw),
                Worst=colDef(width=50, format=colFormat(digits=1)),
                Status=colDef(
                  width=60,
                  style = function(value) {
                    if (value == "High") {
                      color <- mypal[5]
                    } else if (value == "Good") {
                      color <- mypal[4]
                    } else if (value == "Mod") {
                      color <- mypal[3]
                    } else if (value == "Poor") {
                      color <- mypal[2]
                    } else if (value == "Bad") {
                      color <- mypal[1]
                    } else {
                      color <- "#BBBBBB"
                    }
                    list( backgroundColor = color)
                  }
                ))
              )
    
  })
  
  output$titleTblAgg<- renderText({
    shiny::req(values$wbselected)
    if(values$wbselected==""){
      s<-NULL
    }else{
      s <- "Aggregated WB status"
    }
    return(s)
  })
  
  output$titleTblInd<- renderText({
    shiny::req(values$wbselected)
    if(values$wbselected==""){
      s<-NULL
    }else{
      s <- "Indicator status"
    }
    return(s)
  })
  
  
  output$tblagg <- reactable::renderReactable({
    
    # browser()
    shiny::req(values$wbselected)
    ClassList<-c("Bad","Poor","Mod","Good","High")
    df<-df_wb %>%
      dplyr::select(WB,Period,scenario,Worst_Biological,Biological,Worst_Supporting,Supporting,EQR,Status) 
    
    if(values$wbselected==""){
      df<-data.frame()
    }else{
      cat(file=stderr(),"values$wbselected=",values$wbselected,"\n")
      
      df<-df %>% 
        dplyr::filter(WB==values$wbselected) %>%
        dplyr::filter(Period==values$period) %>%
        dplyr::filter(scenario==scenario_sel()) %>%
        mutate(EQR_Biological=round(Biological,3),
               EQR_Supporting=round(Supporting,3),
               EQR=round(EQR,3)) %>%
        dplyr::select(WorstBio=Worst_Biological,EQRbio=EQR_Biological,
                    WorstSup=Worst_Supporting,EQRsup=EQR_Supporting,
                    EQR,Status)  
      
    }
    
    mypal <- c("#ff000030","#ff8c2b30","#ffff0030","#00d60030","#007eff30")
    colw=125
    reactable(df, class = "noParenthesis",
              highlight =TRUE,
              compact=TRUE,
              defaultPageSize = 20,
              style = "white-space: nowrap;",
              #groupBy = "Kvalitetselement",
              columns = list(
                WorstBio=colDef(width=colw, name="Worst Biological"),
                EQRbio=colDef(width=80, name="EQR",
                              style = list(borderRight = "1px solid #ddd")),
                WorstSup=colDef(width=colw, name="Worst Supporting"),
                EQRsup=colDef(width=80, name="EQR",
                              style = list(borderRight = "1px solid #ddd")),
                EQR=colDef(width=110, name="EQR overall"),
                
                Status=colDef(
                  width=60,
                  style = function(value) {
                    if (value == "High") {
                      color <- mypal[5]
                    } else if (value == "Good") {
                      color <- mypal[4]
                    } else if (value == "Mod") {
                      color <- mypal[3]
                    } else if (value == "Poor") {
                      color <- mypal[2]
                    } else if (value == "Bad") {
                      color <- mypal[1]
                    } else {
                      color <- "#BBBBBB"
                    }
                    list( backgroundColor = color)
                  }
                ))
    )
    
    
  })
  
 
   
}

shinyApp(ui, server)

