library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(shinydashboard)
library(shinyjs)
library(terra)
library(reactable)
library(htmltools)
library(shiny.router)
library(stringr)
library(wesanderson)
library(RColorBrewer)
library(viridis)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

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

plottitle<-function(parameter){
  params<-c("Ecological Status",
            "Chl_summer",
            "Chl",
            "MSMDI",
            "NQI1","H","Secchi","DO_bot",
            "NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
            "TN_summer","TN_winter","TP_summer","TP_winter")
  titles<-c("Ecological status","Chl a [µg/l]","Chl a 90. pct [µg/l]",
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
            "TN _winter [µg-N/l]",
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



# ----------------- UI -------------------------------------------------------- 
#shinyjs::

ui <- dashboardPage(skin = "black",title="MARTINI Status Assessment",
                    dashboardHeader(title = "MARTINI\nOslofjord"),
                    dashboardSidebar(sidebarMenu(id="tabs",
                                                 menuItem("Map", tabName = "Map", icon = icon("map-marker")),
                                                 menuItem("About", tabName = "about", icon = icon("book"))#,
                    ), 
                    collapsed=T),
                    dashboardBody(useShinyjs(),
                                  tabItems(
                      # tab content
                      tabItem(tabName = "Map",
                              fluidRow(
                                column(1, HTML("<br>"),p(actionButton("resetzoom", "Reset zoom"))),
                                
                                column(1,
                                       # selectInput("selPeriod",label="Period:",
                                       #               c("2017-2019","2017","2018","2019"
                                       #               )),
                                       
                                       selectInput("selScenario", label="Scenario:",
                                                   c("Baseline",
                                                     "DIN -100%",
                                                     "DIP -100%",
                                                     "DIN -100% DIP -100%",
                                                     "optimistic-realistic"
                                                   ))
                              ),
                              
                              # "NQI1","H" 
                              # currently no results for these parameters
                              
                              column(2,
                                     uiOutput("selectParam", inline=T)
                                     ),
                                    column(1,
                                           p(disabled(checkboxInput("scaleDiscrete","Discrete colours",value=F))),
                                           p(checkboxInput("showStatus","Show status",value=TRUE))
                                    
                              ),
                              column(1,
                                     selectInput("selPal", label="Palette:",
                                                 c("Spectral" = "spectral",
                                                   "s3pcpn" = "AS",
                                                   "Viridis" = "viridis",
                                                   "Wes" = "wes",                                                                                     "Rainbow" = "rainbow"
                                                 ))),
                             column(2, offset = 2,
                                    router_ui(route("/",""),
                                              route("",""),
                                              route("index",""),
                                              route("chl90",""))

                             )),
                              fluidRow(                                
                                column(5,
                                       leafletOutput("mymap",height="660px"),""),
                                column(6,
                                       h3(htmlOutput("WBinfo")),
                                       htmlOutput("titleTblInd"),
                                       reactableOutput("tblind"),
                                       HTML("<BR>"),
                                       htmlOutput("titleTblAgg"),
                                       reactableOutput("tblagg"),
                                       HTML("<BR>"),
                                       htmlOutput("titleTblAggBase"),
                                       reactableOutput("tblaggBase")  
                                       
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
  
  router_server()
  
  values <- reactiveValues()
  values$wbselected <- ""
  values$parameter <- "Ecological Status"
  values$period<-"2017-2019"
  values$run <- FALSE
  values$lng=10.7
  values$lat=59.46
  values$zoom=9
  
 
  
  useChl90 <- reactive({
    s <- get_page()
    res <- ifelse(stringr::str_detect(s, "chl90"),T,F)
    #cat(paste0(s,"\n"))
    return(res)
  })
    
  revList<-c("DO_bot","Secchi","MSMDI")
  
  scenario_comparison <- "baseline" 
  
 # waterbodies <- sf::st_read("nve/oslofjord_wbs.shp", quiet=T)
  waterbodies <- sf::st_read("shp/oslofjord/oslofjord_waterbodies.shp",
                        quiet=T, check_ring_dir=T, promote_to_multi=F)

  df_WB<-read.table(file="nve/WBlist.txt",header=T,stringsAsFactors=F,sep=";")
  df_ind1 <- read.table(file="indicator_results_OF.csv",sep=";",header=T)
  df_wb1 <- read.table(file="WB_results_OF.csv",sep=";",header=T)
  df_ind2 <- read.table(file="indicator_results_OF_chl90.csv",sep=";",header=T)
  df_wb2 <- read.table(file="WB_results_OF_chl90.csv",sep=";",header=T)
  df_wb_obs <- read.table(file="EQR_status.txt",sep=";",header=T)
  
  param_lims <- read.table(file="param_limits.csv",sep=";",header=T)
  
  # obs_stns <- sf::st_read("shp/obs_stns.shp", quiet=T)
  obs_stns <-  read.table("obs_stations.csv", sep=";", header=T)

  # "NQI1","H" - currently no results for these parameters
  params<-c("Ecological Status","Chl_summer","Chl","MSMDI",
            "Secchi","DO_bot","NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
            "TN_summer","TN_winter","TP_summer","TP_winter")
  
  df_ind <- reactive({
    if(useChl90()){
      return(df_ind2)
    }else{
      return(df_ind1)
    }
  })

  df_wb <- reactive({
    if(useChl90()){
      return(df_wb2)
    }else{
      return(df_wb1)
    }
  })
  
    
  param_select_list <- reactive({
    if(useChl90()){
      list_param_sel <- c("Ecological Status"="Ecological Status",
                          "Chl a (90pct.)"="Chl",
                          "MSMDI"="MSMDI",
                          "Secchi (summer)"="Secchi",
                          "DO (bottom)"="DO_bot",
                          "NH4 (summer)"="NH4_summer",
                          "NH4 (winter)"="NH4_winter",
                          "NO3 (summer)"="NO3_summer",
                          "NO3 (winter)"="NO3_winter",
                          "PO4 (summer)"="PO4_summer",
                          "PO4 (winter)"="PO4_winter"
                          #"TN_summer","TN_winter",
                          #"TP_summer","TP_winter"
      )
    }else{
      list_param_sel <- c("Ecological Status"="Ecological Status",
                          "Chl a (summer)"="Chl_summer",
                          "MSMDI"="MSMDI",
                          "Secchi (summer)"="Secchi",
                          "DO (bottom)"="DO_bot",
                          "NH4 (summer)"="NH4_summer",
                          "NH4 (winter)"="NH4_winter",
                          "NO3 (summer)"="NO3_summer",
                          "NO3 (winter)"="NO3_winter",
                          "PO4 (summer)"="PO4_summer",
                          "PO4 (winter)"="PO4_winter"
                          #"TN_summer","TN_winter",
                          #"TP_summer","TP_winter"
      )
    }
    return(list_param_sel)
  })
  
  
  output$selectParam <- renderUI({
    list_param_sel <- param_select_list()
    
    tagList(selectInput(
      "selParam",
      "Display variable:",
      choices = list_param_sel,
      selected = list_param_sel[1],
      multiple = FALSE, 
      #width="200px",
      selectize = T
    ))
  })
  
  round_sig <- function(x, n, max_dec=3){
    base <- ifelse(x==0, -2, ceiling(log10(abs(x))))
    ndig <- n - base 
    ndig <- ifelse(ndig<0,0,ndig)
    ndig <- ifelse(ndig>max_dec,max_dec,ndig)
    return(round(x,ndig))    
  }
  
 
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
  
  output$WBinfo <-renderText({
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
    #cat(file=stderr(),"output$SelectedWB=",values$wbselected,"\n")
    if (values$wbselected=="") {
      "No waterbody selected. Please select a waterbody using the map."
    }else{
      #load("indicators.Rda")
      
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
      df_ind_wb <- df_ind() %>% 
        dplyr::filter(WB==values$wbselected)
      type<-df_ind_wb$type[1]
      Salinity<-df_ind_wb$Salinity[1]
      CoastType<-df_ind_wb$Type[1]
      #cat(file=stderr(),values$wbselected," ",WB_name,", ",type," ",CoastType,", ",Salinity,"\n")
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
    req(input$selParam)
    values$parameter<-input$selParam
    values$period<-  "2017-2019"  #input$selPeriod
    if(values$parameter=="Ecological Status"){
      return(NULL)
    }else{
      #browser()
      #"
      scenario <- switch(input$selScenario,
                         'Baseline' = 'baseline',
                         'DIN -100%' = 'DIN100pc',
                         'DIP -100%' = 'DIP100pc',
                         'DIN -100% DIP -100%' =  'DINP100pc',
                         'optimistic-realistic' = 'DINRA80-J10'
                         )
      
      rfile<-values$period
      rfile<-ifelse(rfile=="2017-2019","",paste0("_",rfile))
      rfile<-paste0("raster/",values$parameter,rfile,".tif")
      
      rfile<- paste0("raster_OF800/", scenario, "_", values$parameter,".tif")
      #cat(paste0(rfile,"\n"))
      return(terra::rast(rfile))
    }
  })
  

  scenario_sel <-  reactive({
    scenario <- switch(input$selScenario,
                       'Baseline' = 'baseline',
                       'DIN -100%' = 'DIN100pc',
                       'DIP -100%' = 'DIP100pc',
                       'DIN -100% DIP -100%' =  'DINP100pc',
                       'optimistic-realistic' = 'DINRA80-J10'
    )
    return(scenario)
  }) 
  
  wbstatus <- reactive({
    
    if(values$parameter=="Ecological Status"){
      if(input$selScenario=="Observations"){
        df <-  df_wb_obs %>%
          dplyr::select(WB=Vannforeko, Status=Class)

      }else{
      df<-df_wb() %>% 
        dplyr::filter(Period==values$period) %>%
        dplyr::filter(scenario==scenario_sel()) %>%
        dplyr::select(WB,Status)
      }
    }else{
    
    df<-df_ind() %>% 
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
      
      pal_map <- plot_pal(input$selPal)

      colorvals <- param_lims %>%
        filter(param==values$parameter)
      colorvals <- colorvals[1,c("min","max")]
      
      # the colour scale limits are taken from max and min values of the rasters
      # if the extreme value in a raster is equal to the colorval limit, then
      # rounding at many decimals can cause the ggplot to give a warning:
      # Some values were outside the color scale and will be treated as NA.
      # So we have extended the limits by a small amount
      colorvals[1] <-  colorvals[1] * 0.9999
      colorvals[2] <-  colorvals[2] * 1.0001
      
      
      colorsdiscrete<-input$scaleDiscrete
      if(colorsdiscrete==F){
        palrev <- colorNumeric(pal_map, colorvals,na.color = "transparent", reverse=T)
        pal <- colorNumeric(pal_map,colorvals,na.color = "transparent")
      }else{
        palrev <- colorBin(pal_map, colorvals,bins=20,na.color = "transparent", reverse=T)
        pal <- colorBin(pal_map, colorvals,bins=20,na.color = "transparent")
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
    #print(click$id)
    
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
  


  tblind_df <- reactive({
    shiny::req(values$wbselected)
    
    df <- df_ind() %>%
      dplyr::select(WB,Indicator,Indikator, IndikatorDesc, Period,scenario,Kvalitetselement,Value,EQR,
                    Ref,HG,GM,MP,PB,Worst,Status)
    if(values$wbselected==""){
      df<-data.frame()
    }else{
      
      dfc <- df %>% 
        dplyr::filter(WB==values$wbselected) %>%
        dplyr::filter(Period==values$period) %>%
        dplyr::filter(scenario==scenario_comparison) 
      
      if(nrow(dfc)>0){
        dfc$Indicator <- factor(dfc$Indicator,levels=params)
        dfc <- dfc %>%
          arrange(Indicator) %>%
          mutate(EQR=round_sig(EQR,2)) %>%
          mutate(Value=round_sig(Value,3)) %>%
          select(Indicator, Value_comp=Value, EQR_comp=EQR, Status_comp=Status)
        
        
        df<-df %>% 
          dplyr::filter(WB==values$wbselected) %>%
          dplyr::filter(Period==values$period) %>%
          dplyr::filter(scenario==scenario_sel())
        
        df$Indicator <- factor(df$Indicator,levels=params)
        
        df<-df %>%
          arrange(Indicator) %>%
          mutate(across(c(Value,Ref,HG,GM,MP,PB,Worst),
                        ~ round_sig(.x, n=3))) %>%
          mutate(EQR=round_sig(EQR,2)) %>%
          dplyr::select(-c(WB,Period)) %>%
          relocate(Kvalitetselement,Indikator) 
        
        
        df<-df %>%
          left_join(dfc, by="Indicator") %>%
          relocate(Ref,HG,GM,MP,PB,Worst, .after=last_col())
      }else{
        df <- data.frame()
      }
    }
    return(df)
  })
  
  
  output$tblind <- reactable::renderReactable({
    
    shiny::req(values$wbselected)
    
    df <- tblind_df() 
    
    if(nrow(df)>0){
    show_base <- ifelse(scenario_sel()==scenario_comparison, F, T)
    
    colw=38
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
                Indikator= colDef(name="Indikator", width=100, sticky = "left"),
                Kvalitetselement=colDef(width=110, sticky = "left"),
                Ref=colDef(width=colw),
                HG=colDef(width=colw),
                GM=colDef(width=colw),
                MP=colDef(width=colw),
                PB=colDef(width=colw),
                Worst=colDef(width=50, format=colFormat(digits=1)),
                Value=colDef(width=40,
                             show=show_base),
                scenario = colDef(show = F), 
                EQR=colDef(width=colw, aggregate = "min",
                           show=show_base),
                Status=colDef(
                  show=show_base,
                  width=50,
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
                ),
                Value_comp=colDef(width=40,
                                show = T,
                                name="Value"
                ),
                EQR_comp=colDef(width=colw, 
                                show = T,
                                aggregate = "min",
                                name="EQR"
                ),
                Status_comp=colDef(
                  name="Status",
                  show = T,
                  width=50,
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
                )),
              columnGroups = list(
                colGroup(name = "Thresholds", columns = c("Ref","HG","GM","MP","PB","Worst")),
                colGroup(name = "Scenario", columns = c("Value",
                                                        "EQR",
                                                        "Status")),
                colGroup(name = "Baseline", columns = c("Value_comp", "EQR_comp", "Status_comp"))
              )
    )}else{
      NULL
      }
    
  })
  
  output$titleTblAgg<-renderText({
    shiny::req(values$wbselected)
    if(values$wbselected==""){
      s<-NULL
    }else{
      if(nrow(tblind_df())>0){
        s <- "<b>Aggregated WB status</b>"
      }else{
        s <- NULL
      }
    }
    return(s)
  })
  output$titleTblAggBase<- renderText({
    shiny::req(values$wbselected)
    if(values$wbselected=="" | scenario_sel()==scenario_comparison){
      s<-NULL
    }else{
      if(nrow(tblind_df())>0){
        s <- "Aggregated WB status (Baseline)"
      }else{
        s <- NULL
      }
    }
    return(s)
  })
  
  
  output$titleTblInd<- renderText({
    shiny::req(values$wbselected)
    if(values$wbselected==""){
      s<-NULL
    }else{
      if(nrow(tblind_df())>0){
        s <- "<b>Indicator status</b>"
      }else{
        s <- "<h4><i>no results</i></h4>"
      }
    }
    return(s)
  })
  
  
  output$tblaggBase <- reactable::renderReactable({
    
    shiny::req(values$wbselected)

    ClassList<-c("Bad","Poor","Mod","Good","High")
    df<-df_wb() %>%
      dplyr::select(WB,Period,scenario,Worst_Biological,Biological,Worst_Supporting,Supporting,EQR,Status) 
    show_base <- ifelse(scenario_sel()==scenario_comparison, F, T)
    
    if(scenario_sel()==scenario_comparison){
      df<-data.frame()
    }else{
      if(values$wbselected==""){
        df<-data.frame()
      }else{
        
      df<-df %>% 
        dplyr::filter(WB==values$wbselected) %>%
        dplyr::filter(Period==values$period) %>%
        dplyr::filter(scenario==scenario_comparison) 
      
      if(nrow(df)==0){
        df<-data.frame()
      }else{
      df<-df %>%
        mutate(EQR_Biological=round_sig(Biological,3),
               EQR_Supporting=round_sig(Supporting,3),
               EQR=round_sig(EQR,2)) %>%
        dplyr::select(WorstBio=Worst_Biological,EQRbio=EQR_Biological,
                      WorstSup=Worst_Supporting,EQRsup=EQR_Supporting,
                      EQR,Status)   
      
    
    
    mypal <- c("#ff000030","#ff8c2b30","#ffff0030","#00d60030","#007eff30")
    colw=120
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
                EQRsup=colDef(width=80, name="EQR avg",
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
    
      }}}
    
  })
  
  
  output$tblagg <- reactable::renderReactable({
    
    # browser()
    shiny::req(values$wbselected)
    
    ClassList<-c("Bad","Poor","Mod","Good","High")
    df<-df_wb() %>%
      dplyr::select(WB,Period,scenario,Worst_Biological,Biological,Worst_Supporting,Supporting,EQR,Status) 
    show_base <- ifelse(scenario_sel()==scenario_comparison, F, T)
    
    if(values$wbselected==""){
      df<-data.frame()
    }else{
      df<-df %>% 
        dplyr::filter(WB==values$wbselected) %>%
        dplyr::filter(Period==values$period) %>%
        dplyr::filter(scenario==scenario_sel()) 
      
      if(nrow(df)>0){
      df<-df %>%
        mutate(EQR_Biological=round_sig(Biological,3),
               EQR_Supporting=round_sig(Supporting,3),
               EQR=round_sig(EQR,2)) %>%
        dplyr::select(WorstBio=Worst_Biological,EQRbio=EQR_Biological,
                      WorstSup=Worst_Supporting,EQRsup=EQR_Supporting,
                      EQR,Status)   
      }
    }
    
    if(nrow(df)>0){
    mypal <- c("#ff000030","#ff8c2b30","#ffff0030","#00d60030","#007eff30")
    colw=120
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
                EQRsup=colDef(width=80, name="EQR avg",
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
    )}else{
      NULL
    }
    
    
  })
  
   
}

shinyApp(ui, server)

