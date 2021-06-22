library(shiny)
library(dplyr)
library(leaflet)
library(rgdal)
library(raster)
library(shinydashboard)
library(DT)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
values <- reactiveValues()
values$wbselected <- ""
values$parameter <- "Chl"


plottitle<-function(parameter){
  params<-c("Chl","DO_bot","NH4_summer","NH4_winter",
            "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
            "TN_summer","TN_winter","TP_summer","TP_winter")
  titles<-c("Chl a [µg/l]","DO bottom [ml/l]","NH4 summer [µg-N/l]","NH4 winter [µg-N/l]",
            "NO3 summer [µg-N/l]","NO3 winter [µg-N/l]","PO4 summer [µg-P/l]","PO4 winter [µg-P/l]",
            "TN summer [µg-N/l]","TN _winter [µg-N/l]","TP summer [µg-P/l]","TP winter [µg-P/l]")
  
  title<-titles[params==parameter]
  return(title)
  
}


waterbodies <- shapefile("nve/CoastalWBs_WGS84_no_holes_simple.shp")


# ----------------- UI -------------------------------------------------------- 

ui <- dashboardPage(skin = "black",title="MARTINI Status Assessment",
                    dashboardHeader(title = "MARTINI"),
                    dashboardSidebar(sidebarMenu(id="tabs",
                                                 menuItem("Map", tabName = "Map", icon = icon("map-marker")),
                                                 menuItem("Indicators", tabName = "indicators", icon=icon("bar-chart")),
                                                 #menuItem("Status", tabName = "status", icon = icon("bar-chart")),
                                                 menuItem("Options", tabName = "options", icon = icon("cog"))#,
                    )),
                    dashboardBody(tabItems(
                      # tab content
                      tabItem(tabName = "Map",
                              fluidRow(
                                
                                column(6,
                                       leafletOutput("mymap",height="600px"),""),
                                       
                                
                                column(3,selectInput("selParam",label="Display variable:",
                                                     c("Chl","DO_bot","NH4_summer","NH4_winter",
                                                       "NO3_summer","NO3_winter","PO4_summer","PO4_winter",
                                                       "TN_summer","TN_winter","TP_summer","TP_winter"
                                                       )),
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
                              fluidRow( column(10,
                                               h3(htmlOutput("SelectedWBStatus"))
                                               
                              ))),
                      tabItem(tabName = "status",
                              fluidRow( column(6,""))),
                      tabItem(tabName = "options",
                              fluidRow( column(6,"")))
                      
                      )))

# ----------------- Server -------------------------------------------------------- 

server <- function(input, output, session) {
  
  revList<-c("DO_bot")
  
  df_WB<-read.table(file="nve/WBlist.txt",header=T,stringsAsFactors=F,sep=";")
  df_ind <- read.table(file="indicator_results.txt",sep="\t",header=T)
  
  
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
      Salinity<-df_ind$Salinitet[1]
      CoastType<-df_ind$Kysttype[1]
      cat(file=stderr(),values$wbselected," ",WB_name,", ",type," ",CoastType,", ",Salinity,"\n")
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
      paste0("<b>",values$wbselected," ",WB_name,"</b><br>",type," ",CoastType,", ",Salinity,"</b>")
            
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
  
  observeEvent(input$selParam, ignoreInit = FALSE,{
    values$parameter<-input$selParam
    rfile<-paste0("raster/",values$parameter,".grd")
    r <- raster(rfile)
 
    if(input$selParam %in% revList){
      pal <- colorNumeric("viridis", values(r),na.color = "transparent")
    }else{
      pal <- colorNumeric("viridis", values(r),na.color = "transparent",reverse=T)
    }
    withProgress(message = 'Updating map...', value = 0, {
    leafletProxy("mymap") %>%
      clearImages() %>%
      addRasterImage(r, colors=pal, opacity = 0.7)
    })
    
  })
  
  #waterbodies$highlight <- as.factor(waterbodies$highlight)
  #factpal <- colorFactor(c("#000000", "#FF0000"), waterbodies$highlight)
  #factpalFill <- colorFactor(c("#0033FF", "#FF0000"), waterbodies$highlight)
  #waterbodies$highlight <- 0
  
  labs <- lapply(seq(nrow(waterbodies@data)), function(i) {
    paste0(waterbodies@data[i, "Vannfore_1"], '<br>', 
            waterbodies@data[i, "Vannforeko"] ) 
  })
  
  
  output$mymap <- renderLeaflet({
    rfile<-paste0("raster/",values$parameter,".grd")
    r <- raster(rfile)
    if(values$parameter %in% revList){
      pal <- colorNumeric("viridis", values(r),na.color = "transparent")
    }else{
      pal <- colorNumeric("viridis", values(r),na.color = "transparent",reverse=T)
    }
    
    leaflet() %>% 
      #addTiles() %>% 
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
      addRasterImage(r, colors = pal, opacity=0.7) %>%
      addLegend(pal = pal,values=values(r),title=plottitle(values$parameter),  
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                ) %>%
 
      
      addPolygons(data = waterbodies, 
                  fillColor = "transparent",
                #fillColor = "#0033FF", #~factpalFill(highlight),
                color = "black", #~factpal(highlight),
               opacity = 0.5,
                fillOpacity = 0.1,
                weight = 1,
                layerId = waterbodies@data$Vannforeko,
                
                # Highlight WBs upon mouseover
                highlight = highlightOptions(
                  weight = 3,
                  fillColor = "#FF3300" ,#"transparent",
                  fillOpacity = 0.3,
                  color = "red",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE),  
                
                # # Add label info when mouseover
               label = lapply(labs, htmltools::HTML),
                #label =  HTML(paste0("<p>",waterbodies$Vannfore_1,"</p><p>",waterbodies$Vannforeko,"</p>")),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")#,
 
                )  %>%
      setView(lng=9.208247,lat=58.273135,zoom=7)
  })

  
  observeEvent(input$mymap_shape_click, {
    #create object for clicked polygon
    click <- input$mymap_shape_click
    
    #waterbodies@data$highlight<-0
    #browser()
    if(values$wbselected==click$id){
      values$wbselected <-""
      
    }else{
     values$wbselected <- click$id
     #waterbodies@data[waterbodies@data$Vannforeko==click$id,"highlight"]<-1
    
    }
    print(click$id)
    #match<-"0101000032-4-C"
    
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
    #load("indicators.Rda")
    df_ind <- read.table(file="indicator_results.txt",sep="\t",header=T)
    ClassList<-c("Bad","Poor","Moderate","Good","High")
    df<-df_ind %>%
      dplyr::select(WB,Indicator,Unit,Kvalitetselement,value,EQR,
                    Ref,HG,GM,MP,PB,Worst,ClassID)
    if(values$wbselected==""){
      df<-data.frame()
    }else{
      cat(file=stderr(),"values$wbselected=",values$wbselected,"\n")

      df<-df %>% 
        filter(WB==values$wbselected) %>%
        mutate(Class=ClassList[ClassID]) %>%
        mutate(value=round(value,3),EQR=round(EQR,3)) %>%
        dplyr::select(-c(WB,ClassID))
    }
    
    return(df)
    
  },options=list(dom='t',pageLength = 99,autoWidth=TRUE))
   
}

shinyApp(ui, server)

