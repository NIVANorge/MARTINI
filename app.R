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

#data <- read.csv("data/data.csv")
#map <- readOGR("shp/nve_kystsone_f.shp",layer = "nve_kystsone_f", GDAL1_integer64_policy = TRUE)

waterbodies <- shapefile("nve/CoastalWBs_WGS84_no_holes_simple.shp")
#waterbodies@data <- waterbodies@data %>%
#  select(Vannforeko,Vannfore_1)
#df_wb_info <- 
#waterbodies@data$highlight<-0
#match<-"0101000032-4-C"
#waterbodies@data[waterbodies@data$Vannforeko==match,"highlight"]<-1



# ----------------- UI -------------------------------------------------------- 

ui <- dashboardPage(skin = "black",title="MARTINI Status Assessment",
                    dashboardHeader(title = "MARTINI"),
                    dashboardSidebar(sidebarMenu(id="tabs",
                                                 menuItem("Map", tabName = "Map", icon = icon("map-marker")),
                                                 menuItem("Waterbody", tabName = "indicators", icon=icon("bar-chart")),
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
                                               ))),
                      tabItem(tabName = "status",
                              fluidRow( column(6,""))),
                      tabItem(tabName = "options",
                              fluidRow( column(6,"")))
                      
                      )))

# ----------------- Server -------------------------------------------------------- 

server <- function(input, output, session) {
  
  df_WB<-read.table(file="nve/WBlist.txt",header=T,stringsAsFactors=F,sep=";")
  
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
    if (values$wbselected=="") {
      "No waterbody selected"
    }else{
      load("indicators.Rda")
      df_ind <- df_ind %>% filter(WB==values$wbselected)
      Salinity<-df_ind$Salinitet[1]
      CoastType<-df_ind$Kysttype[1]
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
      paste0("<b>",values$wbselected," ",WB_name,", ",CoastType,", ",Salinity,"</b>")
            
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
 
    pal <- colorNumeric("viridis", values(r),na.color = "transparent")
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
  
  output$mymap <- renderLeaflet({
    rfile<-paste0("raster/",values$parameter,".grd")
    r <- raster(rfile)
    pal <- colorNumeric("viridis", values(r),na.color = "transparent")
    
    leaflet() %>% 
      #addTiles() %>% 
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
      addRasterImage(r, colors = pal, opacity=0.7) %>%
      addLegend(pal = pal,values=values(r),title=plottitle(values$parameter)) %>%
 
      addPolygons(data = waterbodies, 
                fillColor = "#0033FF", #~factpalFill(highlight),
                color = "black", #~factpal(highlight),
               opacity = 0.5,
                fillOpacity = 0.1,
                weight = 1,
                layerId = waterbodies@data$Vannforeko,
                
                # Highlight WBs upon mouseover
                highlight = highlightOptions(
                  weight = 3,
                  fillOpacity = 0,
                  color = "red",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE),  
                
                # # Add label info when mouseover
                label = waterbodies$Vannfore_1,
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
    
    waterbodies@data$highlight<-0
    #browser()
    if(values$wbselected==click$id){
      values$wbselected <-""
      
    }else{
     values$wbselected <- click$id
     waterbodies@data[waterbodies@data$Vannforeko==click$id,"highlight"]<-1
    
    }
    print(click$id)
    #match<-"0101000032-4-C"
    
    })
  
  observeEvent(input$goWB, {  
    updateTabItems(session, "tabs", "indicators")
  })
  
  # table of indicator results
  #dtind
  output$dtind <- DT::renderDataTable({
    load("indicators.Rda")
    ClassList<-c("Bad","Poor","Moderate","Good","High")
    df<-df_ind %>%
      dplyr::select(WB,Indicator,Unit,type,Kvalitetselement,value,EQR,
                    Ref,HG,GM,MP,PB,Worst,ClassID)
    if(values$wbselected==""){
      dt<-data.frame()
    }else{
      df<-df %>% 
        filter(WB==values$wbselected) %>%
        mutate(Class=ClassList[ClassID]) %>%
        dplyr::select(-c(WB,ClassID))
    dt<- datatable(df,rownames=T,
                   options=list(dom = 't',pageLength = 99,autoWidth=TRUE  )
                   ) %>%
      formatRound(columns=c("value"), digits=4) %>%
      formatRound(columns=c("EQR"), digits=3)
    }
    return(dt)
  })
    
  #   dt<-df.count %>% 
  #   group_by_(.dots=Groups) %>% 
  #   summarize(Classes = spk_chr(f,type='bar',barWidth=barwidthpx,chartRangeMin=ymin, chartRangeMax=ymax,
  #                               colorMap=barcolours)) %>% 
  #   ungroup() %>%
  #   mutate(pGES=ifelse(Class=='','',pGES)) %>%
  #   select(-one_of(remove)) %>%
  #   datatable(escape = F,rownames = F,selection = 'single',
  #             options = list(dom=sDOM,fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}')))
  # if(!is.null(roundlist)){
  #   dt<-dt %>% formatRound(columns=roundlist, digits=3)
  #   ClassOutputTableDT(
  #     values$res4MC,
  #     roundlist = c("EQR","pGES"),
  #     Groups = grplist,
  #     remove = rmlist,
  #     ClassVar = "ClassMC"
  #  )
}

shinyApp(ui, server)

