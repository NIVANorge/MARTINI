library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(shinydashboard)
library(shinyjs)
library(terra)
library(reactable)
library(htmltools)
#library(shiny.router)
library(stringr)
library(wesanderson)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(ggnewscale)
library(basemaps)

source("functions.R")

# ----------------- UI -------------------------------------------------------- 


ui <- dashboardPage(skin = "black",title="MARTINI Status Assessment",
              dashboardHeader(title = "MARTINI\nOslofjord"),
              dashboardSidebar(sidebarMenu(id="tabs",
                    menuItem("Map", tabName = "Map", icon = icon("map-marker")),
                    menuItem("Options", tabName = "Options", icon = icon("cog")),
                    menuItem("About", tabName = "about", icon = icon("book"))#,
              ), 
              collapsed=F),
              dashboardBody(useShinyjs(),
                            tabItems(
                      # tab content
                  tabItem(tabName = "Map",
                         fluidRow(
                          
                            column(1, HTML("<br>"),p(actionButton("resetzoom", "Reset zoom"))),
                            column(2,
                                       uiOutput("selectScenario", inline=T)
                              ),
                              
                            column(2,
                                     uiOutput("selectParam", inline=T)
                                     ),
                            column(6,h3(htmlOutput("WBinfo")))),
                         fluidRow(
                           column(3,""),     
                            column(1,
                                  p(disabled(checkboxInput("showStatus","Show status",value=TRUE)))
                              ),
                              column(1,
                                   p(disabled(checkboxInput("scaleDiscrete","Discrete colour",value=T)))
                            ),
                           column(6,htmlOutput("titleTblInd"))
                           ),
                         
                         
                              fluidRow(                                
                                column(5,
                                       leafletOutput("mymap",height="660px"),
                                       column(9,""),
                                       column(3,
                                              div(uiOutput("selectPalette", inline=T))
                                              )#,
                                       #p(downloadButton(label="Download png", outputId = "dl"))
                                       ),
                                column(7,
                                       reactableOutput("tblind"),
                                       HTML("<BR>"),
                                       htmlOutput("titleTblAgg"),
                                       reactableOutput("tblagg"),
                                       HTML("<BR>"),
                                       htmlOutput("titleTblAggBase"),
                                       reactableOutput("tblaggBase")  
                                       )
                              )
                              ),
                  # ---------------------- Options -----------------------------------
                  tabItem(tabName = "Options",
                          fluidRow(column(6,  h3("Options"))),
                          fluidRow(column(6,  
    p("By default, the integrated ecological status assessment is made using all available modelled indicator parameters."),
    p("It is also possible to exclude individual indicators or groups of indicators from the aggregation process to investigate how this affects theintegrated status."),
    p("Do this by changing the selection of indicators in the table below.")),
    #column(1,p("")),
    column(3,
           p(em("more explanation required...")),
           p("Select the set of threshold values to be used when calculating EQR values and status class from model results.")  
    )
    ),
                          
                          fluidRow(
                            column(6,
                                   p(strong("Select Indicators:")),
                                 reactableOutput("tblSelectIndicators")),
                            column(3,
                                   uiOutput("selectThresholds", inline=T),
                                   p(strong("veileder rev. (2023)"),
                                     em("description...")),
                                   p(strong("kystrev (2025)"),
                                     em("description...")),
                                   p(strong("model-based"),
                                     em("description...")),
                                   br(), br(),
                                   uiOutput("selectAggregation", inline=T),
                                   p(strong("aggregration between seasons"),
                                     "the average EQR values are first calculated for", 
                                     em("summer"), "and", em("winter"), "eutrophication indicators.",
                                     "The EQR for", em("eutrophication"), 
                                     "is taken as the average of summer and winter values.",
                                     "Finally, the worst EQR value of", 
                                     em("eutrophication"), "and", 
                                     em("organic"), 
                                     "determines the overall result for the",
                                     em("Physical-Chemical"), "quality element."),
                                   p(strong("no aggregration between seasons"),
                                     "the average EQR values are  calculated for", 
                                     em("summer"), "and", em("winter"), 
                                     "eutrophication indicators.", 
                                     "The overall",em("Physical-Chemical"), 
                                     "EQR is given by the worst (lowest) of (i)", 
                                     em("organic,"), ", (ii)", em("summer"), "and (iii)",
                                     em("winter"), "EQR values."),
                                   p(strong("one-out all-out"),
                                     "the EQR value for the", em("Physical-Chemical"),
                                     "quality element", 
                                     "is given by the worst (lowest) individual EQR", "
                                     value from all supporting indicators.",
                                     "the aggregated" , em("Biological"), "EQR value", 
                                     "is given by the worst (lowest) individual EQR", "
                                     value from all biological indicators,", 
                                     em("without any averaging within quality elements")),
                                 )
                            
                          ),
                          fluidRow(
                            column(3,
                                  ""
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
  
  #router_server()
  
  values <- reactiveValues()
  values$wbselected <- ""
  values$parameter <- "Ecological Status"
  values$period<-"2017-2019"
  values$run <- FALSE
  values$lng=10.7
  values$lat=59.46
  values$zoom=8
  
  values$discrete_scale <- FALSE
  values$show_status <- TRUE
  
  # --------------- indicators_included() ------------
  indicators_included <- reactive({
    df_ind() %>% 
        pull(Indicator) %>%
        unique() %>%
        sort()
  })
  
  # --------------- scenarios_included() ------------
  scenarios_included <- reactive({
    df_ind() %>% 
      pull(scenario) %>%
      unique() %>%
      sort()
  })
  
 
  
  # --------------- SETUP ------------
  revList<-c("DO_bot","Secchi","MSMDI")
  
  scenario_comparison <- "baseline" 
  
 # waterbodies <- sf::st_read("nve/oslofjord_wbs.shp", quiet=T)
  waterbodies <- sf::st_read("shp/oslomod3.shp",
                        quiet=T, check_ring_dir=T, promote_to_multi=F)

  df_WB<-read.table(file="nve/WBlist.txt",header=T,stringsAsFactors=F,sep=";")
  df_ind1 <- read.table(file="indicator_results_OF.csv",sep=";",header=T)
  df_wb1 <- read.table(file="WB_results_OF.csv",sep=";",header=T)
  
  df_wb_obs <- read.table(file="EQR_status.txt",sep=";",header=T)
  
  param_lims <- read.table(file="param_limits.csv",sep=";",header=T)
  
  
  obs_stns <-  read.table("obs_stations.csv", sep=";", header=T)

  # "NQI1","H" - currently no results for these parameters
  
  params_ind<-c("Chl_summer","Chl","MSMDI",
            "DO_bot","Secchi",
            "NH4_summer","NO3_summer",
            "PO4_summer","TN_summer",
            "TP_summer","NH4_winter",
            "NO3_winter","PO4_winter",
            "TN_winter","TP_winter")
  params<-c("Ecological Status", params_ind)
  
  
  values$include <- data.frame(Indicator=params_ind) %>%
    mutate(selected=T)
  

  # --------------- df_ind() ------------
  df_ind <- reactive({
    df <- df_ind1 %>%
      filter(version==thresholds_version_selected())
    return(df)
  })
  
  # --------------- df_wb() ------------
  df_wb <- reactive({
      return(df_wb1)
  })
  
  wb_type <- reactive({
    
    type <- df_ind1 %>%
      filter(version==thresholds_version_selected()) %>%
      filter(WB==values$wbselected)
    type <- type$Type %>% 
      unique()
    type <- type[1]
    return(type)
  })
  
  
  # --------------- scenario_select_list() ------------
  # generate the selection of scenarios which can be chosen
  scenario_select_list <- reactive({
    list_scenario_sel <- c(
      "Baseline" = "baseline",
      "Pristine" = "Pristine",
      "Scenario A" = "Scenario A",
      "Scenario B" = "Scenario B")

    list_scenario_sel <- list_scenario_sel[list_scenario_sel %in% scenarios_included()]
    return(list_scenario_sel)
  })
  
  
  # --------------- param_select_list() ------------
  param_select_list <- reactive({
      list_param_sel <- c("Ecological Status"="Ecological Status",
                          "Phys./chem. Status"="Supporting Status",
                          "Chl a (90pct.)"="Chl",
                          "Chl a (summer)"="Chl_summer",
                          "MSMDI"="MSMDI",
                          "Secchi (summer)"="Secchi",
                          "DO (bottom)"="DO_bot",
                          "NH4 (summer)"="NH4_summer",
                          "NH4 (winter)"="NH4_winter",
                          "NO3 (summer)"="NO3_summer",
                          "NO3 (winter)"="NO3_winter",
                          "PO4 (summer)"="PO4_summer",
                          "PO4 (winter)"="PO4_winter",
                          "TN_summer","TN_winter",
                          "TP_summer","TP_winter"
      )
      
    indicators_available <- c("Ecological Status", "Supporting Status",
                              indicators_included())
    
    list_param_sel <- list_param_sel[list_param_sel %in% indicators_available]
    return(list_param_sel)
  })
  
  # --------------- output$selectScenario() ------------
  output$selectScenario <- renderUI({
    list_scenario_sel <- scenario_select_list()
    tagList(selectInput(
      "selScenario",
      "Scenario:",
      choices = list_scenario_sel,
      selected = list_scenario_sel[1],
      multiple = FALSE, 
      width="400px",
      selectize = T
    ))
  })
  
  # --------------- output$selectParam ------------
  output$selectParam <- renderUI({
    list_param_sel <- param_select_list()
    
    tagList(selectInput(
      "selParam",
      "Display variable:",
      choices = list_param_sel,
      selected = list_param_sel[1],
      multiple = FALSE, 
      width="300px",
      selectize = T
    ))
  })
  
  
  # --------------- output$selectThresholds ------------
  
  threshold_versions <- reactive({
   c(#"veileder (2018)"="2018",
     "veileder rev. (2023)"="2023",
     "kystrev (2025)"="2025",
     "model-based"="model"
   )

  })
  
  
  output$selectThresholds <- renderUI({
    list_threshold_versions <- threshold_versions()
    
    tagList(selectInput(
      "selVersion",
      "Thresholds:",
      choices = list_threshold_versions,
      selected = list_threshold_versions[1],
      multiple = FALSE, 
      width="300px",
      selectize = T
    ))
  })
  
  # --------------- output$selectAggregation ------------
  
  aggregation_methods <- reactive({
    c("aggregration between seasons" = "seasons",
      "no aggregration between seasons"="no seasons",
      "one-out all-out"="OOAO"
    )
    
  })
  
  output$selectAggregation <- renderUI({
    list_aggregation_methods <- aggregation_methods()
    
    tagList(selectInput(
      "selAggregation",
      "Aggregation:",
      choices = list_aggregation_methods,
      selected = list_aggregation_methods[1],
      multiple = FALSE, 
      width="300px",
      selectize = T
    ))
  })
  
  
  
  output$selectPalette <- renderUI({
  
      tagList(selectInput("selPal",
                          width="150px",
                          label="Palette:",
              c("Spectral" = "spectral",
                "s3pcpn" = "AS",
                "Viridis" = "viridis",
                "Wes" = "wes",                                                                  "Rainbow" = "rainbow")))
  
    })
  
  # --------------- function round_sig() ------------
  round_sig <- function(x, n, max_dec=3){
    base <- ifelse(x==0, -2, ceiling(log10(abs(x))))
    ndig <- n - base 
    ndig <- ifelse(ndig<0,0,ndig)
    ndig <- ifelse(ndig>max_dec,max_dec,ndig)
    return(round(x,ndig))    
  }
  
  # --------------- labs() ------------
  labs <- lapply(seq(nrow(waterbodies)), function(i) {
    paste0(waterbodies$Vannfore_1[i], '<br>', 
           waterbodies$Vannforeko[i] ) 
  })
  
  # --------------- observeEvent(values$parameter) ------------
  observeEvent(values$parameter, {
    if(values$parameter!="Ecological Status"){
      #values$discrete_scale <- input$scaleDiscrete
      #values$show_status <- input$showStatus
      shinyjs::enable("scaleDiscrete")
      shinyjs::enable("showStatus")
      updateCheckboxInput(
        inputId =  "scaleDiscrete", 
        value = values$discrete_scale
      )
      updateCheckboxInput(
        inputId =  "showStatus", 
        value = values$show_status
      )
    }else{
      shinyjs::disable("scaleDiscrete")
      shinyjs::disable("showStatus")
      updateCheckboxInput(
        inputId =  "scaleDiscrete", 
        value = FALSE
      )
      updateCheckboxInput(
        inputId =  "showStatus", 
        value = TRUE
      )
      
    }
  })
  
  # --------------- output$WBinfo ------------
  output$WBinfo <-renderText({
    if (values$wbselected=="") {
     s<-  ""
    }else{
      
      type <- wb_type()
      scenario <- input$selScenario
      scenario <- ifelse(scenario=="", "", paste0(" [", scenario, "]"))
      WB_name<-df_WB[df_WB$VANNFOREKOMSTID==values$wbselected,"VANNFOREKOMSTNAVN"]
      s <- paste0(WB_name," ",  values$wbselected, scenario)
      
      if(!is.null(type)){
       # s <- paste0(s, " (Type: ", type,")")  
      }
       
    }
    return(s)
  })
  
  # --------------- output$SelectedWB ------------
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
  
  # --------------- output$SelectedWBStatus ------------
  output$SelectedWBStatus <- renderText({
    if (values$wbselected=="") {
      ""
    }else{
      "Aggregated status"
    }
  })

  # --------------- tagList() ------------
  tagList(
    sliderInput("n", "N", 1, 1000, 500),
    textInput("label", "Label")
  )
  
  # --------------- output$WBbutton ------------
  output$WBbutton <- renderUI({
    if (values$wbselected=="") {
      ""
    }else{
      buttontext <-paste0("Show ",values$wbselected)
      tagList(actionButton("goWB", buttontext))
    }
  })
  
  # --------------- rs() ------------
  rs <- reactive({
    req(input$selParam)
    values$parameter<-input$selParam
    values$period<-  "2017-2019"  #input$selPeriod
    if(values$parameter %in% c("Ecological Status",
                               "Supporting Status")){
      return(NULL)
    }else{
      
      #"
      scenario <- input$selScenario
      
      rfile<- paste0("raster_OF800/", scenario, "_", values$parameter,".tif")
      #cat(paste0(rfile,"\n"))
      return(terra::rast(rfile))
    }
  })
  
  # --------------- scenario_sel() ------------
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
  
  # --------------- wb_status_overall() -------------
  wb_status_overall <- reactive({

    req(df_ind())
    req(input$selScenario)
    req(values$include)
    #req(input$selAggregation)
    
    df <- df_ind()
    
    agg_method <- input$selAggregation
    if(is.null(agg_method)){
      list_aggregation_methods <- aggregation_methods()
      agg_method <- list_aggregation_methods[1]
    }
      
      
    
    
    scenarios <- c("baseline",input$selScenario)
    
    df <- df %>%
      filter(scenario %in% scenarios)
    
    ind_incl <- values$include 
    
    df <- ind_incl %>%
      filter(selected==T) %>%
      select(-selected) %>%
      left_join(df, by="Indicator")
    
    #browser()
    
    dfagg <- aggregate_wb(df, agg_method)

    return(dfagg)
  })
  
  thresholds_version_selected <- reactive({
    
    if(is.null( input$selVersion)){
      list_threshold_versions <- threshold_versions()
      version <- list_threshold_versions[1]
    }else{
      version <- input$selVersion
    }
    return(version)
  })
  
  
  # --------------- wbstatus() -------------
  wbstatus <- reactive({
   
    
    if(values$parameter=="Ecological Status"){
      if(input$selScenario=="Observations"){
        df <-  df_wb_obs %>%
          dplyr::select(WB=Vannforeko, Status=Class)

      }else{

        
        df<- wb_status_overall() %>%
          dplyr::filter(scenario==input$selScenario) %>%
          dplyr::select(WB,Status)
      }
    }else if(values$parameter=="Supporting Status"){
        df<- wb_status_overall() %>%
          dplyr::filter(scenario==input$selScenario) %>%
          dplyr::select(WB,Status=StatusSup)
    }else{
    
    df<-df_ind() %>% 
      dplyr::filter(version==thresholds_version_selected()) %>%
      dplyr::filter(Indicator==values$parameter) %>%
      dplyr::filter(Period==values$period) %>%
      dplyr::filter(scenario==input$selScenario) %>%
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

    
    # dfagg <- df_ind() %>%
    #   filter(scenario==input$selScenario) %>%
    #   split(.$WB) %>%
    #   purrr::map(aggregate, map_status=T) %>%
    #   bind_rows(.id="WB")
    
    
    waterbodies
  }) 
  
  # --------------- colorpal() -------------
  # create color pal
  colorpal <- reactive({
    mypal <- c("#ff0000","#ff8c2b","#ffff00","#00d600","#007eff")
      
     colorFactor(palette=mypal, domain=wbstatus()$Status, na.color="#CCCCCC")})

  # --------------- colorpalrev() -------------
  colorpalrev <- reactive({
    mypal <- c("#ff0000","#ff8c2b","#ffff00","#00d600","#007eff")
    
    colorFactor(palette=mypal, domain=wbstatus()$Status, na.color="#CCCCCC")})
  
  
  output$mymap <- renderLeaflet({
    
    map_obj()
    
  })
  
  # ------ output$mymap -------
  map_obj <- reactive({
    #browser()
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
  
  
  # ------ observeEvent(input$show) -------
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Somewhat important message",
      "This is a somewhat important message.",
      
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
                 
    ))
  })
  
  # ------ observeEvent(input$mymap_zoom) -------
  observeEvent(input$mymap_zoom, {
    values$zoom<- input$mymap_zoom
  })
  
  # ------ observeEvent(input$mymap_center$lng) -------
  observeEvent(input$mymap_center$lng, {
    values$lng<-input$mymap_center$lng
  })
  
  # ------ observeEvent(input$mymap_center$lat) -------
  observeEvent(input$mymap_center$lat, {
    values$lat<-input$mymap_center$lat
  })
  
  
  # ------ observeEvent(input$resetzoom) -------
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
  
  
  # ---------- observeEvent(input$mymap_shape_click) -------
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

  png_map <- reactive({
    
    
  })
  
  plot_ext <- reactive({
    
    if(values$wbselected==""){
      ext <- waterbodies  %>%
        select(Vannforeko) %>%
        sf::st_transform(crs=sf::st_crs(3857)) %>%
        sf::st_bbox()
      width_buffer <- 10000
    }else{
      ext <- waterbodies  %>%
        select(Vannforeko) %>%
        filter(Vannforeko == values$wbselected) %>%
        sf::st_transform(crs=sf::st_crs(3857)) %>%
        sf::st_bbox()
      width_buffer <- 5000
    }
    
    
    x0 <- ext$xmin-width_buffer
    x1 <- ext$xmax+width_buffer
    y0 <- ext$ymin-width_buffer
    y1 <- ext$ymax+width_buffer
    
    
    pts <- rbind(c(x0,y0), c(x1,y0), c(x1,y1), c(x0,y1), c(x0,y0))
    po <- st_polygon(list(pts)) %>%
      sf::st_sfc(crs=sf::st_crs(3857))
    
    ext <- po %>%  
      sf::st_bbox()
    
    return(ext)
    
  })  %>% shiny::bindCache(values$wbselected)
  
  
  plot_basemap <- reactive({
    
    map <- basemaps::basemap_ggplot(plot_ext(), map_service = "esri", map_res = 2,
                                  map_type = "world_light_gray_base") 
    
    return(map)
  }) %>% shiny::bindCache(values$wbselected)
  
  

  
  
  # ---------- observeEvent(input$goWB) -------
  observeEvent(input$goWB, {  
    updateTabItems(session, "tabs", "indicators")
  })
  

  # ---------- observeEvent(input$scaleDiscrete) -------
   observeEvent(input$scaleDiscrete, {  
     if(values$parameter!="Ecological Status"){
       values$discrete_scale <- input$scaleDiscrete
     }
   })

  # ---------- observeEvent(input$showStatus) -------
  observeEvent(input$showStatus, {  
    if(values$parameter!="Ecological Status"){
      values$show_status <- input$showStatus
    }
  })
  # 
  
  
  
  # ------------------- tblwb_df() ----------
  tblwb_df <- reactive({
    shiny::req(values$wbselected)
    shiny::req(wb_status_overall())
    
    df_wb <- wb_status_overall()
    df <- data.frame()
    
    if(values$wbselected==""){
      df<-data.frame()
    }else{
      if(nrow(df_wb)>0){
        df <- df_wb %>%
          filter(WB==values$wbselected)
      }
    }

    return(df)
  })
  
  dfSelectIndicators <- reactive({
    
    df <- df_indicator_list()
    
    df <- df %>%
      rowwise() %>%
      mutate(grp=param_group(Indicator)) %>%
      ungroup() %>%
      relocate(grp) 
    
    df$Indicator <- factor(df$Indicator,levels=params)
    df <- df %>%
      arrange(Indicator)  
    
    return(df)
    
  })
  
  
  
  # --------------- output$tblindxxxxx ----------------------
  output$tblSelectIndicators <- reactable::renderReactable({
    df <- dfSelectIndicators()
    
    df_sel <- isolate(values$include)
    
    sel_index <- df %>%
      select(Indicator) %>%
      left_join(df_sel, by="Indicator") %>%
      mutate(id=row_number()) %>%
      filter(selected==T) %>%
      pull(id)
  
    
    
    colwidths <- 140
    #browser()
    reactable(df, class = "noParenthesis",
              highlight =TRUE,
              compact=TRUE,
              fullWidth = FALSE,
              resizable = TRUE,
              defaultPageSize = 20,
              style = "white-space: nowrap;",
              sortable = FALSE,
              onClick = "select",
              defaultExpanded = TRUE,
              #groupBy = "Kvalitetselement",
              selection = "multiple",
              defaultSelected = sel_index,
              groupBy = c("Kvalitetselement","grp"),
              columns = list(
                Indicator = colDef(show = F), #colDef(width=100), #show = F,name="WB [Period]"),
                IndikatorDesc = colDef(show=F), 
                Indikator= colDef(name="Indikator", width=colwidths, sticky = "left"),
                Kvalitetselement=colDef(width=colwidths, sticky = "left"),
                grp = colDef(name="Gruppe",
                             width=colwidths, 
                             sticky = "left")),
              theme = reactableTheme(
                tableBodyStyle =  list(
                  color = "#999999",
                  backgroundColor = "#dddddd"
                ),
                rowSelectedStyle = list(
                  color = "#000000",
                  backgroundColor = "#ffffff"
                ))
          )
    
  })
  
  
  

  # ------------------- tblind_df() ----------
  tblind_df <- reactive({
    shiny::req(values$wbselected)

    if(values$wbselected==""){
      df<-data.frame()
    }else{
      
      df <- df_ind()
      
      df <- df %>%
        mutate(version=as.character(version)) %>%
        dplyr::select(version, WB, Indicator,
                      Indikator, IndikatorDesc, 
                      Period,scenario,Kvalitetselement,Value,EQR,
                    Ref,HG,GM,MP,PB,Worst,Status)
      dfc <- df %>% 
        dplyr::filter(version==thresholds_version_selected()) %>%
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
          dplyr::filter(version==thresholds_version_selected()) %>%
          dplyr::filter(WB==values$wbselected) %>%
          dplyr::filter(Period==values$period) %>%
          dplyr::filter(scenario==input$selScenario)
        
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
        
        df <- df %>%
          mutate(diff_abs = Value - Value_comp) %>%
          mutate(diff_pct = 100*(diff_abs / Value_comp))
        
        df <- df %>%
          mutate(diff = ifelse(Indicator=="DO_bot",
             paste0(formatC(flag="+", diff_abs, digits=2)," ml/l"),
             paste0(formatC(flag="+", diff_pct,  digits=2),"%")
                               )) %>%
          select(-c(diff_abs, diff_pct)) %>%
          relocate(diff , .after=Value)
        
        # nsmall=1,
        
      }else{
        df <- data.frame()
      }
    }
    return(df)
  })

  
  # --------------- output$tblind ----------------------
  output$tblind <- reactable::renderReactable({
   
    shiny::req(values$wbselected)
    input$selAggregation
    df <- tblind_df() 
    
    if(nrow(df)==0){
      return(NULL)
    }
    
    df <- df %>%
      rowwise() %>%
      mutate(grp=param_group(Indicator)) %>%
      ungroup() %>%
      relocate(grp) %>%
      relocate(version, .before = "Ref")
    
    
    if(nrow(df)>0){
    show_base <- ifelse(input$selScenario==scenario_comparison, F, T)
    
    colwidthgrps <- ifelse(show_base, 80, 120)
    
    #df_sel <- isolate(values$include)
    df_sel <- values$include
    
    sel_index <- df %>%
      select(Indicator) %>%
      left_join(df_sel, by="Indicator") %>%
      mutate(id=row_number()) %>%
      filter(selected==T) %>%
      pull(id)
    
    colw=38
    mypal <- c("#ff000030","#ff8c2b30","#ffff0030","#00d60030","#007eff30")
    reactable(df, class = "noParenthesis",
              highlight =TRUE,
              compact=TRUE,
              fullWidth = FALSE,
              resizable = TRUE,
              defaultPageSize = 20,
              style = "white-space: nowrap;",
              sortable = FALSE,
              #onClick = "select",
              defaultExpanded = TRUE,
              #groupBy = "Kvalitetselement",
              #selection = "multiple",
              defaultSelected = sel_index,
              groupBy = c("Kvalitetselement","grp"),
              columns = list(
                Indicator = colDef(show = F), #colDef(width=100), #show = F,name="WB [Period]"),
                
                IndikatorDesc = colDef(show=F), 
                Indikator= colDef(name="Indikator", width=100, sticky = "left"),
                Kvalitetselement=colDef(width=colwidthgrps, sticky = "left"),
                grp = colDef(name="Gruppe",
                             width=colwidthgrps, 
                             sticky = "left"),
                version = colDef(name="version",
                                 width=50),
                Ref=colDef(width=colw),
                HG=colDef(width=colw),
                GM=colDef(width=colw),
                MP=colDef(width=colw),
                PB=colDef(width=colw),
                Worst=colDef(width=50, format=colFormat(digits=1)),
                Value=colDef(width=40,
                             #aggregate = "count",
                             show=show_base
                             ),
                diff=colDef(width=60, name="Diff.",
                             show=show_base),
                scenario = colDef(show = F), 
                EQR=colDef(width=colw,
                           #aggregate = "min",
                           show=show_base),
                Status=colDef(
                  show=show_base,
                  width=50,
                  style = function(value) {
                    value <- ifelse(is.na(value),"",value)
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
                                #aggregate = "min",
                                name="EQR"
                ),
                Status_comp=colDef(
                  name="Status",
                  show = T,
                  width=50,
                  style = function(value) {
                    value <- ifelse(is.na(value),"",value)
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
                colGroup(name = "Thresholds", 
                         columns = c("version","Ref","HG","GM","MP","PB","Worst")),
                colGroup(name = "Scenario", columns = c("Value",
                                                        "diff",
                                                        "EQR",
                                                        "Status")),
                colGroup(name = "Baseline", columns = c("Value_comp", "EQR_comp", "Status_comp"))
              ),
              theme = reactableTheme(
                tableBodyStyle =  list(
                  color = "#999999",
                  backgroundColor = "#dddddd"
                ),
                rowSelectedStyle = list(
                  color = "#000000",
                  backgroundColor = "#ffffff"
                                     ))
    )}else{
      NULL
      }
    
  })
  
  # --------- ind_tbl_selected() -------------
  ind_tbl_selected <- reactive({
    #shiny::req(values$wbselected)
    sel <- reactable::getReactableState("tblSelectIndicators", "selected")
    #sel <- reactable::getReactableState("tblind", "selected")
    if(length(sel)==0){
      n <- df_indicator_list() %>% length()
      sel <- rep(1, n)
    }
    return(sel)
  })
  
  observeEvent(ind_tbl_selected(),
               ignoreInit = T,{
    
    req(tblind_df())
    
    df_inc <- values$include
    ix <- ind_tbl_selected()
    
   
    df <- tblind_df() 
    if(nrow(df)>0){
       
      df <- df%>%
        select(Indicator) %>%
        mutate(id=row_number()) %>%
        mutate(selected_new = ifelse(id %in% ix, T, F))
      df_inc <- df_inc %>%
        left_join(df, by="Indicator")
      
      df_inc <- df_inc %>%
        mutate(selected=ifelse(is.na(selected_new),
                               selected,
                               selected_new)) %>%
        select(-c(id,selected_new))
      values$include <- df_inc
    }
    
  })
  
  
  # ---------------- output$titleTblAgg ------------------
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
  
  # ---------------- output$titleTblAggBase ------------------
  output$titleTblAggBase<- renderText({
    shiny::req(values$wbselected)
    if(values$wbselected=="" | input$selScenario==scenario_comparison){
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
  
  # ----------- output$titleTblInd ------------
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
  
  # ----------- output$tblaggBase ------------
  output$tblaggBase <- reactable::renderReactable({
    
    shiny::req(values$wbselected)

    ClassList<-c("Bad","Poor","Mod","Good","High")
    
    df <- tblwb_df() 
    
    show_base <- ifelse(input$selScenario==scenario_comparison, F, T)
    
    if(input$selScenario==scenario_comparison){
      df<-data.frame()
    }else{
      if(values$wbselected==""){
        df<-data.frame()
      }else{
        
      df<-df %>% 
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
              fullWidth = FALSE,
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
                    value <- ifelse(is.na(value),"",value)
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
  
 
  
  # ---------------- output$tblagg ------------------
  output$tblagg <- reactable::renderReactable({
    
    
    shiny::req(values$wbselected)
    shiny::req(input$selScenario)

    ClassList<-c("Bad","Poor","Mod","Good","High")
    
    df<-tblwb_df()
  
    
    show_base <- ifelse(input$selScenario==scenario_comparison, F, T)
    
    if(values$wbselected==""){
      df<-data.frame()
    }else{
      if(nrow(df)>0){
              df<-df %>% 
        dplyr::filter(scenario==input$selScenario) 
      }
      
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
              fullWidth = FALSE,
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
                    value <- ifelse(is.na(value),"",value)
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


                    
