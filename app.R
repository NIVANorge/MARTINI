library(shiny)
library(leaflet)
library(rgdal)
library(raster)
library(shinydashboard)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

#data <- read.csv("data/data.csv")
#map <- readOGR("shp/nve_kystsone_f.shp",layer = "nve_kystsone_f", GDAL1_integer64_policy = TRUE)

waterbodies <- shapefile("shp/martini_shapes.shp")
waterbody1 <- shapefile("shp/martini_shape_select.shp")
#r <- raster("C:/Data/GitHub/MARTINI/raster/chl")
r <- raster("./raster/chl")
pal <- colorNumeric(c( "#dffbf3", "#c8dda9","#018e18"), values(r),na.color = "transparent")



ui <- dashboardPage(skin = "black",title="MARTINI Status Assessment",
                    dashboardHeader(title = "MARTINI"),
                    dashboardSidebar(sidebarMenu(id="tabs",
                                                 menuItem("Map", tabName = "Map", icon = icon("map-marker")),
                                                 menuItem("Indicators", tabName = "indicators", icon = icon("tasks")),
                                                 #menuItem("Data", tabName = "data", icon = icon("database"))
                                                 menuItem("Status", tabName = "status", icon = icon("bar-chart")),
                                                 #menuItem("Download", tabName = "download", icon = icon("file"))
                                                 menuItem("Options", tabName = "options", icon = icon("cog"))#,
                    )),
                    dashboardBody(tabItems(
                      # tab content
                      tabItem(tabName = "Map",
                              fluidRow(
                                
                                column(6,
                                       leafletOutput("mymap",height="600px"),""),
                                       
                                
                                column(3,selectInput("selParam",label="Display variable:",c("Chlorophyll a")
                                       #actionButton("recalc", "New points")
                                       ))
                              )),
                      tabItem(tabName = "indicators",
                              fluidRow( column(6,
                                               selectInput("selInd",label="",c("Chlorophyll a ","Total phosphorous","Phosphate P","Total nitrogen","Nitrate N","Ammonium N","Secchi depth","Oxygen","What else?"),multiple=T)
                                               ))),
                      tabItem(tabName = "status",
                              fluidRow( column(6,""))),
                      tabItem(tabName = "options",
                              fluidRow( column(6,"")))
                      
                      )))

server <- function(input, output, session) {
  
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet(waterbodies) %>% 
      #addProviderTiles(providers$OpenStreetMap) %>% 
      addTiles() %>% 
      addRasterImage(r, colors = pal, opacity=0.9) %>%
      addLegend(pal = pal, values = values(r),
                #labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                title = "Chl a [µg/l]") %>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      #addPolygons(fill = TRUE, stroke = FALSE, color = "#03F",opacity=0.1)  %>%
      #addPolylines(stroke=TRUE,color="#03F",opacity=0.9,weight=0.5) %>%
      #addPolylines(data=waterbody1,stroke=TRUE,color="#FF0000",opacity=1,weight=2, popup=~as.character(Name))
    
    addPolygons(#data = hood_shp, 
                fillColor = "#03F",
                color = "transparent",
                fillOpacity = 0.1,
                
                # Highlight neighbourhoods upon mouseover
                highlight = highlightOptions(
                  weight = 3,
                  fillOpacity = 0,
                  color = "red",
                  opacity = 1.0,
                  bringToFront = TRUE,
                  sendToBack = TRUE),  
                
                # # Add label info when mouseover
                label = waterbodies$Name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                popup=paste0("<div><a target='_blank' href='",waterbodies$Name,"'>Go to ",waterbodies$Name,"</a></div>")
                
                ) 
  })
}

shinyApp(ui, server)



#leaflet(data = llanos) %>% addTiles() 
# addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% addLegend("bottomright", colors = "#03F", labels = "Llanos ecoregion"