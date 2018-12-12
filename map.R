
library(leaflet)
library(raster)


vann<- shapefile("shp/martini_shapes.shp")


leaflet() %>% addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>%   
  addPolygons(data=vann , fill ="#03F" , stroke = TRUE, color = "#03F", 
              popup = paste0("WB: ", as.character(vann$Name)), 
              group = "Vann F") %>% 
  # add a legend
  addLegend("bottomright", colors = c("#03F"), labels = c("Vann F")) %>%   
  # add layers control
  addLayersControl(
    overlayGroups = c("Vann F"),
    options = layersControlOptions(collapsed = FALSE)
  )


