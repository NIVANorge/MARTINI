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
                                            fluidRow( column(6,""))
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
  
  # router_server()
  
}

shinyApp(ui, server)
