packages <- c(
  
  'dplyr',
  'shiny',
  'leaflet',
  'raster',
  'shinydashboard',
  'DT',
  'shinyjs',
  'sf',
  'reactable',
  'shiny.router',
  'stringr',
  'viridis',
  'wesanderson',
  
  '')
options(Ncpus = -1)
for (pkg in packages) {
  if (pkg == '') {
    next
  }
  install.packages(pkg)
  if ( ! library(pkg, character.only=TRUE, logical.return=TRUE) ) {
    quit(status=1, save='no')
  }
}
