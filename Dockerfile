FROM rocker/shiny


RUN apt-get update -y && \
    apt-get install -y \
    gdal-bin \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libspatialindex-dev \
    proj-bin \
    proj-data

ENV CPLUS_INCLUDE_PATH /usr/include/gdal

# Install R packages
RUN install2.r --error --ncpus 4 --deps TRUE tidyverse shiny leaflet rgdal raster shinydashboard DT shinyjs
RUN rm -rf /tmp/*

COPY /app /martini

USER shiny
CMD ["R", "-e", "shiny::runApp('/martini', host = '0.0.0.0', port = 3838)"]