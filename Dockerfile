FROM rocker/shiny
WORKDIR /srv/shiny-server/martini_app

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
RUN install2.r --error --ncpus 4 --deps TRUE tidyverse shiny leaflet rgdal raster shinydashboard DT
RUN rm -rf /tmp/*

ADD shiny.conf /etc/shiny-server/shiny-server.conf
ADD app /srv/shiny-server/martini_app

USER shiny