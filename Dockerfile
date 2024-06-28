FROM rocker/shiny:4.3.3


RUN apt-get update -y && \
    apt-get install -y \
    gdal-bin \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libspatialindex-dev \
    libudunits2-dev \
    proj-bin \
    proj-data

ENV CPLUS_INCLUDE_PATH /usr/include/gdal

# Install R packages
COPY dependencies.R .

RUN R --slave --no-restore -e 'source("dependencies.R")'

RUN rm -rf /tmp/*

COPY /app /martini

USER shiny
CMD ["R", "-e", "shiny::runApp('/martini', host = '0.0.0.0', port = 3838)"]
