# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
  

# Install R packages that are required
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('andrewsali/shinycssloaders')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"


# copy the app to the image
COPY welthungerhilfe_index_viz.Rproj /srv/shiny-server/
COPY Data/all_map_data.feather /srv/shiny-server/Data/
COPY Data/all_data.rds /srv/shiny-server/Data/
COPY app.R /srv/shiny-server/
COPY generate_maps.R /srv/shiny-server/

EXPOSE 3838

RUN sudo chown -R shiny:shiny /srv/shiny-server

CMD ["/usr/bin/shiny-server.sh"]