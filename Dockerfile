# Use the official Shiny image from Rocker
FROM rocker/shiny:latest

# Install required R packages including Auth0 dependencies
RUN install2.r --error --skipinstalled \
    auth0 \
    shiny \
    tidyverse \
    readxl \
    openxlsx \
    shinyjs \
    httr \
    yaml

# Copy your Shiny app into the Shiny Server directory
COPY . /srv/shiny-server/

# Optional: log Shiny output to stdout for easier debugging in Portainer
ENV APPLICATION_LOGS_TO_STDOUT=true

# Expose the default Shiny port
EXPOSE 3838

# Set working directory (optional but good practice)
WORKDIR /srv/shiny-server

# Optionally, if you want to include _auth0.yml in the image (not recommended for secrets)
#COPY _auth0.yml /srv/shiny-server/_auth0.yml

RUN echo "options(auth0_find_config_file = '/srv/shiny-server/app/_auth0.yml')" > /usr/local/lib/R/etc/Rprofile.site
