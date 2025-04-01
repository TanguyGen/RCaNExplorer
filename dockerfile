FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libglpk-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages from CRAN
RUN R -e "install.packages(c( \
    'shiny', 'visNetwork', 'dplyr', 'ggplot2', 'tidyr', \
    'shinyjs', 'plotly', 'stringr', 'tibble', 'purrr', \
    'patchwork', 'shinycssloaders', 'coda' \
    ), repos='https://cloud.r-project.org')"

# Copy the app into the container
COPY ./ /srv/shiny-server/

# Fix permissions for Shiny to access the app directory
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the default Shiny port
EXPOSE 3838

# Start Shiny server
CMD ["/usr/bin/shiny-server"]
