FROM rocker/shiny-verse:3.5.3

LABEL maintainer="Travis Gerke (Travis.Gerke@moffitt.org)"

# Install system dependencies for required packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libssl-dev \
  libxml2-dev \
  libmagick++-dev \
  libv8-3.14-dev \
  libglu1-mesa-dev \
  freeglut3-dev \
  mesa-common-dev \
  libudunits2-dev \
  libpoppler-cpp-dev \
  libwebp-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
  
RUN install2.r --error \
  shinyAce \
  shinydashboard \
  shinyWidgets \
  DiagrammeR \
  ggdag \
  igraph \
  pdftools \
  shinyBS \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
  
RUN Rscript -e "devtools::install_github('metrumresearchgroup/texPreview', ref = 'e954322')" \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install TinyTeX
RUN install2.r --error tinytex \
  && wget -qO- \ 
     "https://github.com/yihui/tinytex/raw/master/tools/install-unx.sh" | \ 
     sh -s - --admin --no-path \
  && mv ~/.TinyTeX /opt/TinyTeX \
  && /opt/TinyTeX/bin/*/tlmgr path add \ 
  && tlmgr install metafont mfware inconsolata tex ae parskip listings \ 
  && tlmgr install standalone varwidth xcolor colortbl multirow psnfss setspace pgf \
  && tlmgr path add \ 
  && Rscript -e "tinytex::r_texmf()" \ 
  && chown -R root:staff /opt/TinyTeX \ 
  && chmod -R a+w /opt/TinyTeX \ 
  && chmod -R a+wx /opt/TinyTeX/bin \ 
  && echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
  
RUN install2.r --error shinyjs \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN install2.r --error plotly shinycssloaders \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN installGithub.r gadenbuie/shinyThings@4e8becb2972aa2f7f1960da6e5fe6ad39aeceda0 \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

ARG SHINY_APP_IDLE_TIMEOUT=600
RUN sed -i "s/directory_index on;/app_idle_timeout ${SHINY_APP_IDLE_TIMEOUT};/g" /etc/shiny-server/shiny-server.conf
COPY . /srv/shiny-server/shinyDAG
RUN chown -R shiny:shiny /srv/shiny-server/
