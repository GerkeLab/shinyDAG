FROM rocker/shiny-verse:3.5.1
LABEL maintainer="Travis Gerke (Travis.Gerke@moffitt.org)"

# Install system dependencies for required packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libssl-dev \
  libxml2-dev \
  libmagick++-dev \
  libv8-3.14-dev
  
RUN install2.r --error --deps TRUE shinyAce
RUN install2.r --error --deps TRUE shinydashboard
RUN install2.r --error --deps TRUE shinyWidgets
RUN install2.r --error --deps TRUE DiagrammeR
RUN apt-get update -qq && apt-get -y --no-install-recommends install libudunits2-dev
RUN install2.r --error --deps TRUE ggdag
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libglu1-mesa-dev freeglut3-dev mesa-common-dev
RUN install2.r --error --deps TRUE igraph
#RUN install2.r --error --deps TRUE texPreview
RUN Rscript -e "devtools::install_github('metrumresearchgroup/texPreview', ref = 'e954322')"
RUN apt-get update -qq && apt-get -y --no-install-recommends install libpoppler-cpp-dev
RUN install2.r --error pdftools

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
  && echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron
  
ARG SHINY_APP_IDLE_TIMEOUT=600
RUN sed -i "s/directory_index on;/app_idle_timeout ${SHINY_APP_IDLE_TIMEOUT};/g" /etc/shiny-server/shiny-server.conf
COPY . /srv/shiny-server/ShinyDAG
RUN chown -R shiny:shiny /srv/shiny-server/