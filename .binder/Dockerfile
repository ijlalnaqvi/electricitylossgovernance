FROM rocker/binder:4.0.2
LABEL maintainer='Ijlal Naqvi'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/anirudhgovind/electricitylossgovernance/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2021-05-31/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
