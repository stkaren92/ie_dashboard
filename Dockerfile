FROM rocker/shiny:4.5.2

COPY --chmod=755 ./rocker_scripts/install_reqs.sh /rocker_scripts/install_reqs.sh
RUN /rocker_scripts/install_reqs.sh

COPY . /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server/data