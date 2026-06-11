#!/bin/bash

set -e

## build ARGs
NCPUS=${NCPUS:--1}

# a function to install apt packages only if they are not installed
function apt_install() {
    if ! dpkg -s "$@" >/dev/null 2>&1; then
        if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
            apt-get update
        fi
        apt-get install -y --no-install-recommends "$@"
    fi
}

apt_install \
    make \
    libpng-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libudunits2-dev \
    zlib1g-dev \
    libx11-dev \
    libcurl4-openssl-dev \
    libfreetype6-dev \
    libjpeg-dev \
    libtiff-dev \
    libwebp-dev \
    libicu-dev \
    libfontconfig1-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libxml2-dev

install2.r --error --skipinstalled -n "$NCPUS" \
    bslib \
    grid \
    gridExtra \
    leaflet \
    markdown \
    RColorBrewer \
    shiny \
    shinythemes \
    shinydashboard \
    sf \
    terra \
    tidyverse

## a bridge to far? -- brings in another 60 packages
# install2.r --error --skipinstalled -n "$NCPUS" tidymodels

# Clean up
rm -rf /var/lib/apt/lists/*
rm -rf /tmp/downloaded_packages

## Strip binary installed lybraries from RSPM
## https://github.com/rocker-org/rocker-versioned2/issues/340
strip /usr/local/lib/R/site-library/*/libs/*.so

# Check the leaflet version
echo -e "Check the leaflet package...\n"

R -q -e "library(leaflet)"

echo -e "\nInstall leaflet package, done!"
