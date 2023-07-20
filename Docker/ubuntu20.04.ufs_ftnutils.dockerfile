
# =========================================================================
# Docker: Dockerfile

# Description
# -----------
# This Docker recipe file builds a Docker image containing the following packages:
# - Ubuntu 20.04 base Linux image;
# - `ufs_ftnutils` package.

# Author(s)
# ---------
# Henry R. Winterbottom; 19 July 2023

# History
# -------
# 2023-07-19: Henry R. Winterbottom -- Initial implementation.

# License
# -------
# LGPL v2.1

# History
# -------
#    2023-06-21: Henry R. Winterbottom -- Initial implementation.

# -------------------------
# * * * W A R N I N G * * *
# -------------------------

# It is STRONGLY urged that users do not make modifications below this
# point; changes below are not supported.

# ----

FROM noaaufsrnr/ubuntu20.04.base
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
ENV GIT_URL="https://www.github.com/HenryWinterbottom-NOAA/ufs_ftnutils.git"
ENV GIT_BRANCH="develop"

LABEL "author"="Henry R. Winterbottom (henry.winterbottom@noaa.gov)"
LABEL "description"="Ubuntu 20.04 `ufs_ftnutils` package."
LABEL "maintainer"="Henry R. Winterbottom"
LABEL "tag"="latest"
LABEL "version"="0.0.1"

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    g++ \
    gcc \
    make \
    gfortran \
    cmake && \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
    libnetcdff-dev && \
    rm -rf /var/lib/apt/lists/*	

RUN git clone ${GIT_URL} -b ${GIT_BRANCH} /ufs_ftnutils && \
    cd /ufs_ftnutils && \
    mkdir -p /ufs_ftnutils/build && \
    cd /ufs_ftnutils/build && \
    cmake -DNETCDF=/usr ../ && \
    make && \
    make install