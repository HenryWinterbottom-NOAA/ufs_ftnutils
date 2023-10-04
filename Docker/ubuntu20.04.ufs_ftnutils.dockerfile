# File: Docker/ubuntu20.04.ufs_ftnutils.dockerfile
# Author: Henry R. Winterbottom
# Date: 21 June 2023

# -------------------------
# * * * W A R N I N G * * *
# -------------------------

# It is STRONGLY urged that users do not make modifications below this
# point; changes below are not supported.

# -------------------------
# * * * W A R N I N G * * *
# -------------------------

FROM noaaufsrnr/ubuntu20.04.base
ENV UFS_FTNUTILS_GIT_URL="https://www.github.com/HenryWinterbottom-NOAA/ufs_ftnutils.git"
ENV UFS_FTNUTILS_GIT_BRANCH="develop"
ENV FTNUTILS_ROOT="/opt/ufs_ftnutils"

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
RUN $(command -v apt-get) update -y && \
    $(command -v apt-get) install -y --no-install-recommends \
    g++ \
    gcc \
    make \
    gfortran \
    cmake && \
    $(command -v apt-get) update -y && \
    $(command -v apt-get) install -y --no-install-recommends libnetcdff-dev && \
    $(command -v rm) -rf /var/lib/apt/lists/*	

RUN $(command -v git) clone --recursive "${UFS_FTNUTILS_GIT_URL}" --branch "${UFS_FTNUTILS_GIT_BRANCH}" "${FTNUTILS_ROOT}" && \
    cd "${FTNUTILS_ROOT}" && \
    $(command -v mkdir) -p "${FTNUTILS_ROOT}/build" && \
    cd "${FTNUTILS_ROOT}/build" && \
    $(command -v cmake) -DNETCDF=/usr ../ && \
    $(command -v make) && \
    $(command -v make) install