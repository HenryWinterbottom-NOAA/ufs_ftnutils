[![License](https://img.shields.io/badge/License-LGPL_v2.1-black)](https://github.com/HenryWinterbottom-NOAA/ufs_pyutils/blob/develop/LICENSE)
![Linux](https://img.shields.io/badge/Linux-ubuntu%7Ccentos-lightgrey)
[![Disclaimer](https://img.shields.io/badge/Disclaimer-Disclaimer-yellow)](https://github.com/HenryWinterbottom/ufs_ftnutils/blob/develop/DISCLAIMER.md)

[![Dependencies](https://img.shields.io/badge/Dependencies-fson-orange)](https://github.com/josephalevin/fson)
[![](https://img.shields.io/badge/netCDF-orange)](https://github.com/Unidata)

# Cloning

To clone the repository do as follows.

~~~
user@host:$ git clone https://github.com/HenryWinterbottom-NOAA/ufs_ftnutils
~~~

# Building and Installing

Build the `ufs_ftnutils` as follows.

~~~
user@host:$ cd /path/to/ufs_ftnutils
user@host:$ mkdir -p /path/to/ufs_ftnutils/build
user@host:$ cd /path/to/ufs_ftnutils/build
user@host:$ cmake -DNETCDF=/path/to/netcdf ../
user@host:$ make
user@host:$ make install
~~~

If successful, the respective modules and libraries will be written to
`/path/to/ufs_ftnutils/include` and `/path/to/ufs_ftnutils/lib`,
respectively. They can then be linked as follows.

~~~
user@host:$ cd /path/to/dependent/application/source_code
user@host:$ /path/to/gfortran source_code.F90 -I/ufs_ftnutils/include -I/netcdf/include -L/ufs_ftnutils/libs -L/netcdf/lib -lfson -lftnutils -lnetcdf -lnetcdf 
~~~

In the above example, `/ufs_ftnutils/include` and `/ufs_ftnutils/lib`
typically point to `/path/to/ufs_ftnutils/include` and
`/path/to/ufs_ftnutils/lib`, respectively. The
[`/netcdf`](https://downloads.unidata.ucar.edu/netcdf/) path is the
path to the netCDF modules and libraries on the host platform. Note
that the dependent packages must be compiled with the exact compiler
version as `ufs_ftnutils` and netCDF.

# Docker Images

A Docker image containing the latest `ufs_ftnuils` package, can be
gathered as follows.

~~~
user@host:$ /path/to/docker pull noaaufsrnr/ubuntu20.04.ufs_ftnutils:latest
~~~

# Known Issues

This code base is supported and tested against the [GNU
Fortran](https://fortran-lang.org/learn/os_setup/install_gfortran/)
compiler. Compliance with other Fortran compilers is not guaranteed.

# Forking

If a user wishes to contribute modifications done within their
respective fork(s) to the authoritative repository, we request that
the user first submit an issue and that the fork naming conventions
follow those listed below.

- `docs/user_fork_name`: Documentation additions and/or corrections for the application(s).

- `feature/user_fork_name`: Additions, enhancements, and/or upgrades for the application(s).

- `fix/user_fork_name`: Bug-type fixes for the application(s) that do not require immediate attention.

- `hotfix/user_fork_name`: Bug-type fixes which require immediate attention to fix issues that compromise the integrity of the respective application(s).  