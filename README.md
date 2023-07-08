[![License](https://img.shields.io/badge/License-LGPL_v2.1-black)](https://github.com/HenryWinterbottom-NOAA/ufs_pyutils/blob/develop/LICENSE)
![Linux](https://img.shields.io/badge/Linux-ubuntu%7Ccentos-lightgrey)

[![Dependencies](https://img.shields.io/badge/Dependencies-fson-orange)](https://github.com/josephalevin/fson)

# Cloning

This repository utilizes several sub-modules from various sources. To
obtain the entire system, do as follows.

~~~
user@host:$ git clone --recursive https://github.com/HenryWinterbottom-NOAA/ufs_ftnutils
~~~

# Building and Installing

Build the `ufs_ftnutils` as follows.

~~~
user@host:$ cd /path/to/ufs_ftnutils
user@host:$ mkdir -p /path/to/ufs_ftnutils/build
user@host:$ cd /path/to/ufs_ftnutils/build
user@host:$ cmake ../
user@host:$ make
user@host:$ make install
~~~

If successful, the respective modules and libraries will be written to
`/path/to/ufs_ftnutils/include` and `/path/to/ufs_ftnutils/lib`,
respectively.

# Forking

If a user wishes to contribute modifications done within their
respective fork(s) to the authoritative repository, we request that
the user first submit an issue and that the fork naming conventions
follow those listed below.

- `docs/user_fork_name`: Documentation additions and/or corrections for the application(s).

- `feature/user_fork_name`: Additions, enhancements, and/or upgrades for the application(s).

- `fix/user_fork_name`: Bug-type fixes for the application(s) that do not require immediate attention.

- `hotfix/user_fork_name`: Bug-type fixes which require immediate attention to fix issues that compromise the integrity of the respective application(s).  