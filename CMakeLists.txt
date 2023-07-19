# (C) Copyright 2023 - Henry R. Winterbottom

# This library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 2.1 of the
# License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA

cmake_minimum_required(VERSION 3.16)
project(UFS_FTNUTILS)
enable_language(Fortran C)

set(DDEFS "NETCDF")
foreach(item ${DDEFS})
    list(APPEND DEFINITIONS "-D${item}")
endforeach(item ${DDEFS})
add_definitions(${DEFINITIONS})

set(CMAKE_UFS_FTNUTILS_PATH ${CMAKE_CURRENT_SOURCE_DIR})
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_UFS_FTNUTILS_PATH}/include)
set(INSTALL_PATH ${CMAKE_UFS_FTNUTILS_PATH}/lib)
set(CMAKE_Fortran_FLAGS_RELEASE "-g -fcheck=all -fbacktrace -Wall")
set(CMAKE_Fortran_FLAGS_DEBUG "-O0")

add_subdirectory(${CMAKE_UFS_FTNUTILS_PATH}/sorc/fson)
add_subdirectory(${CMAKE_UFS_FTNUTILS_PATH}/sorc/slint)
add_subdirectory(${CMAKE_UFS_FTNUTILS_PATH}/sorc/ftnutils)

set(DIRECTORY_PERMISSIONS "OWNER_WRITE" "OWNER_READ" "OWNER_EXECUTE" "GROUP_READ" "GROUP_EXECUTE" "WORLD_READ" "WORLD_EXECUTE")
install(DIRECTORY DESTINATION ${INSTALL_PATH} DIRECTORY_PERMISSIONS ${DIRECTORY_PERMISSIONS})
install(TARGETS ${TARGET} DESTINATION ${INSTALL_PATH})
