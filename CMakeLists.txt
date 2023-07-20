## file: CMakeLists.txt
## details: Build the `ufs_ftnutils` package.
## author: Henry R. Winterbottom
## date: 20 July 2023
## version: 0.0.1
## license: LGPL v2.1

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
