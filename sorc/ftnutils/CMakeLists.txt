## file: sorc/ufs_ftnutils/CMakeLists.txt
## details: Build the `ufs_ftnutils` package.
## author: Henry R. Winterbottom
## date: 20 July 2023
## version: 0.0.1
## license: LGPL v2.1

include_directories(${CMAKE_SOURCE_DIR}/ftnutils ${NETCDF}/include)
set(LLIBS "m" ${NETCDF}/lib -lnetcdf -lnetcdff)
set(TARGET "ftnutils")
file(GLOB f90_objs *.F90)
set(CMAKE_Fortran_FLAGS "-O3 -g")
set(F90_OBJS ${f90_objs})
add_library(ftnutils SHARED ${f90_objs})
target_include_directories(ftnutils PRIVATE ${CMAKE_UFS_FTNUTILS_PATH}/include)
install(TARGETS ${TARGET} DESTINATION ${INSTALL_PATH})
