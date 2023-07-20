## file: /sorc/slint/CMakeLists.txt
## details: Build the `slint` package.
## author: Henry R. Winterbottom
## date: 20 July 2023
## version: 0.0.1
## license: LGPL v2.1

include_directories(${CMAKE_UFS_FTNUTILS_PATH}/sorc/slint)
set(LLIBS "m")
set(TARGET "slint")
set(CMAKE_Fortran_FLAGS "-O3 -g")
file(GLOB f90_objs *.F90)
set(F90_OBJS ${f90_objs})
add_library(slint SHARED ${f90_objs})
target_include_directories(slint PRIVATE ${CMAKE_UFS_FTNUTILS_PATH}/include)
install(TARGETS ${TARGET} DESTINATION ${INSTALL_PATH})
