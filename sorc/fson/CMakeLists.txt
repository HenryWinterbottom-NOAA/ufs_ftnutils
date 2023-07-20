## file: sorc/fson/CMakeLists.txt
## details: Build the `fson` package.
## author: Henry R. Winterbottom
## date: 20 July 2023
## version: 0.0.1
## license: LGPL v2.1

include_directories(${CURRENT_SOURCE_DIR}/fson)
set(LLIBS "m")
set(TARGET "fson")
set(CMAKE_Fortran_FLAGS "-O3 -g")
file(GLOB f90_objs *.f90)
set(F90_OBJS ${f90_objs})
add_library(fson SHARED ${f90_objs})
target_include_directories(fson PRIVATE ${CMAKE_UFS_FTNUTILS_PATH}/include)
install(TARGETS ${TARGET} DESTINATION ${INSTALL_PATH})
