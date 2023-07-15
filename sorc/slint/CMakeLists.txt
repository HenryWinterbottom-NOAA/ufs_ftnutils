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

include_directories(${CMAKE_UFS_FTNUTILS_PATH}/sorc/slint)
set (LLIBS "m")
set (TARGET "slint")
set (CMAKE_Fortran_FLAGS "-O3 -g")
file (GLOB f90_objs *.F90)
set (F90_OBJS ${f90_objs})
add_library (slint SHARED ${f90_objs})
target_include_directories(slint PRIVATE ${CMAKE_UFS_FTNUTILS_PATH}/include)
install ( TARGETS ${TARGET} DESTINATION ${INSTALL_PATH} )

