!! (C) Copyright 2023 - Henry R. Winterbottom

!! This library is free software; you can redistribute it and/or
!! modify it under the terms of the GNU Lesser General Public License
!! as published by the Free Software Foundation; either version 2.1 of
!! the License, or (at your option) any later version.

!! This library is distributed in the hope that it will be useful, but
!! WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!! Lesser General Public License for more details.

!! You should have received a copy of the GNU Lesser General Public
!! License along with this library; if not, write to the Free Software
!! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!! 02110-1301 USA

!> @brief: ftnutils_constants
!! @details: This module contains constant variable values.
!! @author: Henry R. Winterbottom
!! @date: 03 July 2023
!! @version: 0.0.1
!! @license: LGPL v2.1
module ftnutils_constants
  use ftnutils_kinds, only: rdouble, rsingle
  implicit none

  real(rdouble), parameter :: pi = acos(-1.0)
  real(rdouble), parameter :: deg2rad = pi/180.0_rdouble
  real(rdouble), parameter :: rad2deg = 1.0_rdouble/deg2rad
end module ftnutils_constants
