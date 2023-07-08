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

!> @brief: ftnutils_kinds
!! @details: This module contains supported data types.
!! @author: Henry R. Winterbottom
!! @date: 03 July 2023
!! @version: 0.0.1
!! @license: LGPL v2.1
module ftnutils_kinds
  implicit none
  private

  !> @brief: The available data types object.
  integer, public, parameter :: ibyte = selected_int_kind(1) 
  integer, public, parameter :: ishort = selected_int_kind(4)
  integer, public, parameter :: ilong = selected_int_kind(8)
  integer, public, parameter :: maxchar = 1024
  integer, public, parameter :: rsingle = selected_real_kind(6)
  integer, public, parameter :: rdouble = selected_real_kind(15)
end module ftnutils_kinds
