!> @file: ftnutils_kinds.F90
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
