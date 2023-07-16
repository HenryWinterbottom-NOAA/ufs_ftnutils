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

!> @file: ftnutils_errors.F90
!! @details: This module contains the base-class object for all
!!           exception classes.
!! @author: Henry R. Winterbottom
!! @date: 02 July 2023
!! @version: 0.0.1
!! @license: LGPL v2.1
module ftnutils_errors
   use ftnutils_log, only: Logger
   implicit none
   private

   !> @brief: The Error base-class object.
   type, public :: Error
      type(Logger) :: logobj
   contains
      procedure, public :: raise
   end type Error
contains

   !> @brief: Raises the respective exception.
  !! @param: msg
  !!
  !!    - The string to accompany the respective exception.
   subroutine raise(this, msg)
      class(Error) :: this
      character(len=500) :: msg

      call this%logobj%error(msg=msg)
      stop
   end subroutine raise
end module ftnutils_errors
