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

!> @brief: ftnutils_netcdf
!! @details: This module contains the base-class object for the netCDF
!!           API.
!! @author: Henry R. Winterbottom
!! @date: 08 July 2023
!! @version: 0.0.1
!! @license: LGPL v2.1
module ftnutils_netcdf
  use netcdf
  use ftnutils_kinds, only: maxchar, rdouble, rsingle
  use ftnutils_log, only: Logger
  implicit none
  private

  type, public :: NCDATA
     type(Logger) :: logcls
     character(len=maxchar) :: ncfile
     logical :: read = .false.
     logical :: read_write = .false.
     logical :: write = .false.
     integer :: ncfileid
     integer :: ncstatus
   contains
     procedure, public :: ncopen
     procedure, public :: ncread
  end type NCDATA
  
contains

  !> @brief: # TODO
  subroutine read_scalar(this, varname, vararr)
    class(NCDATA) :: this
    character(len=maxchar) :: varname
    real(rsingle) :: vararr(*)

    call this%ncopen()

  end subroutine read_scalar

  !> @brief: # TODO
  subroutine ncopen(this)
    class(NCDATA), intent(inout) :: this
    character(len=maxchar) :: filename
    character(len=500) :: msg

    filename = this%ncfile
    if(this%read) then
       this%ncstatus = nf90_open(trim(adjustl(filename)), nf90_nowrite, &
            this%ncfileid)
    elseif(this%read_write) then
       this%ncstatus = nf90_open(trim(adjustl(filename)), nf90_write, &
            this%ncfileid)
    elseif(this%write) then
       this%ncstatus = nf90_open(trim(adjustl(filename)), nf90_clobber, &
            this%ncfileid)
    else
       msg = "FAILED" 
       call this%logcls%error(msg=msg)
    end if
  end subroutine ncopen

  !> @brief: # TODO
  subroutine ncread(this)
    class(NCDATA), intent(inout) :: this    
    call this%ncopen()

  end subroutine ncread
end module ftnutils_netcdf
