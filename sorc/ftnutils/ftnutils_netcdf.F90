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
   use ftnutils_errors, only: Error
   use ftnutils_kinds, only: maxchar, rdouble, rsingle
   use ftnutils_log, only: Logger
   implicit none
   private
   public :: ncread
   public :: ncreaddim
   public :: ncvardims
   public :: ncwrite
   public :: ncwritedef

   interface ncread
      module procedure read_arr1d_double
      module procedure read_arr1d_single
      module procedure read_arr2d_double
      module procedure read_arr2d_single
      module procedure read_arr3d_double
      module procedure read_arr3d_single
      module procedure read_scalar_double
      module procedure read_scalar_single
   end interface ncread

   interface ncwrite
      module procedure write_scalar_double
      module procedure write_scalar_single
   end interface ncwrite

   type, public :: NCDATA
      type(Logger) :: logcls
      character(len=maxchar) :: ncfile
      logical :: read = .false.
      logical :: read_write = .false.
      logical :: write = .false.
      integer :: ncdimid
      integer :: ncfileid
      integer :: ncvarid
      integer :: ncstatus
   contains
      procedure, public :: ncclose
      procedure, public :: ncopen
      procedure, public :: ncreaddim
   end type NCDATA
contains

   !> @brief: Closes an open netCDF file object.
  !!
  !! @params[inout]: this
  !!
  !!    - The respective netCDF class.
   subroutine ncclose(this)
      class(NCDATA), intent(inout) :: this
      character(len=500) :: msg

      this%ncstatus = nf90_close(this%ncfileid)
      if (this%ncstatus /= 0) call ncerror(nccls=this)
   end subroutine ncclose

   !> @brief: Defines the integer dimension variable identification key
  !!         within an open netCDF-formatted file path.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: dimname
  !!
  !!    - The netCDF dimension variable name.
   subroutine ncdimid(nccls, dimname)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: dimname
      character(len=500) :: msg

      nccls%ncstatus = nf90_inq_dimid(nccls%ncfileid, trim(adjustl(dimname)), &
                                      nccls%ncdimid)
      write (msg, 500) trim(adjustl(dimname)), nccls%ncdimid
      call nccls%logcls%info(msg=msg)
500   format("netCDF dimension variable", 1x, a, 1x, "has ID", 1x, i3, 1x, ".")
   end subroutine ncdimid

   !> @brief: Raises an exception for errors encountered by the
  !!         respective netCDF class.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
   subroutine ncerror(nccls)
      class(NCDATA), intent(in) :: nccls
      type(Error) :: errcls
      character(len=maxchar) :: msg

      write (msg, 500) trim(nf90_strerror(nccls%ncstatus))
      call errcls%raise(msg=msg)
500   format("NetCDF failed with error", 1x, a, 1x, ". Aborting!!!")
   end subroutine ncerror

   !> @brief: Defines/opens a netCDF file object.
  !!
  !! @params[inout]: this
  !!
  !!    - The respective netCDF class.
   subroutine ncopen(this)
      class(NCDATA), intent(inout) :: this
      character(len=maxchar) :: filename
      character(len=500) :: msg

      filename = this%ncfile
      if (this%read) then
         this%ncstatus = nf90_open(trim(adjustl(filename)), nf90_nowrite, &
                                   this%ncfileid)
      elseif (this%read_write) then
         this%ncstatus = nf90_open(trim(adjustl(filename)), nf90_write, &
                                   this%ncfileid)
      elseif (this%write) then
         this%ncstatus = nf90_open(trim(adjustl(filename)), nf90_clobber, &
                                   this%ncfileid)
      else
         call this%logcls%error(msg=msg)
      end if
      if (this%ncstatus /= 0) call ncerror(nccls=this)
   end subroutine ncopen

   !> @brief: Reads dimension variable value.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: dimname
  !!
  !!    - The netCDF dimension variable name.
  !!
  !! @returns: dimval
  !!
  !!    - The netCDF dimension variable value.
   subroutine ncreaddim(nccls, dimname, dimval)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: dimname
      character(len=500) :: msg
      integer, intent(out) :: dimval

      nccls%ncstatus = nf90_inq_dimid(nccls%ncfileid, trim(adjustl(dimname)), &
                                      nccls%ncdimid)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
      nccls%ncstatus = nf90_inquire_dimension(nccls%ncfileid, nccls%ncdimid, &
                                              len=dimval)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
      write (msg, 500) trim(adjustl(dimname)), nccls%ncdimid
      call nccls%logcls%info(msg=msg)
500   format("netCDF dimension", 1x, a, 1x, "has ID", 1x, i3, 1x, ".")
   end subroutine ncreaddim

   !> @brief: Returns the number of dimensions for the specified
  !!         variable `varname`.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns: ndims
  !!
  !!    - The number of dimensions for the specified variable
  !!      `varname`.
   subroutine ncvardims(nccls, varname, ndims)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      integer :: ndims

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_inquire_variable(nccls%ncfileid, nccls%ncvarid, &
                                             ndims=ndims)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine ncvardims

   !> @brief: Defines the integer variable identification key within an
  !!         open netCDF-formatted file path.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
   subroutine ncvarid(nccls, varname)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      character(len=500) :: msg

      nccls%ncstatus = nf90_inq_varid(nccls%ncfileid, trim(adjustl(varname)), &
                                      nccls%ncvarid)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
      write (msg, 500) trim(adjustl(varname)), nccls%ncvarid
      call nccls%logcls%info(msg=msg)
500   format("netCDF variable", 1x, a, 1x, "has ID", 1x, i3, 1x, ".")
   end subroutine ncvarid

   !

   !> @brief: Read a double-precision 1-dimensional variable array.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns[out]: varrar
  !!
  !!    - The netCDF variable 1-dimensional array values.
   subroutine read_arr1d_double(nccls, varname, vararr)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      real(rdouble), dimension(:), intent(out) :: vararr

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr1d_double

   !> @brief: Read a single-precision 1-dimensional variable array.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns: varrar
  !!
  !!    - The netCDF variable 1-dimensional array values.
   subroutine read_arr1d_single(nccls, varname, vararr)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      real(rsingle), dimension(:), intent(out) :: vararr

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr1d_single

   !> @brief: Read a double-precision 2-dimensional variable array.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns: varrar
  !!
  !!    - The netCDF variable 2-dimensional array values.
   subroutine read_arr2d_double(nccls, varname, vararr)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      real(rdouble), dimension(:, :), intent(out) :: vararr

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr2d_double

   !> @brief: Read a single-precision 2-dimensional variable array.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns: varrar
  !!
  !!    - The netCDF variable 2-dimensional array values.
   subroutine read_arr2d_single(nccls, varname, vararr)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      real(rsingle), dimension(:, :), intent(out) :: vararr

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr2d_single

   !> @brief: Read a double-precision 3-dimensional variable array.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns: varrar
  !!
  !!    - The netCDF variable 3-dimensional array values.
   subroutine read_arr3d_double(nccls, varname, vararr)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      real(rdouble), dimension(:, :, :), intent(out) :: vararr

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr3d_double

   !> @brief: Read a single-precision 3-dimensional variable array.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns: varrar
  !!
  !!    - The netCDF variable 3-dimensional array values.
   subroutine read_arr3d_single(nccls, varname, vararr)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      real(rsingle), dimension(:, :, :), intent(out) :: vararr

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr3d_single

   !> @brief: Read a double-precision scalar variable.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns: varrar
  !!
  !!    - The netCDF variable value(s).
   subroutine read_scalar_double(nccls, varname, vararr)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      real(rdouble), intent(out) :: vararr

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_scalar_double

   !> @brief: Read a single-precision scalar variable.
  !!
  !! @params[inout]: nccls
  !!
  !!    - The netCDF object.
  !!
  !! @params[in]: varname
  !!
  !!    - The netCDF variable name.
  !!
  !! @returns: varrar
  !!
  !!    - The netCDF variable value(s).
   subroutine read_scalar_single(nccls, varname, vararr)
      class(NCDATA), intent(inout) :: nccls
      character(len=maxchar) :: varname
      real(rsingle), intent(out) :: vararr

      call ncvarid(nccls=nccls, varname=varname)
      nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
      if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_scalar_single

end module ftnutils_netcdf
