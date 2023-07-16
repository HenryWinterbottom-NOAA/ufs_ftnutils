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

!> @file: ftnutils_log.F90
!! @details: This module contains the Logger object and associated
!!           methods and functions.
!! @author: Henry R. Winterbottom
!! @date: 02 July 2023
!! @version: 0.0.1
!! @license: LGPL v2.1
module ftnutils_log
   use ftnutils_kinds, only: maxchar
   implicit none
   private

   !> @brief: The Logger base-class log information object.
  !! @param: msg : The message to be passed to the object for the
  !!               logger API; this string has not yet been formatted
  !!               for date and timestamp and (optional) colorization.
  !! @param: msgstr : The formatted message string to be passed to the
  !!                  respective logger instance.
  !! @param: type : The logger type; available types are `error`,
  !!                `info`, `status`, and `warn`.
   type, private :: LoggerInfo
      character(len=500) :: msg
      character(len=500) :: msgstr
      character(len=10) :: type
   end type LoggerInfo

   !> @brief: The Logger base-class object.
   type, public :: Logger
   contains
      procedure, private :: build_msgstr
      procedure, private :: datetime
      procedure, private :: write_log
      procedure, public :: debug
      procedure, public :: error
      procedure, public :: info
      procedure, public :: status
      procedure, public :: warn
   end type Logger
contains

   !> @brief: Builds the logger object message string including the
  !!         message type (`type`) and the current timestamp string
  !!         formatted as `%Y-%m-%d %H:%M:%S` assuming the POSIX
  !!         convention.
  !! @param[in]: msg: The message passed to the logger object.
  !! @param[in]: type: The message type (e.g., `INFO`, `WARNING`,
  !!             etc.,).
  !! @returns: msgstr: The formatted message string.
   function build_msgstr(this, type, msg) result(msgstr)
      class(Logger) :: this
      character(len=500), intent(in) :: msg
      character(len=10), intent(in) :: type
      character(len=500) :: msgstr
      character(len=19) :: datestr

      datestr = this%datetime()
      write (msgstr, 500) trim(adjustl(datestr)), trim(adjustl(type)), &
         trim(adjustl(msg))
500   format(a19, " :: ", a, " :: ", a)
   end function build_msgstr

   !> @brief: Defines a formatted date and timestamp string; the current
  !!         timestamp string formatted as `%Y-%m-%d %H:%M:%S`
  !!         assuming the POSIX convention; note the timezone defaults
  !!         to the timezone for the respective host platform but may
  !!         be explicitly specified by defining `TZ` in the run-time
  !!         environment.
  !! @returns: datestr: The formatted date and timestamp string.
   function datetime(this) result(datestr)
      class(Logger) :: this
      character(len=19) :: datestr
      character(len=5) :: zone
      integer, dimension(8) :: values

      call date_and_time(VALUES=values, ZONE=zone)
      write (datestr, 500) values(1), values(2), values(3), values(5), &
         values(6), values(7)
500   format(i4.4, "-", i2.2, "-", i2.2, 1x, i2.2, ":", i2.2, ":", i2.2)
   end function datetime

   !! # TODO: Add `debug` logger messages; an environment variable
   !! # should be set at the application driver level.

   !> @brief: Writes `DEGUG` type logger messages.
  !! @param[in]: msg : A string to accompany the logger message.
   subroutine debug(this, msg)
      class(Logger) :: this
      type(LoggerInfo) :: loginfo
      character(len=500) :: msg
      character(len=256), parameter :: envvar = "DEBUG"
      character(len=256) :: envval
      logical :: env_debug
      integer :: status

      !! # TODO: Bug is here; see ChatGPT solution.
      call GET_ENVIRONMENT_VARIABLE(envvar, envval)
      if (envval(1:1) == "T") then
         env_debug = .true.
      else if (envval(1:1) == "t") then
         env_debug = .true.
      else
         env_debug = .false.
      end if
      loginfo%type = "DEBUG"
      loginfo%msg = this%build_msgstr(type=loginfo%type, msg=msg)
      write (loginfo%msgstr, *) achar(27)//"[0;32m "//trim(adjustl(loginfo%msg))//achar(27)//"[0m"
      if (env_debug) call this%write_log(loginfo=loginfo)
   end subroutine debug
   
   !> @brief: Writes `ERROR` type logger messages.
  !! @param[in]: msg : A string to accompany the logger message.
   subroutine error(this, msg)
      class(Logger) :: this
      type(LoggerInfo) :: loginfo
      character(len=500) :: msg

      loginfo%type = "ERROR"
      loginfo%msg = this%build_msgstr(type=loginfo%type, msg=msg)
      write (loginfo%msgstr, *) achar(27)//"[48;5;196m "//trim(adjustl(loginfo%msg))//achar(27)//"[0m"
      call this%write_log(loginfo=loginfo)
   end subroutine error

   !> @brief: Writes `INFO` type logger messages.
  !! @param[in]: msg : A string to accompany the logger message.
   subroutine info(this, msg)
      class(Logger) :: this
      type(LoggerInfo) :: loginfo
      character(len=500) :: msg

      loginfo%type = "INFO"
      loginfo%msg = this%build_msgstr(type=loginfo%type, msg=msg)
      write (loginfo%msgstr, *) achar(27)//"[38;5;247m "//trim(adjustl(loginfo%msg))//achar(27)//"[0m"
      call this%write_log(loginfo=loginfo)
   end subroutine info

   !> @brief: Writes `STATUS` type logger messages.
  !! @param[in]: msg : A string to accompany the logger message.
   subroutine status(this, msg)
      class(Logger) :: this
      type(LoggerInfo) :: loginfo
      character(len=500) :: msg

      loginfo%type = "STATUS"
      loginfo%msg = this%build_msgstr(type=loginfo%type, msg=msg)
      write (loginfo%msgstr, *) achar(27)//"[0;36m "//trim(adjustl(loginfo%msg))//achar(27)//"[0m"
      call this%write_log(loginfo=loginfo)
   end subroutine status

   !> @brief: Writes `WARNING` type logger messages.
  !! @param[in]: msg : A string to accompany the logger message.
   subroutine warn(this, msg)
      class(Logger) :: this
      type(LoggerInfo) :: loginfo
      character(len=500) :: msg

      loginfo%type = "WARNING"
      loginfo%msg = this%build_msgstr(type=loginfo%type, msg=msg)
      write (loginfo%msgstr, *) achar(27)//"[0;93m "//trim(adjustl(loginfo%msg))//achar(27)//"[0m"
      call this%write_log(loginfo=loginfo)
   end subroutine warn

   !> @brief: Writes the logger message, and it's associated
  !!          attributes, to `stdout`.
  !! @params[in]: loginfo : A LoggerInfo object containing the logger
  !!                        message and associated attributes.
   subroutine write_log(this, loginfo)
      class(Logger) :: this
      class(LoggerInfo), intent(in) :: loginfo

      write (6, 500) trim(adjustl(loginfo%msgstr))
500   format(a)
   end subroutine write_log
end module ftnutils_log
