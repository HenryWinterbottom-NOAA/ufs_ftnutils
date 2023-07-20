!> @file: ftnutils_constants.F90
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
