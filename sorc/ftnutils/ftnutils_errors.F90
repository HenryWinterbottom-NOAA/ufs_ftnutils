! > @file sorc/ftnutils/ftnutils_errors.F90
! ! @details This module contains the base-class object for all
! !          exception classes.
! ! @author Henry R. Winterbottom
! ! @date 02 July 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
module ftnutils_errors
    use ftnutils_kinds, only: maxchar
    use ftnutils_log, only: logger
    implicit none
    private

    ! > @brief The Error base-class object.
    type, public :: Error
    type(logger) :: logobj
contains
    procedure, public :: raise
    end type Error
contains

    ! > @brief Raises the respective exception.
    ! ! @param msg
    ! !    - The string to accompany the respective exception.
    subroutine raise(this, msg)
        class(error), intent(in) :: this
        character(len=maxchar) :: msg

        call this%logobj%error(msg = msg)
        stop
    end subroutine raise
end module ftnutils_errors
