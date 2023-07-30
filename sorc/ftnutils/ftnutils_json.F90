! > @file ftnutils_json.F90
! ! @details This module contains the base-class object for the JSON
! !          API.
! ! @author Henry R. Winterbottom
! ! @date 05 July 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
module ftnutils_json
    use fson
    use fson_value_m
    use ftnutils_kinds, only: ilong, maxchar
    use ftnutils_log, only: Logger
    implicit none
    private

    ! > @brief Data structure containing attributes collected, or to be
    ! !        collected, from a JSON formatted file.
    type, private :: json_info_struct
    type(fson_value), pointer :: json_file
    character(len=maxchar) :: key
    character(len=maxchar) :: filename
    integer :: nrecs
    end type json_info_struct

    ! > @brief The JSON base-class object.
    type, public :: json
    type(logger) :: logcls
    type(json_info_struct) :: info
contains
    procedure, public :: init
    procedure, public :: read_int
    procedure, public :: read_str
    end type json
contains

    ! > @brief Initializes the JSON file object and determines the total
    ! !        number of JSON records.
    subroutine init(this)
        class(json), intent(inout) :: this
        character(len=maxchar) :: msg

        this%info%json_file => fson_parse(trim(adjustl(this%info%filename)))
        this%info%nrecs = fson_value_count(this%info%json_file)
        write (msg, 500) this%info%nrecs, trim(adjustl(this%info%filename))
        call this%logcls%debug(msg = msg)
500     format("Found ", i3, " JSON record(s) in file ", a, ".")
    end subroutine init

    ! > @brief Reads a JSON-formatted file path and returns the
    ! !        contents.
    ! !
    ! ! @returns attrval
    ! !    - The integer type value for the respective JSON attribute key
    ! !      `attrkey`.
    function read_int(this) result(attrval)
        class(json), intent(inout) :: this
        character(len=maxchar) :: msg
        integer(ilong) :: attrval

        call fson_get(this%info%json_file, this%info%key, attrval)
        write (msg, 500) trim(adjustl(this%info%filename)), &
            trim(adjustl(this%info%key)), attrval
        call this%logcls%debug(msg = msg)
500     format("JSON-formatted file path ", a, " key ", a, " value is ", i10, ".")
    end function read_int

    ! > @brief Reads a JSON formatted file path and returns the
    ! !        contents.
    ! !
    ! ! @returns attrval
    ! !    - The string type value for the respective JSON attribute key
    ! !      `attrkey`.
    function read_str(this) result(attrval)
        class(json), intent(in) :: this
        character(len=maxchar) :: msg
        character(len=maxchar) :: attrval

        call fson_get(this%info%json_file, this%info%key, attrval)
        write (msg, 500) trim(adjustl(this%info%filename)), &
            trim(adjustl(this%info%key)), trim(adjustl(attrval))
        call this%logcls%debug(msg = msg)
500     format("JSON-formatted file path ", a, " key ", a, " value is ", a, ".")
    end function read_str
end module ftnutils_json
