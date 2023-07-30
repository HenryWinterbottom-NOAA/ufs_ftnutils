! > @file sorc/ftnutils/ftnutils_log.F90
! ! @details This module contains the Logger object and associated
! !          methods and functions.
! ! @author Henry R. Winterbottom
! ! @date 02 July 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
module ftnutils_log
    use ftnutils_kinds, only: ilong, maxchar
    implicit none
    private

    ! > @brief The Logger base-class log information object.
    ! ! @params[in] msg
    ! !    - The message to be passed to the object for the logger API;
    ! !      this string has not yet been formatted for date and
    ! !      timestamp and (optional) colorization.
    ! !
    ! ! @param[in] type
    ! !     - The logger type; available types are `debug`, `error`,
    ! !       `info`, `status`, and `warn`.
    ! !
    ! ! @returns msgstr
    ! !    - The formatted message string to be passed to the respective
    ! !      logger instance.
    ! !
    ! ! @details The `debug` type messages will only be printed if the
    ! !          environment variable `DEBUG` is set to `true` of
    ! !          `True`.
    ! !
    ! ! @details The timezone if determined from the local platform.
    type, private :: LoggerInfo
    character(len=maxchar) :: msg
    character(len=maxchar) :: msgstr
    character(len=10) :: type
    end type LoggerInfo

    ! > @brief The Logger base-class object.
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

    ! > @brief Builds the logger object message string including the
    ! !         message type (`type`) and the current timestamp string
    ! !         formatted as `%Y-%m-%d %H:%M:%S` assuming the POSIX
    ! !         convention.
    ! !
    ! ! @param[in] msg
    ! !    - The message passed to the logger object.
    ! !
    ! ! @param[in] type
    ! !    - The message type (e.g., `INFO`, `WARNING`, etc.,).
    ! !
    ! ! @returns msgstr
    ! !    - The formatted message string.
    function build_msgstr(this, type, msg) result(msgstr)
        class(logger) :: this
        character(len=maxchar), intent(in) :: msg
        character(len=10), intent(in) :: type
        character(len=maxchar) :: msgstr
        character(len=19) :: datestr

        datestr = this%datetime()
        write (msgstr, 500) trim(adjustl(datestr)), trim(adjustl(type)), &
            trim(adjustl(msg))
500     format(a19, " :: ", a, " :: ", a)
    end function build_msgstr

    ! > @brief Defines a formatted date and timestamp string; the current
    ! !         timestamp string formatted as `%Y-%m-%d %H:%M:%S`
    ! !         assuming the POSIX convention.
    ! !
    ! ! @returns datestr The formatted date and timestamp string.
    function datetime(this) result(datestr)
        class(logger) :: this
        character(len=19) :: datestr
        character(len=5) :: zone
        integer(ilong), dimension(8) :: values

        call date_and_time(VALUES = values, ZONE = zone)
        write (datestr, 500) values(1), values(2), values(3), values(5), &
            values(6), values(7)
500     format(i4.4, "-", i2.2, "-", i2.2, 1x, i2.2, ":", i2.2, ":", i2.2)
    end function datetime

    ! > @brief Writes `DEGUG` type logger messages.
    ! !
    ! ! @param[in] msg
    ! !    - A string to accompany the logger message.
    subroutine debug(this, msg)
        class(logger) :: this
        type(loggerinfo) :: loginfo
        character(len=maxchar) :: msg
        character(len=256), parameter :: envvar = "DEBUG"
        character(len=256) :: envval
        logical :: env_debug
        integer(ilong) :: status

        call get_environment_variable(envvar, envval)
        if (envval(1:1) == "T") then
            env_debug = .true.
        else if (envval(1:1) == "t") then
            env_debug = .true.
        else
            env_debug = .false.
        end if
        loginfo%type = "DEBUG"
        loginfo%msg = this%build_msgstr(type = loginfo%type, msg = msg)
        write (loginfo%msgstr, *) achar(27) //"[0;32m "// trim(adjustl(loginfo%msg)) // achar(27) //"[0m"
        if (env_debug) call this%write_log(loginfo = loginfo)
    end subroutine debug

    ! > @brief Writes `ERROR` type logger messages.
    ! !
    ! ! @param[in] msg
    ! !    - A string to accompany the logger message.
    subroutine error(this, msg)
        class(logger) :: this
        type(loggerinfo) :: loginfo
        character(len=maxchar) :: msg

        loginfo%type = "ERROR"
        loginfo%msg = this%build_msgstr(type = loginfo%type, msg = msg)
        write (loginfo%msgstr, *) achar(27) //"[48;5;196m "// trim(adjustl(loginfo%msg)) // achar(27) //"[0m"
        call this%write_log(loginfo = loginfo)
    end subroutine error

    ! > @brief Writes `INFO` type logger messages.
    ! !
    ! ! @param[in] msg
    ! !    - A string to accompany the logger message.
    subroutine info(this, msg)
        class(logger) :: this
        type(loggerinfo) :: loginfo
        character(len=maxchar) :: msg

        loginfo%type = "INFO"
        loginfo%msg = this%build_msgstr(type = loginfo%type, msg = msg)
        write (loginfo%msgstr, *) achar(27) //"[38;5;247m "// trim(adjustl(loginfo%msg)) // achar(27) //"[0m"
        call this%write_log(loginfo = loginfo)
    end subroutine info

    ! > @brief Writes `STATUS` type logger messages.
    ! !
    ! ! @param[in] msg
    ! !    - A string to accompany the logger message.
    subroutine status(this, msg)
        class(logger) :: this
        type(loggerinfo) :: loginfo
        character(len=maxchar) :: msg

        loginfo%type = "STATUS"
        loginfo%msg = this%build_msgstr(type = loginfo%type, msg = msg)
        write (loginfo%msgstr, *) achar(27) //"[0;36m "// trim(adjustl(loginfo%msg)) // achar(27) //"[0m"
        call this%write_log(loginfo = loginfo)
    end subroutine status

    ! > @brief Writes `WARNING` type logger messages.
    ! !
    ! ! @param[in] msg:
    ! !    - A string to accompany the logger message.
    subroutine warn(this, msg)
        class(logger) :: this
        type(loggerinfo) :: loginfo
        character(len=maxchar) :: msg

        loginfo%type = "WARNING"
        loginfo%msg = this%build_msgstr(type = loginfo%type, msg = msg)
        write (loginfo%msgstr, *) achar(27) //"[0;93m "// trim(adjustl(loginfo%msg)) // achar(27) //"[0m"
        call this%write_log(loginfo = loginfo)
    end subroutine warn

    ! > @brief: Writes the logger message, and it's associated
    ! !         attributes, to `stdout`.
    ! !
    ! ! @params[in] loginfo
    ! !    - A LoggerInfo object containing the logger message and
    ! !      associated attributes.
    subroutine write_log(this, loginfo)
        class(logger) :: this
        class(loggerinfo), intent(in) :: loginfo

        write (6, 500) trim(adjustl(loginfo%msgstr))
500     format(a)
    end subroutine write_log
end module ftnutils_log
