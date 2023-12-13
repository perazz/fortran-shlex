! *************************************************************************************************
!                                    ____  ___________________  __
!                                   / __ \/ ____/ ____/ ____/ |/ /
!                                  / /_/ / __/ / / __/ __/  |   /
!                                 / _, _/ /___/ /_/ / /___ /   |
!                                /_/ |_/_____/\____/_____//_/|_|
!
! MIT License
!
! (C) Federico Perini, 2023
!     A Fortran port of the Python standard library shlex module.
!
!     https://github.com/kokke/tiny-regex-c
!     Code inspired by Rob Pike's regex code described in:
!     http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html
!
! *************************************************************************************************
module shlex_module
    use iso_fortran_env, only: output_unit
    implicit none
    private

    integer, parameter, public :: SCK = selected_char_kind("ascii")

    ! Shlex: return tokens
    public :: shlex
    ! Split: return split strings
    public :: split

    ! Turn on verbosity for debugging
    logical, parameter :: DEBUG = .false.

    ! Character types
    integer, parameter :: CHAR_UNKNOWN           = 0
    integer, parameter :: CHAR_SPACE             = 1
    integer, parameter :: CHAR_ESCAPING_QUOTE    = 2
    integer, parameter :: CHAR_NONESCAPING_QUOTE = 3
    integer, parameter :: CHAR_ESCAPE            = 4
    integer, parameter :: CHAR_COMMENT           = 5
    integer, parameter :: CHAR_EOF               = 6

    ! Error types
    integer, parameter :: NO_ERROR               = 0
    integer, parameter :: SYNTAX_ERROR           = 1
    integer, parameter :: EOF_ERROR              = 2

    character(kind=SCK), parameter, public :: NEWLINE    = achar(10,kind=SCK)  ! \n or line feed
    character(kind=SCK), parameter, public :: TAB        = achar( 9,kind=SCK)  ! \t or tabulation character
    character(kind=SCK), parameter, public :: CARRIAGE   = achar(13,kind=SCK)  ! \t or tabulation character

    integer, parameter :: MAX_CHAR_CLASS_LEN = 1024

    ! Character type sets
    character(kind=SCK,len=*), parameter :: SPACE_CHARS = " "//NEWLINE//TAB//CARRIAGE
    character(kind=SCK,len=*), parameter :: ESCAPING_QUOTE_CHARS = '"'
    character(kind=SCK,len=*), parameter :: NONESCAPING_QUOTE_CHARS = "'"
    character(kind=SCK,len=*), parameter :: ESCAPE_CHARS = "\"
    character(kind=SCK,len=*), parameter :: COMMENT_CHARS = "#"

    ! Token types
    integer, parameter :: TOKEN_UNKNOWN = 0
    integer, parameter :: TOKEN_WORD    = 1
    integer, parameter :: TOKEN_SPACE   = 2
    integer, parameter :: TOKEN_COMMENT = 3

    ! Lexer state
    integer, parameter :: STATE_START            = 0 ! No characters read yet
    integer, parameter :: STATE_INWORD           = 1 ! Processing characters in a word
    integer, parameter :: STATE_ESCAPING         = 2 ! Just found an escape character: next has to be literal
    integer, parameter :: STATE_ESCAPING_QUOTED  = 3 ! Just found an e2cape character within a quoted string
    integer, parameter :: STATE_QUOTING_ESCAPING = 4 ! Within a quoted string that supports escaping ("...")
    integer, parameter :: STATE_QUOTING          = 5 ! Within a quoted string that does not support escaping ('...')
    integer, parameter :: STATE_COMMENT          = 6 ! Within a comment

    type, public :: shlex_token

        integer :: type = TOKEN_UNKNOWN
        character(kind=SCK,len=:), allocatable :: string

    end type shlex_token


    type, public :: shlex_lexer

        ! The input string
        integer :: input_position = 0
        integer :: input_length   = -1

        contains

           procedure :: destroy
           procedure :: new

    end type shlex_lexer

    contains

    elemental subroutine destroy_token(this)
       class(shlex_token), intent(inout) :: this
       this%type = TOKEN_UNKNOWN
       if (allocated(this%string)) deallocate(this%string)
    end subroutine destroy_token

    elemental type(shlex_token) function new_token(type,string) result(token)
       integer, intent(in) :: type
       character(kind=SCK,len=*), intent(in) :: string
       call destroy_token(token)
       token%type = type
       token%string = string
    end function new_token

    ! Return
    elemental integer function CHAR_TYPE(c)
       character(kind=SCK), intent(in) :: c

       if (scan(c,SPACE_CHARS)>0) then
          CHAR_TYPE = CHAR_SPACE
       elseif (scan(c,ESCAPING_QUOTE_CHARS)>0) then
          CHAR_TYPE = CHAR_ESCAPING_QUOTE
       elseif (scan(c,NONESCAPING_QUOTE_CHARS)>0) then
          CHAR_TYPE = CHAR_NONESCAPING_QUOTE
       elseif (scan(c,ESCAPE_CHARS)>0) then
          CHAR_TYPE = CHAR_ESCAPE
       elseif (scan(c,COMMENT_CHARS)>0) then
          CHAR_TYPE = CHAR_COMMENT
       else
          CHAR_TYPE = CHAR_UNKNOWN
       end if

    end function CHAR_TYPE

    ! High level interface: return a list of strings
    function split(pattern,success) result(list)
        character(*),      intent(in)  :: pattern
        logical, optional, intent(out) :: success
        character(kind=SCK,len=:), allocatable :: list(:)

        type(shlex_token), allocatable :: tokens(:)

        integer :: n,maxlen,i,l

        tokens = shlex(pattern,success)

        n      = size(tokens)
        maxlen = 0
        do i=1,n
           maxlen = max(maxlen,len(tokens(i)%string))
        end do

        allocate(character(kind=SCK,len=maxlen) :: list(n))
        do i=1,n
            list(i) = tokens(i)%string
        end do

    end function split

    ! High level interface: return a list of tokens
    function shlex(pattern,success) result(list)
        character(*),      intent(in)  :: pattern
        logical, optional, intent(out) :: success
        type(shlex_token), allocatable :: list(:)

        type(shlex_lexer) :: s
        type(shlex_token) :: next,error

        ! Initialize lexer
        call s%new(pattern)

        allocate(list(0))
        error = new_token(NO_ERROR,"SUCCESS")
        do while (error%type==NO_ERROR)

            next = scan_stream(s,pattern,error)
            select case (error%type)
               case (EOF_ERROR)
                  ! Finished reading
                  error = new_token(NO_ERROR,"SUCCESS")
                  exit
               case (SYNTAX_ERROR)
                  ! Something happened
                  exit
               case default
                  ! Keep reading
                  list = [list,next]
            end select

        end do

        if (present(success)) success = error%type==NO_ERROR
        return

    end function shlex

    !

    type(shlex_token) function scan_stream(this,pattern,error) result(token)
        class(shlex_lexer), intent(inout) :: this
        character(kind=SCK,len=*), intent(in) :: pattern
        type(shlex_token), intent(out) :: error

        integer :: state,next_type,token_type
        character(kind=SCK) :: next_char
        character(kind=SCK,len=:), allocatable :: value

        state      = STATE_START
        token_type = TOKEN_UNKNOWN
        allocate(character(kind=SCK,len=0) :: value)

        read_chars: do

           ! Get next character
           this%input_position = this%input_position + 1
           if (this%input_position<=this%input_length) then
              if (len(pattern)>=this%input_position) then
                 next_char = pattern(this%input_position:this%input_position)
                 next_type = CHAR_TYPE(next_char)
                 error     = new_token(NO_ERROR,"SUCCESS")
              else
                 ! Should never happen
                 call destroy_token(token)
                 error = new_token(SYNTAX_ERROR,"END-OF-RECORD reading pattern")
                 return
              endif
           else
              next_char = ""
              next_type = CHAR_EOF
              error = new_token(NO_ERROR,"SUCCESS")
           end if

           select case (state)

              ! No characters read yet
              case (STATE_START)

                   select case (next_type)
                      case (CHAR_EOF)
                         call destroy_token(token)
                         error = new_token(EOF_ERROR,"END-OF-FILE encountered")
                         return
                      case (CHAR_SPACE)
                         ! do nothing
                      case (CHAR_ESCAPING_QUOTE)
                         token_type = TOKEN_WORD
                         state      = STATE_QUOTING_ESCAPING
                      case (CHAR_NONESCAPING_QUOTE)
                         token_type = TOKEN_WORD
                         state      = STATE_QUOTING
                      case (CHAR_ESCAPE)
                         token_type = TOKEN_WORD
                         state      = STATE_ESCAPING
                      case (CHAR_COMMENT)
                         token_type = TOKEN_COMMENT
                         state      = STATE_COMMENT
                      case default
                         token_type = TOKEN_WORD
                         state      = STATE_INWORD
                         value      = value//next_char
                   end select

              ! Into a regular word
              case (STATE_INWORD)

                   select case (next_type)
                      case (CHAR_EOF, CHAR_SPACE)
                         token = new_token(token_type,value)
                         return
                      case (CHAR_ESCAPING_QUOTE)
                         state = STATE_QUOTING_ESCAPING
                      case (CHAR_NONESCAPING_QUOTE)
                         state = STATE_QUOTING
                      case (CHAR_ESCAPE)
                         state = STATE_ESCAPING
                      case default
                         value = value//next_char
                   end select

              ! After an escape character
              case (STATE_ESCAPING)

                   select case (next_type)
                      case (CHAR_EOF)
                         ! Error: EOF after an escape character
                         error = new_token(SYNTAX_ERROR,"END-OF-FILE after an escape character")
                         token = new_token(token_type,value)
                         return
                      case default
                         state = STATE_INWORD
                         value = value//next_char
                   end select

              ! Inside escaping double quotes
              case (STATE_ESCAPING_QUOTED)

                   select case (next_type)
                      case (CHAR_EOF)
                         ! Error: EOF when expecting closing quote
                         error = new_token(SYNTAX_ERROR,"END-OF-FILE when expecting escaped closing quote")
                         token = new_token(token_type,value)
                         return
                      case default
                         state = STATE_QUOTING_ESCAPING
                         value = value//next_char
                   end select

              ! Inside escaping double quotes
              case (STATE_QUOTING_ESCAPING)

                   select case (next_type)
                      case (CHAR_EOF)
                         ! Error: EOF when expecting closing quote
                         error = new_token(SYNTAX_ERROR,"END-OF-FILE when expecting closing quote")
                         token = new_token(token_type,value)
                         return
                      case (CHAR_ESCAPING_QUOTE)
                         state = STATE_INWORD
                      case (CHAR_ESCAPE)
                         state = STATE_ESCAPING_QUOTED
                      case default
                         value = value//next_char
                   end select

              ! Inside non-escaping single quotes
              case (STATE_QUOTING)

                   select case (next_type)
                      case (CHAR_EOF)
                         ! Error: EOF when expecting closing quote
                         error = new_token(SYNTAX_ERROR,"END-OF-FILE when expecting closing quote")
                         token = new_token(token_type,value)
                         return
                      case (CHAR_NONESCAPING_QUOTE)
                         state = STATE_INWORD
                      case default
                         value = value//next_char
                   end select

              ! Inside a comment string
              case (STATE_COMMENT)

                   select case (next_type)
                      case (CHAR_EOF)
                         token = new_token(token_type,value)
                         return
                      case (CHAR_SPACE)

                         if (next_char==NEWLINE) then
                            state = STATE_START
                            token = new_token(token_type,value)
                            return
                         else
                            value = value//next_char
                         end if
                      case default
                         value = value//next_char
                   end select

              ! Invalid state
              case default
                 print *, 'STATE=',state
                 error = new_token(SYNTAX_ERROR,"Internal error: invalid state at [["//pattern(1:this%input_position)//']]')
                 call destroy_token(token)
                 return
           end select

        end do read_chars

    end function scan_stream

    ! Cleanup
    elemental subroutine destroy(this)
       class(shlex_lexer), intent(inout) :: this

       this%input_length   = -1
       this%input_position = 0

    end subroutine destroy

    ! Initialize lexer
    pure subroutine new(this,pattern)
       class(shlex_lexer), intent(inout) :: this
       character(kind=SCK, len=*), intent(in) :: pattern

       call this%destroy()

       this%input_position = 0
       this%input_length = len(pattern)

    end subroutine new

    function print_token(token) result(msg)
        class(shlex_token), intent(in) :: token
        character(:,kind=SCK), allocatable :: msg

        character(len=MAX_CHAR_CLASS_LEN,kind=SCK) :: buffer
        integer :: lt

        write(buffer,1) token%type,trim(token%string)

        lt = len_trim(buffer)
        allocate(character(len=lt,kind=SCK) :: msg)
        if (lt>0) msg(1:lt) = buffer(1:lt)

        1 format('type=',i0,:,1x,'char=',a)

    end function print_token

end module shlex_module

