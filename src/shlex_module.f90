! *************************************************************************************************
!                                   _____ __  ____    _______  __
!                                  / ___// / / / /   / ____/ |/ /
!                                  \__ \/ /_/ / /   / __/  |   /
!                                 ___/ / __  / /___/ /___ /   |
!                                /____/_/ /_/_____/_____//_/|_|
!
! MIT License
!
! (C) Federico Perini, 2023
!     A Fortran port of the Python standard library shlex module.
!
!     https://github.com/perazz/fortran-shlex
!
! *************************************************************************************************
module shlex_module
    use iso_fortran_env, only: output_unit
    implicit none
    private

    integer, parameter, public :: SCK = selected_char_kind("ascii")

    ! Shlex: return tokens
    public :: shlex
    interface shlex
        module procedure shlex_bool
        module procedure shlex_error        
    end interface

    ! Split: return split strings
    public :: split
    interface split
        module procedure split_bool
        module procedure split_error
        module procedure split_joined_bool
        module procedure split_joined_error
    end interface

    ! Turn on verbosity for debugging
    logical, parameter :: DEBUG = .true.

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
    integer, parameter :: TOKEN_UNKNOWN         = 0
    integer, parameter :: TOKEN_WORD            = 1    
    integer, parameter :: TOKEN_SPACE           = 2
    integer, parameter :: TOKEN_COMMENT         = 3
    integer, parameter :: TOKEN_QUOTED_WORD     = 4 ! Words in non-escaping quotes
    integer, parameter :: TOKEN_ESC_QUOTED_WORD = 5 ! Words in escaping quotes
    
    ! Lexer types
    integer, parameter :: LEXER_POSIX           = 0 ! Lexer for posix shells 
    integer, parameter :: LEXER_WINDOWS         = 1 ! Lexer for Windows shells
 
    ! Lexer state
    integer, parameter :: STATE_START            = 0 ! No characters read yet
    integer, parameter :: STATE_INWORD           = 1 ! Processing characters in a word
    integer, parameter :: STATE_ESCAPING         = 2 ! Just found an escape character: next has to be literal
    integer, parameter :: STATE_ESCAPING_QUOTED  = 3 ! Just found an escape character within a quoted string
    integer, parameter :: STATE_QUOTING_ESCAPING = 4 ! Within a quoted string that supports escaping ("...")
    integer, parameter :: STATE_QUOTING          = 5 ! Within a quoted string that does not support escaping ('...')
    integer, parameter :: STATE_COMMENT          = 6 ! Within a comment

    type, public :: shlex_token

        integer :: type = TOKEN_UNKNOWN
        character(kind=SCK,len=:), allocatable :: string
        
        contains
        
           procedure :: print => print_token

    end type shlex_token


    type, public :: shlex_lexer

        ! The input string
        integer :: input_position = 0
        integer :: input_length   = -1
        
        ! Settings
        integer :: lexer       = LEXER_POSIX
        logical :: keep_quotes = .false.

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

    ! High level interface: return a list of strings, with error type
    function split_bool(pattern,success) result(list)
        character(*),      intent(in)  :: pattern
        logical, optional, intent(out) :: success
        character(kind=SCK,len=:), allocatable :: list(:)
        type(shlex_token) :: error

        list = split_error(pattern,error)
        if (present(success)) success = error%type==NO_ERROR

    end function split_bool

    ! High level interface: return a list of strings
    function split_error(pattern,error) result(list)
        character(*),      intent(in)  :: pattern
        type(shlex_token), intent(out) :: error
        character(kind=SCK,len=:), allocatable :: list(:)

        type(shlex_token), allocatable :: tokens(:)

        tokens = shlex(pattern,error)
        call tokens_to_strings(tokens,list)

    end function split_error
    
    ! Convert a list of tokens to strings
    pure subroutine tokens_to_strings(tokens,list)
        type(shlex_token), optional, intent(in) :: tokens(:)
        character(kind=SCK,len=:), allocatable, intent(out) :: list(:)
        
        integer :: n,maxlen,i
        
        n      = size(tokens)
        maxlen = 0
        
        do i=1,n
           maxlen = max(maxlen,len(tokens(i)%string))
        end do

        allocate(character(kind=SCK,len=maxlen) :: list(n))
        do i=1,n
            list(i) = tokens(i)%print()
        end do        
        
    end subroutine tokens_to_strings

    ! High level interface: also join spaced flags like -I /path -> -I/path
    function split_joined_bool(pattern, join_spaced, keep_quotes, success) result(list)
        character(*),      intent(in)    :: pattern
        logical,           intent(in)    :: join_spaced
        logical,  optional, intent(in)   :: keep_quotes
        logical,           intent(out)   :: success
        character(kind=SCK,len=:), allocatable :: list(:)
        type(shlex_token) :: error

        list = split_joined_error(pattern, join_spaced, keep_quotes, error)
        success = error%type==NO_ERROR        
        
    end function split_joined_bool

    ! High level interface: also join spaced flags like -I /path -> -I/path
    function split_joined_error(pattern, join_spaced, keep_quotes, error) result(list)
        character(*),      intent(in)  :: pattern
        type(shlex_token), intent(out) :: error
        logical,           intent(in)  :: join_spaced
        logical, optional, intent(in)  :: keep_quotes
        character(kind=SCK,len=:), allocatable :: list(:)

        integer :: i, n, count
        type(shlex_token) :: tok, next_tok
        type(shlex_token), allocatable :: raw(:),joined(:)

        raw = shlex(pattern,error,keep_quotes)

        if (error%type/=NO_ERROR .or. .not. join_spaced) then
            
            call tokens_to_strings(raw,list)
            
        else
            
            n = size(raw)
            
            allocate(joined(n))
            count = 0
            i = 1

            old_tokens: do while (i <= n)
                tok = raw(i)
                
                if (len_trim(tok%string)==2) then 
                    
                    if (tok%string(1:1) == '-' .and. &
                       (tok%string(2:2) >= 'A' .and. tok%string(2:2) <= 'Z' .or. &
                        tok%string(2:2) >= 'a' .and. tok%string(2:2) <= 'z')) then
                        if (i + 1 <= n) then
                            next_tok = raw(i + 1)
                            if (.not. (len_trim(next_tok%string) >= 1 .and. next_tok%string(1:1) == '-')) then
                                count = count + 1
                                joined(count) = join_tokens(tok,next_tok)
                                i = i + 2
                                cycle old_tokens
                            end if
                        end if   
                    endif                    
                    
                end if

                count = count + 1
                joined(count) = tok
                i = i + 1
            end do old_tokens
            
            call tokens_to_strings(joined(:count),list)
        end if

    end function split_joined_error
    
    ! Join two tokens
    elemental type(shlex_token) function join_tokens(a,b) result(join)
        type(shlex_token), intent(in) :: a,b
        
        if (any(a%type==[TOKEN_UNKNOWN,TOKEN_SPACE,TOKEN_COMMENT])) then 
            join = b
        elseif (any(b%type==[TOKEN_UNKNOWN,TOKEN_SPACE,TOKEN_COMMENT])) then 
            join = a
        else
            ! The quoting is certainly lost
            join%string = a%print()//b%print()
            join%type   = max(a%type,b%type)
        end if
        
    end function join_tokens

    ! High level interface: return a list of tokens
    function shlex_bool(pattern,success,keep_quotes) result(list)
        character(*),      intent(in)  :: pattern
        logical, optional, intent(out) :: success
        logical, optional, intent(in)  :: keep_quotes
        type(shlex_token), allocatable :: list(:)
        type(shlex_token) :: error

        list = shlex_error(pattern,error,keep_quotes)
        if (present(success)) success = error%type==NO_ERROR
    end function shlex_bool

    ! High level interface: return a list of tokens
    function shlex_error(pattern,error,keep_quotes) result(list)
        character(*),      intent(in)  :: pattern
        type(shlex_token), intent(out) :: error
        logical, optional, intent(in)  :: keep_quotes
        type(shlex_token), allocatable :: list(:)

        type(shlex_lexer) :: s
        type(shlex_token) :: next

        ! Initialize lexer
        call s%new(LEXER_POSIX,pattern,keep_quotes)

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

        return

    end function shlex_error

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
                         token_type = TOKEN_ESC_QUOTED_WORD
                         state      = STATE_QUOTING_ESCAPING
                         if (this%keep_quotes) value = value//next_char
                      case (CHAR_NONESCAPING_QUOTE)
                         token_type = TOKEN_QUOTED_WORD
                         state      = STATE_QUOTING
                         if (this%keep_quotes) value = value//next_char
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
                         if (this%keep_quotes) value = value//next_char
                      case (CHAR_NONESCAPING_QUOTE)
                         state = STATE_QUOTING
                         if (this%keep_quotes) value = value//next_char
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
                         ! go back to quoting excping
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
                         if (this%keep_quotes) value = value//next_char
                      case (CHAR_ESCAPE)
                         state = STATE_ESCAPING_QUOTED
                         if (this%keep_quotes) value = value//next_char
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
                         if (this%keep_quotes) value = value//next_char
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
       this%lexer          = LEXER_POSIX
       this%keep_quotes    = .false.

    end subroutine destroy

    ! Initialize lexer
    pure subroutine new(this,lexer,pattern,keep_quotes)
       class(shlex_lexer), intent(inout) :: this
       integer, intent(in) :: lexer
       character(kind=SCK, len=*), intent(in) :: pattern
       logical, optional, intent(in) :: keep_quotes
 
       call this%destroy()
       
       this%lexer = lexer       
       this%input_length = len(pattern)
       if (present(keep_quotes)) this%keep_quotes = keep_quotes

    end subroutine new

    pure function print_token(token) result(msg)
        class(shlex_token), intent(in) :: token
        character(:,kind=SCK), allocatable :: msg

        msg = trim(token%string)
        
    end function print_token

end module shlex_module

