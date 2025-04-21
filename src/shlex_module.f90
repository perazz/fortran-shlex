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

    ! Shlex: return posix tokens
    public :: shlex
    interface shlex
        module procedure shlex_bool
        module procedure shlex_error        
    end interface
    
    ! Mslex: return MS Windows tokens
    public :: mslex
    interface mslex
        module procedure mslex_bool
        module procedure mslex_error
    end interface

    ! Split: return split strings
    public :: split
    interface split
        module procedure split_bool
        module procedure split_error
        module procedure split_joined_bool
        module procedure split_joined_error
    end interface
    
    public :: ms_split
    interface ms_split
        module procedure mslex_split_bool
        module procedure mslex_split_error
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
    
    character(*), parameter :: CHAR_NAME(0:6) = [character(17) :: 'UNKNOWN', &
                                               'SPACE  ', &
                                               'ESCAPING QUOTE', &
                                               'NONESCAPING QUOTE', &
                                               'ESCAPE', &
                                               'COMMENT', &
                                               'EOF']

    ! Error types
    integer, parameter :: NO_ERROR               = 0
    integer, parameter :: SYNTAX_ERROR           = 1
    integer, parameter :: EOF_ERROR              = 2

    character(kind=SCK), parameter, public :: NULL_CHAR  = achar(0, kind=SCK)
    character(kind=SCK), parameter, public :: NEWLINE    = achar(10,kind=SCK)  ! \n or line feed
    character(kind=SCK), parameter, public :: TAB        = achar( 9,kind=SCK)  ! \t or tabulation character
    character(kind=SCK), parameter, public :: CARRIAGE   = achar(13,kind=SCK)  ! \t or tabulation character

    integer, parameter :: MAX_CHAR_CLASS_LEN = 1024

    ! Character type sets
    character(kind=SCK,len=*), parameter :: SPACE_CHARS   = " "//NEWLINE//TAB//CARRIAGE
    character(kind=SCK,len=*), parameter :: DOUBLE_QUOTE  = '"'
    character(kind=SCK,len=*), parameter :: SINGLE_QUOTE  = "'"
    character(kind=SCK,len=*), parameter :: ESCAPE_CHARS  = "\"
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

    ! Scanning modes
    integer, parameter :: MS_START   = 0
    integer, parameter :: MS_SPACES  = 1
    integer, parameter :: MS_SLASHES = 2
    integer, parameter :: MS_QUOTES  = 3
    integer, parameter :: MS_TEXT    = 4

    type, public :: shlex_token

        integer :: type = TOKEN_UNKNOWN
        character(kind=SCK,len=:), allocatable :: string
        
        contains
        
           procedure :: print => print_token

    end type shlex_token
    
    type, public :: mslex_group
        
        character(kind=SCK,len=:), allocatable :: spaces
        character(kind=SCK,len=:), allocatable :: slashes
        character(kind=SCK,len=:), allocatable :: quotes
        character(kind=SCK,len=:), allocatable :: text
        
    end type mslex_group
    
    type, public :: shlex_lexer
        
        ! The input string
        integer :: input_position = 0
        integer :: input_length   = -1
        
        ! Settings
        integer :: lexer       = LEXER_POSIX
        logical :: keep_quotes = .false.

        contains
        
           procedure, non_overridable :: parse_char

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
    elemental integer function POSIX_CHAR_TYPE(c) result(CHAR_TYPE)
       character(kind=SCK), intent(in) :: c

       if (scan(c,SPACE_CHARS)>0) then
          CHAR_TYPE = CHAR_SPACE
       elseif (scan(c,DOUBLE_QUOTE)>0) then
          CHAR_TYPE = CHAR_ESCAPING_QUOTE
       elseif (scan(c,SINGLE_QUOTE)>0) then
          CHAR_TYPE = CHAR_NONESCAPING_QUOTE
       elseif (scan(c,ESCAPE_CHARS)>0) then
          CHAR_TYPE = CHAR_ESCAPE
       elseif (scan(c,COMMENT_CHARS)>0) then
          CHAR_TYPE = CHAR_COMMENT
       else
          CHAR_TYPE = CHAR_UNKNOWN
       end if

    end function POSIX_CHAR_TYPE

    elemental integer function MS_CHAR_TYPE(c) result(CHAR_TYPE)
        character(kind=SCK), intent(in) :: c

        ! Handle whitespace outside of quotes
        if (scan(c, SPACE_CHARS) > 0) then
            CHAR_TYPE = CHAR_SPACE
        ! Double quote handling
        elseif (scan(c,DOUBLE_QUOTE)>0) then
            CHAR_TYPE = CHAR_NONESCAPING_QUOTE
        else
            CHAR_TYPE = CHAR_UNKNOWN
        end if

    end function MS_CHAR_TYPE


    ! Current char type: 
    elemental subroutine parse_char(lex,pattern,CHAR_TYPE,CHAR_VALUE,error)
       class(shlex_lexer), intent(in)    :: lex
       character(*),       intent(in)    :: pattern
       integer,            intent(out)   :: CHAR_TYPE
       character(kind=SCK),intent(out)   :: CHAR_VALUE
       type(shlex_token),  intent(inout) :: error
       
       associate(pos=>lex%input_position, length=>lex%input_length)
       
       if (pos<=0) then 
        
           CHAR_TYPE  = CHAR_UNKNOWN
           CHAR_VALUE = NULL_CHAR
           
       elseif (pos>length) then  
          
           CHAR_TYPE  = CHAR_EOF
           CHAR_VALUE = NULL_CHAR
          
       else
            
           select case (lex%lexer)
              case (LEXER_POSIX)            
                 CHAR_VALUE = pattern(pos:pos)
                 CHAR_TYPE  = POSIX_CHAR_TYPE(CHAR_VALUE)                
              case (LEXER_WINDOWS)
                 CHAR_VALUE = pattern(pos:pos)
                 CHAR_TYPE  = MS_CHAR_TYPE(CHAR_VALUE)
              case default
                 CHAR_VALUE = NULL_CHAR
                 CHAR_TYPE  = CHAR_UNKNOWN
                 error      = new_token(SYNTAX_ERROR,"INVALID LEXER")
           end select              
        
       end if
       
       endassociate       
    
    end subroutine parse_char
    
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
    
    ! High level interface: return a list of strings, with error type
    function mslex_split_bool(pattern,success) result(list)
        character(*),      intent(in)  :: pattern
        logical, optional, intent(out) :: success
        character(kind=SCK,len=:), allocatable :: list(:)
        type(shlex_token) :: error

        list = mslex_split_error(pattern,error)
        if (present(success)) success = error%type==NO_ERROR

    end function mslex_split_bool

    ! High level interface: return a list of strings
    function mslex_split_error(pattern,error) result(list)
        character(*),      intent(in)  :: pattern
        type(shlex_token), intent(out) :: error
        character(kind=SCK,len=:), allocatable :: list(:)

        type(shlex_token), allocatable :: tokens(:)

        tokens = mslex(pattern,error)
        call tokens_to_strings(tokens,list)

    end function mslex_split_error    
    
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
    function mslex_bool(pattern,success,keep_quotes) result(list)
        character(*),      intent(in)  :: pattern
        logical, optional, intent(out) :: success
        logical, optional, intent(in)  :: keep_quotes
        type(shlex_token), allocatable :: list(:)
        type(shlex_token) :: error

        list = mslex_error(pattern,error,keep_quotes)
        if (present(success)) success = error%type==NO_ERROR
    end function mslex_bool

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


    
    pure integer function n_previous_escapes(this,pattern) result(prev)
        class(shlex_lexer), intent(in) :: this
        character(kind=SCK,len=*), intent(in) :: pattern
        
        integer :: pos
        
        prev = 0
        pos  = this%input_position
        
        do while (pos>1)
            pos = pos-1
            if (scan(pattern(pos:pos),ESCAPE_CHARS)>0) then 
                prev = prev+1
            else
                exit
            end if
        end do
        
    end function n_previous_escapes

    pure integer function n_next_quotes(this,pattern) result(next)
        class(shlex_lexer), intent(in) :: this
        character(kind=SCK,len=*), intent(in) :: pattern
        
        integer :: pos
        
        next = 0
        pos  = this%input_position
        
        do while (pos<this%input_length)
            pos = pos+1
            if (pattern(pos:pos)==DOUBLE_QUOTE) then 
                next = next+1
            else
                exit
            end if
        end do
        
    end function n_next_quotes
    
    ! High level interface: return a list of tokens
    function mslex_error(pattern,error,keep_quotes) result(list)
        character(*),      intent(in)  :: pattern
        type(shlex_token), intent(out) :: error
        logical, optional, intent(in)  :: keep_quotes
        type(shlex_token), allocatable :: list(:)

        type(shlex_lexer) :: s
        type(mslex_group), allocatable :: groups(:)
        type(mslex_group) :: next

        ! Initialize lexer
        call s%new(LEXER_WINDOWS,pattern,keep_quotes)

        allocate(groups(0))
        error = new_token(NO_ERROR,"SUCCESS")
        do while (error%type==NO_ERROR)

            next = scan_stream_msvcrt(s,pattern,error)
            
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
                  groups = [groups,next]
            end select
            
        end do
        
        print *, 'groups ',size(groups)
        
        list = parse_msvcrt_groups(groups)
        
        print *, 'groups ',size(groups),' tokens = ',size(list)

        return

    end function mslex_error    

    type(mslex_group) function scan_stream_msvcrt(this, pattern, error) result(group)
        class(shlex_lexer), intent(inout) :: this
        character(kind=SCK,len=*), intent(in) :: pattern
        type(shlex_token), intent(out) :: error

        character(kind=SCK) :: c
        integer :: state, start, in_group
        
        associate(pos => this%input_position)

        group%spaces  = ""
        group%slashes = ""
        group%quotes  = ""
        group%text    = ""

        if (pos>=this%input_length) then
            error = new_token(EOF_ERROR, "END OF FILE")
        else
            error = new_token(NO_ERROR, "SUCCESS")
        end if
        
        state = MS_START
        in_group = 0

        do while (pos < this%input_length)
            
            pos = pos+1
            
            ! Identify next group
            ! Identify starting group                    
            if (in_group<=0 .and. scan(pattern(pos:pos),SPACE_CHARS)>0) then 
                
                in_group = 1
                start = pos
                
            elseif (in_group<=1 .and. scan(pattern(pos:pos),ESCAPE_CHARS)>0) then 
                
                in_group = 2
                start = pos
                
            elseif (in_group<=2 .and. scan(pattern(pos:pos),DOUBLE_QUOTE)>0) then 
                
                ! (\"+) group 3: one or more double quotes
                in_group = 3
                start = pos
                
            else
                
                ! (.[^\s\\\"]*) group 4: normal characters (text), starting with any character
                in_group = 4
                start = pos
                
            end if            
            
            select case (in_group)
            
                case (1)
                    
                    ! Check where is the end of this group
                    do while (scan(pattern(pos:pos),SPACE_CHARS)>0)
                        pos = pos+1
                        if (pos>this%input_length) exit
                    end do
                    pos = pos-1
                    
                    ! Store token
                    group%spaces = pattern(start:pos)
                    print *, 'SPACES <'//group%spaces//'>'
                    
                case (2)
                    
                    ! Check where is the end of this group
                    do while (scan(pattern(pos:pos),ESCAPE_CHARS)>0)
                        pos = pos+1
                        if (pos>this%input_length) exit
                    end do
                    pos = pos-1
                    
                    ! Store token
                    group%slashes = pattern(start:pos)      
                    print *, 'SLASHES <'//group%slashes//'>'
                    
                case (3)
                    
                    ! Check where is the end of this group
                    do while (scan(pattern(pos:pos),DOUBLE_QUOTE)>0)
                        pos = pos+1
                        if (pos>this%input_length) exit
                    end do
                    pos = pos-1
                    
                    ! Store token
                    group%quotes = pattern(start:pos)   
                    print *, 'QUOTES <'//group%quotes//'>'
                    
                case (4)
                    
                    ! Check where is the end of this group
                    do while (scan(pattern(pos:pos),SPACE_CHARS//ESCAPE_CHARS//DOUBLE_QUOTE)==0)
                        pos = pos+1
                        if (pos>this%input_length) exit
                    end do
                    pos = pos-1
                    
                    ! Store token
                    group%text = pattern(start:pos)       
                    
                    ! After group 4, we exit
                    print *, 'TEXT <'//group%text//'>'
                    exit        
                    
            end select

        end do
        
        ! Group 2 adn 3 are linked: (\s+)|(\\*)(\"+)|(.[^\s\\\"]*)
        ! slashes with no quotes means that it all goes into text
        if (len(group%slashes)>0 .and. len(group%quotes)<=0) then 
           group%text = group%slashes//group%quotes//group%text
           group%slashes = ""
           group%quotes=""
        endif

        endassociate
        
    end function scan_stream_msvcrt

    function parse_msvcrt_groups(groups) result(list)
        type(mslex_group), optional, intent(in) :: groups(:)
        type(shlex_token), allocatable :: list(:)

        character(kind=SCK,len=:), allocatable :: buffer
        integer :: i
        logical :: quote_mode
        integer :: n_slashes, n_quotes, magic_sum
        logical :: slashes_odd

        quote_mode = .false.        
        allocate(list(0))
        if (.not.present(groups)) return
        if (size(groups)<=0) return

        group_loop: do i = 1, size(groups)
        
            print *, 'group ',group_pretty_print(groups(i))
        
            if (len(groups(i)%spaces) > 0) then
                
                if (quote_mode) then
                    print *, 'add <',groups(i)%spaces,'> due to quote mode'
                    call yield(buffer,groups(i)%spaces)
                elseif (allocated(buffer)) then
                    
                    ! End of quote-delimited group: emit buffer (even if "")
                    if (.not.allocated(buffer)) buffer = ""
                    print *, 'return token <',buffer,'>'
                    list = [list, new_token(TOKEN_WORD, buffer)]
                    deallocate(buffer)
                end if
            endif

            if (len(groups(i)%quotes) > 0) then
                n_slashes   = len(groups(i)%slashes)
                n_quotes    = len(groups(i)%quotes)
                slashes_odd = mod(n_slashes, 2) /= 0
                call yield(buffer,repeat('\', n_slashes / 2))
                magic_sum   = n_quotes + merge(1, 0, quote_mode) + 2 * merge(1, 0, slashes_odd)
                call yield(buffer,repeat('"', magic_sum / 3))
                quote_mode  = mod(magic_sum, 3) == 1
                
                print *, 'slash=',n_slashes,'quot ',n_quotes,' odd=',slashes_odd,' sum=',magic_sum,' quotemode',quote_mode
            endif

            if (len(groups(i)%text) > 0) call yield(buffer,groups(i)%text)
            
        end do group_loop

        ! Always emit buffer (even if it's "")
        if (allocated(buffer)) then 
            print *, 'return token <',buffer,'>'
            list = [list, new_token(TOKEN_WORD, buffer)]
        endif
        
        contains
        
           pure subroutine yield(buffer,text)
               character(:), allocatable, intent(inout) :: buffer
               character(*), intent(in) :: text
               if (.not.allocated(buffer)) allocate(character(0) :: buffer)
               buffer = buffer//text
           end subroutine
        
    end function parse_msvcrt_groups

    type(shlex_token) function scan_stream(this,pattern,error) result(token)
        class(shlex_lexer), intent(inout) :: this
        character(kind=SCK,len=*), intent(in) :: pattern
        type(shlex_token), intent(out) :: error

        integer :: state,next_type,token_type,prev_escapes,next_quotes
        logical :: in_quotes_quote,start_quote
        character(kind=SCK) :: next_char
        character(kind=SCK,len=:), allocatable :: value

        state      = STATE_START
        token_type = TOKEN_UNKNOWN
        allocate(character(kind=SCK,len=0) :: value)
        error = new_token(NO_ERROR,"SUCCESS")

        read_chars: do

           ! Get next character
           this%input_position = this%input_position + 1
           call this%parse_char(pattern,next_type,next_char,error)
           if (DEBUG) print *, 'NEW CHAR: [',next_char,'] type=',CHAR_NAME(next_type),' state=',error%print()
           
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
                        
                         ! Quote with no previous escapes (start) is necessarily a quoting character
                         if (this%lexer==LEXER_WINDOWS) then 
                            start_quote = ms_start_quoting(this, pattern, value, error)
                         else
                            start_quote = .true.
                         end if
                         
                         if (DEBUG) print *, 'Quote detected, start quoting = ',start_quote
                         
                         if (start_quote) then 
                             token_type = TOKEN_QUOTED_WORD
                             state      = STATE_QUOTING
                             if (this%keep_quotes) value = value//next_char                            
                         endif
                                               
                      case (CHAR_ESCAPE) ! posix only                        
                         token_type = TOKEN_WORD
                         state      = STATE_ESCAPING   
                      case (CHAR_COMMENT) ! posix only
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
                        
                         if (this%lexer==LEXER_WINDOWS) then 
                             prev_escapes = n_previous_escapes(this,pattern)
                             start_quote = ms_start_quoting(this, pattern, value, error)
                             
                             if (start_quote) then 
                                 value = value(1:len(value) - prev_escapes/2)
                                 state = STATE_QUOTING
                                 if (this%keep_quotes) value = value // next_char                                
                             else
                                 value = value // next_char
                             endif
                             
                         else
                             start_quote = .true.   
                             state = STATE_QUOTING
                             if (this%keep_quotes) value = value // next_char                             
                         end if
                        
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
                        
                         if (this%lexer == LEXER_WINDOWS) then 
                             ! Windows is very forgiving: no need to close the quote
                             if (this%keep_quotes) value = value // DOUBLE_QUOTE
                             if (DEBUG) print *, 'Unfinished quote... <', value, '>'
                         else
                             ! POSIX requires closing quote
                             error = new_token(SYNTAX_ERROR, "END-OF-FILE when expecting closing quote")
                         end if
                         token = new_token(token_type, value)
                         return

                      case (CHAR_NONESCAPING_QUOTE)

                         if (this%lexer == LEXER_WINDOWS) then 
                             next_quotes = n_next_quotes(this, pattern)
                             if (DEBUG) print *, 'next quotes: ', next_quotes
                         else
                             next_quotes = 0
                         end if

                         if (next_quotes > 0) then 
                            if (mod(next_quotes + 1, 2) == 0) then
                                ! EVEN: stay in quoting
                                value = value // repeat(next_char, (next_quotes + 1) / 2)
                                this%input_position = this%input_position + next_quotes
                                if (DEBUG) print *, ' remain in quoting'
                            else
                                ! ODD: last quote ends quoting
                                value = value // repeat(next_char, next_quotes / 2)
                                this%input_position = this%input_position + next_quotes
                                state = STATE_INWORD
                                if (this%keep_quotes) value = value // next_char
                                if (DEBUG) print *, ' end of quoting, go back to word'

                                ! Check for immediate EOF or whitespace -> return token early
                                if (this%input_position >= this%input_length) then
                                   token = new_token(token_type, value)
                                   print *, 'EOF reached'
                                   return
                                elseif (pattern(this%input_position+1:this%input_position+1) == ' ') then
                                   print *, 'end quoting due to whitespace'
                                   token = new_token(token_type, value)
                                   return
                                end if
                            end if

                         else
                             ! Single quote -> end quoting
                             state = STATE_INWORD
                             if (this%keep_quotes) value = value // next_char

                             ! Check for immediate EOF or whitespace -> return token early
                             if (this%input_position >= this%input_length) then
                                token = new_token(token_type, value)
                                return
                             end if

                             if (this%input_position < this%input_length) then
                                 if (pattern(this%input_position+1:this%input_position+1) == ' ') then
                                     token = new_token(token_type, value)
                                     return
                                 end if
                             end if
                         end if

                      case default
                         value = value // next_char
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

    ! Check if should start quoting accortding to MS rules
    logical function ms_start_quoting(this, pattern, value, error) result(start_quoting)
        class(shlex_lexer), intent(inout) :: this
        character(kind=SCK, len=*), intent(in) :: pattern
        character(kind=SCK, len=:), allocatable, intent(inout) :: value
        type(shlex_token), intent(out) :: error

        integer :: prev_escapes, next_quotes, total_quotes
        logical :: at_start, ends_at_eof

        start_quoting = .false.

        if (this%lexer /= LEXER_WINDOWS) return
        if (this%input_position > this%input_length) return
        if (pattern(this%input_position:this%input_position) /= DOUBLE_QUOTE) return

        ! Determine escapes before this quote
        prev_escapes = n_previous_escapes(this, pattern)
        at_start = this%input_position == 1

        if (DEBUG) print *, 'start_quoting: prev_escapes =', prev_escapes

        ! Can start quoting if it's the first char or an even number of escapes
        if (at_start .or. mod(prev_escapes, 2) == 0) then
            ! Peek ahead to see how many more quotes follow
            next_quotes = n_next_quotes(this, pattern)
            total_quotes = 1 + next_quotes

            ! Check whether the final quote ends at EOF
            ends_at_eof = .false.!next_quotes>1 .and. this%input_position + next_quotes >= this%input_length

            if ((at_start .or. mod(total_quotes, 2) /= 0) .and. .not.ends_at_eof) then
                ! ODD total + not at EOF: start quoting, fold literal quotes
                value = value // repeat(DOUBLE_QUOTE, next_quotes / 2)
                this%input_position = this%input_position + next_quotes
                start_quoting = .true.           
                if (DEBUG) print *, 'Start quoting, move forward by ', next_quotes
            else
                ! EVEN total, or ODD but last quote is at EOF: treat all as literal
                value = value // repeat(DOUBLE_QUOTE, total_quotes / 2)
                this%input_position = this%input_position + next_quotes
                start_quoting = .false.
                if (DEBUG) print *, 'Do not start quoting, move forward by ', next_quotes
            end if
        end if
    end function ms_start_quoting



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

    subroutine iter_arg_msvcrt(matches, n_matches, token, quote_mode)
        ! Input: array of regex match groups
        ! Output: a single token string
        ! State: quote_mode

        character(len=*), dimension(:,:), intent(in)  :: matches  ! (4, n_matches)
        integer,                         intent(in)   :: n_matches
        character(len=:), allocatable,   intent(out)  :: token
        logical,                         intent(inout):: quote_mode

        integer :: i, n_slashes, n_quotes, magic_sum
        logical :: slashes_odd
        character(len=:), allocatable :: space, slashes, quotes, text

        token = ""
        do i = 1, n_matches
            space   = matches(1,i)
            slashes = matches(2,i)
            quotes  = matches(3,i)
            text    = matches(4,i)

            if (len_trim(space) > 0) then
                if (quote_mode) then
                    token = token // space
                else
                    exit  ! space ends the argument if not quoted
                end if

            else if (len_trim(quotes) > 0) then
                n_slashes = len_trim(slashes)
                n_quotes  = len_trim(quotes)
                slashes_odd = mod(n_slashes, 2) /= 0

                ! Append literal backslashes
                token = token // repeat('\', n_slashes / 2)

                ! Compute and append quotes
                magic_sum = n_quotes + merge(1, 0, quote_mode) + 2 * merge(1, 0, slashes_odd)
                token = token // repeat('"', magic_sum / 3)

                ! Toggle quote_mode
                quote_mode = mod(magic_sum, 3) == 1

            else if (len_trim(text) > 0) then
                token = token // text
            end if
        end do
    end subroutine iter_arg_msvcrt

    pure function group_pretty_print(this) result(msg)
        class(mslex_group), intent(in) :: this
        character(:), allocatable :: msg

        character(:), allocatable :: s, bs, qs, tx
        s  = "spaces:  [" // this%spaces  // "]"
        bs = "slashes: [" // this%slashes // "]"
        qs = "quotes:  [" // this%quotes  // "]"
        tx = "text:    [" // this%text    // "]"

        msg = s // ' | ' // bs // ' | ' // qs // ' | ' // tx
        
    end function group_pretty_print

end module shlex_module

