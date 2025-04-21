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
program shlex_tests
    use shlex_module
    use iso_fortran_env, only: output_unit
    implicit none
    
    integer :: ms

    integer :: nfailed = 0
    integer :: npassed = 0

    call add_test(test_1())
    call add_test(test_2())
    call add_test(test_3())
    call add_test(test_4())
    call add_test(test_5())
    call add_test(test_joined_1())
    call add_test(test_joined_2())
    call add_test(test_joined_3())
    call add_test(test_joined_4())
    call add_test(test_quotes_1())
        
    do ms=1,200
        call add_test(test_mslex(ms))
        if (nfailed>0) stop 1
    end do
    


    if (nfailed<=0) then
        print "(*(a,:,i0))", 'SUCCESS! all ',npassed,' tests passed.'
        stop 0
    else
        print "(*(a,:,i0))", 'ERROR: ',nfailed,' tests failed, ',npassed,' passed.'
        stop 1
    end if


    contains

    subroutine add_test(successful_test)
        logical, intent(in) :: successful_test
        if (successful_test) then
            npassed = npassed+1
        else
            nfailed = nfailed+1
        end if
    end subroutine add_test

    !
    logical function test_1() result(success)

       character(*), parameter :: string     = '~/a && b-c --color=auto || d *.py?'
       character(*), parameter :: results(7) = [character(12) :: '~/a', '&&', 'b-c', '--color=auto', '||', 'd', '*.py?']

       integer :: i
       type(shlex_token), allocatable :: tokens(:)

       tokens = shlex(string,success); if (.not.success) return
       success = size(tokens)==size(results); if (.not.success) return
       do i=1,size(tokens)
          success = tokens(i)%string==trim(results(i))
          if (.not.success) return
       end do
    end function test_1

    logical function test_2() result(success)

       character(*), parameter :: string     = &
       'one two "three four" "five \"six\"" seven#eight # nine # ten'//new_line('a')//' eleven ''twelve\\'' thirteen=13 fourteen/14'
       character(*), parameter :: results(*) = [character(13) :: 'one','two','three four','five "six"','seven#eight', &
                                                ' nine # ten','eleven','twelve\\','thirteen=13','fourteen/14']

       integer :: i
       type(shlex_token), allocatable :: tokens(:)

       tokens = shlex(string,success)
       if (.not.success) return
       success = size(tokens)==size(results)
       do i=1,size(tokens)
          success = tokens(i)%string==trim(results(i))
          if (.not.success) print *, 'token=',tokens(i)%string,' expected=',results(i)
          if (.not.success) return
       end do
    end function test_2

    logical function test_3() result(success)

       character(*), parameter :: string     = &
       'one two "three four" "five \"six\"" seven#eight # nine # ten'//new_line('a')//' eleven ''twelve\\'' thirteen=13 fourteen/14'
       character(*), parameter :: results(*) = [character(13) :: 'one','two','three four','five "six"','seven#eight', &
                                                ' nine # ten','eleven','twelve\\','thirteen=13','fourteen/14']

       integer :: i
       character(len=:), allocatable :: tokens(:)

       tokens = split(string,success)
       if (.not.success) return
       success = size(tokens)==size(results)
       do i=1,size(tokens)
          success = tokens(i)==trim(results(i))
          if (.not.success) print *, 'token=',tokens(i),' expected=',results(i)
          if (.not.success) return
       end do
    end function test_3

    logical function test_4() result(success)

       character(*), parameter :: string     = &
       'gfortran.exe -Impif90/include -Lmpif90/lib '
       character(*), parameter :: results(*) = &
       [character(17) :: 'gfortran.exe', '-Impif90/include', '-Lmpif90/lib', '-l:libmsmpi.dll.a']

       integer :: i
       character(len=:), allocatable :: tokens(:)

       tokens = split(string,success)
       if (.not.success) return
       success = size(tokens)==size(results)
       do i=1,size(tokens)
          success = tokens(i)==trim(results(i))
          if (.not.success) print *, 'token=',tokens(i),' expected=',results(i)
          if (.not.success) return
       end do
    end function test_4

    logical function test_5() result(success)

       character(*), parameter :: string     = &
       'gfortran -I/opt/homebrew/Cellar/open-mpi/4.1.5/include -Wl,-flat_namespace -Wl,-commons,use_dylibs '&
       //'-I/opt/homebrew/Cellar/open-mpi/4.1.5/lib -L/opt/homebrew/Cellar/open-mpi/4.1.5/lib -L/opt/homebrew'&
       //'/opt/libevent/lib -lmpi_usempif08 -lmpi_usempi_ignore_tkr -lmpi_mpifh -lmpi'
       character(*), parameter :: results(*) = &
       [character(45) :: 'gfortran', '-I/opt/homebrew/Cellar/open-mpi/4.1.5/include', &
                         '-Wl,-flat_namespace', '-Wl,-commons,use_dylibs', &
                         '-I/opt/homebrew/Cellar/open-mpi/4.1.5/lib', &
                         '-L/opt/homebrew/Cellar/open-mpi/4.1.5/lib', &
                         '-L/opt/homebrew/opt/libevent/lib', &
                         '-lmpi_usempif08', '-lmpi_usempi_ignore_tkr','-lmpi_mpifh','-lmpi']

       integer :: i
       character(len=:), allocatable :: tokens(:)

       tokens = split(string,success)
       if (.not.success) return
       success = size(tokens)==size(results)
       do i=1,size(tokens)
          success = tokens(i)==trim(results(i))
          if (.not.success) print *, 'token=',tokens(i),' expected=',results(i)
          if (.not.success) return
       end do
    end function test_5

    ! fpm case input
    logical function test_joined_1() result(success)

        character(*), parameter :: string = &
        '-I/path/to/include -I /test -I"/path/to/include with spaces" -I "spaces here too" -L/path/to/lib -lmylib -O2 -g -Wall'

        character(*), parameter :: results(*) = [character(40) :: &
            '-I/path/to/include', '-I/test', '-I/path/to/include with spaces', '-Ispaces here too', &
            '-L/path/to/lib', '-lmylib', '-O2', '-g', '-Wall']

        integer :: i
        character(len=:), allocatable :: tokens(:)

        tokens = split(string, join_spaced=.true., success=success)
        if (.not.success) return
        success = size(tokens) == size(results)

        do i = 1, size(tokens)
            success = tokens(i) == trim(results(i))
            if (.not.success) print *, 'token=', tokens(i), ' expected=', results(i)
            if (.not.success) return
        end do

    end function test_joined_1

    ! mixed spacing, no quoted flags
    logical function test_joined_2() result(success)

        character(*), parameter :: string = '-I include -L lib -O3 -Wall -lm'
        character(*), parameter :: results(*) = [character(10) :: '-Iinclude', '-Llib', '-O3', '-Wall', '-lm']

        integer :: i
        character(len=:), allocatable :: tokens(:)

        tokens = split(string, join_spaced=.true., success=success)
        if (.not.success) return
        success = size(tokens) == size(results)

        do i = 1, size(tokens)
            success = tokens(i) == trim(results(i))
            if (.not.success) print *, 'token=', tokens(i), ' expected=', results(i)
            if (.not.success) return
        end do

    end function test_joined_2

    ! ensure flags with attached args are left untouched
    logical function test_joined_3() result(success)

        character(*), parameter :: string = '-I/path -I /spaced -DMACRO=1 -lfoo'
        character(*), parameter :: results(*) = [character(20) :: '-I/path', '-I/spaced', '-DMACRO=1', '-lfoo']

        integer :: i
        character(len=:), allocatable :: tokens(:)

        tokens = split(string, join_spaced=.true., success=success)
        if (.not.success) return
        success = size(tokens) == size(results)

        do i = 1, size(tokens)
            success = tokens(i) == trim(results(i))
            if (.not.success) print *, 'token=', tokens(i), ' expected=', results(i)
            if (.not.success) return
        end do

    end function test_joined_3

    ! test from fpm netCDF metapackage
    logical function test_joined_4() result(success)

        character(*), parameter :: string = &
        '-I/path/to/include -I /test -I"/path/to/include with spaces" -I "spaces here too" -L/path/to/lib -lmylib'
        character(*), parameter :: results(*) = [character(64) :: '-I/path/to/include','-I/test',&
        '-I"/path/to/include with spaces"','-I"spaces here too"','-L/path/to/lib','-lmylib']

        integer :: i
        character(len=:), allocatable :: tokens(:)

        tokens = split(string, join_spaced=.true., keep_quotes=.true., success=success)
        if (.not.success) return
        success = size(tokens) == size(results)

        do i = 1, size(tokens)
            success = tokens(i) == trim(results(i))
            if (.not.success) print *, 'token=', tokens(i), ' expected=', results(i)
            if (.not.success) return
        end do

    end function test_joined_4

    ! test from fpm netCDF metapackage
    logical function test_quotes_1() result(success)

        character(*), parameter :: string = &
        '-I/path/to/include -I /test -I"/path/to/include with spaces" -I "spaces here too" -L/path/to/lib -lmylib'
        character(*), parameter :: results(*) = [character(64) :: '-I/path/to/include','-I','/test',&
        '-I"/path/to/include with spaces"','-I','"spaces here too"','-L/path/to/lib','-lmylib']

        integer :: i
        character(len=:), allocatable :: tokens(:)

        tokens = split(string, join_spaced=.false., keep_quotes=.true., success=success)
        if (.not.success) return
        success = size(tokens) == size(results)

        do i = 1, size(tokens)
            success = tokens(i) == trim(results(i))
            if (.not.success) print *, 'token=', tokens(i), ' expected=', results(i)
            if (.not.success) return
        end do

    end function test_quotes_1


    ! Test case: empty string []
    logical function test_mslex(id) result(success)        
        integer, intent(in) :: id
        
        integer :: i
        type(shlex_token) :: error        
        character(:), allocatable :: pattern,results(:),tokens(:)
        
        ! Get test 
        call get_mslex_test(id,pattern,results)
        
        print "(///,'Parsing pattern: <',a,'>'///)", pattern

        tokens = ms_split(pattern, error)
        
        success = error%type==0
        
        if (.not.success) then 
            print *, 'MSLEX parsing failed for case ',id,' pattern=',pattern
            print *, 'error=',error%print()
            return
        endif        
        success = size(tokens) == size(results)        
        if (.not.success) print *, 'MSLEX failed for case ',id,' pattern=',pattern
        if (.not.success) print *, 'N tokens = ',size(tokens),' expected # = ',size(results)
        if (.not.success) print *, 'error=',error%print()
        if (.not.success) return
        do i = 1, size(tokens)
            success = trim(tokens(i)) == trim(results(i))
            if (.not.success) print *, 'token ',i,': <', tokens(i), '> expected=<', results(i),'>',' success=',success
            if (.not.success) print *, 'char tokens ',iachar(trim(tokens(i)))
            if (.not.success) print *, 'char exp    ',iachar(trim(results(i)))
            
            print *, 'tokens(i)=<',tokens(i),'> len=',len(tokens(i)),' char=',iachar(tokens(i)), 'len trim=',len_trim(tokens(i))
            print *, 'res   (i)=<',results(i),'> len=',len(results(i)),' chars=',iachar(results(i)),' len trim=',len_trim(results(i))
            
            
            
            if (.not.success) print *, 'error=',error%print()
            if (.not.success) print *, 'pattern=<',pattern,'>'
            if (.not.success) return
        end do
    end function test_mslex

    subroutine get_mslex_test(id,pattern,expected_result)
        integer, intent(in) :: id
        character(:), allocatable, intent(out) :: pattern, expected_result(:)
        
        select case (id)
          case (1)
             pattern = ''
             allocate(character(len=0) :: expected_result(0))

          case (2)
             pattern = '"'
             allocate(character(len=0) :: expected_result(1))
             expected_result(1) = ''

          case (3)
             pattern = '""'
             allocate(character(len=0) :: expected_result(1))
             expected_result(1) = ''

          case (4)
             pattern = '"""'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '"'

          case (5)
             pattern = '""""'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '"'

          case (6)
             pattern = '"""""'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '"'

          case (7)
             pattern = '""""""'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '""'

          case (8)
             pattern = '"""""""'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '""'

          case (9)
             pattern = '""""""""'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '""'

          case (10)
             pattern = '"""""""""'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '"""'

          case (11)
             pattern = '""""""""""'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '"""'

          case (12)
             pattern = ' "'
             allocate(character(len=0) :: expected_result(1))
             expected_result(1) = ''

          case (13)
             pattern = ' ""'
             allocate(character(len=0) :: expected_result(1))
             expected_result(1) = ''

          case (14)
             pattern = ' """'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '"'

          case (15)
             pattern = ' """"'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '"'

          case (16)
             pattern = ' """""'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '"'

          case (17)
             pattern = ' """"""'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '""'

          case (18)
             pattern = ' """""""'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '""'

          case (19)
             pattern = ' """"""""'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '""'

          case (20)
             pattern = ' """"""""""'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '"""'

          case (21)
             pattern = ' '
             allocate(character(len=0) :: expected_result(0))

          case (22)
             pattern = '" '
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = ' '

          case (23)
             pattern = '"" '
             allocate(character(len=0) :: expected_result(1))
             expected_result(1) = ''

          case (24)
             pattern = '""" '
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '"'

          case (25)
             pattern = '"""" '
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '" '

          case (26)
             pattern = '""""" '
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '"'

          case (27)
             pattern = '"""""" '
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '""'

          case (28)
             pattern = '""""""" '
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '"" '

          case (29)
             pattern = '"""""""" '
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '""'

          case (30)
             pattern = '""""""""" '
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '"""'

          case (31)
             pattern = '"""""""""" '
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = '""" '

          case (32)
             pattern = 'x'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = 'x'

          case (33)
             pattern = 'x"'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = 'x'

          case (34)
             pattern = 'foo'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = 'foo'

          case (35)
             pattern = 'foo    "bar baz"'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'foo'
             expected_result(2) = 'bar baz'

          case (36)
             pattern = '"abc" d e'
             allocate(character(len=3) :: expected_result(3))
             expected_result(1) = 'abc'
             expected_result(2) = 'd'
             expected_result(3) = 'e'

          case (37)
             pattern = '"a\bc" d e'
             allocate(character(len=4) :: expected_result(3))
             expected_result(1) = 'a\bc'
             expected_result(2) = 'd'
             expected_result(3) = 'e'

          case (38)
             pattern = 'a\\\b d"e f"g h'
             allocate(character(len=5) :: expected_result(3))
             expected_result(1) = 'a\\\b'
             expected_result(2) = 'de fg'
             expected_result(3) = 'h'

          case (39)
             pattern = 'a\\\"b c d'
             allocate(character(len=4) :: expected_result(3))
             expected_result(1) = 'a\"b'
             expected_result(2) = 'c'
             expected_result(3) = 'd'

          case (40)
             pattern = 'a\\\\"b c" d e'
             allocate(character(len=6) :: expected_result(3))
             expected_result(1) = 'a\\b c'
             expected_result(2) = 'd'
             expected_result(3) = 'e'

          case (41)
             pattern = '"" "" ""'
             allocate(character(len=0) :: expected_result(3))
             expected_result(1) = ''
             expected_result(2) = ''
             expected_result(3) = ''

          case (42)
             pattern = ' x'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = 'x'

          case (43)
             pattern = '" x'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = ' x'

          case (44)
             pattern = '"" x'
             allocate(character(len=1) :: expected_result(2))
             expected_result(1) = ''
             expected_result(2) = 'x'

          case (45)
             pattern = '""" x'
             allocate(character(len=1) :: expected_result(2))
             expected_result(1) = '"'
             expected_result(2) = 'x'

          case (46)
             pattern = '"""" x'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '" x'

          case (47)
             pattern = '""""" x'
             allocate(character(len=1) :: expected_result(2))
             expected_result(1) = '"'
             expected_result(2) = 'x'

          case (48)
             pattern = '"""""" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '""'
             expected_result(2) = 'x'

          case (49)
             pattern = '""""""" x'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = '"" x'

          case (50)
             pattern = '"""""""" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '""'
             expected_result(2) = 'x'

          case (51)
             pattern = '""""""""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '"""'
             expected_result(2) = 'x'

          case (52)
             pattern = '"""""""""" x'
             allocate(character(len=5) :: expected_result(1))
             expected_result(1) = '""" x'

          case (53)
             pattern = '""""""""""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '"""'
             expected_result(2) = 'x'

          case (54)
             pattern = '"""""""""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '""""'
             expected_result(2) = 'x'

          case (55)
             pattern = '""""""""""""" x'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = '"""" x'

          case (56)
             pattern = '"aaa x'
             allocate(character(len=5) :: expected_result(1))
             expected_result(1) = 'aaa x'

          case (57)
             pattern = '"aaa" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = 'aaa'
             expected_result(2) = 'x'

          case (58)
             pattern = '"aaa"" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = 'aaa"'
             expected_result(2) = 'x'

          case (59)
             pattern = '"aaa""" x'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = 'aaa" x'

          case (60)
             pattern = '"aaa"""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = 'aaa"'
             expected_result(2) = 'x'

          case (61)
             pattern = '"aaa""""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'aaa""'
             expected_result(2) = 'x'

          case (62)
             pattern = '"aaa"""""" x'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = 'aaa"" x'

          case (63)
             pattern = '"aaa""""""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'aaa""'
             expected_result(2) = 'x'

          case (64)
             pattern = '"aaa"""""""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa"""'
             expected_result(2) = 'x'

          case (65)
             pattern = '"aaa""""""""" x'
             allocate(character(len=8) :: expected_result(1))
             expected_result(1) = 'aaa""" x'

          case (66)
             pattern = '"aaa"""""""""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa"""'
             expected_result(2) = 'x'

          case (67)
             pattern = '"aaa""""""""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa""""'
             expected_result(2) = 'x'

          case (68)
             pattern = '"aaa"""""""""""" x'
             allocate(character(len=9) :: expected_result(1))
             expected_result(1) = 'aaa"""" x'

          case (69)
             pattern = '"aaa\ x'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = 'aaa\ x'

          case (70)
             pattern = '"aaa\" x'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = 'aaa" x'

          case (71)
             pattern = '"aaa\"" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = 'aaa"'
             expected_result(2) = 'x'

          case (72)
             pattern = '"aaa\""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'aaa""'
             expected_result(2) = 'x'

          case (73)
             pattern = '"aaa\"""" x'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = 'aaa"" x'

          case (74)
             pattern = '"aaa\""""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'aaa""'
             expected_result(2) = 'x'

          case (75)
             pattern = '"aaa\"""""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa"""'
             expected_result(2) = 'x'

          case (76)
             pattern = '"aaa\""""""" x'
             allocate(character(len=8) :: expected_result(1))
             expected_result(1) = 'aaa""" x'

          case (77)
             pattern = '"aaa\"""""""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa"""'
             expected_result(2) = 'x'

          case (78)
             pattern = '"aaa\""""""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa""""'
             expected_result(2) = 'x'

          case (79)
             pattern = '"aaa\"""""""""" x'
             allocate(character(len=9) :: expected_result(1))
             expected_result(1) = 'aaa"""" x'

          case (80)
             pattern = '"aaa\""""""""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa""""'
             expected_result(2) = 'x'

          case (81)
             pattern = '"aaa\"""""""""""" x'
             allocate(character(len=8) :: expected_result(2))
             expected_result(1) = 'aaa"""""'
             expected_result(2) = 'x'

          case (82)
             pattern = '"aaa\\ x'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = 'aaa\\ x'

          case (83)
             pattern = '"aaa\\" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = 'aaa\'
             expected_result(2) = 'x'

          case (84)
             pattern = '"aaa\\"" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'aaa\"'
             expected_result(2) = 'x'

          case (85)
             pattern = '"aaa\\""" x'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = 'aaa\" x'

          case (86)
             pattern = '"aaa\\"""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'aaa\"'
             expected_result(2) = 'x'

          case (87)
             pattern = '"aaa\\""""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa\""'
             expected_result(2) = 'x'

          case (88)
             pattern = '"aaa\\"""""" x'
             allocate(character(len=8) :: expected_result(1))
             expected_result(1) = 'aaa\"" x'

          case (89)
             pattern = '"aaa\\""""""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa\""'
             expected_result(2) = 'x'

          case (90)
             pattern = '"aaa\\"""""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa\"""'
             expected_result(2) = 'x'

          case (91)
             pattern = '"aaa\\""""""""" x'
             allocate(character(len=9) :: expected_result(1))
             expected_result(1) = 'aaa\""" x'

          case (92)
             pattern = '"aaa\\"""""""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa\"""'
             expected_result(2) = 'x'

          case (93)
             pattern = '"aaa\\""""""""""" x'
             allocate(character(len=8) :: expected_result(2))
             expected_result(1) = 'aaa\""""'
             expected_result(2) = 'x'

          case (94)
             pattern = '"aaa\\"""""""""""" x'
             allocate(character(len=10) :: expected_result(1))
             expected_result(1) = 'aaa\"""" x'

          case (95)
             pattern = '"aaa\\\ x'
             allocate(character(len=8) :: expected_result(1))
             expected_result(1) = 'aaa\\\ x'

          case (96)
             pattern = '"aaa\\\" x'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = 'aaa\" x'

          case (97)
             pattern = '"aaa\\\"" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'aaa\"'
             expected_result(2) = 'x'

          case (98)
             pattern = '"aaa\\\""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa\""'
             expected_result(2) = 'x'

          case (99)
             pattern = '"aaa\\\"""" x'
             allocate(character(len=8) :: expected_result(1))
             expected_result(1) = 'aaa\"" x'

          case (100)
             pattern = '"aaa\\\""""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa\""'
             expected_result(2) = 'x'

          case (101)
             pattern = '"aaa\\\"""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa\"""'
             expected_result(2) = 'x'

          case (102)
             pattern = '"aaa\\\""""""" x'
             allocate(character(len=9) :: expected_result(1))
             expected_result(1) = 'aaa\""" x'

          case (103)
             pattern = '"aaa\\\"""""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa\"""'
             expected_result(2) = 'x'

          case (104)
             pattern = '"aaa\\\""""""""" x'
             allocate(character(len=8) :: expected_result(2))
             expected_result(1) = 'aaa\""""'
             expected_result(2) = 'x'

          case (105)
             pattern = '"aaa\\\"""""""""" x'
             allocate(character(len=10) :: expected_result(1))
             expected_result(1) = 'aaa\"""" x'

          case (106)
             pattern = '"aaa\\\""""""""""" x'
             allocate(character(len=8) :: expected_result(2))
             expected_result(1) = 'aaa\""""'
             expected_result(2) = 'x'

          case (107)
             pattern = '"aaa\\\"""""""""""" x'
             allocate(character(len=9) :: expected_result(2))
             expected_result(1) = 'aaa\"""""'
             expected_result(2) = 'x'

          case (108)
             pattern = '"aaa\\\\ x'
             allocate(character(len=9) :: expected_result(1))
             expected_result(1) = 'aaa\\\\ x'

          case (109)
             pattern = '"aaa\\\\" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'aaa\\'
             expected_result(2) = 'x'

          case (110)
             pattern = '"aaa\\\\"" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa\\"'
             expected_result(2) = 'x'

          case (111)
             pattern = '"aaa\\\\""" x'
             allocate(character(len=8) :: expected_result(1))
             expected_result(1) = 'aaa\\" x'

          case (112)
             pattern = '"aaa\\\\"""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'aaa\\"'
             expected_result(2) = 'x'

          case (113)
             pattern = '"aaa\\\\""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa\\""'
             expected_result(2) = 'x'

          case (114)
             pattern = '"aaa\\\\"""""" x'
             allocate(character(len=9) :: expected_result(1))
             expected_result(1) = 'aaa\\"" x'

          case (115)
             pattern = '"aaa\\\\""""""" x'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'aaa\\""'
             expected_result(2) = 'x'

          case (116)
             pattern = '"aaa\\\\"""""""" x'
             allocate(character(len=8) :: expected_result(2))
             expected_result(1) = 'aaa\\"""'
             expected_result(2) = 'x'

          case (117)
             pattern = '"aaa\\\\""""""""" x'
             allocate(character(len=10) :: expected_result(1))
             expected_result(1) = 'aaa\\""" x'

          case (118)
             pattern = '"aaa\\\\"""""""""" x'
             allocate(character(len=8) :: expected_result(2))
             expected_result(1) = 'aaa\\"""'
             expected_result(2) = 'x'

          case (119)
             pattern = '"aaa\\\\""""""""""" x'
             allocate(character(len=9) :: expected_result(2))
             expected_result(1) = 'aaa\\""""'
             expected_result(2) = 'x'

          case (120)
             pattern = '"aaa\\\\"""""""""""" x'
             allocate(character(len=11) :: expected_result(1))
             expected_result(1) = 'aaa\\"""" x'

          case (121)
             pattern = '\ x'
             allocate(character(len=1) :: expected_result(2))
             expected_result(1) = '\'
             expected_result(2) = 'x'

          case (122)
             pattern = '\" x'
             allocate(character(len=1) :: expected_result(2))
             expected_result(1) = '"'
             expected_result(2) = 'x'

          case (123)
             pattern = '\"" x'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '" x'

          case (124)
             pattern = '\""" x'
             allocate(character(len=1) :: expected_result(2))
             expected_result(1) = '"'
             expected_result(2) = 'x'

          case (125)
             pattern = '\"""" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '""'
             expected_result(2) = 'x'

          case (126)
             pattern = '\""""" x'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = '"" x'

          case (127)
             pattern = '\"""""" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '""'
             expected_result(2) = 'x'

          case (128)
             pattern = '\""""""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '"""'
             expected_result(2) = 'x'

          case (129)
             pattern = '\"""""""" x'
             allocate(character(len=5) :: expected_result(1))
             expected_result(1) = '""" x'

          case (130)
             pattern = '\""""""""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '"""'
             expected_result(2) = 'x'

          case (131)
             pattern = '\"""""""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '""""'
             expected_result(2) = 'x'

          case (132)
             pattern = '\""""""""""" x'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = '"""" x'

          case (133)
             pattern = '\"""""""""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '""""'
             expected_result(2) = 'x'

          case (134)
             pattern = '\\ x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '\\'
             expected_result(2) = 'x'

          case (135)
             pattern = '\\" x'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '\ x'

          case (136)
             pattern = '\\"" x'
             allocate(character(len=1) :: expected_result(2))
             expected_result(1) = '\'
             expected_result(2) = 'x'

          case (137)
             pattern = '\\""" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '\"'
             expected_result(2) = 'x'

          case (138)
             pattern = '\\"""" x'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = '\" x'

          case (139)
             pattern = '\\""""" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '\"'
             expected_result(2) = 'x'

          case (140)
             pattern = '\\"""""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '\""'
             expected_result(2) = 'x'

          case (141)
             pattern = '\\""""""" x'
             allocate(character(len=5) :: expected_result(1))
             expected_result(1) = '\"" x'

          case (142)
             pattern = '\\"""""""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '\""'
             expected_result(2) = 'x'

          case (143)
             pattern = '\\""""""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '\"""'
             expected_result(2) = 'x'

          case (144)
             pattern = '\\"""""""""" x'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = '\""" x'

          case (145)
             pattern = '\\""""""""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '\"""'
             expected_result(2) = 'x'

          case (146)
             pattern = '\\"""""""""""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = '\""""'
             expected_result(2) = 'x'

          case (147)
             pattern = '\\\ x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '\\\'
             expected_result(2) = 'x'

          case (148)
             pattern = '\\\" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '\"'
             expected_result(2) = 'x'

          case (149)
             pattern = '\\\"" x'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = '\" x'

          case (150)
             pattern = '\\\""" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '\"'
             expected_result(2) = 'x'

          case (151)
             pattern = '\\\"""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '\""'
             expected_result(2) = 'x'

          case (152)
             pattern = '\\\""""" x'
             allocate(character(len=5) :: expected_result(1))
             expected_result(1) = '\"" x'

          case (153)
             pattern = '\\\"""""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '\""'
             expected_result(2) = 'x'

          case (154)
             pattern = '\\\""""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '\"""'
             expected_result(2) = 'x'

          case (155)
             pattern = '\\\"""""""" x'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = '\""" x'

          case (156)
             pattern = '\\\""""""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '\"""'
             expected_result(2) = 'x'

          case (157)
             pattern = '\\\"""""""""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = '\""""'
             expected_result(2) = 'x'

          case (158)
             pattern = '\\\""""""""""" x'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = '\"""" x'

          case (159)
             pattern = '\\\"""""""""""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = '\""""'
             expected_result(2) = 'x'

          case (160)
             pattern = '\\\\ x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '\\\\'
             expected_result(2) = 'x'

          case (161)
             pattern = '\\\\" x'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = '\\ x'

          case (162)
             pattern = '\\\\"" x'
             allocate(character(len=2) :: expected_result(2))
             expected_result(1) = '\\'
             expected_result(2) = 'x'

          case (163)
             pattern = '\\\\""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '\\"'
             expected_result(2) = 'x'

          case (164)
             pattern = '\\\\"""" x'
             allocate(character(len=5) :: expected_result(1))
             expected_result(1) = '\\" x'

          case (165)
             pattern = '\\\\""""" x'
             allocate(character(len=3) :: expected_result(2))
             expected_result(1) = '\\"'
             expected_result(2) = 'x'

          case (166)
             pattern = '\\\\"""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '\\""'
             expected_result(2) = 'x'

          case (167)
             pattern = '\\\\""""""" x'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = '\\"" x'

          case (168)
             pattern = '\\\\"""""""" x'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = '\\""'
             expected_result(2) = 'x'

          case (169)
             pattern = '\\\\""""""""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = '\\"""'
             expected_result(2) = 'x'

          case (170)
             pattern = '\\\\"""""""""" x'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = '\\""" x'

          case (171)
             pattern = '\\\\""""""""""" x'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = '\\"""'
             expected_result(2) = 'x'

          case (172)
             pattern = '\\\\"""""""""""" x'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = '\\""""'
             expected_result(2) = 'x'

          case (173)
             pattern = '"x"'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = 'x'

          case (174)
             pattern = '"^x"'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^x'

          case (175)
             pattern = '"^^x"'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '^^x'

          case (176)
             pattern = '"x'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = 'x'

          case (177)
             pattern = '"^x'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^x'

          case (178)
             pattern = '"^^x'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '^^x'

          case (179)
             pattern = '"'
             allocate(character(len=0) :: expected_result(1))
             expected_result(1) = ''

          case (180)
             pattern = '"^'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '^'

          case (181)
             pattern = '"^^'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^^'

          case (182)
             pattern = '"^ '
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^ '

          case (183)
             pattern = ':dir'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = ':dir'

          case (184)
             pattern = ';;;a,, b, c==='
             allocate(character(len=6) :: expected_result(3))
             expected_result(1) = ';;;a,,'
             expected_result(2) = 'b,'
             expected_result(3) = 'c==='

          case (185)
             pattern = '^;;a'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = '^;;a'

          case (186)
             pattern = 'a "<>||&&'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'a'
             expected_result(2) = '<>||&&'

          case (187)
             pattern = 'a "<>||&&^'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'a'
             expected_result(2) = '<>||&&^'

          case (188)
             pattern = 'a "<>||&&^^'
             allocate(character(len=8) :: expected_result(2))
             expected_result(1) = 'a'
             expected_result(2) = '<>||&&^^'

          case (189)
             pattern = '"foo &whoami bar"'
             allocate(character(len=15) :: expected_result(1))
             expected_result(1) = 'foo &whoami bar'

          case (190)
             pattern = '^^'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^^'

          case (191)
             pattern = '"^"'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '^'

          case (192)
             pattern = '"^^"'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^^'

          case (193)
             pattern = 'foo^bar'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = 'foo^bar'

          case (194)
             pattern = 'foo^^bar'
             allocate(character(len=8) :: expected_result(1))
             expected_result(1) = 'foo^^bar'

          case (195)
             pattern = '"foo^bar"'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = 'foo^bar'

          case (196)
             pattern = '"foo^^bar"'
             allocate(character(len=8) :: expected_result(1))
             expected_result(1) = 'foo^^bar'

          case (197)
             pattern = '"x"'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = 'x'

          case (198)
             pattern = '"^x"'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^x'

          case (199)
             pattern = '"^^x"'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '^^x'

          case (200)
             pattern = '"x'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = 'x'

          case (201)
             pattern = '"^x'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^x'

          case (202)
             pattern = '"^^x'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = '^^x'

          case (203)
             pattern = '"'
             allocate(character(len=0) :: expected_result(1))
             expected_result(1) = ''

          case (204)
             pattern = '"^'
             allocate(character(len=1) :: expected_result(1))
             expected_result(1) = '^'

          case (205)
             pattern = '"^^'
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^^'

          case (206)
             pattern = '"^ '
             allocate(character(len=2) :: expected_result(1))
             expected_result(1) = '^ '

          case (207)
             pattern = ':dir'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = ':dir'

          case (208)
             pattern = ';;;a,, b, c==='
             allocate(character(len=6) :: expected_result(3))
             expected_result(1) = ';;;a,,'
             expected_result(2) = 'b,'
             expected_result(3) = 'c==='

          case (209)
             pattern = 'a "<>||&&'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'a'
             expected_result(2) = '<>||&&'

          case (210)
             pattern = 'a "<>||&&^'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'a'
             expected_result(2) = '<>||&&^'

          case (211)
             pattern = 'a "<>||&&^^'
             allocate(character(len=8) :: expected_result(2))
             expected_result(1) = 'a'
             expected_result(2) = '<>||&&^^'

          case (212)
             pattern = 'foo'
             allocate(character(len=3) :: expected_result(1))
             expected_result(1) = 'foo'

          case (213)
             pattern = 'foo^'
             allocate(character(len=4) :: expected_result(1))
             expected_result(1) = 'foo^'

          case (214)
             pattern = 'foo^^'
             allocate(character(len=5) :: expected_result(1))
             expected_result(1) = 'foo^^'

          case (215)
             pattern = 'foo^^^'
             allocate(character(len=6) :: expected_result(1))
             expected_result(1) = 'foo^^^'

          case (216)
             pattern = 'foo^^^^'
             allocate(character(len=7) :: expected_result(1))
             expected_result(1) = 'foo^^^^'

          case (217)
             pattern = 'foo^ bar'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = 'foo^'
             expected_result(2) = 'bar'

          case (218)
             pattern = 'foo^^ bar'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'foo^^'
             expected_result(2) = 'bar'

          case (219)
             pattern = 'foo^^^ bar'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'foo^^^'
             expected_result(2) = 'bar'

          case (220)
             pattern = 'foo^^^^ bar'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'foo^^^^'
             expected_result(2) = 'bar'

          case (221)
             pattern = '"foo^" bar'
             allocate(character(len=4) :: expected_result(2))
             expected_result(1) = 'foo^'
             expected_result(2) = 'bar'

          case (222)
             pattern = '"foo^^" bar'
             allocate(character(len=5) :: expected_result(2))
             expected_result(1) = 'foo^^'
             expected_result(2) = 'bar'

          case (223)
             pattern = '"foo^^^" bar'
             allocate(character(len=6) :: expected_result(2))
             expected_result(1) = 'foo^^^'
             expected_result(2) = 'bar'

          case (224)
             pattern = '"foo^^^^" bar'
             allocate(character(len=7) :: expected_result(2))
             expected_result(1) = 'foo^^^^'
             expected_result(2) = 'bar'

          case default
              stop 'invalid mslex test ID'
        end select
        
        
    end subroutine get_mslex_test
        
        
  
             
    



end program shlex_tests
