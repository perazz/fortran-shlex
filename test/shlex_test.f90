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


end program shlex_tests
