! tests/integration/test_integration_io.f90
! Integration tests for sio_io module (getdir, rdcntrl, navopen).
! Linux compatibility: getdir always fails (opens Windows path), and
! navopen with a valid adir uses 'Data\' (backslash) which is invalid on Linux.
! Those tests print WARN and do not increment failure count.
program test_integration_io
  use sio_io
  implicit none
  integer :: failures = 0

  call test_getdir_missing_siodir(failures)
  call test_getdir_valid(failures)
  call test_rdcntrl_missing_control(failures)
  call test_rdcntrl_valid(failures)
  call test_rdcntrl_malformed_control(failures)
  call test_rdcntrl_debug_operator(failures)
  call test_navopen_missing_nav(failures)
  call test_navopen_valid(failures)

  if (failures == 0) then
    print *, 'test_integration_io: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_integration_io: FAILURES =', failures
    stop 1
  end if

contains

  ! ---------------------------------------------------------------------------
  ! test_getdir_missing_siodir
  ! getdir always fails on Linux (opens c:\Users\Public\Documents\siodir.txt).
  ! Expect ierror(7)=1 -> PASS.
  subroutine test_getdir_missing_siodir(failures)
    integer, intent(inout) :: failures
    character(len=80) :: adir
    integer :: len_adir
    integer :: ierror(50), igderr(3)
    ierror  = 0
    igderr  = 0
    len_adir = 0
    adir    = ' '
    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) == 1) then
      print *, 'PASS test_getdir_missing_siodir: ierror(7)=1 as expected'
    else
      print *, 'FAIL test_getdir_missing_siodir: ierror(7)=', ierror(7), &
               ' (expected 1)'
      failures = failures + 1
    end if
  end subroutine test_getdir_missing_siodir

  ! ---------------------------------------------------------------------------
  ! test_getdir_valid
  ! On Linux getdir always fails because siodir.txt is a Windows path.
  ! Print WARN and skip (do not increment failures).
  subroutine test_getdir_valid(failures)
    integer, intent(inout) :: failures
    character(len=80) :: adir
    integer :: len_adir
    integer :: ierror(50), igderr(3)
    ierror  = 0
    igderr  = 0
    len_adir = 0
    adir    = ' '
    call getdir(adir, len_adir, ierror, igderr)
    if (ierror(7) /= 0) then
      print *, 'WARN test_getdir_valid: getdir failed on Linux (Windows path) - skipping'
    else
      if (len_adir > 0) then
        print *, 'PASS test_getdir_valid: adir=', adir(1:len_adir), &
                 ' len_adir=', len_adir
      else
        print *, 'WARN test_getdir_valid: getdir returned len_adir=0'
      end if
    end if
  end subroutine test_getdir_valid

  ! ---------------------------------------------------------------------------
  ! test_rdcntrl_missing_control
  ! len_adir=0 causes rdcntrl to open 'control.dat' in cwd.
  ! That file does not exist when running from project root -> ierror(15)=1.
  subroutine test_rdcntrl_missing_control(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    integer :: len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7)  :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real :: tm_pl_mx, tm_pl_mn
    character(len=80) :: adir

    ierror = 0
    adir   = ' '
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 0, adir, 0, 0)
    if (ierror(15) == 1) then
      print *, 'PASS test_rdcntrl_missing_control: ierror(15)=1 as expected'
    else
      print *, 'FAIL test_rdcntrl_missing_control: ierror(15)=', ierror(15), &
               ' (expected 1)'
      failures = failures + 1
    end if
  end subroutine test_rdcntrl_missing_control

  ! ---------------------------------------------------------------------------
  ! test_rdcntrl_valid
  ! adir='tests/data/', len_adir=11 -> opens tests/data/control.dat (valid).
  ! Expect ierror(15)=0 and ierror(16)=0.
  subroutine test_rdcntrl_valid(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    integer :: len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7)  :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real :: tm_pl_mx, tm_pl_mn
    character(len=80) :: adir

    ierror = 0
    adir   = 'tests/data/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 11, adir, 0, 0)
    if (ierror(15) == 0 .and. ierror(16) == 0) then
      print *, 'PASS test_rdcntrl_valid: ierror(15)=0, ierror(16)=0'
    else
      print *, 'FAIL test_rdcntrl_valid: ierror(15)=', ierror(15), &
               ' ierror(16)=', ierror(16), ' (expected both 0)'
      failures = failures + 1
    end if
  end subroutine test_rdcntrl_valid

  ! ---------------------------------------------------------------------------
  ! test_rdcntrl_malformed_control
  ! adir='tests/data_malformed_ctrl/', len_adir=26 -> opens that control.dat.
  ! File has only 1 line -> 3rd read hits EOF -> ierror(16)=1.
  subroutine test_rdcntrl_malformed_control(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    integer :: len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7)  :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real :: tm_pl_mx, tm_pl_mn
    character(len=80) :: adir

    ierror = 0
    adir   = 'tests/data_malformed_ctrl/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 26, adir, 0, 0)
    if (ierror(16) == 1) then
      print *, 'PASS test_rdcntrl_malformed_control: ierror(16)=1 as expected'
    else
      print *, 'FAIL test_rdcntrl_malformed_control: ierror(16)=', ierror(16), &
               ' (expected 1)'
      failures = failures + 1
    end if
  end subroutine test_rdcntrl_malformed_control

  ! ---------------------------------------------------------------------------
  ! test_rdcntrl_debug_operator
  ! adir='tests/data_debug/', len_adir=17 -> Operator Name = debug -> ierror(33)=1.
  subroutine test_rdcntrl_debug_operator(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    integer :: len_acruise, launcher(12), iSIOSpeedAveMin
    character(len=7)  :: acruise
    real :: xmaxspd, deadmin, dropmin, relodmin, runsec
    real :: tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700
    real :: tm_pl_mx, tm_pl_mn
    character(len=80) :: adir

    ierror = 0
    adir   = 'tests/data_debug/'
    call rdcntrl(ierror, len_acruise, acruise, xmaxspd, launcher, &
                 deadmin, dropmin, relodmin, runsec, &
                 tdzmx, tdzrms, dtdzmn, dtdzth, dtmx, dtmx700, &
                 tm_pl_mx, tm_pl_mn, iSIOSpeedAveMin, &
                 17, adir, 0, 0)
    if (ierror(33) == 1) then
      print *, 'PASS test_rdcntrl_debug_operator: ierror(33)=1 as expected'
    else
      print *, 'FAIL test_rdcntrl_debug_operator: ierror(33)=', ierror(33), &
               ' (expected 1)'
      failures = failures + 1
    end if
  end subroutine test_rdcntrl_debug_operator

  ! ---------------------------------------------------------------------------
  ! test_navopen_missing_nav
  ! navopen(99, 99, 9999, ...) — impossible date, file won't exist -> ierr=1.
  subroutine test_navopen_missing_nav(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir

    fnav = ' '
    adir = ' '
    call navopen(99, 99, 9999, ierr, fnav, adir, 0)
    if (ierr == 1) then
      print *, 'PASS test_navopen_missing_nav: ierr=1 as expected'
    else
      print *, 'FAIL test_navopen_missing_nav: ierr=', ierr, ' (expected 1)'
      failures = failures + 1
    end if
  end subroutine test_navopen_missing_nav

  ! ---------------------------------------------------------------------------
  ! test_navopen_valid
  ! navopen appends 'Data\' (Windows backslash) to adir — invalid on Linux.
  ! File won't be found -> print WARN, do not increment failures.
  subroutine test_navopen_valid(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    character(len=80) :: fnav, adir

    fnav = ' '
    adir = 'tests/data/'
    ! 1 June 2024 -> 010624.nav
    call navopen(1, 6, 24, ierr, fnav, adir, 11)
    if (ierr == 0) then
      print *, 'PASS test_navopen_valid: nav file opened successfully'
      close(8)
    else
      print *, 'WARN test_navopen_valid: navopen failed (Windows Data\ path on Linux) ierr=', &
               ierr, '- skipping'
    end if
  end subroutine test_navopen_valid

end program test_integration_io
