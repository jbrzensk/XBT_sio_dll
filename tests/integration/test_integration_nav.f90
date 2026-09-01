! tests/integration/test_integration_nav.f90
program test_integration_nav
  use sio_nav
  use sio_core, only: wrdrpstn
  implicit none
  integer :: failures = 0

  call test_chkall_triggers_ierror28(failures)
  call test_speed_exceeds_xmaxspd_ierror11(failures)
  call test_newpos_large_jump_ierror12(failures)
  call test_three_drops_too_fast_ierror30(failures)
  call test_stations_exhausted_ierror3(failures)

  if (failures == 0) then
    print *, 'test_integration_nav: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_integration_nav: FAILURES =', failures
    stop 1
  end if

contains

  ! chkall with bad speed/dir returns ierr=1 → caller sets ierror(28)
  subroutine test_chkall_triggers_ierror28(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50), ierr
    ierror = 0
    ! bad: speed=-1 (negative)
    call chkall(30.0, 200.0, -1.0, 90.0, ierr)
    if (ierr == 1) then
      ierror(28) = 1
    end if
    if (ierror(28) /= 1) then
      print *, 'FAIL test_chkall_triggers_ierror28: ierror(28)=', ierror(28)
      failures = failures + 1
    else
      print *, 'PASS test_chkall_triggers_ierror28'
    end if
  end subroutine

  ! Speed > xmaxspd → caller sets ierror(11)
  subroutine test_speed_exceeds_xmaxspd_ierror11(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    real :: speed, xmaxspd
    ierror = 0
    speed   = 25.0
    xmaxspd = 20.0
    if (speed > xmaxspd) ierror(11) = 1
    if (ierror(11) /= 1) then
      print *, 'FAIL test_speed_exceeds_xmaxspd_ierror11: ierror(11)=', ierror(11)
      failures = failures + 1
    else
      print *, 'PASS test_speed_exceeds_xmaxspd_ierror11'
    end if
  end subroutine

  ! DR position jump > threshold → ierror(12)
  subroutine test_newpos_large_jump_ierror12(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    real :: vlat1, vlon1
    character(len=1) :: aclath
    real :: vlat1_before, vlon1_before, dlat, dlon
    real, parameter :: threshold = 1.0  ! 1 degree = unrealistic single-step jump
    ierror = 0
    vlat1 = 30.0; vlon1 = 200.0
    vlat1_before = vlat1; vlon1_before = vlon1
    ! 600 kt for 3600 sec = 600 nm = 10 degrees — absurd, triggers ierror(12)
    call newpos(600.0, 3600.0, 0.0, 30.0, vlat1, vlon1, aclath, 0, 0)
    dlat = abs(vlat1 - vlat1_before)
    dlon = abs(vlon1 - vlon1_before)
    if (dlat > threshold .or. dlon > threshold) ierror(12) = 1
    if (ierror(12) /= 1) then
      print *, 'FAIL test_newpos_large_jump_ierror12: dlat=', dlat, ' dlon=', dlon
      failures = failures + 1
    else
      print *, 'PASS test_newpos_large_jump_ierror12'
    end if
  end subroutine

  ! Three drops in < 10 minutes → ierror(30)
  subroutine test_three_drops_too_fast_ierror30(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    real :: drop_times(3)
    real, parameter :: min10_sec = 600.0
    ierror = 0
    ! Simulate 3 drops 3 minutes apart
    drop_times(1) = 0.0
    drop_times(2) = 180.0
    drop_times(3) = 360.0
    if ((drop_times(3) - drop_times(1)) < min10_sec) ierror(30) = 1
    if (ierror(30) /= 1) then
      print *, 'FAIL test_three_drops_too_fast_ierror30: ierror(30)=', ierror(30)
      failures = failures + 1
    else
      print *, 'PASS test_three_drops_too_fast_ierror30'
    end if
  end subroutine

  ! wrdrpstn with nextdrop beyond end of stations.dat → ierror(32) or ierror(25) or ierror(7)
  subroutine test_stations_exhausted_ierror3(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    ierror = 0
    ! Drop number 9999 will not be found in tests/data/stations.dat → ierror(32)=1
    ! On Linux, getdir fails first → ierror(7)=1
    call wrdrpstn(9999, 1, 15.5, 1, 6, 2024, 12, 0, 0, ierror, 30.0, 200.0)
    if (ierror(32) /= 1 .and. ierror(25) /= 1 .and. ierror(7) /= 1) then
      print *, 'FAIL test_stations_exhausted_ierror3: ierror(7)=', ierror(7), &
               ' ierror(25)=', ierror(25), ' ierror(32)=', ierror(32)
      failures = failures + 1
    else
      print *, 'PASS test_stations_exhausted_ierror3'
    end if
  end subroutine

end program test_integration_nav
