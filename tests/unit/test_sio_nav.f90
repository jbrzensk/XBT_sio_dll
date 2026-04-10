! tests/unit/test_sio_nav.f90
program test_sio_nav
  use sio_nav
  implicit none
  integer :: failures = 0

  call test_chkall_valid(failures)
  call test_chkall_bad_speed(failures)
  call test_chkall_bad_dir(failures)
  call test_chkall_bad_lat(failures)
  call test_chkall_bad_lon(failures)
  call test_chkwrite_valid(failures)
  call test_chkwrite_bad_lat(failures)
  call test_chkwrite_bad_lon(failures)
  call test_newpos_north(failures)
  call test_newpos_east(failures)
  call test_newpos_lon_wrap(failures)

  if (failures == 0) then
    print *, 'test_sio_nav: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_sio_nav: FAILURES =', failures
    stop 1
  end if

contains

  subroutine test_chkall_valid(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, 200.0, 10.0, 90.0, ierr)
    if (ierr /= 0) then
      print *, 'FAIL test_chkall_valid: ierr =', ierr, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_chkall_valid'
    end if
  end subroutine

  subroutine test_chkall_bad_speed(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, 200.0, 100.0, 90.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_bad_speed: ierr =', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkall_bad_speed'
    end if
  end subroutine

  subroutine test_chkall_bad_dir(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, 200.0, 10.0, 400.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_bad_dir: ierr =', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkall_bad_dir'
    end if
  end subroutine

  subroutine test_chkall_bad_lat(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(100.0, 200.0, 10.0, 90.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_bad_lat: ierr =', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkall_bad_lat'
    end if
  end subroutine

  subroutine test_chkall_bad_lon(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkall(30.0, 400.0, 10.0, 90.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkall_bad_lon: ierr =', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkall_bad_lon'
    end if
  end subroutine

  subroutine test_chkwrite_valid(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkwrite(30.0, 200.0, ierr)
    if (ierr /= 0) then
      print *, 'FAIL test_chkwrite_valid: ierr =', ierr, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_chkwrite_valid'
    end if
  end subroutine

  subroutine test_chkwrite_bad_lat(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkwrite(95.0, 200.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkwrite_bad_lat: ierr =', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkwrite_bad_lat'
    end if
  end subroutine

  subroutine test_chkwrite_bad_lon(failures)
    integer, intent(inout) :: failures
    integer :: ierr
    call chkwrite(30.0, 400.0, ierr)
    if (ierr /= 1) then
      print *, 'FAIL test_chkwrite_bad_lon: ierr =', ierr, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_chkwrite_bad_lon'
    end if
  end subroutine

  subroutine test_newpos_north(failures)
    integer, intent(inout) :: failures
    real :: vlat1, vlon1
    character(len=1) :: aclath
    ! Heading north at 60 kt for 3600 sec = 60 nm = 1 degree lat north
    vlat1 = 30.0; vlon1 = 200.0
    call newpos(60.0, 3600.0, 0.0, 30.0, vlat1, vlon1, aclath, 0, 0)
    if (abs(vlat1 - 31.0) > 0.05 .or. aclath /= 'N') then
      print *, 'FAIL test_newpos_north: vlat1=', vlat1, ' aclath=', aclath
      failures = failures + 1
    else
      print *, 'PASS test_newpos_north'
    end if
  end subroutine

  subroutine test_newpos_east(failures)
    integer, intent(inout) :: failures
    real :: vlat1, vlon1
    character(len=1) :: aclath
    ! Heading east at 60 kt for 3600 sec at lat=0: lon increases 1 degree
    vlat1 = 0.0; vlon1 = 200.0
    call newpos(60.0, 3600.0, 90.0, 0.0, vlat1, vlon1, aclath, 0, 0)
    if (abs(vlon1 - 201.0) > 0.05) then
      print *, 'FAIL test_newpos_east: vlon1=', vlon1, ' expected ~201.0'
      failures = failures + 1
    else
      print *, 'PASS test_newpos_east'
    end if
  end subroutine

  subroutine test_newpos_lon_wrap(failures)
    integer, intent(inout) :: failures
    real :: vlat1, vlon1
    character(len=1) :: aclath
    ! Heading east at 60 kt for 3600 sec starting at lon=359.5: should wrap to ~0.5
    vlat1 = 0.0; vlon1 = 359.5
    call newpos(60.0, 3600.0, 90.0, 0.0, vlat1, vlon1, aclath, 0, 0)
    if (vlon1 > 360.0 .or. vlon1 < 0.0) then
      print *, 'FAIL test_newpos_lon_wrap: vlon1=', vlon1, ' out of 0-360 range'
      failures = failures + 1
    else
      print *, 'PASS test_newpos_lon_wrap: vlon1=', vlon1
    end if
  end subroutine

end program test_sio_nav
