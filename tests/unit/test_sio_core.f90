! tests/unit/test_sio_core.f90
program test_sio_core
  use sio_core
  implicit none
  integer :: failures = 0

  call test_wrdrpstn_sets_error_on_missing_file(failures)
  call test_prstat_no_crash_with_valid_inputs(failures)

  if (failures == 0) then
    print *, 'test_sio_core: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_sio_core: FAILURES =', failures
    stop 1
  end if

contains

  ! wrdrpstn with no stations.dat present must set ierror(25)=1 or ierror(29)=1
  subroutine test_wrdrpstn_sets_error_on_missing_file(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    ierror = 0
    call wrdrpstn(1, 1, 15.5, 1, 6, 2024, 12, 0, 0, ierror, 30.0, 200.0)
    if (ierror(25) /= 1 .and. ierror(29) /= 1) then
      print *, 'FAIL test_wrdrpstn_sets_error_on_missing_file: ierror(25)=', &
               ierror(25), ' ierror(29)=', ierror(29)
      failures = failures + 1
    else
      print *, 'PASS test_wrdrpstn_sets_error_on_missing_file'
    end if
  end subroutine

  ! prstat should not crash with valid inputs; iw=0 means no file writes
  subroutine test_prstat_no_crash_with_valid_inputs(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    ierror = 0
    ! iw=0 means no log writing — safe to call with ifile=0
    call prstat(0, 1, 1, 15.5, 30.0, 200.0, 12, 0, 0, 1, 6, 2024, &
                ierror, 0, ' ', 0, 0)
    ! Test passes if we get here without crashing
    print *, 'PASS test_prstat_no_crash_with_valid_inputs'
  end subroutine

end program test_sio_core
