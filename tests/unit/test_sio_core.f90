! tests/unit/test_sio_core.f90
program test_sio_core
  use sio_core
  implicit none
  integer :: failures = 0

  call test_wrdrpstn_sets_error_on_missing_file(failures)
  call test_prstat_sets_error_on_missing_file(failures)

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

  subroutine test_prstat_sets_error_on_missing_file(failures)
    integer, intent(inout) :: failures
    integer :: ido
    integer :: iDropNo(10), iTubeNo(10)
    real    :: c700m(10), cLat(10), cLon(10), csst(10)
    integer :: ihour(10), imin_arr(10), isec(10)
    integer :: iday(10), imonth(10), iyear(10)
    integer :: icheckprof(10), iedited(10), iNavNo(10), ixmit(10)
    integer :: ierror(50)
    ierror = 0; ido = 0
    call prstat(ido, iDropNo, iTubeNo, c700m, cLat, cLon, ihour, &
                imin_arr, isec, iday, imonth, iyear, icheckprof, &
                iedited, iNavNo, csst, ixmit, ierror)
    ! Should set ierror(25)=1 (stations.dat not found) or ierror(7)=1 (no siodir.txt)
    if (ierror(25) /= 1 .and. ierror(7) /= 1 .and. ierror(17) /= 1) then
      print *, 'FAIL test_prstat_sets_error_on_missing_file'
      failures = failures + 1
    else
      print *, 'PASS test_prstat_sets_error_on_missing_file'
    end if
  end subroutine

end program test_sio_core
