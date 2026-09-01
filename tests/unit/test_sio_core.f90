! tests/unit/test_sio_core.f90
program test_sio_core
  use sio_core
  implicit none
  integer :: failures = 0

  call test_wrdrpstn_sets_error_on_missing_file(failures)
  call test_prstat_sets_error_on_missing_file(failures)
  call test_make_dos_date_single_digit(failures)
  call test_make_dos_date_double_digit(failures)
  call test_make_dos_date_epoch(failures)

  if (failures == 0) then
    print *, 'test_sio_core: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_sio_core: FAILURES =', failures
    stop 1
  end if

contains

  ! wrdrpstn with no stations.dat present must set ierror(25)=1 or ierror(29)=1
  ! or ierror(7)=1/ierror(17)=1 (getdir failure causes early return)
  subroutine test_wrdrpstn_sets_error_on_missing_file(failures)
    integer, intent(inout) :: failures
    integer :: ierror(50)
    ierror = 0
    call wrdrpstn(1, 1, 15.5, 1, 6, 2024, 12, 0, 0, ierror, 30.0, 200.0)
    if (ierror(25) /= 1 .and. ierror(29) /= 1 .and. ierror(7) /= 1 .and. ierror(17) /= 1) then
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

  ! make_dos_date: single-digit day and month → '0d', '0m', '2024'
  ! Exercises the iday <= 9 and imon <= 9 branches.
  subroutine test_make_dos_date_single_digit(failures)
    integer, intent(inout) :: failures
    character(len=2) :: adosday, adosmon
    character(len=4) :: adosyear
    call make_dos_date(1, 6, 2024, adosday, adosmon, adosyear)
    if (adosday /= '01' .or. adosmon /= '06' .or. adosyear /= '2024') then
      print *, 'FAIL test_make_dos_date_single_digit: day="', adosday, &
               '" mon="', adosmon, '" year="', adosyear, '"'
      failures = failures + 1
    else
      print *, 'PASS test_make_dos_date_single_digit'
    end if
  end subroutine

  ! make_dos_date: double-digit day and month → 'dd', 'mm', '2023'
  ! Exercises the iday > 9 and imon > 9 branches.
  subroutine test_make_dos_date_double_digit(failures)
    integer, intent(inout) :: failures
    character(len=2) :: adosday, adosmon
    character(len=4) :: adosyear
    call make_dos_date(15, 12, 2023, adosday, adosmon, adosyear)
    if (adosday /= '15' .or. adosmon /= '12' .or. adosyear /= '2023') then
      print *, 'FAIL test_make_dos_date_double_digit: day="', adosday, &
               '" mon="', adosmon, '" year="', adosyear, '"'
      failures = failures + 1
    else
      print *, 'PASS test_make_dos_date_double_digit'
    end if
  end subroutine

  ! make_dos_date at epoch year 2000 and single-digit day/month
  subroutine test_make_dos_date_epoch(failures)
    integer, intent(inout) :: failures
    character(len=2) :: adosday, adosmon
    character(len=4) :: adosyear
    call make_dos_date(9, 9, 2000, adosday, adosmon, adosyear)
    if (adosday /= '09' .or. adosmon /= '09' .or. adosyear /= '2000') then
      print *, 'FAIL test_make_dos_date_epoch: day="', adosday, &
               '" mon="', adosmon, '" year="', adosyear, '"'
      failures = failures + 1
    else
      print *, 'PASS test_make_dos_date_epoch'
    end if
  end subroutine

end program test_sio_core
