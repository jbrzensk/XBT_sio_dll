! tests/unit/test_sio_time.f90
program test_sio_time
  use sio_time
  implicit none
  integer :: failures = 0

  call test_timetohms_basic(failures)
  call test_timetohms_multiday(failures)
  call test_gettmtg_monday_noon(failures)
  call test_gettmtg_sunday_midnight(failures)
  call test_yrdy_jan1(failures)
  call test_yrdy_feb1_leap(failures)
  call test_yrdy_mar1_nonleap(failures)
  call test_yrdy_crossyear(failures)
  call test_compare_first_later(failures)
  call test_compare_first_earlier(failures)
  call test_compare_equal(failures)
  call test_findtime_later(failures)
  call test_findtime_earlier(failures)
  call test_findtime_equal(failures)

  if (failures == 0) then
    print *, 'test_sio_time: ALL TESTS PASSED'
    stop 0
  else
    print *, 'test_sio_time: FAILURES =', failures
    stop 1
  end if

contains

  subroutine test_timetohms_basic(failures)
    integer, intent(inout) :: failures
    integer :: ihr, imin, isec
    ! 3661 seconds = 1h 1m 1s
    call timetohms(3661.0, ihr, imin, isec)
    if (ihr /= 1 .or. imin /= 1 .or. isec /= 1) then
      print *, 'FAIL test_timetohms_basic: ihr=', ihr, ' imin=', imin, ' isec=', isec
      failures = failures + 1
    else
      print *, 'PASS test_timetohms_basic'
    end if
  end subroutine

  subroutine test_timetohms_multiday(failures)
    integer, intent(inout) :: failures
    integer :: ihr, imin, isec
    ! 86400+3600 = day+1hr → timetohms strips the day, returns 1:00:00
    call timetohms(90000.0, ihr, imin, isec)
    if (ihr /= 1 .or. imin /= 0 .or. isec /= 0) then
      print *, 'FAIL test_timetohms_multiday: ihr=', ihr, ' imin=', imin, ' isec=', isec
      failures = failures + 1
    else
      print *, 'PASS test_timetohms_multiday'
    end if
  end subroutine

  subroutine test_gettmtg_monday_noon(failures)
    integer, intent(inout) :: failures
    real :: timetag
    ! Monday(1) 12:00:00 = 86400 + 43200 = 129600
    call gettmtg(1, 12, 0, 0, timetag)
    if (abs(timetag - 129600.0) > 0.5) then
      print *, 'FAIL test_gettmtg_monday_noon: timetag =', timetag, ' expected 129600'
      failures = failures + 1
    else
      print *, 'PASS test_gettmtg_monday_noon'
    end if
  end subroutine

  subroutine test_gettmtg_sunday_midnight(failures)
    integer, intent(inout) :: failures
    real :: timetag
    ! Sunday(0) 00:00:00 = 0
    call gettmtg(0, 0, 0, 0, timetag)
    if (abs(timetag) > 0.5) then
      print *, 'FAIL test_gettmtg_sunday_midnight: timetag =', timetag, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_gettmtg_sunday_midnight'
    end if
  end subroutine

  subroutine test_yrdy_jan1(failures)
    integer, intent(inout) :: failures
    real :: yrday
    ! Jan 1 2024 = days since Jan 1, 2000: 24*365 + 6 leap years = 8766.0
    call yrdy(2024, 1, 1, 0, 0, 0, yrday)
    if (abs(yrday - 8766.0) > 0.01) then
      print *, 'FAIL test_yrdy_jan1: yrday =', yrday, ' expected 8766.0'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_jan1'
    end if
  end subroutine

  subroutine test_yrdy_feb1_leap(failures)
    integer, intent(inout) :: failures
    real :: yrday
    ! Feb 1 2024 = 8766 + 31 = 8797.0
    call yrdy(2024, 2, 1, 0, 0, 0, yrday)
    if (abs(yrday - 8797.0) > 0.01) then
      print *, 'FAIL test_yrdy_feb1_leap: yrday =', yrday, ' expected 8797.0'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_feb1_leap'
    end if
  end subroutine

  subroutine test_yrdy_mar1_nonleap(failures)
    integer, intent(inout) :: failures
    real :: yrday
    ! Mar 1 2023 (non-leap): 23*365 + 6 leap yrs = 8401 + (31+28) = 8460.0
    call yrdy(2023, 3, 1, 0, 0, 0, yrday)
    if (abs(yrday - 8460.0) > 0.01) then
      print *, 'FAIL test_yrdy_mar1_nonleap: yrday =', yrday, ' expected 8460.0'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_mar1_nonleap'
    end if
  end subroutine

  subroutine test_yrdy_crossyear(failures)
    integer, intent(inout) :: failures
    real :: yrday_dec31, yrday_jan1
    ! Dec 31 2024 < Jan 1 2025 (monotonically increasing across year boundary)
    ! Dec 31 2024: 2024 is leap (366 days), so day 366 = 8766 + 365 = 9131.0
    call yrdy(2024, 12, 31, 0, 0, 0, yrday_dec31)
    ! Jan 1 2025: 25*365 + 7 leap yrs = 9132.0
    call yrdy(2025, 1, 1, 0, 0, 0, yrday_jan1)
    if (yrday_jan1 <= yrday_dec31) then
      print *, 'FAIL test_yrdy_crossyear: jan1_2025=', yrday_jan1, ' dec31_2024=', yrday_dec31
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_crossyear'
    end if
  end subroutine

  subroutine test_compare_first_later(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    ! First date 2024/01/02, second 2024/01/01 → iflg=1
    call compare(2, 1, 2024, 12, 0, 0, 1, 1, 2024, 12, 0, 0, iflg)
    if (iflg /= 1) then
      print *, 'FAIL test_compare_first_later: iflg =', iflg, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_compare_first_later'
    end if
  end subroutine

  subroutine test_compare_first_earlier(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    ! First date 2024/01/01, second 2024/01/02 → iflg=0
    call compare(1, 1, 2024, 12, 0, 0, 2, 1, 2024, 12, 0, 0, iflg)
    if (iflg /= 0) then
      print *, 'FAIL test_compare_first_earlier: iflg =', iflg, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_compare_first_earlier'
    end if
  end subroutine

  subroutine test_findtime_later(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    ! incoming 13:00:00 > reference 12:00:00 → iflg=1
    call findtime(12, 0, 0, 13, 0, 0, iflg)
    if (iflg /= 1) then
      print *, 'FAIL test_findtime_later: iflg =', iflg, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_findtime_later'
    end if
  end subroutine

  subroutine test_findtime_earlier(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    ! incoming 11:00:00 < reference 12:00:00 → iflg=0
    call findtime(12, 0, 0, 11, 0, 0, iflg)
    if (iflg /= 0) then
      print *, 'FAIL test_findtime_earlier: iflg =', iflg, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_findtime_earlier'
    end if
  end subroutine

  subroutine test_findtime_equal(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    ! equal times → iflg=0
    call findtime(12, 30, 45, 12, 30, 45, iflg)
    if (iflg /= 0) then
      print *, 'FAIL test_findtime_equal: iflg =', iflg, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_findtime_equal'
    end if
  end subroutine

  subroutine test_compare_equal(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    ! identical date/time → iflg=0 (not strictly more recent)
    call compare(1, 1, 2024, 12, 30, 45, 1, 1, 2024, 12, 30, 45, iflg)
    if (iflg /= 0) then
      print *, 'FAIL test_compare_equal: iflg =', iflg, ' expected 0'
      failures = failures + 1
    else
      print *, 'PASS test_compare_equal'
    end if
  end subroutine

end program test_sio_time
