! tests/unit/test_sio_time.f90
program test_sio_time
  use sio_time
  implicit none
  integer :: failures = 0

  call test_timetohms_basic(failures)
  call test_timetohms_multiday(failures)
  call test_timetohms_zero(failures)
  call test_gettmtg_monday_noon(failures)
  call test_gettmtg_sunday_midnight(failures)
  call test_yrdy_jan1(failures)
  call test_yrdy_feb1_leap(failures)
  call test_yrdy_mar1_nonleap(failures)
  call test_yrdy_crossyear(failures)
  call test_yrdy_epoch(failures)
  call test_yrdy_two_digit_year(failures)
  call test_yrdy_minute_resolution(failures)
  call test_drops_too_close_10min(failures)
  call test_drops_too_close_11min(failures)
  call test_drops_too_close_outside_margin(failures)
  call test_drops_too_close_no_history(failures)
  call test_drops_too_close_real_sweep(failures)
  call test_yrdy_before_epoch(failures)
  call test_compare_first_later(failures)
  call test_compare_first_earlier(failures)
  call test_compare_equal(failures)
  call test_compare_year_later(failures)
  call test_compare_month_later(failures)
  call test_compare_hour_later(failures)
  call test_findtime_later(failures)
  call test_findtime_earlier(failures)
  call test_findtime_equal(failures)
  call test_dayofw_valid_range(failures)
  call test_getdat_valid_range(failures)

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

  ! timetohms(0.0): ix=0, no day stripping → 0:00:00
  subroutine test_timetohms_zero(failures)
    integer, intent(inout) :: failures
    integer :: ihr, imin, isec
    call timetohms(0.0, ihr, imin, isec)
    if (ihr /= 0 .or. imin /= 0 .or. isec /= 0) then
      print *, 'FAIL test_timetohms_zero: ihr=', ihr, ' imin=', imin, ' isec=', isec
      failures = failures + 1
    else
      print *, 'PASS test_timetohms_zero'
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
    ! Jan 1 2024 = days since Jan 1, 2020: 4*365 + 1 leap year (2020) = 1461.0
    call yrdy(2024, 1, 1, 0, 0, 0, yrday)
    if (abs(yrday - 1461.0) > 0.01) then
      print *, 'FAIL test_yrdy_jan1: yrday =', yrday, ' expected 1461.0'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_jan1'
    end if
  end subroutine

  subroutine test_yrdy_feb1_leap(failures)
    integer, intent(inout) :: failures
    real :: yrday
    ! Feb 1 2024 = 1461 + 31 = 1492.0
    call yrdy(2024, 2, 1, 0, 0, 0, yrday)
    if (abs(yrday - 1492.0) > 0.01) then
      print *, 'FAIL test_yrdy_feb1_leap: yrday =', yrday, ' expected 1492.0'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_feb1_leap'
    end if
  end subroutine

  subroutine test_yrdy_mar1_nonleap(failures)
    integer, intent(inout) :: failures
    real :: yrday
    ! Mar 1 2023 (non-leap): 3*365 + 1 leap yr (2020) = 1096 + (31+28) = 1155.0
    call yrdy(2023, 3, 1, 0, 0, 0, yrday)
    if (abs(yrday - 1155.0) > 0.01) then
      print *, 'FAIL test_yrdy_mar1_nonleap: yrday =', yrday, ' expected 1155.0'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_mar1_nonleap'
    end if
  end subroutine

  subroutine test_yrdy_crossyear(failures)
    integer, intent(inout) :: failures
    real :: yrday_dec31, yrday_jan1
    ! Dec 31 2024 < Jan 1 2025 (monotonically increasing across year boundary)
    call yrdy(2024, 12, 31, 0, 0, 0, yrday_dec31)
    call yrdy(2025, 1, 1, 0, 0, 0, yrday_jan1)
    if (yrday_jan1 <= yrday_dec31) then
      print *, 'FAIL test_yrdy_crossyear: jan1_2025=', yrday_jan1, ' dec31_2024=', yrday_dec31
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_crossyear'
    end if
  end subroutine

  ! yrdy at epoch (Jan 1, 2020) = 0.0
  subroutine test_yrdy_epoch(failures)
    integer, intent(inout) :: failures
    real :: yrday
    call yrdy(2020, 1, 1, 0, 0, 0, yrday)
    if (abs(yrday) > 0.01) then
      print *, 'FAIL test_yrdy_epoch: yrday=', yrday, ' expected 0.0'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_epoch'
    end if
  end subroutine

  ! Callers pass 2-digit years (stations.dat, .nav files, Seas); yrdy must
  ! treat 26 the same as 2026.
  subroutine test_yrdy_two_digit_year(failures)
    integer, intent(inout) :: failures
    real :: y2, y4
    call yrdy(26, 9, 8, 12, 2, 0, y2)
    call yrdy(2026, 9, 8, 12, 2, 0, y4)
    if (abs(y2 - y4) > 0.0001) then
      print *, 'FAIL test_yrdy_two_digit_year: yrdy(26)=', y2, ' yrdy(2026)=', y4
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_two_digit_year'
    end if
  end subroutine

  ! Drops 5 minutes apart (9/8/26 11:57 and 12:02) must differ by ~5 minutes
  ! with 2-digit years; single-precision magnitude must not swamp minutes.
  subroutine test_yrdy_minute_resolution(failures)
    integer, intent(inout) :: failures
    real :: a, b, dmin
    call yrdy(26, 9, 8, 11, 57, 0, a)
    call yrdy(26, 9, 8, 12, 2, 0, b)
    dmin = (b - a) * 1440.0
    if (abs(dmin - 5.0) > 1.1) then
      print *, 'FAIL test_yrdy_minute_resolution: diff minutes=', dmin, ' expected ~5'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_minute_resolution'
    end if
  end subroutine

  ! drops_too_close(yrday1, yrday2): 3-drops-in-10-minutes failsafe with a
  ! 1.1 minute margin.  Base value 100.0 keeps single precision exact enough
  ! to test the threshold itself.
  subroutine check_close(name, gap_min, yrday1, expected, failures)
    character(len=*), intent(in) :: name
    real,    intent(in)    :: gap_min, yrday1
    logical, intent(in)    :: expected
    integer, intent(inout) :: failures
    logical :: got
    got = drops_too_close(yrday1, 100.0 + gap_min / 1440.0)
    if (got .neqv. expected) then
      print *, 'FAIL ', name, ': gap=', gap_min, ' got=', got, ' expected=', expected
      failures = failures + 1
    else
      print *, 'PASS ', name
    end if
  end subroutine

  ! Drops 5 min apart -> 3 drops span exactly 10 min -> must trip
  subroutine test_drops_too_close_10min(failures)
    integer, intent(inout) :: failures
    call check_close('test_drops_too_close_10min', 10.0, 100.0, .true., failures)
  end subroutine

  subroutine test_drops_too_close_11min(failures)
    integer, intent(inout) :: failures
    call check_close('test_drops_too_close_11min', 11.0, 100.0, .true., failures)
  end subroutine

  subroutine test_drops_too_close_outside_margin(failures)
    integer, intent(inout) :: failures
    call check_close('test_drops_too_close_outside_margin', 11.3, 100.0, .false., failures)
  end subroutine

  ! yrday1 <= 0 means fewer than 3 prior drops -> never trip
  subroutine test_drops_too_close_no_history(failures)
    integer, intent(inout) :: failures
    call check_close('test_drops_too_close_no_history', 2.0, -1.0, .false., failures)
  end subroutine

  ! Real 2-digit dates through yrdy, swept over every 7 s start time in the
  ! 11:00 hour of 9/8/26 (rounding depends on the time of day).  yrdy must
  ! resolve well enough that a 10:40 gap is ALWAYS inside the 10 + 1.1 min
  ! window and an 11:30 gap is NEVER inside it.  (Single-precision resolution
  ! at 2026 is ~21 s, so gaps within ~0.35 min of 11.1 are inherently
  ! ambiguous and are not tested.)
  subroutine test_drops_too_close_real_sweep(failures)
    integer, intent(inout) :: failures
    integer :: t0, nbad
    real :: a, b
    nbad = 0
    do t0 = 11*3600, 12*3600 - 1, 7
      call yrdy(26, 9, 8, t0/3600, mod(t0,3600)/60, mod(t0,60), a)
      call yrdy(26, 9, 8, (t0+640)/3600, mod(t0+640,3600)/60, mod(t0+640,60), b)
      if (.not. drops_too_close(a, b)) nbad = nbad + 1
      call yrdy(26, 9, 8, (t0+690)/3600, mod(t0+690,3600)/60, mod(t0+690,60), b)
      if (drops_too_close(a, b)) nbad = nbad + 1
    end do
    if (nbad > 0) then
      print *, 'FAIL test_drops_too_close_real_sweep: misclassified=', nbad
      failures = failures + 1
    else
      print *, 'PASS test_drops_too_close_real_sweep'
    end if
  end subroutine

  ! Dates before the epoch are negative but still continuous across it
  subroutine test_yrdy_before_epoch(failures)
    integer, intent(inout) :: failures
    real :: a, b
    call yrdy(2019, 12, 31, 0, 0, 0, a)
    call yrdy(2020, 1, 1, 0, 0, 0, b)
    if (abs((b - a) - 1.0) > 0.0001) then
      print *, 'FAIL test_yrdy_before_epoch: diff=', b - a, ' expected 1.0'
      failures = failures + 1
    else
      print *, 'PASS test_yrdy_before_epoch'
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

  ! compare: first year > second year → iflg=1 (exercises nyear > iyear branch)
  subroutine test_compare_year_later(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    call compare(1, 1, 2025, 0, 0, 0, 1, 1, 2024, 0, 0, 0, iflg)
    if (iflg /= 1) then
      print *, 'FAIL test_compare_year_later: iflg=', iflg, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_compare_year_later'
    end if
  end subroutine

  ! compare: same year, first month > second month → iflg=1 (nyear==iyear path, nmon>imon branch)
  subroutine test_compare_month_later(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    call compare(1, 6, 2024, 0, 0, 0, 1, 3, 2024, 0, 0, 0, iflg)
    if (iflg /= 1) then
      print *, 'FAIL test_compare_month_later: iflg=', iflg, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_compare_month_later'
    end if
  end subroutine

  ! compare: same date, first hour > second hour → iflg=1 (exercises nhr > ihr branch)
  subroutine test_compare_hour_later(failures)
    integer, intent(inout) :: failures
    integer :: iflg
    call compare(1, 6, 2024, 14, 0, 0, 1, 6, 2024, 12, 0, 0, iflg)
    if (iflg /= 1) then
      print *, 'FAIL test_compare_hour_later: iflg=', iflg, ' expected 1'
      failures = failures + 1
    else
      print *, 'PASS test_compare_hour_later'
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

  ! dayofw: smoke test using system clock — only verify result is in [0,6]
  subroutine test_dayofw_valid_range(failures)
    integer, intent(inout) :: failures
    integer :: iweekday
    call dayofw(iweekday)
    if (iweekday < 0 .or. iweekday > 6) then
      print *, 'FAIL test_dayofw_valid_range: iweekday=', iweekday, ' expected 0-6'
      failures = failures + 1
    else
      print *, 'PASS test_dayofw_valid_range: iweekday=', iweekday
    end if
  end subroutine

  ! getdat: smoke test using system clock — verify year, month, day are valid
  subroutine test_getdat_valid_range(failures)
    integer, intent(inout) :: failures
    integer(kind=2) :: iyr, imo, iday
    call getdat(iyr, imo, iday)
    if (iyr < 2000 .or. imo < 1 .or. imo > 12 .or. iday < 1 .or. iday > 31) then
      print *, 'FAIL test_getdat_valid_range: iyr=', iyr, ' imo=', imo, ' iday=', iday
      failures = failures + 1
    else
      print *, 'PASS test_getdat_valid_range: iyr=', iyr, ' imo=', imo, ' iday=', iday
    end if
  end subroutine

end program test_sio_time
