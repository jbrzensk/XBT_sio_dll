! src/sio_time.f90
module sio_time
  implicit none
  private
  public :: compare, dayofw, gettmtg, findtime, yrdy, timetohms, gettim, getdat
  public :: drops_too_close, pc_new_minute, clock_seconds
  public :: set_test_clock, advance_test_clock, use_real_clock, pc_clock_count

  ! Test clock. Off in production (Seas never calls the setters, which are
  ! not exported): gettim, getdat, dayofw and pc_clock_count read the PC
  ! clock. A test turns it on to get simulated time instead, so sioloop can
  ! be driven second by second without waiting.
  logical         :: tclock_on = .false.
  integer(kind=8) :: tclock_sec = 0            ! seconds since 00:00 of tclock_ymd
  integer         :: tclock_ymd(3) = (/ 2000, 1, 1 /)

contains

  ! Compare two date/times; set iflg=1 if first (n) is later than second (i).
  ! siosub.for:819.
  ! nday,nmon,nyear,nhr,nmin,nsec — first date/time (e.g. from .nav file)
  ! iday,imon,iyear,ihr,imin,isec — second date/time (e.g. from navtrk.dat)
  ! iflg — output: 0=don't use first, 1=first is more recent
  subroutine compare(nday, nmon, nyear, nhr, nmin, nsec, &
                     iday, imon, iyear, ihr, imin, isec, iflg)
    integer, intent(in)  :: nday, nmon, nyear, nhr, nmin, nsec
    integer, intent(in)  :: iday, imon, iyear, ihr, imin, isec
    integer, intent(out) :: iflg
    iflg = 0
    if (nyear > iyear) then
      iflg = 1
      return
    elseif (nyear < iyear) then
      return
    end if
    if (nmon > imon) then
      iflg = 1
      return
    elseif (nmon < imon) then
      return
    end if
    if (nday > iday) then
      iflg = 1
      return
    elseif (nday < iday) then
      return
    end if
    if (nhr > ihr) then
      iflg = 1
      return
    elseif (nhr < ihr) then
      return
    end if
    if (nmin > imin) then
      iflg = 1
      return
    elseif (nmin < imin) then
      return
    end if
    if (nsec > isec) then
      iflg = 1
      return
    elseif (nsec < isec) then
      return
    end if
  end subroutine compare

  ! Return current day-of-week: 0=Sun,1=Mon,...,6=Sat. siosub.for:912.
  ! Uses DATE_AND_TIME intrinsic.
  ! iweekday — output: 0–6
  subroutine dayofw(iweekday)
    integer, intent(out) :: iweekday
    integer :: idt(8)
    integer :: y, m, d, k, j, h
    call clock_now(idt)
    ! DATE_AND_TIME values(7) is not day-of-week in standard Fortran.
    ! Use the date to compute day-of-week via Zeller's congruence.
    ! idt(1)=year, idt(2)=month, idt(3)=day
    y = idt(1)
    m = idt(2)
    d = idt(3)
    ! Zeller's congruence (0=Sat,1=Sun,...,6=Fri) — adjust to 0=Sun..6=Sat
    if (m < 3) then
      m = m + 12
      y = y - 1
    end if
    k = mod(y, 100)
    j = y / 100
    h = mod(d + (13*(m+1))/5 + k + k/4 + j/4 + 5*j, 7)
    ! h: 0=Sat,1=Sun,2=Mon,...,6=Fri → convert to 0=Sun..6=Sat
    iweekday = mod(h + 6, 7)
  end subroutine dayofw

  ! Compute GPS timetag (seconds since Sunday 00:00:00). siosub.for:1768.
  ! iweekday — 0=Sun … 6=Sat
  ! ihr,imin,isec — current time
  ! timetag — output: seconds into GPS week
  subroutine gettmtg(iweekday, ihr, imin, isec, timetag)
    integer, intent(in)  :: iweekday, ihr, imin, isec
    real,    intent(out) :: timetag
    timetag = real(iweekday) * 86400.0 + real(ihr) * 3600.0 &
              + real(imin) * 60.0 + real(isec)
  end subroutine gettmtg

  ! Compare two times; iflg=1 if (ihr,imin,isec) > (nhr,nmin,nsec). siosub.for:1575.
  ! nhr,nmin,nsec — reference time (from nav file)
  ! ihr,imin,isec — incoming time
  ! iflg — output: 0=incoming not greater, 1=incoming greater
  subroutine findtime(nhr, nmin, nsec, ihr, imin, isec, iflg)
    integer, intent(in)  :: nhr, nmin, nsec, ihr, imin, isec
    integer, intent(out) :: iflg
    integer :: itotal, ntotal
    itotal = ihr * 3600 + imin * 60 + isec
    ntotal = nhr * 3600 + nmin * 60 + nsec
    if (itotal > ntotal) then
      iflg = 1
    else
      iflg = 0
    end if
  end subroutine findtime

  ! Convert year/month/day/hour/min/sec to days since Jan 1, 2020 (epoch=0).
  ! sio.for:3550 — modernized: returns epoch-based value.
  ! kkyr,kmo,kday,khr,kmn,ksc — input date/time. kkyr may be 2-digit (yy < 100
  !          is taken as 20yy; stations.dat, .nav files and Seas all pass yy)
  ! yrday — output: fractional days since Jan 1, 2020 (monotonically increasing
  !          across years, negative before 2020; differences in days, so
  !          multiply by 1440 for minutes)
  ! The 2020 epoch keeps the single-precision result small: resolution is
  ! ~21 s through 2031 and ~42 s through 2042 (an epoch of 2000 gave ~84 s,
  ! too coarse for the 3-drops-in-10-minutes failsafe). All callers use only
  ! differences/comparisons, so the epoch itself does not matter to them.
  subroutine yrdy(kkyr, kmo, kday, khr, kmn, ksc, yrday)
    integer, intent(in)  :: kkyr, kmo, kday, khr, kmn, ksc
    real,    intent(out) :: yrday
    integer, parameter :: epoch = 2020
    ! L(epoch-1) = 2019/4 - 2019/100 + 2019/400 = 504 - 20 + 5 (integer division)
    integer, parameter :: epoch_leaps = 489
    integer :: days_in_month(12)
    integer :: i, leap, doy, ydays, lcount, kyr
    data days_in_month / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
    kyr = kkyr
    if (kyr < 100) kyr = kyr + 2000
    ! Gregorian leap year for this year
    leap = 0
    if (mod(kyr, 4) == 0) then
      leap = 1
      if (mod(kyr, 100) == 0 .and. mod(kyr, 400) /= 0) leap = 0
    end if
    days_in_month(2) = 28 + leap
    ! Day of year (1-based)
    doy = kday
    do i = 1, kmo - 1
      doy = doy + days_in_month(i)
    end do
    ! Leap years in [epoch, kyr-1] (negative count of [kyr, epoch-1] when
    ! kyr < epoch): L(kyr-1) - L(epoch-1), L(y) = y/4 - y/100 + y/400.
    ! Verified: lcount(2024)=1, lcount(2020)=0, lcount(2021)=1.
    lcount = ((kyr-1)/4 - (kyr-1)/100 + (kyr-1)/400) - epoch_leaps
    ! Days from Jan 1 of the epoch to Jan 1 of kyr
    ydays = (kyr - epoch) * 365 + lcount
    ! Total: full days to start of this date + fractional day. Sum in double
    ! and round once, so the only error is the final single-precision step.
    yrday = real(real(ydays + doy - 1, kind=8) &
                 + real(khr, kind=8) / 24.0d0 &
                 + real(kmn, kind=8) / 1440.0d0 &
                 + real(ksc, kind=8) / 86400.0d0)
  end subroutine yrdy

  ! Autolauncher failsafe (ierror(30)): true when the drop about to happen is
  ! within 10 minutes of the drop two before the last one, i.e. 3 drops in
  ! 10 minutes. A 1.1 minute margin absorbs stations.dat minute rounding and
  ! single-precision yrday resolution, so drops exactly 5 min apart trip it.
  ! yrday1 — yearday of the drop two before the last (<= 0: no such drop)
  ! yrday2 — yearday now
  logical function drops_too_close(yrday1, yrday2)
    real, intent(in) :: yrday1, yrday2
    real, parameter :: window_min = 10.0
    real, parameter :: margin_min = 1.1
    drops_too_close = .false.
    if (yrday1 <= 0.0) return
    drops_too_close = (yrday2 - yrday1) * 1440.0 < window_min + margin_min
  end function drops_too_close

  ! True when the PC clock has moved forward into a new minute: drives the
  ! once-a-minute GPS average and DED write in sioloop. GPS seconds cannot:
  ! a stale or garbled sentence makes csec jump back and re-fire it.
  ! tprev, tnow — PC seconds of day from consecutive sioloop calls.
  ! Forward = wrapped step under 12 h, so midnight counts and a clock set
  ! back (time sync, DST) does not re-open a minute already closed.
  logical function pc_new_minute(tprev, tnow)
    real, intent(in) :: tprev, tnow
    real :: step
    step = modulo(tnow - tprev, 86400.0)
    pc_new_minute = step > 0.0 .and. step < 43200.0 .and. &
                    int(tnow / 60.0) /= int(tprev / 60.0)
  end function pc_new_minute

  ! Whole seconds elapsed on the monotonic system clock (system_clock counts)
  ! since cbase. cbase advances by exactly those seconds, so the remainder
  ! carries (calls every 0.99 s still count). Drives sioloop's itime: PC time
  ! of day jumps when the clock is changed (time sync, DST, ship's time zone),
  ! and a 1 s step back read as +86399 s.
  ! cbase < 0: no reference yet -> isec = 0, cbase = cnow.
  ! One counter wrap at cmax is handled; a step of over a day (or a counter
  ! going backwards) counts nothing and resyncs, as the old time-of-day
  ! difference never counted whole days either.
  subroutine clock_seconds(cnow, crate, cmax, cbase, isec)
    integer(kind=8), intent(in)    :: cnow, crate, cmax
    integer(kind=8), intent(inout) :: cbase
    integer,         intent(out)   :: isec
    integer(kind=8) :: dc, adv
    isec = 0
    if (cbase < 0 .or. crate <= 0) then
      cbase = cnow
      return
    end if
    if (cnow >= cbase) then
      dc = cnow - cbase
    else
      dc = (cmax - cbase) + cnow + 1          ! counter wrapped
    end if
    if (dc / crate > 86400_8) then
      cbase = cnow
      return
    end if
    isec = int(dc / crate)
    adv = int(isec, 8) * crate
    if (cbase <= cmax - adv) then
      cbase = cbase + adv
    else
      cbase = adv - (cmax - cbase) - 1        ! base wraps too
    end if
  end subroutine clock_seconds

  subroutine set_test_clock(iyr, imo, iday, ihr, imin, isec)
    integer, intent(in) :: iyr, imo, iday, ihr, imin, isec
    tclock_ymd = (/ iyr, imo, iday /)
    tclock_sec = int(ihr, 8) * 3600_8 + int(imin, 8) * 60_8 + int(isec, 8)
    tclock_on = .true.
  end subroutine set_test_clock

  subroutine advance_test_clock(nsec)
    integer, intent(in) :: nsec
    tclock_sec = tclock_sec + int(nsec, 8)
  end subroutine advance_test_clock

  subroutine use_real_clock()
    tclock_on = .false.
  end subroutine use_real_clock

  ! Steady clock behind sioloop's itime: system_clock, or the test clock at
  ! one count per simulated second (no wrap at midnight)
  subroutine pc_clock_count(count, rate, cmax)
    integer(kind=8), intent(out) :: count, rate, cmax
    if (tclock_on) then
      count = tclock_sec
      rate  = 1
      cmax  = huge(count)
    else
      call system_clock(count, rate, cmax)
    end if
  end subroutine pc_clock_count

  ! Current date and time as date_and_time's values (1 yr, 2 mon, 3 day,
  ! 5 hr, 6 min, 7 sec, 8 ms): the PC clock, or the test clock when on
  subroutine clock_now(idt)
    integer, intent(out) :: idt(8)
    integer(kind=8) :: sod
    integer :: y, m, d, ndays, k
    if (.not. tclock_on) then
      call date_and_time(values=idt)
      return
    end if
    sod   = modulo(tclock_sec, 86400_8)
    ndays = int((tclock_sec - sod) / 86400_8)
    y = tclock_ymd(1); m = tclock_ymd(2); d = tclock_ymd(3)
    do k = 1, abs(ndays)
      if (ndays > 0) then
        d = d + 1
        if (d > days_in_month(y, m)) then
          d = 1; m = m + 1
          if (m > 12) then; m = 1; y = y + 1; end if
        end if
      else
        d = d - 1
        if (d < 1) then
          m = m - 1
          if (m < 1) then; m = 12; y = y - 1; end if
          d = days_in_month(y, m)
        end if
      end if
    end do
    idt = 0
    idt(1) = y; idt(2) = m; idt(3) = d
    idt(5) = int(sod / 3600_8)
    idt(6) = int(mod(sod, 3600_8) / 60_8)
    idt(7) = int(mod(sod, 60_8))
  end subroutine clock_now

  integer function days_in_month(y, m)
    integer, intent(in) :: y, m
    integer, parameter :: mdays(12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
    days_in_month = mdays(m)
    if (m == 2 .and. ((mod(y, 4) == 0 .and. mod(y, 100) /= 0) .or. mod(y, 400) == 0)) &
      days_in_month = 29
  end function days_in_month

  ! Convert timetag (seconds in GPS week) to hours/minutes/seconds. siosub.for:2172.
  ! timetag — input seconds (may span multiple days)
  ! ihr,imin,isec — output time of day
  subroutine timetohms(timetag, ihr, imin, isec)
    real,    intent(in)  :: timetag
    integer, intent(out) :: ihr, imin, isec
    real :: x
    integer :: ix
    ix = int(timetag / 86400.0)
    x  = timetag
    if (ix > 0) x = timetag - real(ix * 86400)
    ihr  = int(x / 3600.0)
    imin = int((x - ihr * 3600.0) / 60.0)
    isec = int(x - ihr * 3600.0 - imin * 60.0)
  end subroutine timetohms

  ! Get current system time using DATE_AND_TIME. siosub.for:2384.
  ! ihr,imin,isec,ihsec — output hours, minutes, seconds, hundredths
  ! Note: original used integer*2; use integer(kind=2) to match DLL ABI.
  subroutine gettim(ihr, imin, isec, ihsec)
    integer(kind=2), intent(out) :: ihr, imin, isec, ihsec
    integer :: idt(8)
    call clock_now(idt)
    ihr   = int(idt(5), kind=2)
    imin  = int(idt(6), kind=2)
    isec  = int(idt(7), kind=2)
    ihsec = int(idt(8) / 10, kind=2)
  end subroutine gettim

  ! Get current system date using DATE_AND_TIME. siosub.for:2395.
  ! iyr,imo,iday — output year, month, day
  ! Note: original used integer*2; use integer(kind=2) to match DLL ABI.
  subroutine getdat(iyr, imo, iday)
    integer(kind=2), intent(out) :: iyr, imo, iday
    integer :: idt(8)
    call clock_now(idt)
    iyr  = int(idt(1), kind=2)
    imo  = int(idt(2), kind=2)
    iday = int(idt(3), kind=2)
  end subroutine getdat

end module sio_time
