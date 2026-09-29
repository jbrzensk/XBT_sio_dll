! src/sio_time.f90
module sio_time
  implicit none
  private
  public :: compare, dayofw, gettmtg, findtime, yrdy, timetohms, gettim, getdat
  public :: drops_too_close

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
    call date_and_time(values=idt)
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
    call date_and_time(values=idt)
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
    call date_and_time(values=idt)
    iyr  = int(idt(1), kind=2)
    imo  = int(idt(2), kind=2)
    iday = int(idt(3), kind=2)
  end subroutine getdat

end module sio_time
