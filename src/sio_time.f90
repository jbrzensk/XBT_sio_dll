! src/sio_time.f90
module sio_time
  implicit none
  private
  public :: compare, dayofw, gettmtg, findtime, yrdy, timetohms, gettim, getdat

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

  ! Convert year/month/day/hour/min/sec to days since Jan 1, 2000 (epoch=0).
  ! sio.for:3550 — modernized: accepts 4-digit year, returns epoch-based value.
  ! kkyr,kmo,kday,khr,kmn,ksc — input date/time (kkyr must be 4-digit year)
  ! yrday — output: fractional days since Jan 1, 2000 (monotonically increasing
  !          across years; differences in days, so multiply by 1440 for minutes)
  ! NOTE: callers that used to pass 2-digit years must normalize to 4-digit first
  !       (e.g. if yy < 87 then yyyy = 2000+yy, else yyyy = 1900+yy).
  subroutine yrdy(kkyr, kmo, kday, khr, kmn, ksc, yrday)
    integer, intent(in)  :: kkyr, kmo, kday, khr, kmn, ksc
    real,    intent(out) :: yrday
    integer :: days_in_month(12)
    integer :: i, leap, doy, ydays, lcount
    data days_in_month / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
    ! Gregorian leap year for this year
    leap = 0
    if (mod(kkyr, 4) == 0) then
      leap = 1
      if (mod(kkyr, 100) == 0 .and. mod(kkyr, 400) /= 0) leap = 0
    end if
    days_in_month(2) = 28 + leap
    ! Day of year (1-based)
    doy = kday
    do i = 1, kmo - 1
      doy = doy + days_in_month(i)
    end do
    ! Number of leap years in [2000, kkyr-1] using Gregorian formula.
    ! Verified: lcount(2024)=6, lcount(2000)=0, lcount(2001)=1.
    if (kkyr > 2000) then
      ! Leap years in [2000, kkyr-1]: formula verified for 2001..2100+
      ! The constant 484 = floor(1999/4) - floor(1999/100) + floor(1999/400) = 499-19+4
      lcount = (kkyr-1)/4 - (kkyr-1)/100 + (kkyr-1)/400 - 484
    else
      lcount = 0
    end if
    ! Days from Jan 1, 2000 to Jan 1 of kkyr
    ydays = (kkyr - 2000) * 365 + lcount
    ! Total: full days to start of this date + fractional day
    yrday = real(ydays) + real(doy - 1) &
            + real(khr)  / 24.0 &
            + real(kmn)  / 1440.0 &
            + real(ksc)  / 86400.0
  end subroutine yrdy

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
