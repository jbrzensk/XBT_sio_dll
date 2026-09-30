! src/sio_nav.f90
module sio_nav
  use sio_math,    only: dpolft, dp1vlu
  use sio_time,    only: gettmtg
  use sio_convert, only: dec2deg, deg2dec
  implicit none
  private
  public :: ave, newpos, xbteta, interp, planinfo, chkall, chkbuf, chkwrite
  public :: dr_elapsed, past_station, ave_consistent, check_time, check_fix
  public :: drop_countdown, postrust_at_begin

  ! Position trust (fix 3) when sioloop starts after siobegin, which Seas
  ! calls after every launch. siobegin reloads the last position from
  ! navtrk.dat/.nav and dead reckoning starts at once, while the GPS time and
  ! fix checks restart with no reference. Untrusted until the first fresh GPS
  ! average agrees with dead reckoning from the reloaded position (1-2 min).
  logical, parameter :: postrust_at_begin = .false.

  integer, parameter :: nerr = 50

contains

  ! Average GPS positions, compute speed and direction. siosub.for:17.
  ! ibuf       — number of GPS fixes in buffers (1..200)
  ! xlat(200)  — latitude buffer
  ! xlon(200)  — longitude buffer
  ! timetag(200) — GPS timetag buffer
  ! s10,d10    — output: averaged speed (kt), direction (deg true)
  ! timeave,vlat,vlon — in/out: last averaged position timetag/lat/lon
  ! ierror(38) — jptr counter; ierror(39) — icall counter
  ! iSIOSpeedAveMin — minutes of data to use for speed/dir averaging
  subroutine ave(ibuf, xlat, xlon, timetag, avlath, avlonh, &
                 s10, d10, timeave, vlat, vlon, ierror, &
                 ierr, iSIOSpeedAveMin, iw, ifile)
    integer,          intent(in)    :: ibuf, iw, ifile
    real,             intent(inout) :: xlat(200), xlon(200), timetag(200)
    real,             intent(inout) :: s10, d10, timeave, vlat, vlon
    character(len=1), intent(out)   :: avlath, avlonh
    integer,          intent(inout) :: ierror(nerr), iSIOSpeedAveMin
    integer,          intent(out)   :: ierr

    ! SAVE state for persistent averaging buffers
    real,    save :: tbuf(10), xltbuf(10), xlnbuf(10)
    real,    save :: ylatsav, ylonsav
    integer, save :: iSIOsave = 0, iSIOset = 0

    real    :: w(200), r(200), b(220)
    real    :: deg2rad, eps
    real    :: stime, svlat, svlon
    real    :: timeave1
    real    :: yfit, yp(1), yplat, yplon, yfitlat
    real    :: speed, dir
    real    :: x10, y10, t10, x
    real    :: ylatdif, ylondif
    real    :: prev_ylat, prev_ylon
    integer :: jptr, icall, ifirst, ndeg, iptr
    integer :: i, ideg
    real    :: xlatm, xlonm

    deg2rad = 3.141592654 / 180.0

    ! Guard against empty buffer (documented range is 1..200)
    if (ibuf < 1) then
      ierr = -1
      return
    end if

    ! Recover persistent counters from ierror
    jptr  = ierror(38)
    icall = ierror(39)
    ifirst = icall + 1

    ! Save incoming averaged position
    stime = timeave
    svlat = vlat
    svlon = vlon

    ! First call: zero out the averaging buffers
    if (ifirst == 1) then
      do i = 1, 10
        tbuf(i)   = 0.0
        xltbuf(i) = 0.0
        xlnbuf(i) = 0.0
      end do
    end if

    ! Debug output
    if (iw == 1 .and. ierror(33) /= 0) then
      write(ifile,*) 'in ave, iSIOSpeedAveMin=', iSIOSpeedAveMin
      write(ifile,*) 'jptr,icall,ifirst=', jptr, icall, ifirst
      write(ifile,*) 'stime,svlat,svlon=', stime, svlat, svlon
    end if

    w(1) = -1.0
    eps  = 0.0

    ! Saturday night rollover: GPS timetag wraps from ~604800 back to ~0
    if (timetag(1) > 600000.0 .and. timetag(ibuf) < 6000.0) then
      do i = 1, ibuf
        if (timetag(i) < 600.0) timetag(i) = timetag(i) + 604800.0
      end do
    end if

    if (iw == 1) then
      write(ifile,*) 'begin ave, ibuf=', ibuf, ' timeave=', timeave
    end if

    ! Compute mean timetag; subtract from each timetag for fitting
    timeave1 = 0.0
    do i = 1, ibuf
      timeave1 = timeave1 + timetag(i)
    end do
    timeave1 = timeave1 / real(ibuf)

    do i = 1, ibuf
      timetag(i) = timetag(i) - timeave1
    end do

    ! Handle 0/360 lon crossing in buffer
    if ( (xlon(1) > 359.0 .and. xlon(ibuf) < 1.0) .or. &
         (xlon(1) < 1.0   .and. xlon(ibuf) > 359.0) ) then
      do i = 1, ibuf
        if (xlon(i) < 1.0) xlon(i) = 360.0 + xlon(i)
      end do
    end if

    ! Polynomial fit for latitude
    call dpolft(ibuf, timetag, xlat, w, 1, ndeg, eps, r, ierr, b)
    if (ierr /= 1) return

    x = 0.0
    call dp1vlu(1, 1, x, yfit, yp, b)

    ! Compute lat speed (deg/sec → nm/hr)
    yp(1) = -99.0
    yplat = -99.0
    if ( (timeave1 - stime) < 150.0 .and. (timeave1 - stime) > 0.0 ) then
      yp(1) = (yfit - vlat) / (timeave1 - stime)
    end if
    if (yp(1) /= -99.0) yplat = 216000.0 * yp(1)
    yfitlat  = yfit
    prev_ylat = ylatsav    ! read before overwriting (C3: save prev for sanity check)
    ylatsav  = yfitlat
    vlat     = yfitlat
    call dec2deg('lat', ideg, xlatm, avlath, vlat)

    ! Polynomial fit for longitude
    w(1) = -1.0
    eps  = 0.0
    call dpolft(ibuf, timetag, xlon, w, 1, ndeg, eps, r, ierr, b)
    if (ierr /= 1) return

    ! Accept timeave1 as new timeave only after successful lon fit
    timeave = timeave1
    x = 0.0
    call dp1vlu(1, 1, x, yfit, yp, b)

    ! Fix up lon wrap artifact (subtract, not 360-yfit which would invert the sign)
    if (yfit > 360.0) yfit = yfit - 360.0

    yp(1) = -99.0
    yplon = -99.0
    if ( (timeave - stime) < 150.0 .and. (timeave - stime) > 0.0 ) then
      yp(1) = (yfit - vlon) / (timeave1 - stime)
    end if
    if (yp(1) /= -99.0) yplon = 216000.0 * cos(yfitlat * deg2rad) * yp(1)

    prev_ylon = ylonsav    ! read before overwriting (C3: save prev for sanity check)
    ylonsav   = yfit
    vlon      = yfit
    call dec2deg('lon', ideg, xlonm, avlonh, vlon)

    ! Compute instantaneous speed and direction from polynomial derivatives
    speed = -99.0
    dir   = -99.0
    if (yplat /= -99.0) then
      speed = sqrt(yplat**2 + yplon**2)
      if (speed == 0.0) then
        dir = acos(max(-1.0, min(1.0, yplon))) * (1.0 / deg2rad)
      else
        dir = acos(max(-1.0, min(1.0, yplon / speed))) * (1.0 / deg2rad)
      end if
      if (yplat < 0.0) dir = -dir
      dir = 90.0 - dir
      if (dir < 0.0) dir = dir + 360.0

      ! Adaptive averaging window: slow speed → shrink window
      if (speed < 8.0) then
        if (iSIOSpeedAveMin >= 5) then
          iSIOsave = iSIOSpeedAveMin
          iSIOset  = 2
          iSIOSpeedAveMin = 2
          jptr  = 0
          icall = 0
        end if
      else if (speed >= 8.0 .and. iSIOset == 2) then
        iSIOSpeedAveMin = iSIOsave
        iSIOset = 0
      end if
    end if

    if (iw == 1) write(ifile,*) 'current speed&dir=', speed, dir

    ! Check for large position jump (DR sanity) — compare with PREVIOUS fitted position
    if (ifirst /= 1) then
      ylatdif = prev_ylat - vlat
      ylondif = prev_ylon - vlon
      if ( abs(ylatdif) > 1.0 .or. &
           (abs(ylondif) > 1.0 .and. abs(ylondif) < 350.0) ) then
        ierror(12) = 1
      end if
    end if

    ! Reset if we crossed the 360 lon boundary
    if (jptr > 1 .and. abs(xlnbuf(1) - vlon) > 350.0) then
      ifirst = 1
      jptr   = 0
      icall  = 0
    end if

    ! Reset on Saturday night rollover
    if (jptr > 1 .and. abs(tbuf(1) - timeave) > 35000.0) then
      ifirst = 1
      jptr   = 0
      icall  = 0
      if (iw == 1) write(ifile,*) 'reset jptr in ave', jptr, ifirst, icall
    end if

    ! Clamp iSIOSpeedAveMin to valid range
    if (iSIOSpeedAveMin < 1 .or. iSIOSpeedAveMin > 10) iSIOSpeedAveMin = 10

    d10 = -99.0
    s10 = -99.0

    jptr  = jptr + 1
    icall = icall + 1

    ! Need at least 2 points in ring buffer before computing s10/d10
    if (jptr == 1) then
      tbuf(jptr)   = timeave
      xltbuf(jptr) = vlat
      xlnbuf(jptr) = vlon
      ierror(38)   = jptr
      ierror(39)   = icall
      return
    end if

    ! Wrap ring buffer pointer
    if (jptr >= iSIOSpeedAveMin + 1) jptr = 1
    iptr = 1
    if (icall >= iSIOSpeedAveMin + 1) iptr = jptr

    t10 = timeave - tbuf(iptr)

    if (t10 > 650.0 .or. t10 <= 0.0) then
      ! Gap too large or non-positive (rollover/reset) — fall back to instantaneous
      s10 = speed
      d10 = dir
    else
      x10 = 216000.0 * cos(vlat * deg2rad) * (vlon - xlnbuf(iptr)) / t10
      y10 = 216000.0 * (vlat - xltbuf(iptr)) / t10
      s10 = sqrt(x10*x10 + y10*y10)
      if (s10 == 0.0) then
        d10 = acos(max(-1.0, min(1.0, x10))) * (1.0 / deg2rad)
      else
        d10 = acos(max(-1.0, min(1.0, x10 / s10))) * (1.0 / deg2rad)
      end if
      if (y10 < 0.0) d10 = -d10
      d10 = 90.0 - d10
      if (d10 < 0.0) d10 = d10 + 360.0
    end if

    tbuf(jptr)   = timeave
    xltbuf(jptr) = vlat
    xlnbuf(jptr) = vlon

    ierror(38) = jptr
    ierror(39) = icall

    if (iw == 1) write(ifile,*) 's10,d10', s10, d10

  end subroutine ave

  ! Dead-reckon a new position from speed, direction, elapsed time. siosub.for:1955.
  subroutine newpos(speed, change, dir, vlat, vlat1, vlon1, aclath, &
                   ierrlev, ifile)
    real,             intent(in)    :: speed, change, dir, vlat
    real,             intent(inout) :: vlat1, vlon1
    character(len=1), intent(out)   :: aclath
    integer,          intent(in)    :: ierrlev, ifile

    real, parameter :: deg2rad = 3.141592654 / 180.0
    real :: speedsec, distnew, dxlatnm1, dxlonnm1, dxlat1, dxlon1, x

    aclath = 'N'
    if (vlat < 0.0) aclath = 'S'

    ! A stale or garbled GPS time gives change <= 0; never dead-reckon on it.
    if (change <= 0.0) return

    ! Convert knots → nm/sec
    speedsec = speed / 3600.0
    ! Distance travelled in 'change' seconds
    distnew  = change * speedsec

    if (ierrlev >= 6) write(ifile,*) ' newpos: distnew =', distnew

    ! Resolve into lat/lon components (nm)
    dxlatnm1 = distnew * cos(dir * deg2rad)
    dxlonnm1 = distnew * sin(dir * deg2rad)

    ! Convert nm to degrees
    dxlat1 = dxlatnm1 / 60.0
    x = cos(vlat * deg2rad)
    if (x == 0.0) then
      dxlon1 = dxlonnm1
    else
      dxlon1 = dxlonnm1 / (60.0 * x)
    end if

    ! Signed components carry the direction (cos/sin of dir); the old
    ! quadrant-plus-abs() form moved the ship forward even for negative time.
    vlat1 = vlat1 + dxlat1
    vlon1 = vlon1 + dxlon1

    ! Wrap longitude to [0, 360)
    if (vlon1 > 360.0) vlon1 = vlon1 - 360.0
    if (vlon1 < 0.0)   vlon1 = vlon1 + 360.0

    if (ierrlev >= 6) write(ifile,*) 'out newpos,vlat1,vlon1', vlat1, vlon1

  end subroutine newpos

  ! Sanity-check the GPS-derived dead-reckoning elapsed time against the PC
  ! clock. Stale buffered sentences and garbled time fields (9/8/26) must not
  ! move the DR position; when GPS and PC disagree, trust the PC clock.
  ! gps_change - DR seconds from GPS time (gpstime - timeave)
  ! itime      - PC-clock seconds counter now (sioloop's itime)
  ! itimeave   - itime at the last accepted GPS average (<0, or > itime after
  !              siobegin reset the counter: unknown)
  real function dr_elapsed(gps_change, itime, itimeave, iw, ifile)
    real,    intent(in) :: gps_change
    integer, intent(in) :: itime, itimeave, iw, ifile
    real, parameter :: tol = 60.0    ! seconds of GPS/PC disagreement allowed
    real :: pc_change

    dr_elapsed = gps_change
    if (itimeave >= 0 .and. itime >= itimeave) then
      pc_change = real(itime - itimeave)
      if (gps_change < 0.0 .or. abs(gps_change - pc_change) > tol) then
        if (iw == 1) write(ifile,*) 'DR time rejected: gps=', gps_change, &
                                    ' pc=', pc_change
        dr_elapsed = pc_change
      end if
    else if (gps_change < 0.0) then
      dr_elapsed = 0.0               ! no PC reference yet: never go backwards
    end if
  end function dr_elapsed

  ! Has the (dead-reckoned) ship position passed the XBT drop station?
  ! Geometry extracted unchanged from sioloop; sioloop arms the drop
  ! (stoptime/idsec2) when this is true.
  ! ispec1   - 1 lat-based plan, 0 lon-based plan
  ! iplandir - plan direction N=1, E=2, S=3, W=4 (anything else: never past)
  ! dir,speed,xmaxspd - ship course/speed and max allowed speed
  ! vlat1,vlon1 - ship position (0-360 E); xlat,xlon - station
  ! trusted  - position source is trusted (fix 3); false: never past
  logical function past_station(ispec1, iplandir, dir, speed, xmaxspd, &
                                vlat1, vlon1, xlat, xlon, trusted)
    integer, intent(in) :: ispec1, iplandir
    real,    intent(in) :: dir, speed, xmaxspd, vlat1, vlon1, xlat, xlon
    logical, intent(in) :: trusted
    real    :: dxlat, dxlon
    integer :: idirck

    past_station = .false.
    if (.not. trusted) return
    if (iplandir < 1 .or. iplandir > 4) return

    dxlat = vlat1 - xlat
    dxlon = vlon1 - xlon
    if (abs(dxlon) > 300.0) then
      if (dxlon > 300.0) then
        dxlon = (vlon1 - xlon) - 360.0
      else
        dxlon = 360.0 + (vlon1 - xlon)
      end if
    end if

    ! Ship direction check (ship circling): only trigger when the ship is
    ! heading the same way as the plan.
    idirck = 1
    if (ispec1 == 0) then
      if (dir >= 0.0 .and. dir <= 180.0 .and. iplandir == 4) idirck = 0
      if (dir <= 360.0 .and. dir >= 180.0 .and. iplandir == 2) idirck = 0
    else if (ispec1 == 1) then
      ! NOTE: can never be true (dir >= 270 and <= 90); kept as in sio.for
      if (dir >= 270.0 .and. dir <= 90.0 .and. iplandir == 3) idirck = 0
      if (dir <= 270.0 .and. dir >= 90.0 .and. iplandir == 1) idirck = 0
    end if
    if (idirck /= 1 .or. speed > xmaxspd) return

    select case (iplandir)
    case (1)
      past_station = dxlat >= 0.0
    case (2)
      past_station = dxlon >= 0.0 .and. abs(dxlon) <= 20.0
    case (3)
      past_station = dxlat <= 0.0
    case (4)
      past_station = dxlon <= 0.0 .and. abs(dxlon) <= 20.0
    end select
  end function past_station

  ! Drop countdown (settle delay). Called once per sioloop call.
  ! Armed on the first trusted fix past the station; stoptime is set once
  ! (it used to be re-set on every past call, so runsec > 0 never dropped).
  ! When it runs out the position is checked again: still past -> drop, not
  ! past -> cancel (a single bad position cannot drop a probe). Losing trust
  ! cancels it at once. runsec = 0 drops on the first trusted past fix.
  ! pastnow  - past_station(...) on this call (already false if untrusted)
  ! trusted  - position trusted on this call (fix 3)
  ! idsec2   - 1 while the countdown runs; stoptime - itime it runs out
  ! fire     - drop now
  ! event    - 0 none, 1 armed, 2 cancelled: trust lost,
  !            3 cancelled: not past when it ran out, 4 fire
  subroutine drop_countdown(pastnow, trusted, itime, runsec, idsec2, stoptime, &
                            fire, event)
    logical, intent(in)    :: pastnow, trusted
    integer, intent(in)    :: itime
    real,    intent(in)    :: runsec
    integer, intent(inout) :: idsec2
    real,    intent(inout) :: stoptime
    logical, intent(out)   :: fire
    integer, intent(out)   :: event
    fire = .false.
    event = 0
    if (idsec2 == 1 .and. .not. trusted) then
      idsec2 = 0
      stoptime = 9.9e9
      event = 2
    end if
    if (idsec2 /= 1 .and. pastnow) then
      stoptime = real(itime) + runsec
      idsec2 = 1
      event = 1
    end if
    if (idsec2 == 1 .and. real(itime) >= stoptime) then
      if (pastnow) then
        fire = .true.
        event = 4
      else
        idsec2 = 0
        stoptime = 9.9e9
        event = 3
      end if
    end if
  end subroutine drop_countdown

  ! Is a new GPS average consistent with dead reckoning from the previous
  ! one?  True when it lies within 0.5 nm of the position predicted from the
  ! previous average using the previous speed and course.  A jump (stale
  ! Garmin sentences, mashed strings, a bad fit after a dropout) fails, so
  ! sioloop will not arm a drop until a following average agrees with it.
  ! vlat_prev,vlon_prev,timeave_prev - previous average (lon 0-360 E, sec)
  ! speed,dir - speed (kt) and course used to dead reckon from it
  ! vlat_new,vlon_new,timeave_new    - new average
  logical function ave_consistent(vlat_prev, vlon_prev, timeave_prev, &
                                  speed, dir, vlat_new, vlon_new, timeave_new)
    real, intent(in) :: vlat_prev, vlon_prev, timeave_prev, speed, dir
    real, intent(in) :: vlat_new, vlon_new, timeave_new
    real, parameter :: tol_nm  = 0.5
    real, parameter :: deg2rad = 3.141592654 / 180.0
    real :: plat, plon, dlon, dn_nm, de_nm
    character(len=1) :: ahem

    plat = vlat_prev
    plon = vlon_prev
    call newpos(max(speed, 0.0), timeave_new - timeave_prev, dir, vlat_prev, &
                plat, plon, ahem, 0, 0)
    dlon = vlon_new - plon
    if (dlon >  180.0) dlon = dlon - 360.0
    if (dlon < -180.0) dlon = dlon + 360.0
    dn_nm = (vlat_new - plat) * 60.0
    de_nm = dlon * 60.0 * cos(vlat_new * deg2rad)
    ave_consistent = sqrt(dn_nm*dn_nm + de_nm*de_nm) <= tol_nm
  end function ave_consistent

  ! Check the GPS time Seas passes in (taken from the NMEA sentence) against
  ! the PC clock. Stale buffered Garmin sentences and mashed strings carry
  ! bad times (9/8/26); a bad time is replaced by the predicted one.
  ! ctag  - incoming GPS time, seconds of day
  ! itime - PC-clock seconds counter (sioloop's itime)
  ! tref,itref - last good GPS time and itime it arrived at (tref<0: none)
  ! tcand,itcand,ncand - run of rejected times that advance consistently with
  !         each other; 5 in a row re-sync the reference (e.g. PC clock step).
  !         A frozen or random time never does.
  ! ok    - ctag agrees with tref + PC elapsed within 30 s
  ! tgood - time to use: ctag if ok, else tref + PC elapsed (wrapped to a day)
  subroutine check_time(ctag, itime, tref, itref, tcand, itcand, ncand, ok, tgood)
    real,    intent(in)    :: ctag
    integer, intent(in)    :: itime
    real,    intent(inout) :: tref, tcand
    integer, intent(inout) :: itref, itcand, ncand
    logical, intent(out)   :: ok
    real,    intent(out)   :: tgood
    real,    parameter :: tol = 30.0     ! s, GPS time vs PC clock
    real,    parameter :: tolc = 10.0    ! s, between consecutive candidates
    integer, parameter :: nadopt = 5
    real :: dc

    ok = .true.
    tgood = ctag
    if (tref < 0.0 .or. itime < itref) then    ! no (valid) reference yet
      tref = ctag; itref = itime; ncand = 0
      return
    end if
    if (abs(day_diff(ctag, tref + real(itime - itref))) <= tol) then
      tref = ctag; itref = itime; ncand = 0
      return
    end if

    ok = .false.
    tgood = modulo(tref + real(itime - itref), 86400.0)
    dc = day_diff(ctag, tcand)
    if (ncand > 0 .and. itime > itcand .and. dc > 0.0 .and. &
        abs(dc - real(itime - itcand)) <= tolc) then
      ncand = ncand + 1
    else
      ncand = 1
    end if
    tcand = ctag; itcand = itime
    if (ncand >= nadopt) then
      tref = ctag; itref = itime; ncand = 0
      ok = .true.
      tgood = ctag
    end if
  end subroutine check_time

  ! Check an incoming GPS position (iupdate=1) for plausibility. Seas turns
  ! any latitude cardinal other than "N" into S (and any longitude cardinal
  ! other than "E" into W), and empty fields into 0, so partial strings
  ! arrive as valid-looking but far-away positions.
  ! clat,clon - incoming fix (decimal deg, lon 0-360 E); itime - PC seconds
  ! xmaxspd   - max believable ship speed (kt)
  ! alat,alon,ita - last accepted fix and the itime it arrived at (ita<0: none)
  ! clatc,clonc,itc,nc - run of rejected fixes consistent with each other;
  !         5 in a row re-sync the reference (e.g. the reference was bad)
  ! ok  - in range, not 0/0, and within xmaxspd*elapsed + 0.1 nm of the
  !       last accepted fix
  subroutine check_fix(clat, clon, itime, xmaxspd, alat, alon, ita, &
                       clatc, clonc, itc, nc, ok)
    real,    intent(in)    :: clat, clon, xmaxspd
    integer, intent(in)    :: itime
    real,    intent(inout) :: alat, alon, clatc, clonc
    integer, intent(inout) :: ita, itc, nc
    logical, intent(out)   :: ok
    integer, parameter :: nadopt = 5
    real,    parameter :: zero = 1.0e-4

    ok = .false.
    if (abs(clat) > 90.0 .or. clon < 0.0 .or. clon > 360.0) return
    if (abs(clat) < zero .and. (clon < zero .or. clon > 360.0 - zero)) return

    if (ita < 0 .or. itime < ita) then
      ok = .true.
    else if (fix_reachable(clat, clon, alat, alon, itime - ita, xmaxspd)) then
      ok = .true.
    else
      if (nc > 0 .and. itime > itc .and. &
          fix_reachable(clat, clon, clatc, clonc, itime - itc, xmaxspd)) then
        nc = nc + 1
      else
        nc = 1
      end if
      clatc = clat; clonc = clon; itc = itime
      if (nc >= nadopt) ok = .true.
    end if
    if (ok) then
      alat = clat; alon = clon; ita = itime; nc = 0
    end if
  end subroutine check_fix

  ! Could the ship have got from (alat,alon) to (clat,clon) in idt seconds?
  logical function fix_reachable(clat, clon, alat, alon, idt, xmaxspd)
    real,    intent(in) :: clat, clon, alat, alon, xmaxspd
    integer, intent(in) :: idt
    real, parameter :: slack_nm = 0.1    ! GPS noise / receiver offsets
    real, parameter :: deg2rad = 3.141592654 / 180.0
    real :: dlon, dn, de
    dlon = clon - alon
    if (dlon >  180.0) dlon = dlon - 360.0
    if (dlon < -180.0) dlon = dlon + 360.0
    dn = (clat - alat) * 60.0
    de = dlon * 60.0 * cos(0.5 * (clat + alat) * deg2rad)
    fix_reachable = sqrt(dn*dn + de*de) <= xmaxspd * real(max(idt, 0)) / 3600.0 + slack_nm
  end function fix_reachable

  ! a - b for times of day, wrapped into (-43200, 43200] seconds
  real function day_diff(a, b)
    real, intent(in) :: a, b
    day_diff = modulo(a - b + 43200.0, 86400.0) - 43200.0
    if (day_diff <= -43200.0) day_diff = day_diff + 86400.0
  end function day_diff

  ! Compute ETA (hours) to next drop positions. Ported from siosub.for:2233.
  ! Unused original params (ctime, xlat, xlon, ixhr, ixmin, ixsec) omitted.
  subroutine xbteta(xlatload, vlat1, vlon1, speed, dir, &
                    ispec, nplan, ierrlev, nlnchr, peta, ifile)
    real,    intent(in)  :: xlatload(12), vlat1, vlon1, speed, dir
    integer, intent(in)  :: ispec(12), nplan, ierrlev, nlnchr, ifile
    real,    intent(out) :: peta(12)

    real    :: deg2rad, dxlatld, dxlatnml, dxlonld, dxlonnm1
    real    :: distld, eta_val, x
    integer :: i, neta

    deg2rad = 3.141592654 / 180.0
    neta = 0
    do i = 1, 12
      peta(i) = 0.0
    end do

    if (ierrlev >= 6) then
      write(ifile,*) 'inside xbteta, ispec=', ispec
      write(ifile,*) 'vlat1=', vlat1, ' vlon1=', vlon1
      write(ifile,*) 'speed=', speed, ' dir=', dir
    end if

    do i = 1, nplan+1
      if (ispec(i) == 1) then
        dxlatld  = vlat1 - xlatload(i)
        dxlatnml = abs(dxlatld * 60.0)
        x = cos(dir * deg2rad)
        if (ierrlev >= 6) write(ifile,*) '  LAT,dxlatld=', dxlatld, &
                                          ' dxlatnml=', dxlatnml, ' x=', x
        if (x /= 0.0) then
          distld = abs(dxlatnml / x)
          if (ierrlev >= 6) write(ifile,*) ' x.ne.0,distld=', distld
        else
          distld = abs(dxlatnml)
          if (ierrlev >= 6) write(ifile,*) '  x.eq.0,distld=', distld
        end if
      else
        dxlonld = vlon1 - xlatload(i)
        if (ierrlev >= 6) write(ifile,*) '  dxlonld=', dxlonld
        if (abs(dxlonld) >= 300.0) then
          if (dxlonld > 300.0) then
            dxlonld = 360.0 - (vlon1 - xlatload(i))
            if (ierrlev >= 6) write(ifile,*) '  new dxlonld=', dxlonld
          else if (dxlonld < -300.0) then
            dxlonld = 360.0 + (vlon1 - xlatload(i))
            if (ierrlev >= 6) write(ifile,*) '  new dxlonld=', dxlonld
          end if
        end if
        dxlonnm1 = abs(dxlonld * (60.0 * cos(vlat1 * deg2rad)))
        x = sin(dir * deg2rad)
        if (ierrlev >= 6) write(ifile,*) '  LON', vlon1, xlatload(i), &
                                          ' dxlonld=', dxlonld, &
                                          ' dxlonnm1=', dxlonnm1, ' x=', x
        if (x /= 0.0) then
          distld = abs(dxlonnm1 / x)
        else
          distld = abs(dxlonnm1)
        end if
        if (ierrlev >= 6) write(ifile,*) '  distld=', distld
      end if

      if (speed == 0.0) then
        eta_val = distld
      else
        eta_val = distld / speed
      end if

      if (ispec(i) == 0) then
        if (x * dxlonld > 0.0) eta_val = -eta_val
      else
        if (x * dxlatld > 0.0) eta_val = -eta_val
      end if

      neta = neta + 1
      peta(neta) = eta_val
      if (ierrlev >= 6) write(ifile,*) '  peta(', neta, ')=', eta_val
    end do

  end subroutine xbteta

  ! Interpolate position at a given yearday. siosub.for:1843.
  ! Linear interpolation of lat/lon at time yrdrop between two known
  ! nav positions: (yrsav, zlat, zlon) [past] and (yrnav, xlat, xlon) [prev].
  subroutine interp(yrdrop, ylat, ylon, yrsav, zlat, zlon, yrnav, &
                    xlat, xlon)
    real, intent(in)  :: yrdrop, ylat, ylon, yrsav, zlat, zlon, yrnav
    real, intent(out) :: xlat, xlon

    real :: frac, yrdenom

    yrdenom = yrnav - yrsav
    if (yrdenom == 0.0) yrdenom = 0.001   ! avoid divide-by-zero

    frac = (yrdrop - yrsav) / yrdenom

    ! Interpolate latitude
    xlat = zlat + (ylat - zlat) * frac

    ! Handle 0/360 lon crossing before interpolating
    if (abs(ylon - zlon) > 300.0) then
      if (ylon > 300.0) then
        ! zlon is near 0, ylon is near 360 → adjust zlon up
        xlon = (zlon + 360.0) + (ylon - (zlon + 360.0)) * frac
      else
        ! zlon is near 360, ylon is near 0 → adjust ylon up
        xlon = zlon + ((ylon + 360.0) - zlon) * frac
      end if
    else
      xlon = zlon + (ylon - zlon) * frac
    end if

  end subroutine interp

  ! Extract plan position information. siosub.for:2019.
  ! Determines ispec (lat=1 or lon=0) and iplandir (N=1,E=2,S=3,W=4).
  subroutine planinfo(xlat, alath, xlat1, ahemi, aspec, ispec, &
                      iplandir, vlat1, vlon1)
    real,             intent(in)  :: xlat, xlat1, vlat1, vlon1
    character(len=1), intent(in)  :: alath, ahemi
    character(len=3), intent(out) :: aspec
    integer,          intent(out) :: ispec, iplandir

    ! vlat1, vlon1, ahemi: API-compatibility arguments retained for callers
    ! that pass ship position; not used in this implementation.
    ! The references below prevent unused-argument warnings from -Wextra.
    if (vlat1 + vlon1 < -1.0e30 .or. ahemi == char(0)) continue

    ! Determine whether plan is latitude-based or longitude-based
    if (alath == 'E' .or. alath == 'e' .or. &
        alath == 'W' .or. alath == 'w') then
      aspec = 'lon'
    else
      aspec = 'lat'
    end if

    ispec = 1
    if (aspec == 'lon') ispec = 0

    iplandir = 1   ! default: northward

    if (aspec == 'lat') then
      if (xlat > xlat1) then
        iplandir = 3   ! S
      else if (xlat < xlat1) then
        iplandir = 1   ! N
      end if
    else
      ! lon-based plan
      if (xlat > xlat1) then
        iplandir = 4   ! W
      else if (xlat < xlat1) then
        iplandir = 2   ! E
      end if
      ! Handle 0/360 wrap-around
      if (xlat > 350.0 .and. xlat1 < 10.0) iplandir = 4   ! W (crossing 0)
      if (xlat < 10.0  .and. xlat1 > 350.0) iplandir = 2  ! E (crossing 0)
    end if

  end subroutine planinfo

  ! Validate speed, direction, lat, lon are in physical range. siosub.for:455.
  subroutine chkall(xlat, xlon, speed, dir, ierr)
    real,    intent(in)  :: xlat, xlon, speed, dir
    integer, intent(out) :: ierr

    ierr = 0
    if (speed < 0.0  .or. speed > 99.99) ierr = 1
    if (dir   < 0.0  .or. dir   > 360.0) ierr = 1
    if (xlat  < -90.0 .or. xlat > 90.0)  ierr = 1
    if (xlon  < 0.0  .or. xlon  > 360.0) ierr = 1

  end subroutine chkall

  ! Validate GPS buffer values for outliers. siosub.for:482.
  ! Each entry is compared with the buffer's median time and position, so a
  ! bad last entry can no longer throw out (or be kept instead of) the good
  ! ones. Entries more than 400 s or 0.5 deg from the median are packed out;
  ! ierr=1 if fewer than 3 entries or the flagged ones are not a minority.
  ! Timetags are not unwrapped at midnight: ave cannot fit across it, so the
  ! minority side of midnight is packed out.
  subroutine chkbuf(ibuf, clatbuf, clonbuf, ctagbuf, ierr, iw, ifile)
    integer, intent(in)    :: iw, ifile
    integer, intent(inout) :: ibuf
    real,    intent(inout) :: clatbuf(200), clonbuf(200), ctagbuf(200)
    integer, intent(out)   :: ierr

    real, parameter :: tagtol = 400.0     ! seconds from median time
    real, parameter :: postol = 0.5       ! degrees from median lat / lon
    real, parameter :: deg2rad = 3.141592654 / 180.0
    real    :: clatbuf1(200), clonbuf1(200), ctagbuf1(200)
    real    :: dlon(200)
    real    :: tagmed, latmed, dlonmed, lonref, sx, sy
    integer :: imark(200)
    integer :: i, ibad, ibufnew

    ierr   = 0
    ibad   = 0
    ibufnew = 0

    if (ibuf < 3) then
      ierr = 1
      return
    end if

    do i = 1, ibuf
      imark(i) = 0
    end do

    ! Saturday-night rollover in timetags
    if (ctagbuf(1) > 604500.0) then
      if (iw == 1) write(ifile,*) 'Sat night? DOS:, ibuf=', ibuf
      do i = 1, ibuf
        if (ctagbuf(i) < 1000.0) ctagbuf(i) = ctagbuf(i) + 604800.0
      end do
    end if

    ! Median references. Longitude: offsets from the circular mean (safe
    ! across 0/360 and for E/W-flipped entries), then the median offset.
    tagmed = median_of(ctagbuf, ibuf)
    latmed = median_of(clatbuf, ibuf)
    sx = 0.0
    sy = 0.0
    do i = 1, ibuf
      sx = sx + cos(clonbuf(i) * deg2rad)
      sy = sy + sin(clonbuf(i) * deg2rad)
    end do
    lonref = clonbuf(1)
    if (sx*sx + sy*sy > 1.0e-6) lonref = atan2(sy, sx) / deg2rad
    do i = 1, ibuf
      dlon(i) = modulo(clonbuf(i) - lonref + 180.0, 360.0) - 180.0
    end do
    dlonmed = median_of(dlon, ibuf)

    ! Flag entries too far from the median time or position
    do i = 1, ibuf
      if ( abs(ctagbuf(i) - tagmed) > tagtol .or. &
           abs(clatbuf(i) - latmed) > postol .or. &
           abs(dlon(i) - dlonmed) > postol ) then
        ibad = ibad + 1
        imark(i) = 1
      end if
    end do

    if (2 * ibad >= ibuf) then
      if (iw == 1) write(ifile,*) 'chkbuf: rejected buffer,', ibad, ' of', ibuf, ' bad'
      ierr = 1
      return
    end if

    ! If no bad points, done
    if (ibad == 0) return
    if (iw == 1) write(ifile,*) 'chkbuf: dropped', ibad, ' of', ibuf

    ! Pack out the bad points
    do i = 1, ibuf
      if (imark(i) == 0) then
        ibufnew = ibufnew + 1
        ctagbuf1(ibufnew) = ctagbuf(i)
        clatbuf1(ibufnew) = clatbuf(i)
        clonbuf1(ibufnew) = clonbuf(i)
      end if
    end do

    ibuf = ibufnew
    do i = 1, ibuf
      ctagbuf(i) = ctagbuf1(i)
      clatbuf(i) = clatbuf1(i)
      clonbuf(i) = clonbuf1(i)
    end do

  end subroutine chkbuf

  ! Median of x(1:n) (the lower middle element when n is even)
  real function median_of(x, n)
    integer, intent(in) :: n
    real,    intent(in) :: x(n)
    real    :: y(n), v
    integer :: i, j
    y = x
    do i = 2, n
      v = y(i)
      j = i - 1
      do while (j >= 1)
        if (y(j) <= v) exit
        y(j + 1) = y(j)
        j = j - 1
      end do
      y(j + 1) = v
    end do
    median_of = y((n + 1) / 2)
  end function median_of

  ! Validate lat/lon before writing to nav file. siosub.for:810.
  subroutine chkwrite(ylat, ylon, ierr)
    real,    intent(in)  :: ylat, ylon
    integer, intent(out) :: ierr

    ierr = 0
    if (ylat < -90.0 .or. ylat > 90.0)  ierr = 1
    if (ylon < 0.0   .or. ylon > 360.0) ierr = 1

  end subroutine chkwrite

end module sio_nav
